-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PSignal_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Lists as Lists
import qualified Util.Segment as Segment
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Ui as Ui

import           Global
import           Types
import           Util.Test


test_at :: Test
test_at = do
    let f x = fmap showPitch . (`PSignal.at` x)
    equal (f 0 (PSignal.constant (twelve "4c"))) (Just ("60nn", "4c"))
    equal (showPitch $ Pitches.transposeC 7 (twelve "4c"))
        ("67nn", "4g")
    equal (f 0 (PSignal.constant (Pitches.transposeC 7 (twelve "4c"))))
        (Just ("67nn", "4g"))

test_interpolate :: Test
test_interpolate = do
    let c4 = twelve "4c"
        g4 = Pitches.transposeC 7 c4

    equal (showPitch g4) ("67nn", "4g")
    equal (showPitch $ DeriveT.interpolate
            (Segment.Sample 0 g4) (Segment.Sample 10 g4) 5)
        ("67nn", "4g")

test_transpose :: Test
test_transpose = do
    let c3 = twelve "3c"
        g3 = Pitches.transposeC 7 c3
    let e = Score.emptyEvent { Score.eventPitch = PSignal.constant g3 }
    equal (showPitch c3) ("48nn", "3c")
    equal (showPitch g3) ("55nn", "3g")
    equal (showPitch <$> Score.pitchAt 0 e) $ Just ("55nn", "3g")
    equal (showPitch <$> Score.initialPitch e) $ Just ("55nn", "3g")

test_apply_controls :: Test
test_apply_controls = do
    let f controls sig = toPairs $ PSignal.applyControls
            (mkcontrols controls) (mksignal sig)
        err = Left "bad transpose"
    equal (f [] [(0, 1)]) [(0, Right 1)]
    -- Non transposing control has no effect.
    equal (f [("normal", [(1, 1), (2, 2)])] [(0, 1)]) [(0, Right 1)]
    -- Transposition won't create pitch where none existed.
    equal (f [(trans1, [(0, 1)])] [(2, 2)]) [(2, Right 3)]
    equal (f [(trans1, [(0, 1)])] []) []

    equal (f [(trans1, [(0, 0), (2, 2)])] [(0, 1)])
        [(0, Right 1), (2, Right 3)]

    -- The first sample doesn't get a "from" sample (0, Right 1)
    equal (f [(trans1, [(0, 1)])] [(0, 1)]) [(0, Right 2)]
    -- Resample control.
    equal (f [(trans1, [(0, 0), (1, 1), (2, 0)])] [(0, 1)])
        [(0, Right 1), (1, Right 2), (2, Right 1)]
    -- Resample both controls.
    equal (f [(trans1, [(0, 0), (2, 2)]), (trans2, [(1, 1)])] [(0, 1)])
        [(0, Right 1), (1, Right 2), (1, Right 2.5), (2, Right 3.5)]
    -- Resample pitch.
    equal (f [(trans1, [(0, 1)])] [(0, 1), (1, 2), (2, 1)])
        [(0, Right 2), (1, Right 3), (2, Right 2)]
    -- Discontinuity, then out of range.
    equal (f [(trans1, [(1, 1), (2, 2), (3, 3)])] [(0, 2)])
        [(0, Right 2), (1, Right 2), (1, Right 3), (2, err), (3, err)]
    -- Control and pitch discontinuity.
    equal (f [(trans1, [(0, 1), (1, 1), (1, 2)])] [(0, 1)])
        [(0, Right 2), (1, Right 2), (1, Right 3)]
    equal (f [(trans1, [(0, 1)])] [(0, 1), (1, 1), (1, 2)])
        [(0, Right 2), (1, Right 2), (1, Right 3)]

test_apply_controls_nontransposing :: Test
test_apply_controls_nontransposing = do
    -- There are controls that influence the tuning but are not transposing.
    -- This just means they don't cause the pitch signal to resample.
    let f controls sig = Lists.dropDups id $
            map (second (controlsOf . PSignal.pitchConfig)) $
            PSignal.toPairs $
            PSignal.applyControls (mkcontrols controls) (mksignal sig)
        controlsOf (PSignal.PitchConfig _ cmap) = Map.toList cmap
    equal (f [("a", [(0, 220)]), (trans1, [(1, 0)])] [(1, 1)])
        [(1, [("a", 220), (trans1, 0)])]
    equal (f [("a", [(0, 220)]), (trans1, [(1, 0), (2, 1)])] [(1, 1)])
        [ (1, [("a", 220), (trans1, 0)])
        , (2, [("a", 220), (trans1, 1)])
        ]
    equal (f [("a", [(0, 220), (2, 440)])] [(0, 1), (1, 2)])
        [(0, [("a", 220)]), (1, [("a", 330)])]

test_apply_environ :: Test
test_apply_environ = do
    let f env1 env2 = toPairs $ PSignal.applyEnviron (mkenv env1) $
            mksignal (mkenv env2)
        toPairs = map (second (PSignal.pitchNote . PSignal.coerce))
            . PSignal.toPairs
        mksignal env = PSignal.fromPairs [(0, envPitch env)]
        mkenv val = Env.fromList [("tuning", Typecheck.toVal (val :: Text))]
    -- apply_environ wins over the environ already in the pitch.
    equal (f "a" "b") [(0, Right "a")]

envPitch :: Env.Environ -> PSignal.Pitch
envPitch env =
    PSignal.pitch defaultScale pitchNn pitchNote
        (PSignal.PitchConfig env mempty)
    where
    pitchNn _ = Right 42
    pitchNote (PSignal.PitchConfig env _) =
        Right $ Pitch.Note $ fromMaybe "?" $ Env.maybeVal "tuning" env

mkcontrols :: [(ScoreT.Control, [(Signal.X, Signal.Y)])]
    -> Map ScoreT.Control (ScoreT.Typed Signal.Control)
mkcontrols = Map.fromList . map (second (ScoreT.untyped . Signal.fromPairs))

twelveSignal :: [(RealTime, Pitch.Note)] -> PSignal.PSignal
twelveSignal = PSignal.fromPairs . map (second twelve)

mksignal :: [(RealTime, Pitch.NoteNumber)] -> PSignal.PSignal
mksignal = PSignal.fromPairs . map (second mkpitch)

defaultScale :: PSignal.Scale
defaultScale = PSignal.Scale "test" (Set.fromList [trans1, trans2])

mkpitch :: Pitch.NoteNumber -> PSignal.Pitch
mkpitch nn =
    PSignal.pitch defaultScale pitchNn (const $ Right $ Pitch.Note $ showt nn)
        mempty
    where
    pitchNn config
        | transposed >= 4 = Left (PSignal.PitchError "bad transpose")
        | otherwise = Right transposed
        where
        transposed = nn + get trans1 + get trans2 / 2
        get c = Pitch.NoteNumber $ Map.findWithDefault 0 c $
            PSignal.pitchControls config

trans1, trans2 :: ScoreT.Control
trans1 = "trans1"
trans2 = "trans2"

toPairs :: PSignal.PSignal -> [(RealTime, Either String Pitch.NoteNumber)]
toPairs =
    -- Segment.add_zero_transition can cause zero-width segments.  PSignal
    -- can't get rid of them since pitches are not in Eq, but they should be
    -- harmless.
    Lists.dropDups id
    . map (second (unerror . PSignal.pitchNn . PSignal.coerce))
        . PSignal.toPairs
    where unerror = either (\(PSignal.PitchError s) -> Left (untxt s)) Right

twelve :: Pitch.Note -> PSignal.RawPitch a
twelve = expectRight . DeriveTest.eval Ui.empty . Eval.evalNote Twelve.scale

showPitch :: PSignal.RawPitch a -> (Text, Text)
showPitch p =
    ( either showt pretty (PSignal.pitchNn (PSignal.coerce p))
    , either showt Pitch.noteText (PSignal.pitchNote (PSignal.coerce p))
    )
