-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PSignal_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Test
import qualified Derive.Env as Env
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


test_apply_controls = do
    let f controls psig = unsignal $ PSignal.apply_controls
            (mkcontrols controls) (mksignal psig)
        mkcontrols = Map.fromList . map (second (Score.untyped . Signal.signal))
        err = Left "bad transpose"
    -- No effect.
    equal (f [("normal", [(1, 1), (2, 2), (3, 3)])] [(0, 1)]) [(0, Right 1)]
    equal (f [(trans1, [(1, 1), (2, 2), (3, 3)])] [(0, 2)])
        [(0, Right 2), (1, Right 3), (2, err), (3, err)]
    equal (f [(trans1, [(0, 0), (1, -1), (2, 0)])] [(0, 2)])
        [(0, Right 2), (1, Right 1), (2, Right 2)]

    equal (f [(trans1, [(1, 1), (3, 3)]), (trans2, [(0, 0)])] [(0, 0)])
        [(0, Right 0), (1, Right 1), (3, Right 3)]
    equal (f [(trans1, [(1, 1), (3, 3)]), (trans2, [(0, 0), (2, 2)])] [(0, 0)])
        [(0, Right 0), (1, Right 1), (2, Right 1), (3, Right 3)]
    -- Transposition won't create pitch where none existed.
    equal (f [(trans1, [(0, 1)])] [(2, 2)]) [(2, Right 3)]

test_apply_environ = do
    let f env1 env2 = unsignal $ PSignal.apply_environ (mkenv env1) $
            mksignal (mkenv env2)
        unsignal = map (second (PSignal.pitch_note . PSignal.coerce))
            . PSignal.unsignal
        mksignal env = PSignal.signal [(0, env_pitch env)]
        mkenv val = Env.from_list [("tuning", Typecheck.to_val (val :: Text))]
    -- apply_environ wins over the environ already in the pitch.
    equal (f "a" "b") [(0, Right "a")]

env_pitch :: Env.Environ -> PSignal.Pitch
env_pitch env =
    PSignal.pitch default_scale pitch_nn pitch_note
        (PSignal.PitchConfig env mempty)
    where
    pitch_nn _ = Right 42
    pitch_note (PSignal.PitchConfig env _) =
        Right $ Pitch.Note $ fromMaybe "?" $ Env.maybe_val "tuning" env


mksignal :: [(RealTime, Pitch.NoteNumber)] -> PSignal.PSignal
mksignal = PSignal.signal . map (second mkpitch)

default_scale :: PSignal.Scale
default_scale = PSignal.Scale "test" (Set.fromList [trans1, trans2])

mkpitch :: Pitch.NoteNumber -> PSignal.Pitch
mkpitch nn =
    PSignal.pitch default_scale pitch_nn (const $ Right $ Pitch.Note $ showt nn)
        mempty
    where
    pitch_nn config
        | nn + t >= 4 = Left (PSignal.PitchError "bad transpose")
        | otherwise = Right (nn + t)
        where
        t = Pitch.NoteNumber $ Map.findWithDefault 0 trans1
            (PSignal.pitch_controls config)

trans1, trans2 :: Score.Control
trans1 = "trans1"
trans2 = "trans2"

unsignal :: PSignal.PSignal -> [(RealTime, Either String Pitch.NoteNumber)]
unsignal = map (second (unerror . PSignal.pitch_nn . PSignal.coerce))
        . PSignal.unsignal
    where unerror = either (\(PSignal.PitchError s) -> Left (untxt s)) Right
