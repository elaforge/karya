-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Typecheck_test where
import qualified Control.Monad as Monad

import qualified Util.Seq as Seq
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_typecheck :: Test
test_typecheck = do
    let f :: Typecheck.Typecheck a => Text -> Either Text (Maybe a)
        f = fmap Typecheck.from_val_simple . Parse.parse_val
    equal (f "1nn") (Right (Just (Pitch.NoteNumber 1)))
    equal (f "1nn") (Right (Just (Pitch.Nn 1)))
    equal (f "1") (Right (Just (Typecheck.DefaultDiatonic (Pitch.Diatonic 1))))
    equal (f "1") (Right (Just (Pitch.Chromatic 1)))
    equal (f "1c")
        (Right (Just (Typecheck.DefaultDiatonic (Pitch.Chromatic 1))))
    equal (f "1nn") (Right (Just (Typecheck.DefaultDiatonic (Pitch.Nn 1))))

test_coerce_numeric :: Test
test_coerce_numeric = do
    let d = Proxy @Double
    equal (run_type d [("c", [(0, 0, "3")])] "%c")
        (Just "3", [])
    -- This winds up at resolve_function, which then gets evaluated at event
    -- time.
    equal (run_type d [("c", [(0, 0, "3"), (1, 0, "4")])] "%c")
        (Just "4", [])
    -- This winds up being defaulted direcly to the signal, and evaluated at
    -- event time.
    equal (run_type d [("capture-val", [(0, 0, "3"), (1, 0, "4")])] "_")
        (Just "4", [])
    -- Same, without the defaulting.
    equal (run_type d [] "(signal 0 3 1 4)") (Just "4", [])

test_coerce_nn :: Test
test_coerce_nn = do
    -- Various things can be coerced to numeric types.
    let nn = Proxy :: Proxy Pitch.NoteNumber
    equal (run_type nn [] "42") (Just "42nn", [])
    equal (run_type nn [] "42nn") (Just "42nn", [])
    strings_like (snd $ run_type nn [] "42c")
        ["expected Maybe Signal (NN) but got Signal (Transposition"]

    -- TODO need to evaluate a pitch in Typecheck for this.
    -- -- coerce VPitch to NoteNumber
    -- equal (run_type nn [] "(4c)") (Just "60nn", [])
    -- -- pitch controls can be coerced to pitch
    -- equal (run_type nn [("*", [(0, 0, "4c")])] "#") (Just "60nn", [])

test_coerce_pitch :: Test
test_coerce_pitch = do
    let pitch = Proxy :: Proxy PSignal.Pitch
    equal (run_type pitch [] "(4c)") (Just "<pitch: 60nn,4c(twelve)>", [])
    equal (run_type pitch [("*", [(0, 0, "4c")])] "#")
        (Just "<pitch: 60nn,4c(twelve)>", [])
    -- It gets the default pitch but there's no pitch signal there.
    strings_like (snd $ run_type pitch [] "#")
        [ "expected Maybe Pitch but couldn't convert PControlRef #:\
          \ no pitch at 1s"
        ]
    strings_like (snd $ run_type pitch [] "#x")
        ["named pitch not found and no default"]
    strings_like (snd $ run_type pitch [] "(# x)")
        ["named pitch not found and no default"]
    equal (run_type pitch [] "(# x (4c))")
        (Just "<pitch: 60nn,4c(twelve)>", [])

test_coerce_control :: Test
test_coerce_control = do
    let double = Proxy :: Proxy Double
    -- controls can be coerced to numbers
    equal (run_type double [("c", [(0, 0, "42")])] "%c") (Just "42", [])
    equal (run_type double [("c", [(0, 0, "42"), (2, 0, "i 43")])] "%c")
        (Just "42.5", [])

test_resolve_signal :: Test
test_resolve_signal = do
    let f with ref = DeriveTest.eval Ui.empty
            (with (Typecheck.resolve_signal ref))
    let mksig = ScoreT.untyped . Signal.constant
    let ref_sig c sig = DeriveT.Ref c (Just (mksig sig)) :: DeriveT.ControlRef
    let ref c = DeriveT.Ref c Nothing :: DeriveT.ControlRef
    let withc = Derive.with_constant_control "c" 2
    right_equal (f id (ref "c")) Nothing
    right_equal (f id (ref_sig "c" 42)) $ Just (mksig 42)
    right_equal (f withc (ref "c")) $
        Just (mksig 2)
    right_equal (f withc (ref_sig "c" 42)) $ Just (mksig 2)
    -- multiple levels of refs
    -- An intermediate lookup failure throws instead of returning Nothing,
    -- which is irregular, but I think I don't mind it for now, since something
    -- fishy may be going on.
    left_like
        (f (Derive.with_val "a" (ref "b") . withc) (ref "a")) $
        "control not found: %b"
    -- a->b->c works even though I'm not sure if it should.
    right_equal
        (f (Derive.with_val "a" (ref "b") . Derive.with_val "b" (ref "c")
                . withc)
            (ref "a")) $
        Just (mksig 2)
    -- This will loop, no cycle detection!  TODO
    -- right_equal
    --     (f (Derive.with_val "a" (ref "b") . Derive.with_val "b" (ref "a"))
    --         (ref "a")) $
    --     Just (mksig 2)

run_type :: forall arg. (ShowVal.ShowVal arg, Typecheck.Typecheck arg)
    => Proxy arg -> [UiTest.TrackSpec] -> Text -> (Maybe Text, [Text])
run_type _ = capture (ShowVal.show_val :: arg -> Text)

capture :: Typecheck.Typecheck arg => (arg -> Text) -> [UiTest.TrackSpec]
    -> Text -> (Maybe Text, [Text])
capture show_val controls arg =
    extract $ DeriveTest.derive_tracks_setup
        (CallTest.with_note_generator "capture" c_capture) ""
        (controls ++ [(">", [(1, 1, "capture " <> arg)])])
    where
    from_str (DeriveT.VStr (Expr.Str str)) = Just str
    from_str _ = Nothing
    extract = first (from_str <=< Monad.join . Seq.head)
        . DeriveTest.extract (Env.lookup "capture" . Score.event_environ)
    c_capture :: Derive.Generator Derive.Note
    c_capture = Derive.generator CallTest.module_ "capture" mempty "" $
        Sig.call (Sig.required "val" "Val.") $ \val _args -> do
            -- srate <- Call.get_srate
            -- srate <- Derive.get_val EnvKey.srate
            -- Debug.traceM "srate" (srate :: Double)
            Derive.with_val "capture" (maybe "?" show_val val) Call.note
