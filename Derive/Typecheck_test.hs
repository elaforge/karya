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
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


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

test_coerce_nn = do
    -- Various things can be coerced to numeric types.
    let nn = Proxy :: Proxy Pitch.NoteNumber
    equal (run_type nn [] "42") (Just "42nn", [])
    equal (run_type nn [] "42nn") (Just "42nn", [])
    strings_like (snd $ run_type nn [] "42c")
        ["expected Num (NN) but got Num (Transposition"]

    -- TODO need to evaluate a pitch in Typecheck for this.
    -- -- coerce VPitch to NoteNumber
    -- equal (run_type nn [] "(4c)") (Just "60nn", [])
    -- -- pitch controls can be coerced to pitch
    -- equal (run_type nn [("*", [(0, 0, "4c")])] "#") (Just "60nn", [])

test_coerce_pitch = do
    let pitch = Proxy :: Proxy PSignal.Pitch
    equal (run_type pitch [] "(4c)") (Just "<pitch: 60nn,4c(twelve)>", [])
    equal (run_type pitch [("*", [(0, 0, "4c")])] "#")
        (Just "<pitch: 60nn,4c(twelve)>", [])
    strings_like (snd $ run_type pitch [] "#")
        ["pitch not found and no default given"]
    equal (run_type pitch [] "(# '' (4c))")
        (Just "<pitch: 60nn,4c(twelve)>", [])


test_coerce_control = do
    let double = Proxy :: Proxy Double
    -- controls can be coerced to numbers
    equal (run_type double [("c", [(0, 0, "42")])] "%c") (Just "42", [])
    equal (run_type double [("c", [(0, 0, "42"), (2, 0, "i 43")])] "%c")
        (Just "42.5", [])


run_type :: forall arg. (ShowVal.ShowVal arg, Typecheck.Typecheck arg)
    => Proxy arg -> [UiTest.TrackSpec] -> Text -> (Maybe Text, [Text])
run_type _ controls arg =
    extract $ DeriveTest.derive_tracks_setup
        (CallTest.with_note_generator "capture" c_capture) ""
        (controls ++ [(">", [(1, 1, "capture " <> arg)])])
    where
    from_str (DeriveT.VStr (Expr.Str str)) = Just str
    from_str _ = Nothing
    extract = first (from_str <=< Monad.join . Seq.head)
        . DeriveTest.extract (Env.lookup "capture" . Score.event_environ)
    show_val :: arg -> Text
    show_val = ShowVal.show_val
    c_capture :: Derive.Generator Derive.Note
    c_capture = Derive.generator CallTest.module_ "capture" mempty "" $
        Sig.call (Sig.required "val" "Val.") $ \val _args ->
            Derive.with_val "capture" (show_val val) Call.note
