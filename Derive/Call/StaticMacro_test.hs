-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.StaticMacro_test where
import Util.Test
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.StaticMacro as StaticMacro
import Derive.Call.StaticMacro (Arg(..))
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import Global


test_generator = do
    let run trans gen call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup trans gen)
                "" [(">", [(0, 1, call)])]
        setup trans gen = CallTest.with_note_generator "m" $
            expect_right "StaticMacro.generator" $
            StaticMacro.generator Module.prelude "m" mempty trans gen
        gen = make_call Note.c_note
        trans = make_call Note.c_note_attributes
    equal (run [trans [attr "a"]] (gen []) "m") (["+a"], [])
    equal (run [] (gen [attr "a"]) "m") (["+a"], [])
    strings_like (snd $ run [trans [Var]] (gen [Var]) "m +a")
        ["expected Instrument * got NotGiven"]
    equal (run [trans [Var]] (gen [Var]) "m +a +b")
        (["+a+b"], [])
    -- Does defaulting work for the sub-calls?
    equal (run [] (make_call c_gen [Var]) "m +a") (["+a"], [])
    equal (run [] (make_call c_gen [Var]) "gen-attr = +a | m") (["+a"], [])

c_gen :: Derive.Generator Derive.Note
c_gen = fst $ Make.transform_notes Module.prelude "gen" mempty
    "doc" (Sig.required "attr" "set attr") $ \attrs -> Call.add_attrs attrs

test_transformer = do
    let run trans call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup trans) ""
                [(">", [(0, 1, call <> " | +z")])]
        setup trans = CallTest.with_note_transformer "m" $
            expect_right "StaticMacro.transformer" $
            StaticMacro.transformer Module.prelude "m" mempty trans
        trans = make_call Note.c_note_attributes
    equal (run [trans [attr "a"]] "m") (["+a+z"], [])
    strings_like (snd $ run [trans [Var], trans [Var]] "m +x")
        ["expected Instrument * got NotGiven"]
    equal (run [trans [Var], trans [Var]] "m +x +y")
        (["+x+y+z"], [])

attr :: Text -> Arg
attr = Given . StaticMacro.Literal . Typecheck.to_val . Score.attr

make_call :: call -> [Arg] -> StaticMacro.Call call
make_call call args = StaticMacro.Call call args
