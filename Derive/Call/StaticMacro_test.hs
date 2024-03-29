-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.StaticMacro_test where
import Util.Test
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.StaticMacro as StaticMacro
import Derive.Call.StaticMacro (Arg(..))
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Library as Library
import qualified Derive.Sig as Sig

import Global


test_generator :: Test
test_generator = do
    let run trans gen call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup trans gen)
                "" [(">", [(0, 1, call)])]
        setup trans gen = CallTest.with_note_generator "m" $ expect_right $
            StaticMacro.generator Module.prelude "m" mempty "doc" trans gen
        gen = make_call c_make_attributes
        trans = make_call c_add_attributes
    equal (run [trans [attr "a"]] (gen []) "m") (["+a"], [])
    equal (run [] (gen [attr "a"]) "m") (["+a"], [])
    strings_like (snd $ run [trans [Var]] (gen [Var]) "m +a")
        ["expected Attributes but got _"]
    equal (run [trans [Var]] (gen [Var]) "m +a +b")
        (["+a+b"], [])
    -- Does defaulting work for the sub-calls?
    equal (run [] (make_call c_gen [Var]) "m +a") (["+a"], [])
    equal (run [] (make_call c_gen [Var]) "gen-attr = +a | m") (["+a"], [])

c_gen :: Derive.Generator Derive.Note
c_gen = Library.generator $ Make.transform_notes Module.prelude "gen" mempty
    "doc" (Sig.required "attr" "set attr") $ \attrs -> Call.add_attributes attrs

test_transformer :: Test
test_transformer = do
    let run trans call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup trans) ""
                [(">", [(0, 1, call <> " | +z")])]
        setup trans = CallTest.with_note_transformer "m" $ expect_right $
            StaticMacro.transformer Module.prelude "m" mempty "doc" trans
        trans = make_call c_add_attributes
    equal (run [trans [attr "a"]] "m") (["+a+z"], [])
    strings_like (snd $ run [trans [Var], trans [Var]] "m +x")
        ["expected Attributes but got _"]
    equal (run [trans [Var], trans [Var]] "m +x +y")
        (["+x+y+z"], [])

attr :: Text -> Arg
attr = StaticMacro.literal . Attrs.attr

make_call :: call -> [Arg] -> StaticMacro.Call call
make_call call args = StaticMacro.Call call args

c_make_attributes :: Derive.Generator Derive.Note
c_make_attributes = Derive.generator Module.prelude "note"
    mempty "" $ Sig.call (Sig.many "attribute" "Set attributes.")
    $ \attrs args -> Call.add_attributes (mconcat attrs) (Call.placed_note args)

c_add_attributes :: Derive.Transformer Derive.Note
c_add_attributes = Derive.transformer Module.prelude "note-attributes"
    mempty "" $ Sig.callt (Sig.many "attribute" "Set attributes.")
    $ \attrs _args -> Call.add_attributes (mconcat attrs)
