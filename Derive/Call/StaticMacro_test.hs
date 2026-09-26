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
    let run trans gen call = DeriveTest.extract DeriveTest.eAttributes $
            DeriveTest.deriveTracksSetup (setup trans gen)
                "" [(">", [(0, 1, call)])]
        setup trans gen = CallTest.withNoteGenerator "m" $ expectRight $
            StaticMacro.generator Module.prelude "m" mempty "doc" trans gen
        gen = makeCall cMakeAttributes
        trans = makeCall cAddAttributes
    equal (run [trans [attr "a"]] (gen []) "m") (["+a"], [])
    equal (run [] (gen [attr "a"]) "m") (["+a"], [])
    stringsLike (snd $ run [trans [Var]] (gen [Var]) "m +a")
        ["expected Attributes but got _"]
    equal (run [trans [Var]] (gen [Var]) "m +a +b")
        (["+a+b"], [])
    -- Does defaulting work for the sub-calls?
    equal (run [] (makeCall cGen [Var]) "m +a") (["+a"], [])
    equal (run [] (makeCall cGen [Var]) "gen-attr = +a | m") (["+a"], [])

cGen :: Derive.Generator Derive.Note
cGen = Library.generator $ Make.transformNotes Module.prelude "gen" mempty
    "doc" (Sig.required "attr" "set attr") $ \attrs -> Call.addAttributes attrs

test_transformer :: Test
test_transformer = do
    let run trans call = DeriveTest.extract DeriveTest.eAttributes $
            DeriveTest.deriveTracksSetup (setup trans) ""
                [(">", [(0, 1, call <> " | +z")])]
        setup trans = CallTest.withNoteTransformer "m" $ expectRight $
            StaticMacro.transformer Module.prelude "m" mempty "doc" trans
        trans = makeCall cAddAttributes
    equal (run [trans [attr "a"]] "m") (["+a+z"], [])
    stringsLike (snd $ run [trans [Var], trans [Var]] "m +x")
        ["expected Attributes but got _"]
    equal (run [trans [Var], trans [Var]] "m +x +y")
        (["+x+y+z"], [])

attr :: Text -> Arg
attr = StaticMacro.literal . Attrs.attr

makeCall :: call -> [Arg] -> StaticMacro.Call call
makeCall call args = StaticMacro.Call call args

cMakeAttributes :: Derive.Generator Derive.Note
cMakeAttributes = Derive.generator Module.prelude "note"
    mempty "" $ Sig.call (Sig.many "attribute" "Set attributes.")
    $ \attrs args -> Call.addAttributes (mconcat attrs) (Call.placedNote args)

cAddAttributes :: Derive.Transformer Derive.Note
cAddAttributes = Derive.transformer Module.prelude "note-attributes"
    mempty "" $ Sig.callt (Sig.many "attribute" "Set attributes.")
    $ \attrs _args -> Call.addAttributes (mconcat attrs)
