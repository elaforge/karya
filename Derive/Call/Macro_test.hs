-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Macro_test where
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Macro as Macro
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Ky as Ky
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import           Global
import           Util.Test


test_generator :: Test
test_generator = do
    let run expr call = DeriveTest.extract DeriveTest.eAttributes $
            DeriveTest.deriveTracksSetup (setup (makeExpr expr)) ""
                [(">", [(0, 1, call)])]
        setup expr = withId <> CallTest.withNoteGenerator "m"
            (Macro.generator Module.prelude "m" mempty "doc" expr)
    equal (run [("+a", []), ("+b", [])] "m") (["+a+b"], [])
    equal (run [("attr", [attr "a"])] "m") (["+a"], [])
    equal (run [("attr", [var "var"])] "m +z") (["+z"], [])
    equal (run [("attr", [var "var"]), ("attr", [var "var"])] "m +x +y")
        (["+x+y"], [])

    let valCall arg = Ky.ValCall (Ky.Call "id" [arg])
    equal (run [("attr", [valCall (attr "a")])] "m") (["+a"], [])
    equal (run [("attr", [valCall (var "var")])] "m +x") (["+x"], [])

test_val :: Test
test_val = do
    let run call = DeriveTest.extract DeriveTest.eAttributes $
            DeriveTest.deriveTracksSetup setup ""
                [(">", [(0, 1, call)])]
        setup = withId <> CallTest.withValCall "m"
            (Macro.valCall Module.prelude "m" mempty "doc" expr)
        expr = Ky.Call "id" [var "arg"]
    equal (run "attr (m +a)") (["+a"], [])

withId :: DeriveTest.Setup
withId = CallTest.withValCall "id" cId

cId :: Derive.ValCall
cId = Derive.valCall "test" "id" mempty "doc" $
    Sig.call (Sig.required "a" "arg") $ \val _args ->
        pure (val :: DeriveT.Val)

test_transformer :: Test
test_transformer = do
    let run expr call = DeriveTest.extract DeriveTest.eAttributes $
            DeriveTest.deriveTracksSetup (setup (makeExpr expr)) ""
                [(">", [(0, 1, call <> " | +z")])]
        setup = CallTest.withNoteTransformer "m"
            . Macro.transformer Module.prelude "m" mempty "doc"
    equal (run [("attr", [attr "a"])] "m") (["+a+z"], [])
    equal (run [("attr", [var "var"]), ("attr", [var "var"])] "m +x +y")
        (["+x+y+z"], [])

var :: Text -> Ky.Term
var = Ky.VarTerm . Ky.Var

attr :: Text -> Ky.Term
attr = Ky.Literal . Typecheck.toVal . Attrs.attr

makeExpr :: [(Expr.Symbol, [Ky.Term])] -> Ky.Expr
makeExpr calls = Ky.Expr $ c :| cs
    where c : cs = map (uncurry Ky.Call) calls
