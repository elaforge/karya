-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Macro_test where
import Util.Test
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Macro as Macro
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import Global


test_generator = do
    let run expr call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup (make_expr expr)) ""
                [(">", [(0, 1, call)])]
        setup expr = with_id <> CallTest.with_note_generator "m"
            (Macro.generator Module.prelude "m" mempty "doc" expr)
    equal (run [("+a", []), ("+b", [])] "m") (["+a+b"], [])
    equal (run [("n", [attr "a"])] "m") (["+a"], [])
    equal (run [("n", [var "var"])] "m +z") (["+z"], [])
    equal (run [("n", [var "var"]), ("n", [var "var"])] "m +x +y")
        (["+x+y"], [])

    let val_call arg = Parse.ValCall (Parse.Call "id" [arg])
    equal (run [("n", [val_call (attr "a")])] "m") (["+a"], [])
    equal (run [("n", [val_call (var "var")])] "m +x") (["+x"], [])

test_val = do
    let run call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup setup ""
                [(">", [(0, 1, call)])]
        setup = with_id <> CallTest.with_val_call "m"
            (Macro.val_call Module.prelude "m" mempty "doc" expr)
        expr = Parse.Call "id" [var "arg"]
    equal (run "n (m +a)") (["+a"], [])

with_id :: DeriveTest.Setup
with_id = CallTest.with_val_call "id" c_id

c_id :: Derive.ValCall
c_id = Derive.val_call "test" "id" mempty "doc" $
    Sig.call (Sig.required "a" "arg") $ \val _args ->
        return (val :: BaseTypes.Val)

test_transformer = do
    let run expr call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup (make_expr expr)) ""
                [(">", [(0, 1, call <> " | +z")])]
        setup = CallTest.with_note_transformer "m"
            . Macro.transformer Module.prelude "m" mempty "doc"
    equal (run [("n", [attr "a"])] "m") (["+a+z"], [])
    equal (run [("n", [var "var"]), ("n", [var "var"])] "m +x +y")
        (["+x+y+z"], [])

var :: Text -> Parse.Term
var = Parse.VarTerm . Parse.Var

attr :: Text -> Parse.Term
attr = Parse.Literal . Typecheck.to_val . Attrs.attr

make_expr :: [(Expr.Symbol, [Parse.Term])] -> Parse.Expr
make_expr calls = Parse.Expr $ c :| cs
    where c : cs = map (uncurry Parse.Call) calls
