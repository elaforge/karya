-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Macro_test where
import Util.Test
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Macro as Macro
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import Global


test_generator = do
    let run expr call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup (make_expr expr)) ""
                [(">", [(0, 1, call)])]
        setup expr =
            CallTest.with_note_generator "m"
                (Macro.generator Module.prelude "m" mempty expr)
            <> CallTest.with_val_call "id" c_id
    equal (run [("+a", []), ("+b", [])] "m") (["+a+b"], [])
    equal (run [("n", [Right (attr "a")])] "m") (["+a"], [])
    equal (run [("n", [Left "var"])] "m +z") (["+z"], [])
    equal (run [("n", [Left "var"]), ("n", [Left "var"])] "m +x +y")
        (["+x+y"], [])

    let val_call arg = Right $ Macro.ValCall (make_call "id" [arg])
    equal (run [("n", [val_call (Right (attr "a"))])] "m") (["+a"], [])
    equal (run [("n", [val_call (Left "var")])] "m +x") (["+x"], [])

c_id :: Derive.ValCall
c_id = Derive.val_call "test" "id" mempty "doc" $
    Sig.call (Sig.required "a" "arg") $ \val _args ->
        return (val :: BaseTypes.Val)

test_transformer = do
    let run expr call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup (make_expr expr)) ""
                [(">", [(0, 1, call <> " | +z")])]
        setup = CallTest.with_note_transformer "m"
            . Macro.transformer Module.prelude "m" mempty
    equal (run [("n", [Right (attr "a")])] "m") (["+a+z"], [])
    equal (run [("n", [Left "var"]), ("n", [Left "var"])] "m +x +y")
        (["+x+y+z"], [])

attr :: Text -> Macro.Term a
attr = Macro.Literal . Typecheck.to_val . Score.attr

make_expr :: [(Text, [Either Text (Macro.Term Macro.Var)])]
    -> Macro.Expr Macro.Var
make_expr calls = Macro.Expr $ c :| cs
    where c : cs = map (uncurry make_call) calls

make_call :: Text -> [Either Text (Macro.Term Macro.Var)]
    -> Macro.Call Macro.Var
make_call sym args = Macro.Call (BaseTypes.Symbol sym)
    (map (first Macro.Var) args)
