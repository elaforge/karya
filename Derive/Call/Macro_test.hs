module Derive.Call.Macro_test where
import Util.Test
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Macro as Macro
import qualified Derive.Call.Module as Module
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Typecheck as Typecheck

import Global


test_generator = do
    let run expr call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup (make_expr expr)) ""
                [(">", [(0, 1, call)])]
        setup = CallTest.with_note_generator "m"
            . Macro.generator Module.prelude "m" mempty
    equal (run [("+a", []), ("+b", [])] "m") (["+a+b"], [])
    equal (run [("n", [Right (Score.attr "a")])] "m") (["+a"], [])
    equal (run [("n", [Left "var"])] "m +z") (["+z"], [])
    equal (run [("n", [Left "var"]), ("n", [Left "var"])] "m +x +y")
        (["+x+y"], [])

test_transformer = do
    let run expr call = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup (setup (make_expr expr)) ""
                [(">", [(0, 1, call <> " | +z")])]
        setup = CallTest.with_note_transformer "m"
            . Macro.transformer Module.prelude "m" mempty
    equal (run [("n", [Right (Score.attr "a")])] "m") (["+a+z"], [])
    equal (run [("n", [Left "var"]), ("n", [Left "var"])] "m +x +y")
        (["+x+y+z"], [])

make_expr :: [(Text, [Either Text Score.Attributes])] -> Macro.Expr Macro.Var
make_expr calls = Macro.Expr $ c :| cs
    where
    c : cs = map make calls
    make (sym, args) = Macro.Call (BaseTypes.Symbol sym)
        (map (Macro.Var *** BaseTypes.Literal . Typecheck.to_val) args)
