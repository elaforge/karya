module Ui.Dump_test where
import Util.Test
import qualified Ui.Dump as Dump


test_parse = do
    let f = Dump.parse
    equal (f "x 10") (Right [("x", "10")])
    equal (f "x 10 sub ()") (Right [("x", "10")])
    equal (f "sub ()") (Right [])
    equal (f "sub (a b)") (Right [("sub.a", "b")])
    equal (f "sub (a (b c) e f)") (Right [("sub.a.b", "c"), ("sub.e", "f")])
    equal (f "x \"a b c\"") (Right [("x", "a b c")])
