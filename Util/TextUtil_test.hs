module Util.TextUtil_test where
import Util.Test
import qualified Util.TextUtil as TextUtil


test_extractDelimited = do
    let f = TextUtil.extractDelimited False '`'
    equal (f "a `b` c") [("a ", Just "b"), (" c", Nothing)]
    equal (f "`b` c") [("", Just "b"), (" c", Nothing)]
    equal (f "`a\\`a` b") [("", Just "a`a"), (" b", Nothing)]
    equal (f "a `b b` c") [("a `", Nothing), ("b b` c", Nothing)]
    equal (f "a\\`b") [("a`b", Nothing)]
    equal (f "a\\``x`\\`") [("a`", Just "x"), ("`", Nothing)]
