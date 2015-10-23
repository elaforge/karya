-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.TextUtil_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.TextUtil as TextUtil
import Global



test_extractDelimited = do
    let f = TextUtil.extractDelimited False '`'
    equal (f "a `b` c") [("a ", Just "b"), (" c", Nothing)]
    equal (f "`b` c") [("", Just "b"), (" c", Nothing)]
    equal (f "`a\\`a` b") [("", Just "a`a"), (" b", Nothing)]
    equal (f "a `b b` c") [("a `", Nothing), ("b b` c", Nothing)]
    equal (f "a\\`b") [("a`b", Nothing)]
    equal (f "a\\``x`\\`") [("a`", Just "x"), ("`", Nothing)]

test_interpolate = do
    let f = first untxt
            . TextUtil.interpolate "hi ${var1} there ${var2}: ${var1}"
            . Map.fromList
    left_like (f []) "not in variables: var1, var2"
    equal (f [("var1", "a"), ("var2", "b")]) (Right "hi a there b: a")
    left_like (f [("var1", "a"), ("var2", "b"), ("var3", "c")])
        "not in template: var3"
