-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Texts_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Texts as Texts


test_replaceMany :: Test
test_replaceMany = do
    let f = Texts.replaceMany
    equal (f [("in", "out"), ("out", "in")] "hi in there out") "hi out there in"

test_extractDelimited :: Test
test_extractDelimited = do
    let f = Texts.extractDelimited False '`'
    equal (f "a `b` c") [("a ", Just "b"), (" c", Nothing)]
    equal (f "`b` c") [("", Just "b"), (" c", Nothing)]
    equal (f "`a\\`a` b") [("", Just "a`a"), (" b", Nothing)]
    equal (f "a `b b` c") [("a `", Nothing), ("b b` c", Nothing)]
    equal (f "a\\`b") [("a`b", Nothing)]
    equal (f "a\\``x`\\`") [("a`", Just "x"), ("`", Nothing)]

test_interpolate :: Test
test_interpolate = do
    let f = Texts.interpolate "hi ${var1} there ${var2}: ${var1}"
            . Map.fromList
    left_like (f []) "template variable not given: var1, var2"
    equal (f [("var1", "a"), ("var2", "b")]) (Right "hi a there b: a")
    left_like (f [("var1", "a"), ("var2", "b"), ("var3", "c")])
        "not in template: var3"

test_columns :: Test
test_columns = do
    let f = Texts.columns
    equal (f 1 [["a1", "a"], ["b", "c"]])
        [ "a1 a"
        , "b  c"
        ]
    equal (f 2 [["a1", "a"], ["b", "c"]])
        [ "a1  a"
        , "b   c"
        ]
    equal (f 1 [["a1", "a"], ["b"]])
        [ "a1 a"
        , "b"
        ]
    equal (f 1 [["a1", "a"], ["b"], ["c", "d", "e"]])
        [ "a1 a"
        , "b"
        , "c  d e"
        ]
    equal (f 1 [["a", "bcd"], ["1", "2"]])
        [ "a bcd"
        , "1 2"
        ]
