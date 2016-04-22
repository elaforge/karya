-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseTitle_test where
import Util.Test
import qualified Derive.ParseTitle as ParseTitle


test_parse_unparse_control = do
    let f = fmap ParseTitle.unparse_control
            . ParseTitle.parse_control
        round_trip text = (f text, Right text)
    uncurry equal (round_trip "*")
    uncurry equal (round_trip "*scale")
    equal (f "*scale #") (Right "*scale")
    uncurry equal (round_trip "*scale #name")
    uncurry equal (round_trip "*a merge")
    uncurry equal (round_trip "*a #name merge")
    uncurry equal (round_trip "tempo")
    uncurry equal (round_trip "a-b")
    uncurry equal (round_trip "add a-b")
    uncurry equal (round_trip "c")
    uncurry equal (round_trip "c:d")
    uncurry equal (round_trip "add c:d")
    left_like (f "c:q") "unknown type"
    uncurry equal (round_trip "%")
    uncurry equal (round_trip "add %")
    left_like (f "$ bad") "parse error"
    left_like (f "a b c") "control track must be one of"


test_parse_unparse_note = do
    let f = fmap ParseTitle.unparse_note . ParseTitle.parse_note
    equal (f ">") (Right ">")
    equal (f ">x | abc") (Right ">x | abc")
