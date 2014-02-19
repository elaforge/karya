-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TrackInfo_test where
import Util.Test
import qualified Derive.TrackInfo as TrackInfo


test_parse_unparse_control = do
    let f = fmap TrackInfo.unparse_control . TrackInfo.parse_control
    equal (f "*") (Right "*")
    equal (f "*scale") (Right "*scale")
    equal (f "*scale #") (Right "*scale")
    equal (f "*scale #name") (Right "*scale #name")
    equal (f "tempo") (Right "tempo")
    equal (f "c") (Right "c")
    equal (f "c:d") (Right "c:d")
    left_like (f "c:q") "control should look like"
    equal (f "%") (Right "%")
    equal (f "add %") (Right "add %")
    left_like (f "$ bad") "parse error"
    left_like (f "a b c") "control track must be one of"

test_parse_unparse_note = do
    let f = fmap TrackInfo.unparse_note . TrackInfo.parse_note
    equal (f ">") (Right ">")
    equal (f ">x | abc") (Right ">x | abc")
