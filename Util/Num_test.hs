-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Num_test where
import qualified Util.Num as Num
import Util.Test


test_showFloatP :: Test
test_showFloatP = do
    let f leadingZero = Num.showFloatP leadingZero 3
    equal (f True 0) "0"
    equal (f False 0) "0"
    equal (f False 3) "3"
    equal (f False 3.0005) "3.0005"
    equal (f True 0.1005) "0.1"
    equal (f True 0.000505) "0.0005"
    equal (f True 0.12345) "0.123"
    equal (f True 0.0005) "0.0005"
    equal (f False 0.12345) ".123"
    equal (f False 0.0005) ".0005"

    -- negative
    equal (f True (-0.1005)) "-0.1"
    equal (f False (-0.1005)) "-.1"
    equal (f False (-0.0005)) "-.0005"
    equal (f False (-1.1005)) "-1.1"
    equal (f True (-0)) "-0"
    equal (f False (-0)) "-0"
