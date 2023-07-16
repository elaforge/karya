-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JavaScales_test where
import qualified Derive.Scale.JavaScales as JavaScales

import           Util.Affine
import           Util.Test


test_add_diatonic :: Test
test_add_diatonic = do
    let f c = JavaScales.add_diatonic lima (Chromatic c)
    -- 51 52 53 55 56 61 62 63
    equal (map (f 0) [0..7]) $ map Chromatic [0, 1, 2, 4, 5, 7, 8, 9]
    -- 52 53 55 56 61 62 63 65
    equal (map (f 1) [0..7]) $ map Chromatic [1, 2, 4, 5, 7, 8, 9, 11]
    equal (f 3 0) (Chromatic 3) -- 54+0 = 54
    equal (f 3 1) (Chromatic 4) -- 54+1 = 55
    equal (f 3 (-1)) (Chromatic 2) -- 54-1 = 53
    equal (map (f 7) [0, -1 .. -7]) $ map Chromatic [7, 5, 4, 2, 1, 0, -2, -3]
    equal (map (f 7) [0, -1 .. -7]) $ map Chromatic [7, 5, 4, 2, 1, 0, -2, -3]

lima :: JavaScales.Layout
lima = JavaScales.make_layout 0 [1, 1, 2, 1, 2]
