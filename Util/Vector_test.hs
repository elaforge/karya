-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Vector_test where
import qualified Data.Vector.Unboxed as Unboxed

import Util.Test
import qualified Util.Vector as Vector


test_bracket = do
    let f vec a = Vector.bracket (Unboxed.fromList vec) a
    equal (f [] 5) Nothing
    equal (f [0] 5) Nothing
    equal (f [0, 1] 0) $ Just (0, 0, 0)
    equal (f [0, 1] 0.5) $ Just (0, 0, 1)
    equal (f [0, 1] 1) $ Just (1, 1, 1)
    equal (f [0, 1] 2) Nothing
