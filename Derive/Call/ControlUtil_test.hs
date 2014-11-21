-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.ControlUtil_test where
import Util.Test
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Perform.Signal as Signal


test_breakpoints = do
    let make start end = ControlUtil.breakpoints 1 id
            . ControlUtil.distribute start end
    let f start end = Signal.unsignal . make start end
    equal (f 4 8  []) []
    equal (f 4 8  [1]) [(4, 1)]
    equal (f 4 8  [0, 1, 0]) [(4, 0), (5, 0.5), (6, 1), (7, 0.5), (8, 0)]
