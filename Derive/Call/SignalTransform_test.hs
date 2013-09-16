-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.SignalTransform_test where
import Util.Test
import qualified Derive.Call.CallTest as CallTest


test_sh_pitch = do
    let run = CallTest.run_pitch
    equal (run [(0, "4c"), (4, "sh .5 | i (5c)")])
        [(0, 60), (2, 66), (4, 72)]
    equal (run [(0, "4c"), (4, "sh 1 | i (5c)")])
        [(0, 60), (1, 63), (2, 66), (3, 69), (4, 72)]

test_sh_control = do
    let run = CallTest.run_control
    equal (run [(0, "0"), (4, "sh .5 | i 4")]) [(0, 0), (2, 2), (4, 4)]
    equal (run [(0, "0"), (4, "sh 1 | i 4")])
        [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
