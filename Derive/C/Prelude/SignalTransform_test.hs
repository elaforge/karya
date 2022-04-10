-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.SignalTransform_test where
import Util.Test
import qualified Derive.C.Prelude.SignalTransform as SignalTransform
import qualified Derive.Call.CallTest as CallTest
import qualified Perform.Signal as Signal


test_sh_pitch :: Test
test_sh_pitch = do
    let run = CallTest.run_pitch ""
    equal (run [(0, "4c"), (4, "sh .5 | i (5c)")])
        [(0, 60), (2, 60), (2, 66), (4, 66), (4, 72)]
    equal (run [(0, "4c"), (4, "sh 1 | i (5c)")])
        [ (0, 60), (1, 60), (1, 63), (2, 63), (2, 66), (3, 66)
        , (3, 69), (4, 69), (4, 72)
        ]

test_sh_control :: Test
test_sh_control = do
    let run = CallTest.run_control
    equal (run [(0, "0"), (4, "sh .5 | i 4")])
        [(0, 0), (2, 0), (2, 2), (4, 2), (4, 4)]
    equal (run [(0, "0"), (3, "sh 1 | i 3")])
        [ (0, 0), (1, 0), (1, 1), (2, 1)
        , (2, 2), (3, 2), (3, 3)
        ]

test_quantize :: Test
test_quantize = do
    let run = CallTest.run_control
    equal (run [(0, "0"), (4, "quantize .25 | i 1")])
        [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]

    -- If I did hard quantization:
    -- equal (run [(0, "0"), (4, "quantize .25 | i 1")])
    --     [ (0, 0), (1, 0), (1, 0.25), (2, 0.25)
    --     , (2, 0.5), (3, 0.5), (3, 0.75), (4, 0.75), (4, 1)
    --     ]

test_slew_limiter :: Test
test_slew_limiter = do
    let f slope = Signal.to_pairs
            . SignalTransform.slew_limiter slope . Signal.from_pairs
    equal (f 1 [(0, 2)]) [(0, 2)]
    equal (f 1 [(0, 0), (2, 2)]) [(0, 0), (2, 2)]
    equal (f 1 [(0, 2), (2, 0)]) [(0, 2), (2, 0)]
    equal (f 1 [(0, 0), (2, 4)]) [(0, 0), (2, 2)]
    equal (f 1 [(0, 4), (2, 0)]) [(0, 4), (2, 2)]
    equal (f 1 [(0, 0), (2, 4), (4, 0)]) [(0, 0), (2, 2), (4, 0)]
