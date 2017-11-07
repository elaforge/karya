-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.SignalTransform_test where
import Util.Test
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.C.Prelude.SignalTransform as SignalTransform
import qualified Perform.Signal as Signal


test_sh_pitch = do
    let run = CallTest.run_pitch ""
    equal (run [(0, "4c"), (4, "sh .5 | i (5c)")])
        [(0, 60), (2, 66), (4, 72)]
    equal (run [(0, "4c"), (4, "sh 1 | i (5c)")])
        [(0, 60), (1, 63), (2, 66), (3, 69), (4, 72)]

test_sh_control = do
    let run = CallTest.run_control
    equal (run [(0, "0"), (4, "sh .5 | i 4")]) [(0, 0), (2, 2), (4, 4)]
    equal (run [(0, "0"), (4, "sh 1 | i 4")])
        [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]

test_quantize = do
    let run = CallTest.run_control
    equal (run [(0, "0"), (4, "quantize .25 | i .5")])
        [(0, 0), (2, 0.25), (3, 0.5)]

test_slew_limiter = do
    let f srate slope = Signal.unsignal
            . SignalTransform.slew_limiter srate slope . Signal.signal
    -- quickcheck: signal never moves faster than srate/slope
    equal (f 1 1 [(0, 2)]) [(0, 2)]
    equal (f 1 1 [(0, 2), (2, 0)]) [(0, 2), (2, 1), (3, 0)]
    equal (f 1 1 [(0, 2), (2, 0), (4, 2)])
        [(0, 2), (2, 1), (3, 0), (4, 1), (5, 2)]
    equal (f 0.5 1 [(0, 2), (2, 0)])
        [(0, 2), (2, 1.5), (2.5, 1), (3, 0.5), (3.5, 0)]
    equal (f 1 1 [(0, 0), (1, 1)]) [(0, 0), (1, 1)]
    equal (f 1 1 [(0, 0), (1, 1), (2, 3)]) [(0, 0), (1, 1), (2, 2), (3, 3)]

    equal (f 1 1 [(0, 0), (2, 8), (4, 0)])
        [(0, 0), (2, 1), (3, 2), (4, 1), (5, 0)]

test_interpolate_slope = do
    let f prev_y xy next = Signal.unsignal $
            SignalTransform.slope_segment 1 0.5 prev_y xy next
    equal (f 2 (0, 1) Nothing) [(0, 1.5), (1, 1.0)]
    equal (f 2 (0, 1) (Just 2)) [(0, 1.5), (1, 1)]
    equal (f 0 (0, 1) Nothing) [(0, 0.5), (1, 1.0)]
    equal (f 0 (1, 1) Nothing) [(1, 0.5), (2, 1.0)]

test_smooth = do
    let f time = Signal.unsignal
            . SignalTransform.smooth id 1 time . Signal.signal
    equal (f 2 [(0, 0), (2, 2)]) [(0, 0), (3, 1), (4, 2)]
    equal (f 2 [(0, 0), (2, 2), (4, 0)])
        [(0, 0), (3, 1), (4, 2), (5, 1), (6, 0)]
    equal (f (-2) [(0, 0), (2, 2), (4, 0)])
        [(0, 0), (1, 1), (2, 2), (3, 1), (4, 0)]
    -- Not enough room.
    equal (f 4 [(0, 0), (2, 2), (4, 0)])
        [(0, 0), (3, 1), (4, 2), (5, 1.5), (6, 1), (7, 0.5), (8, 0)]
    equal (f (-4) [(0, 0), (2, 2), (4, 0)])
        [(0, 0), (1, 1), (2, 2), (3, 1), (4, 0)]
    -- Zero duration.
    equal (f 0 [(0, 0), (1, 1), (2, 0)]) [(0, 0), (1, 1), (2, 0)]

test_smooth_relative = do
    let f time = Signal.unsignal
            . SignalTransform.smooth_relative id 1 (const time)
            . Signal.signal
    equal (f 0 [(0, 0), (4, 4), (8, 0)]) [(0, 0), (4, 4), (8, 0)]
    equal (f 0.5 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (2, 0), (3, 2), (4, 4), (6, 4), (7, 2), (8, 0)]
    equal (f 1 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 3), (6, 2), (7, 1), (8, 0)]
