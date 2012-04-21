module Derive.Call.Control_test where

import Util.Test

import qualified Derive.Call.CallTest as CallTest


run = CallTest.run_control

test_set = do
    equal (run [(0, "1"), (1, "0")]) [(0, 1), (1, 0)]
    equal (run [(0, "1"), (1, "")]) [(0, 1)]

test_linear = do
    -- make sure 0 arg variant works
    equal (run [(0, "1"), (1, "i"), (3, "i 0")])
        [(0, 1), (1, 1), (2, 0.5), (3, 0)]

test_exponential = do
    equal (run [(0, "1"), (4, "e 0")])
        [(0, 1), (1, 0.9375), (2, 0.75), (3, 0.4375), (4, 0)]
    equal (run [(0, "1"), (4, "e 0 1")])
        [(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (4, 0)]
    equal (run [(0, "1"), (4, "e 0 -1")])
        [(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (4, 0)]
    equal (run [(0, "1"), (4, "e 0 -2")])
        [(0, 1), (1, 0.5),
            (2, 0.2928932188134524), (3, 0.1339745962155614), (4, 0)]

test_slide = do
    -- no arg goes to the next event
    equal (run [(0, "1"), (4, "s 0"), (6, "0")])
        [(0, 1), (4, 1), (5, 0.5), (6, 0)]
    equal (run [(0, "1"), (4, "s 0 2")]) [(0, 1), (4, 1), (5, 0.5), (6, 0)]
    -- if the time is too long, it is clipped
    equal (run [(0, "1"), (4, "s 0 4"), (6, "1")])
        [(0, 1), (4, 1), (5, 0.75), (6, 1)]

test_neighbor = do
    equal (run [(0, "n 1 2")]) [(0, 1), (1, 0.5), (2, 0)]
    equal (run [(0, "n 2 1")]) [(0, 2), (1, 0)]
