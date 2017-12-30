module Util.Linear_test where
import qualified Util.Linear as Linear
import Util.Linear (X)
import Util.Test

import Perform.RealTime (large)


type Y = Double

test_from_pairs = do
    let f = to_segments . from_pairs
    equal (f [(0, 0), (1, 1), (2, 0)])
        [((0, 0), (1, 1)), ((1, 1), (2, 0)), ((2, 0), (large, 0))]
    equal (f [(0, 0), (1, 0), (1, 1)])
        [((0, 0), (1, 0)), ((1, 1), (large, 1))]
    equal (f [(0, 0), (1, 0), (1, 1), (1, 2)])
        [((0, 0), (1, 0)), ((1, 2), (large, 2))]
    equal (f [(0, 0), (2, 2), (1, 1), (0, 1), (4, 0)])
        [((0, 0), (2, 2)), ((2, 2), (4, 0)), ((4, 0), (large, 0))]

test_concat = do
    let f = to_segments . Linear.concat . map from_pairs
    equal (f [[(0, 1)], [(4, 2)]]) [((0, 1), (4, 1)), ((4, 2), (large, 2))]
    equal (f [[(4, 2)], [(0, 1)]]) [((0, 1), (large, 1))]
    -- Replace the flat line afterwards.
    equal (f [[(0, 1), (1, 2)], [(1, 4)]])
        [((0, 1), (1, 2)), ((1, 4), (large, 4))]
    -- Extend a flat line.
    equal (f [[(0, 1), (1, 2)], [(4, 4)]])
        [((0, 1), (1, 2)), ((1, 2), (4, 2)), ((4, 4), (large, 4))]
    -- The rightmost one wins.
    equal (f [[(0, 0)], [(2, 2)], [(1, 1)]])
        [((0, 0), (1, 0)), ((1, 1), (large, 1))]

to_segments :: Linear.NumSignal -> [((X, Y), (X, Y))]
to_segments = map (\(Linear.Segment x1 y1 x2 y2) -> ((x1, y1), (x2, y2)))
    . Linear.to_segments

from_pairs :: [(X, Y)] -> Linear.NumSignal
from_pairs = Linear.from_pairs
