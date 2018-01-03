-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Segment_test where
import qualified Util.Segment as Segment
import Util.Segment (X)
import Util.Test

import qualified Perform.RealTime as RealTime
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

test_constant_val = do
    let f = Segment.constant_val
    equal (f $ constant 1) (Just 1)
    equal (f $ Segment.shift 10 $ constant 1) (Just 1)
    equal (f $ from_pairs []) Nothing
    equal (f $ from_pairs [(0, 0)]) Nothing
    equal (f $ from_pairs [(3, 2)]) Nothing

test_concat = do
    let f = to_segments . Segment.concat . map from_pairs
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

large_y :: Y
large_y = RealTime.to_seconds large

test_integrate = do
    let f = Segment.to_pairs . Segment.integrate 1 . from_pairs
    equal (f []) []
    equal (f [(0, 1)]) [(0, 0), (large, large_y)]
    equal (f [(0, 0), (4, 4)])
        [ (0, 0), (1, 0.5), (2, 2), (3, 4.5), (4, 8)
        , (large, 8 + 4 * (large_y - 4))
        ]
    equal (f [(0, 1), (3, 2)])
        [ (0, 0), (1, 1+1/6), (2, 2+2/3), (3, 3+3/2)
        , (large, 3+3/2 + 2 * (large_y - 3))
        ]
    equal (f [(0, 0), (1, 1), (2, 2), (4, 2)])
        [ (0, 0), (1, 0.5), (2, 2), (4, 6), (4, 6)
        , (large, 6 + 2 * (large_y - 4))
        ]

test_linear_operator = do
    let f s1 s2 = to_segments $
            Segment.linear_operator 0 (+) (from_pairs s1) (from_pairs s2)
    -- Degenerate.
    equal (f [] []) []
    equal (f [(0, 1)] []) [((0, 1), (large, 1))]
    -- Simultaneous start.
    equal (f [(0, 1)] [(0, 2)]) [((0, 3), (large, 3))]
    -- Discontinuities.
    equal (f [(0, 1), (1, 1), (1, 2)] [(0, 2), (1, 2), (1, 4)])
        [((0, 3), (1, 3)), ((1, 6), (large, 6))]
    equal (f [(0, 1)] [(1, 2)]) [((0, 1), (1, 1)), ((1, 3), (large, 3))]
    -- Linear interpolation between the segments.
    equal (f [(0, 0), (4, 4)] [(2, 1)])
        [((0, 0), (2, 2)), ((2, 3), (4, 5)), ((4, 5), (large, 5))]


to_segments :: Segment.NumSignal -> [((X, Y), (X, Y))]
to_segments = map (\(Segment.Segment x1 y1 x2 y2) -> ((x1, y1), (x2, y2)))
    . Segment.to_segments

from_pairs :: [(X, Y)] -> Segment.NumSignal
from_pairs = Segment.from_pairs

constant :: Y -> Segment.NumSignal
constant = Segment.constant
