-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Segment_test where
import qualified Util.Segment as Segment
import Util.Segment (Segment(Segment))
import Util.Segment (X)
import Util.Test
import qualified Util.TimeVector as TimeVector

import qualified Ui.Types as Types
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
    equal (f [(0, 0), (1, 0), (1, 0), (1, 1)])
        [((0, 0), (1, 0)), ((1, 1), (large, 1))]

test_constant_val = do
    let f = Segment.constant_val
    equal (f $ constant 1) (Just 1)
    equal (f $ Segment.shift 100 $ constant 1) (Just 1)
    -- Not thrown off by shifts, as long as they're not too large.
    equal (f $ Segment._flatten_shift $ Segment.shift 100 $ constant 1) (Just 1)
    equal (f $ from_pairs []) Nothing
    equal (f $ from_pairs [(0, 0)]) Nothing
    equal (f $ from_pairs [(3, 2)]) Nothing

test_concat = do
    let f = to_pairs . Segment.concat Segment.num_interpolate . map from_pairs
    equal (f []) []
    -- Extend final segment.
    equal (f [[(0, 1)], [(4, 2)]]) [(0, 1), (4, 1), (4, 2)]
    -- Coincident samples.
    equal (f [[(0, 0), (1, 1)], [(1, 1), (2, 2)]])
        [(0, 0), (1, 1), (1, 1), (2, 2)]
    -- interpolate
    equal (f [[(0, 0), (4, 4)], [(2, 8)]]) [(0, 0), (2, 2), (2, 8)]
    -- The rightmost one wins.
    equal (f [[(0, 0)], [(2, 2)], [(1, 1)]]) [(0, 0), (1, 0), (1, 1)]
    -- Suppress duplicates.
    equal (f [[(1, 1)], [(1, 1)], [(1, 1)]]) [(1, 1)]
    equal (f [[(0, 1), (1, 1)], [(1, 2)], [(1, 1)]]) [(0, 1), (1, 1), (1, 1)]
    -- But not legit ones.
    equal (f [[(0, 1), (1, 1)], [(1, 2)]]) [(0, 1), (1, 1), (1, 2)]

test_prepend = do
    let f sig1 sig2 = to_pairs $ Segment.prepend Segment.num_interpolate
            (from_pairs sig1) (from_pairs sig2)
    equal (f [] []) []
    equal (f [] [(0, 0), (1, 1)]) [(0, 0), (1, 1)]
    equal (f [(0, 10), (1, 1)] [(0, 0), (1, 1), (2, 2)])
        [(0, 10), (1, 1), (1, 1), (2, 2)]

test_segment_at = do
    let f x = Segment.segment_at_orientation Types.Positive x . from_pairs
    equal (f 0 []) Nothing
    equal (f 0 [(1, 1)]) Nothing
    equal (f 1 [(1, 1)]) $ Just (Segment 1 1 large 1)
    equal (f 2 [(1, 1)]) $ Just (Segment 1 1 large 1)
    equal (f 2 [(1, 1), (2, 2), (3, 3)]) $ Just (Segment 2 2 3 3)
    equal (f 2 [(1, 1), (3, 3)]) $ Just (Segment 1 1 3 3)
    -- Positive bias.
    equal (f 2 [(0, 0), (2, 0), (2, 2)]) $ Just (Segment 2 2 large 2)
    -- Shift.
    equal (Segment.segment_at 4 $
            Segment.shift 3 (from_pairs [(0, 0), (2, 2)])) $
        Just (Segment 3 0 5 2)

test_segment_at_negative = do
    let f x = Segment.segment_at_orientation Types.Negative x . from_pairs
    equal (f 0 []) Nothing
    equal (f 0 [(1, 1)]) Nothing
    equal (f 1 [(1, 1)]) $ Just (Segment 1 1 large 1)
    equal (f 2 [(1, 1)]) $ Just (Segment 1 1 large 1)
    equal (f 2 [(1, 1), (2, 2), (3, 3)]) $ Just (Segment 1 1 2 2)
    equal (f 2 [(1, 1), (3, 3)]) $ Just (Segment 1 1 3 3)
    -- Negative bias.
    equal (f 2 [(0, 0), (2, 0), (2, 2)]) $ Just (Segment 0 0 2 0)

test_at = do
    let f x = Segment.at Segment.num_interpolate x
            (from_pairs [(1, 1), (2, 2), (2, 3)])
    equal (map f [0, 1, 1.5, 2, 3, 4])
        [Nothing, Just 1, Just 1.5, Just 3, Just 3, Just 3]

test_shift = do
    let shift = Segment.shift
    let at x = Segment.at Segment.num_interpolate x
    equal (at 2 (from_pairs [(2, 2)])) (Just 2)
    equal (at 2 (shift 2 (from_pairs [(2, 2)]))) Nothing
    equal (at 0 (shift (-2) (from_pairs [(2, 2)]))) (Just 2)

    let shifted = shift 2 (from_pairs [(2, 2)])
    equal (to_pairs shifted) [(4, 2)]
    equal (Segment.head shifted) (Just (4, 2))
    equal (Segment.last shifted) (Just (4, 2))
    equal (Segment.drop_after 3 shifted) Segment.empty
    equal (Segment.drop_after 4 shifted) shifted

    let shifted2 = shift 2 (from_pairs [(2, 2), (3, 3)])
    equal (to_pairs $ Segment.drop_before 5 shifted2) [(5, 3)]
    equal (Segment.drop_before 4 shifted2) shifted2

test_drop_after_clip_after = do
    let f x sig =
            ( to_pairs $ Segment.drop_after x $ from_pairs sig
            , to_pairs $ Segment.clip_after Segment.num_interpolate x $
                from_pairs sig
            )
    let s124 = [(1, 1), (2, 2), (4, 4)]
    equal (f 4 s124) ([(1, 1), (2, 2), (4, 4)], [(1, 1), (2, 2), (4, 4)])
    equal (f 3 s124) ([(1, 1), (2, 2), (4, 4)], [(1, 1), (2, 2), (3, 3)])
    equal (f 2 s124) ([(1, 1), (2, 2)], [(1, 1), (2, 2)])
    equal (f 1 s124) ([(1, 1)], [])
    equal (f 0 s124) ([], [])
    equal (f 2 [(0, 0), (2, 0), (2, 2)]) ([(0, 0), (2, 0)], [(0, 0), (2, 0)])

test_num_clip_after = do
    let f x = to_pairs . Segment.num_clip_after x . from_pairs
    equal (f 2 [(0, 0), (1, 1), (4, 1)]) [(0, 0), (1, 1)]
    equal (f 2 [(0, 0), (2, 0), (2, 2)]) [(0, 0)]
    equal (f 2 [(0, 0), (4, 4)]) [(0, 0), (2, 2)]
    equal (f 2 [(2, 1)]) []

test_drop_before_clip_before = do
    let f x sig =
            ( to_pairs $ Segment.drop_before x $ from_pairs sig
            , to_pairs $ Segment.clip_before Segment.num_interpolate x $
                from_pairs sig
            )
    let s124 = [(1, 1), (2, 2), (4, 4)]
    equal (f 5 s124) ([(4, 4)], [(5, 4)])
    equal (f 4 s124) ([(4, 4)], [(4, 4)])
    equal (f 3 s124) ([(2, 2), (4, 4)], [(3, 3), (4, 4)])
    equal (f 2 s124) ([(2, 2), (4, 4)], [(2, 2), (4, 4)])
    equal (f 1 s124) ([(1, 1), (2, 2), (4, 4)], [(1, 1), (2, 2), (4, 4)])
    equal (f 0 s124) ([(1, 1), (2, 2), (4, 4)], [(1, 1), (2, 2), (4, 4)])
    equal (f 2 [(0, 0), (2, 0), (2, 2)]) ([(2, 2)], [(2, 2)])

test_integrate = do
    let f = to_pairs . Segment.integrate 1 . from_pairs
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

test_resample_rate = do
    let f = to_pairs . Segment.resample_rate 1 . from_pairs
    equal (f [(0, 2), (4, 2), (4, 1)]) [(0, 2), (4, 2), (4, 1)]
    equal (f [(0, 0), (4, 4)]) [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
    equal (f [(0, 0), (2, 2), (2, 0), (4, 2)])
        [(0, 0), (1, 1), (2, 2), (2, 0), (3, 1), (4, 2)]

test_map_y = do
    let f = to_pairs . Segment.map_y 1 (1/) . from_pairs
    equal (f [(0, 5), (4, 1)]) [(0, 1/5), (1, 1/4), (2, 1/3), (3, 1/2), (4, 1)]
    equal (f [(0, 2), (4, 2), (4, 1)]) [(0, 1/2), (4, 1/2), (4, 1)]

test_linear_operator = do
    let f s1 s2 = to_segments $
            Segment.linear_operator (+) (from_pairs s1) (from_pairs s2)
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

test_linear_operator2 = do
    let f s1 s2 = to_segments $
            Segment._linear_operator2 sum (from_pairs s1) (from_pairs s2)
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

test_to_piecewise_constant = do
    let f = TimeVector.to_pairs . Segment.to_piecewise_constant 1 . from_pairs
    equal (f []) []
    equal (f [(4, 2)]) [(4, 2)]
    equal (f [(2, 2), (4, 2), (4, 4)]) [(2, 2), (4, 4)]
    equal (f [(2, 2), (4, 4)]) [(2, 2), (3, 3), (4, 4)]
    equal (f [(2, 2), (2, 2), (2, 2)]) [(2, 2)]

test_drop_discontinuity_at = do
    let f x = to_pairs . Segment.drop_discontinuity_at x . from_pairs
    equal (f 1 [(0, 0), (1, 0), (1, 1), (2, 1)])
        [(0, 0), (2, 0), (2, 1)]
    -- Don't get >2 2s in a row.
    equal (f 1 [(0, 0), (1, 0), (1, 1), (2, 1), (2, 0)])
        [(0, 0), (2, 0), (2, 0)]


large_y :: Y
large_y = RealTime.to_seconds large

to_segments :: Segment.NumSignal -> [((X, Y), (X, Y))]
to_segments = map (\(Segment.Segment x1 y1 x2 y2) -> ((x1, y1), (x2, y2)))
    . Segment.to_segments

from_pairs :: [(X, Y)] -> Segment.NumSignal
from_pairs = Segment.from_pairs

to_pairs :: Segment.NumSignal -> [(X, Y)]
to_pairs = Segment.to_pairs

constant :: Y -> Segment.NumSignal
constant = Segment.constant
