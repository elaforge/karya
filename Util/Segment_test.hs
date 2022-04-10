-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeApplications #-}
module Util.Segment_test where
import qualified Data.List as List
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Util.Num as Num
import qualified Util.Segment as Segment
import           Util.Segment (Segment(Segment))
import           Util.Segment (X)
import qualified Util.Seq as Seq
import qualified Util.TimeVector as TimeVector

import qualified Perform.RealTime as RealTime
import           Perform.RealTime (large)
import qualified Ui.Types as Types

import           Types
import           Util.Test


type Y = Double

test_from_pairs :: Test
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

test_constant_val :: Test
test_constant_val = do
    let f = Segment.constant_val
    equal (f $ constant 1) (Just 1)
    equal (f $ Segment.shift 100 $ constant 1) (Just 1)
    -- Not thrown off by shifts, as long as they're not too large.
    equal (f $ Segment._flatten_shift $ Segment.shift 100 $ constant 1) (Just 1)
    equal (f $ from_pairs []) Nothing
    equal (f $ from_pairs [(0, 0)]) Nothing
    equal (f $ from_pairs [(3, 2)]) Nothing

test_constant_val_num :: Test
test_constant_val_num = do
    let f = Segment.constant_val_num
    equal (f 0 $ from_pairs [(2, 1)]) Nothing
    equal (f 0 $ Segment.shift (-2) (from_pairs [(2, 1)])) (Just 1)
    equal (f 0 $ Segment.shift (-2) (from_pairs [(2, 1), (4, 1)])) (Just 1)

test_concat :: Test
test_concat = do
    let f = to_pairs . num_concat . map from_pairs
    equal (f []) []
    -- Extend final segment.
    equal (f [[(0, 1)], [(4, 2)]]) [(0, 1), (4, 1), (4, 2)]
    -- interpolate
    equal (f [[(0, 0), (4, 4)], [(2, 8)]]) [(0, 0), (2, 2), (2, 8)]
    -- The rightmost one wins.
    equal (f [[(0, 0)], [(2, 2)], [(1, 1)]]) [(0, 0), (1, 0), (1, 1)]
    -- Suppress duplicates.
    equal (f [[(1, 1)], [(1, 1)], [(1, 1)]]) [(1, 1)]
    equal (f [[(0, 1), (1, 1)], [(1, 2)], [(1, 1)]]) [(0, 1), (1, 1)]
    equal (f [[(0, 0), (1, 1)], [(1, 1), (2, 2)]]) [(0, 0), (1, 1), (2, 2)]
    equal (f [[(0, 0), (2, 0)], [(2, 0), (4, 1)], [(4, 1), (6, 1)]])
        [(0, 0), (2, 0), (4, 1), (6, 1)]
    equal (f [[(0, 0), (1, 1)], [(2, 2), (4, 4)], [(1, 2)]])
        [(0, 0), (1, 1), (1, 2)]
    -- But not legit ones.
    equal (f [[(0, 1), (1, 1)], [(1, 2)]]) [(0, 1), (1, 1), (1, 2)]

test_concat_ascending :: Test
test_concat_ascending = hedgehog $ property $ do
    (s1, s2) <- Hedgehog.forAll $ (,) <$> gen_signal <*> gen_signal
    let xs = map fst $ to_pairs $ num_concat [s1, s2]
    xs === List.sort xs

test_concat_dups :: Test
test_concat_dups = hedgehog $ Hedgehog.withTests 500 $ property $ do
    sigs <- Hedgehog.forAll $ Gen.list (Range.linear 0 4) gen_signal
    let xs = map fst $ to_pairs $ num_concat sigs
    filter ((>2) . length) (List.group xs) === []

test_prepend :: Test
test_prepend = do
    let f sig1 sig2 = to_pairs $
            Segment.prepend (Just (==)) Segment.num_interpolate
                (from_pairs sig1) (from_pairs sig2)
    equal (f [] []) []
    equal (f [] [(0, 0), (1, 1)]) [(0, 0), (1, 1)]
    equal (f [(0, 10), (1, 1)] [(0, 0), (1, 1), (2, 2)])
        [(0, 10), (1, 1), (2, 2)]

test_segment_at :: Test
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

test_segment_at_negative :: Test
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

test_at :: Test
test_at = do
    let f x = Segment.at Segment.num_interpolate x
            (from_pairs [(1, 1), (2, 2), (2, 3)])
    equal (map f [0, 1, 1.5, 2, 3, 4])
        [Nothing, Just 1, Just 1.5, Just 3, Just 3, Just 3]

test_shift :: Test
test_shift = do
    let shift = Segment.shift
    let at x = Segment.at Segment.num_interpolate x
    equal (at 2 (from_pairs [(2, 2)])) (Just 2)
    equal (at 2 (shift 2 (from_pairs [(2, 2)]))) Nothing
    equal (at 0 (shift (-2) (from_pairs [(2, 2)]))) (Just 2)
    equal (at 0 (shift (-1) (from_pairs [(1, 1), (2, 0.5)]))) (Just 1)
    equal (at 0.5 (shift (-1) (from_pairs [(1, 1), (2, 0.5)]))) (Just 0.75)

    let shifted = shift 2 (from_pairs [(2, 2)])
    equal (to_pairs shifted) [(4, 2)]
    equal (Segment.head shifted) (Just (4, 2))
    equal (Segment.last shifted) (Just (4, 2))
    equal (Segment.drop_after 3 shifted) Segment.empty
    equal (Segment.drop_after 4 shifted) shifted

    let shifted = shift 2 (from_pairs [(2, 2), (3, 3)])
    equal (to_pairs $ Segment.drop_before 5 shifted) [(5, 3)]
    equal (Segment.drop_before 4 shifted) shifted

test_drop_after_clip_after :: Test
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

test_num_clip_after :: Test
test_num_clip_after = do
    let f x = to_pairs . Segment.num_clip_after False x . from_pairs
    equal (f 2 [(0, 0), (1, 1), (4, 1)]) [(0, 0), (1, 1)]
    equal (f 2 [(0, 0), (2, 0), (2, 2)]) [(0, 0)]
    equal (f 2 [(0, 0), (4, 4)]) [(0, 0), (2, 2)]
    equal (f 2 [(2, 1)]) []

test_drop_before_clip_before :: Test
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

test_integrate :: Test
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

test_resample_rate :: Test
test_resample_rate = do
    let f = to_pairs . Segment.resample_rate 1 . from_pairs
    equal (f [(0, 2), (4, 2), (4, 1)]) [(0, 2), (4, 2), (4, 1)]
    equal (f [(0, 0), (4, 4)]) [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
    equal (f [(0, 0), (2, 2), (2, 0), (4, 2)])
        [(0, 0), (1, 1), (2, 2), (2, 0), (3, 1), (4, 2)]

test_map_y :: Test
test_map_y = do
    let f = to_pairs . Segment.map_y 1 (1/) . from_pairs
    equal (f [(0, 5), (4, 1)]) [(0, 1/5), (1, 1/4), (2, 1/3), (3, 1/2), (4, 1)]
    equal (f [(0, 2), (4, 2), (4, 1)]) [(0, 1/2), (4, 1/2), (4, 1)]

test_linear_operator :: Test
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

test_linear_operator2 :: Test
test_linear_operator2 = do
    let f s1 s2 = to_segments $
            Segment._linear_operator2 Num.sum (from_pairs s1) (from_pairs s2)
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

test_to_piecewise_constant :: Test
test_to_piecewise_constant = do
    let f = TimeVector.to_pairs . Segment.to_piecewise_constant 1 . from_pairs
    equal (f []) []
    equal (f [(4, 2)]) [(4, 2)]
    equal (f [(2, 2), (4, 2), (4, 4)]) [(2, 2), (4, 4)]
    equal (f [(2, 2), (4, 4)]) [(2, 2), (3, 3), (4, 4)]
    equal (f [(2, 2), (2, 2), (2, 2)]) [(2, 2)]

test_drop_discontinuity_at :: Test
test_drop_discontinuity_at = do
    let f x = to_pairs . Segment.drop_discontinuity_at x . from_pairs
    equal (f 1 [(0, 0), (1, 0), (1, 1), (2, 1)])
        [(0, 0), (2, 0), (2, 1)]
    -- Don't get >2 2s in a row.
    equal (f 1 [(0, 0), (1, 0), (1, 1), (2, 1), (2, 0)])
        [(0, 0), (2, 0), (2, 0)]

test_invert :: Test
test_invert = do
    let f = to_pairs . Segment.invert
    equal (f $ from_pairs [(0, 1), (1, 2)]) [(1, 0), (2, 1)]
    equal (f $ Segment.shift (-2) $ from_pairs [(2, 1), (3, 2)])
        [(1, 0), (2, 1)]

-- * hedgehog

-- | Xs are ascending.
test_from_samples_ascending :: Test
test_from_samples_ascending = hedgehog $ property $ do
    samples <- Hedgehog.forAll gen_samples
    let xs = map fst $ to_pairs $ from_pairs samples
    xs === List.sort xs

-- | Never more than 2 Xs with the same value.
test_from_samples_dups :: Test
test_from_samples_dups = hedgehog $ property $ do
    samples <- Hedgehog.forAll gen_samples
    let xs = map fst $ to_pairs $ from_pairs samples
    Hedgehog.annotateShow xs
    filter ((>2) . length) (List.group xs) === []

gen_samples :: Hedgehog.Gen [(X, Y)]
gen_samples = Gen.list (Range.linear 0 16) $
    (,) <$> gen_integral_x <*> Gen.element [-1, 1]

-- | Integral RealTimes to encourage collisions.
gen_integral_x :: Hedgehog.Gen RealTime
gen_integral_x = RealTime.seconds . fromIntegral <$>
    Gen.int (Range.linear (-4) 4)

gen_signal :: Hedgehog.Gen Segment.NumSignal
gen_signal = from_pairs . Seq.sort_on fst <$> gen_samples

-- * util

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

num_concat :: [Segment.NumSignal] -> Segment.NumSignal
num_concat = Segment.concat (Just (==)) Segment.num_interpolate
