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
import qualified Util.Lists as Lists
import qualified Util.TimeVector as TimeVector

import qualified Perform.RealTime as RealTime
import           Perform.RealTime (large)
import qualified Ui.Types as Types

import           Types
import           Util.Test


type Y = Double

test_from_pairs :: Test
test_from_pairs = do
    let f = toSegments . fromPairs
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

test_constant_val_from :: Test
test_constant_val_from = do
    let f = Segment.constantValFrom
    equal (f 0 $ fromPairs [(2, 1)]) Nothing
    equal (f 0 $ Segment.shift (-2) (fromPairs [(2, 1)])) (Just 1)
    equal (f 0 $ Segment.shift (-2) (fromPairs [(2, 1), (4, 1)])) (Just 1)

test_concat :: Test
test_concat = do
    let f = toPairs . numConcat . map fromPairs
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
    (s1, s2) <- Hedgehog.forAll $ (,) <$> genSignal <*> genSignal
    let xs = map fst $ toPairs $ numConcat [s1, s2]
    xs === List.sort xs

test_concat_dups :: Test
test_concat_dups = hedgehog $ Hedgehog.withTests 500 $ property $ do
    sigs <- Hedgehog.forAll $ Gen.list (Range.linear 0 4) genSignal
    let xs = map fst $ toPairs $ numConcat sigs
    filter ((>2) . length) (List.group xs) === []

test_prepend :: Test
test_prepend = do
    let f sig1 sig2 = toPairs $
            Segment.prepend (Just (==)) Segment.numInterpolate
                (fromPairs sig1) (fromPairs sig2)
    equal (f [] []) []
    equal (f [] [(0, 0), (1, 1)]) [(0, 0), (1, 1)]
    equal (f [(0, 10), (1, 1)] [(0, 0), (1, 1), (2, 2)])
        [(0, 10), (1, 1), (2, 2)]

test_segment_at :: Test
test_segment_at = do
    let f sig = Segment.segmentAtOrientation Types.Positive (fromPairs sig)
    equal (f [] 0) Nothing
    equal (f [(1, 1)] 0) Nothing
    equal (f [(1, 1)] 1) $ Just (Segment 1 1 large 1)
    equal (f [(1, 1)] 2) $ Just (Segment 1 1 large 1)
    equal (f [(1, 1), (2, 2), (3, 3)] 2) $ Just (Segment 2 2 3 3)
    equal (f [(1, 1), (3, 3)] 2) $ Just (Segment 1 1 3 3)
    -- Positive bias.
    equal (f [(0, 0), (2, 0), (2, 2)] 2) $ Just (Segment 2 2 large 2)
    -- Shift.
    equal (Segment.segmentAt
            (Segment.shift 3 (fromPairs [(0, 0), (2, 2)])) 4) $
        Just (Segment 3 0 5 2)

test_segment_at_negative :: Test
test_segment_at_negative = do
    let f sig = Segment.segmentAtOrientation Types.Negative (fromPairs sig)
    equal (f [] 0) Nothing
    equal (f [(1, 1)] 0) Nothing
    equal (f [(1, 1)] 1) $ Just (Segment 1 1 large 1)
    equal (f [(1, 1)] 2) $ Just (Segment 1 1 large 1)
    equal (f [(1, 1), (2, 2), (3, 3)] 2) $ Just (Segment 1 1 2 2)
    equal (f [(1, 1), (3, 3)] 2) $ Just (Segment 1 1 3 3)
    -- Negative bias.
    equal (f [(0, 0), (2, 0), (2, 2)] 2) $ Just (Segment 0 0 2 0)

test_at :: Test
test_at = do
    let f = Segment.at Segment.numInterpolate
            (fromPairs [(1, 1), (2, 2), (2, 3)])
    equal (map f [0, 1, 1.5, 2, 3, 4])
        [Nothing, Just 1, Just 1.5, Just 3, Just 3, Just 3]

test_shift :: Test
test_shift = do
    let shift = Segment.shift
    let at = Segment.at Segment.numInterpolate
    equal (at (fromPairs [(2, 2)]) 2) (Just 2)
    equal (at (shift 2 (fromPairs [(2, 2)])) 2) Nothing
    equal (at (shift (-2) (fromPairs [(2, 2)])) 0) (Just 2)
    equal (at (shift (-1) (fromPairs [(1, 1), (2, 0.5)])) 0) (Just 1)
    equal (at (shift (-1) (fromPairs [(1, 1), (2, 0.5)])) 0.5) (Just 0.75)

    let shifted = shift 2 (fromPairs [(2, 2)])
    equal (toPairs shifted) [(4, 2)]
    equal (Segment.head shifted) (Just (4, 2))
    equal (Segment.last shifted) (Just (4, 2))
    equal (Segment.dropAfter 3 shifted) Segment.empty
    equal (Segment.dropAfter 4 shifted) shifted

    let shifted = shift 2 (fromPairs [(2, 2), (3, 3)])
    equal (toPairs $ Segment.dropBefore 5 shifted) [(5, 3)]
    equal (Segment.dropBefore 4 shifted) shifted

test_drop_after_clip_after :: Test
test_drop_after_clip_after = do
    let f x sig =
            ( toPairs $ Segment.dropAfter x $ fromPairs sig
            , toPairs $ Segment.clipAfter Segment.numInterpolate x $
                fromPairs sig
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
    let f x = toPairs . Segment.numClipAfter False x . fromPairs
    equal (f 2 [(0, 0), (1, 1), (4, 1)]) [(0, 0), (1, 1)]
    equal (f 2 [(0, 0), (2, 0), (2, 2)]) [(0, 0)]
    equal (f 2 [(0, 0), (4, 4)]) [(0, 0), (2, 2)]
    equal (f 2 [(2, 1)]) []
    let f2 = Segment.numClipAfter False
    equal (at (f2 2 (Segment.constant 1)) 0) (Just 1)

test_drop_before_clip_before :: Test
test_drop_before_clip_before = do
    let f x sig =
            ( toPairs $ Segment.dropBefore x $ fromPairs sig
            , toPairs $ Segment.clipBefore Segment.numInterpolate x $
                fromPairs sig
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
    let f = toPairs . Segment.integrate 1 . fromPairs
    equal (f []) []
    equal (f [(0, 1)]) [(0, 0), (large, largeY)]
    equal (f [(0, 0), (4, 4)])
        [ (0, 0), (1, 0.5), (2, 2), (3, 4.5), (4, 8)
        , (large, 8 + 4 * (largeY - 4))
        ]
    equal (f [(0, 1), (3, 2)])
        [ (0, 0), (1, 1+1/6), (2, 2+2/3), (3, 3+3/2)
        , (large, 3+3/2 + 2 * (largeY - 3))
        ]
    equal (f [(0, 0), (1, 1), (2, 2), (4, 2)])
        [ (0, 0), (1, 0.5), (2, 2), (4, 6), (4, 6)
        , (large, 6 + 2 * (largeY - 4))
        ]

test_resample_rate :: Test
test_resample_rate = do
    let f = toPairs . Segment.resampleRate 1 . fromPairs
    equal (f [(0, 2), (4, 2), (4, 1)]) [(0, 2), (4, 2), (4, 1)]
    equal (f [(0, 0), (4, 4)]) [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
    equal (f [(0, 0), (2, 2), (2, 0), (4, 2)])
        [(0, 0), (1, 1), (2, 2), (2, 0), (3, 1), (4, 2)]

test_map_y :: Test
test_map_y = do
    let f = toPairs . Segment.mapY 1 (1/) . fromPairs
    equal (f [(0, 5), (4, 1)]) [(0, 1/5), (1, 1/4), (2, 1/3), (3, 1/2), (4, 1)]
    equal (f [(0, 2), (4, 2), (4, 1)]) [(0, 1/2), (4, 1/2), (4, 1)]

test_linear_operator :: Test
test_linear_operator = do
    let f s1 s2 = toSegments $
            Segment.linearOperator (+) (fromPairs s1) (fromPairs s2)
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

test_linear_operator_constant :: Test
test_linear_operator_constant = do
    let f s1 s2 = Segment.linearOperator (+) s1 s2
    equal (f (Segment.constant 1) (Segment.constant 2))
        (Segment.Constant 3)
    equal (f (Segment.constant 1) (fromPairs [(1, 3)]))
        (fromPairs [(Segment.beginning, 1), (1, 1), (1, 4)])

test_linear_operator2 :: Test
test_linear_operator2 = do
    let f s1 s2 = toSegments $
            Segment._linearOperator2 Num.sum (fromPairs s1) (fromPairs s2)
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
    let f = TimeVector.toPairs . Segment.toPiecewiseConstant 1 . fromPairs
    equal (f []) []
    equal (f [(4, 2)]) [(4, 2)]
    equal (f [(2, 2), (4, 2), (4, 4)]) [(2, 2), (4, 4)]
    equal (f [(2, 2), (4, 4)]) [(2, 2), (3, 3), (4, 4)]
    equal (f [(2, 2), (2, 2), (2, 2)]) [(2, 2)]

test_drop_discontinuity_at :: Test
test_drop_discontinuity_at = do
    let f x = toPairs . Segment.dropDiscontinuityAt x . fromPairs
    equal (f 1 [(0, 0), (1, 0), (1, 1), (2, 1)])
        [(0, 0), (2, 0), (2, 1)]
    -- Don't get >2 2s in a row.
    equal (f 1 [(0, 0), (1, 0), (1, 1), (2, 1), (2, 0)])
        [(0, 0), (2, 0), (2, 0)]

test_invert :: Test
test_invert = do
    let f = toPairs . Segment.invert
    equal (f $ fromPairs [(0, 1), (1, 2)]) [(1, 0), (2, 1)]
    equal (f $ Segment.shift (-2) $ fromPairs [(2, 1), (3, 2)])
        [(1, 0), (2, 1)]

-- * hedgehog

-- | Xs are ascending.
test_from_samples_ascending :: Test
test_from_samples_ascending = hedgehog $ property $ do
    samples <- Hedgehog.forAll genSamples
    let xs = map fst $ toPairs $ fromPairs samples
    xs === List.sort xs

-- | Never more than 2 Xs with the same value.
test_from_samples_dups :: Test
test_from_samples_dups = hedgehog $ property $ do
    samples <- Hedgehog.forAll genSamples
    let xs = map fst $ toPairs $ fromPairs samples
    Hedgehog.annotateShow xs
    filter ((>2) . length) (List.group xs) === []

genSamples :: Hedgehog.Gen [(X, Y)]
genSamples = Gen.list (Range.linear 0 16) $
    (,) <$> genIntegralX <*> Gen.element [-1, 1]

-- | Integral RealTimes to encourage collisions.
genIntegralX :: Hedgehog.Gen RealTime
genIntegralX = RealTime.seconds . fromIntegral <$>
    Gen.int (Range.linear (-4) 4)

genSignal :: Hedgehog.Gen Segment.NumSignal
genSignal = fromPairs . Lists.sortOn fst <$> genSamples

-- * util

at :: Segment.NumSignal -> X -> Maybe Y
at = Segment.at Segment.numInterpolate

largeY :: Y
largeY = RealTime.toSeconds large

toSegments :: Segment.NumSignal -> [((X, Y), (X, Y))]
toSegments = map (\(Segment.Segment x1 y1 x2 y2) -> ((x1, y1), (x2, y2)))
    . Segment.toSegments

fromPairs :: [(X, Y)] -> Segment.NumSignal
fromPairs = Segment.fromPairs

toPairs :: Segment.NumSignal -> [(X, Y)]
toPairs = Segment.toPairs

constant :: Y -> Segment.NumSignal
constant = Segment.constant

numConcat :: [Segment.NumSignal] -> Segment.NumSignal
numConcat = Segment.concat (Just (==)) Segment.numInterpolate
