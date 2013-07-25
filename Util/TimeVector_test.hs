-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.TimeVector_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.TimeVector as V
import Util.TimeVector (X)

import qualified Perform.RealTime as RealTime


type Y = Double

signal :: [(X, Y)] -> V.Unboxed
signal = V.signal

unsignal :: V.Unboxed -> [(X, Y)]
unsignal = V.unsignal

unsample :: V.Sample y -> (X, y)
unsample s = (V.sx s, V.sy s)

test_at = do
    let range low high sig = [V.at p (signal sig)
            | p <- Seq.range low (high-1) 1] :: [Maybe Y]
    equal (range 0 4 []) (replicate 4 Nothing)
    equal (range 0 5 (zip (Seq.range_ 0 1) [0, 0.25, 0.5, 0.75, 1]))
        (map Just [0, 0.25, 0.5, 0.75, 1])

    -- Values before the first sample are Nothing.
    equal (range 0 4 [(2, 1)]) [Nothing, Nothing, Just 1, Just 1]
    equal (range 0 4 [(0, 1), (3, 1)]) (map Just [1, 1, 1, 1])

    -- A sample at <=0 implies that the sample extends back indefinitely.
    let f p sig = V.at p (signal sig)
    equal (f (-1) [(0, 2)]) (Just 2)
    equal (f (-1) [(1, 2)]) Nothing
    equal (f 0 [(1, 2)]) Nothing

test_at_eta = do
    let f x sig = V.at x (signal sig)
    equal (f 6.666666666666664 [(6.666666666666666, 1)]) (Just 1)
    equal (f 6.6666666665 [(6.666666666666666, 1)]) Nothing

test_ascending = do
    let f p = map unsample $ V.ascending p (signal [(1, 1), (2, 2), (3, 3)])
    equal (f 2) [(2, 2), (3, 3)]
    equal (f 3) [(3, 3)]
    equal (f 4) []

test_descending = do
    let f p = map unsample $ V.descending p (signal [(1, 1), (2, 2), (3, 3)])
    equal (f 3) [(2, 2), (1, 1)]
    equal (f 2) [(1, 1)]
    equal (f 1) []


-- * transformation

test_merge = do
    let f = unsignal . V.merge . map signal
    equal (f []) []
    equal (f [[(0, 0), (1, 1)]]) [(0, 0), (1, 1)]
    equal (f [[(1, 1)], [(2, 2)], [(3, 3)]])
        [(1, 1), (2, 2), (3, 3)]
    equal (f [[(0, 0), (1, 1), (2, 2)], [(1, 3), (2, 4), (3, 5)]])
        [(0, 0), (1, 3), (2, 4), (3, 5)]
    equal (f [[(0, 0), (1, 1)], [(1, 1), (2, 2)]])
        [(0, 0), (1, 1), (2, 2)]

test_interleave = do
    let f v1 v2 = unsignal $ V.interleave (signal v1) (signal v2)
    equal (f [] []) []
    equal (f [(0, 0)] []) [(0, 0)]
    equal (f [(0, 0)] [(0, 1)]) [(0, 0), (0, 1)]
    equal (f [(0, 0)] [(1, 1)]) [(0, 0), (1, 1)]
    equal (f [(0, 0), (2, 2)] [(1, 1)]) [(0, 0), (1, 1), (2, 2)]

test_shift = do
    let vec = signal [(0, 1), (1, 0)]
    let shift x = unsignal $ V.shift x vec
    equal (shift 0) [(0, 1), (1, 0)]
    equal (shift 1) [(1, 1), (2, 0)]

test_drop_after = do
    let f x = unsignal . V.drop_after x . signal
    let vec = [(0, 0), (1, 1), (2, 0)]
    equal (f (-1) vec) [(0, 0)]
    equal (f 0 vec) [(0, 0)]
    equal (f 1 vec) [(0, 0)]
    equal (f 2 vec) [(0, 0), (1, 1)]
    equal (f 3 vec) [(0, 0), (1, 1), (2, 0)]
    equal (f 1 [(0, 0), (1 - RealTime.eta, 1)]) [(0, 0)]

test_drop_before = do
    let f x = unsignal . V.drop_before x . signal
    let vec = [(2, 0), (4, 1)]
    equal (f 0 vec) [(2, 0), (4, 1)]
    equal (f 2 vec) [(2, 0), (4, 1)]
    equal (f 3 vec) [(2, 0), (4, 1)]
    equal (f 4 vec) [(4, 1)]
    equal (f 900 vec) [(4, 1)]
    equal (f 1 []) []

test_drop_before_strict = do
    let f x = unsignal $ V.drop_before_strict x (signal [(2, 0), (4, 1)])
    equal (f 0) [(2, 0), (4, 1)]
    equal (f 2) [(2, 0), (4, 1)]
    equal (f 3) [(4, 1)]
    equal (f 5) []

test_sig_op = do
    let f vec0 vec1 = unsignal $ V.sig_op 0 (+) (signal vec0) (signal vec1)
    equal (f [(1, 1)] []) [(1, 1)]
    equal (f [] [(1, 1)]) [(1, 1)]
    equal (f [(0, 0), (2, 2), (4, 0)] [(0, 1)])
        [(0, 1), (2, 3), (4, 1)]
    equal (f [(0, 0), (2, 2), (4, 0)] [(1, 1), (3, 0)])
        [(0, 0), (1, 1), (2, 3), (3, 2), (4, 0)]

test_concat_map_accum = do
    let go accum x0 y0 x1 y1 = (accum+1, [V.Sample x0 y0, V.Sample x1 y1])
        final accum (V.Sample x y) = [V.Sample (x*10) (y*10+accum)]
        f vec = unsignal (V.concat_map_accum 0 go final 0 (signal vec))
    equal (f []) []
    equal (f [(0, 0), (1, 1), (2, 2)])
        [ (0, 0), (0, 0)
        , (0, 0), (1, 1)
        , (1, 1), (2, 2)
        , (20, 23)
        ]
