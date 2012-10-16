module Util.TimeVector_test where
import qualified Util.Seq as Seq
import Util.Test

import qualified Util.TimeVector as V
import Util.TimeVector (X)


type Y = Double

signal :: [(X, Y)] -> V.Unboxed
signal = V.signal

unsignal :: V.Unboxed -> [(X, Y)]
unsignal = V.unsignal

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

-- * transformation

test_merge = do
    let f = unsignal . V.merge . map signal
    equal (f []) []
    equal (f [[(0, 0), (1, 1)]]) [(0, 0), (1, 1)]
    equal (f [[(1, 1)], [(2, 2)], [(3, 3)]])
        [(1, 1), (2, 2), (3, 3)]
    equal (f [[(0, 0), (1, 1), (2, 2)], [(1, 3), (2, 4), (3, 5)]])
        [(0, 0), (1, 3), (2, 4), (3, 5)]

test_shift = do
    let vec = signal [(0, 1), (1, 0)]
    let shift x = unsignal $ V.shift x vec
    equal (shift 0) [(0, 1), (1, 0)]
    equal (shift 1) [(1, 1), (2, 0)]

test_truncate = do
    let vec = signal [(0, 0), (1, 1), (2, 0)]
    let f p = unsignal . V.truncate p
    equal (f 0 vec) []
    equal (f 1 vec) [(0, 0)]
    equal (f 2 vec) [(0, 0), (1, 1)]
    equal (f 3 vec) [(0, 0), (1, 1), (2, 0)]

test_drop_before = do
    let vec = signal [(2, 0), (4, 1)]
    let f p = unsignal $ V.drop_before p vec
    equal (f 0) [(2, 0), (4, 1)]
    equal (f 2) [(2, 0), (4, 1)]
    equal (f 3) [(2, 0), (4, 1)]
    equal (f 4) [(4, 1)]
    equal (f 900) [(4, 1)]
    equal (unsignal (V.drop_before 1 (signal []))) []

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
