module Perform.SignalBase_test where

import Util.Test

import Perform.Signal (Y) -- use the instances for Y from Signal
import qualified Perform.SignalBase as SignalBase
import Perform.SignalBase (X, Method(..))


mkvec :: [(X, Y)] -> SignalBase.SigVec Y
mkvec = SignalBase.signal

unvec :: SignalBase.SigVec Y -> [(X, Y)]
unvec = SignalBase.unsignal

test_track_signal = do
    let f = unvec . SignalBase.track_signal 1
    -- Make sure Set and Linear work as expected.
    equal (f [(0, Set, 1), (2, Linear, 2)])
        [(0, 1), (1, 1.5), (2, 2)]
    equal (f [(0, Set, 1), (2, Set, 2)])
        [(0, 1), (2, 2)]
    equal (f [(0, Set, 1), (2, Set, 2), (4, Linear, 0), (5, Set, 1)])
        [(0, 1), (2, 2), (3, 1), (4, 0), (5, 1)]

test_sample_function = do
    let f x0 y0 x1 y1 = SignalBase.sample_function id 1 x0 y0 x1 y1 :: [(X, Y)]
    -- Includes the end, not the beginning.
    equal (f 0 0 0 0) []
    equal (f 0 0 0.5 0.5) [(0.5, 0.5)]
    equal (f 0 0 1 1) [(1, 1)]
    equal (f 0 0 3 3) [(1, 1), (2, 2), (3, 3)]


test_at = do
    let tsig = SignalBase.track_signal 1
    let range low high sig =
                map (\p -> SignalBase.at p (tsig sig)) [low..high-1] :: [Y]
    equal (range 0 4 []) [0, 0, 0, 0]
    -- Values before the first sample take its value.
    equal (range 0 4 [(2, Set, 1)]) [1, 1, 1, 1]
    equal (range 0 4 [(0, Set, 1), (3, Set, 1)]) [1, 1, 1, 1]

    equal (range 0 5 [(0, Set, 0), (4, Linear, 1)])
        [0, 0.25, 0.5, 0.75, 1]
    equal (range 0 6 [(0, Set, 0), (4, Linear, 1), (4, Set, 0)])
        [0, 0.25, 0.5, 0.75, 0, 0]

    equal (range 0 5 [(0, Linear, 1), (4, Linear, 0)])
        [1, 0.75, 0.5, 0.25, 0]

    -- Negative index is ok.
    equal (SignalBase.at (-1) (mkvec [(0, 2)])) 2

test_at_linear = do
    let f vec x = SignalBase.at_linear x vec
    equal (map (f (mkvec [(2, 2), (4, 0)])) [0..5]) [2, 2, 2, 1, 0, 0]
    equal (f (mkvec [(0, 2), (2, 0)]) (-1)) 2

-- * transformation

test_merge = do
    let f = unvec . SignalBase.merge . map mkvec
    equal (f []) []
    equal (f [[(0, 0), (1, 1)]]) [(0, 0), (1, 1)]
    equal (f [[(1, 1)], [(2, 2)], [(3, 3)]])
        [(1, 1), (2, 2), (3, 3)]
    equal (f [[(0, 0), (1, 1), (2, 2)], [(1, 3), (2, 4), (3, 5)]])
        [(0, 0), (1, 3), (2, 4), (3, 5)]

test_shift = do
    let vec = mkvec [(0, 1), (1, 0)]
    let shift x = unvec $ SignalBase.shift x vec
    equal (shift 0) [(0, 1), (1, 0)]
    equal (shift 1) [(1, 1), (2, 0)]

test_truncate = do
    let vec = mkvec [(0, 0), (1, 1), (2, 0)]
    let f p = unvec . SignalBase.truncate p
    equal (f 0 vec) []
    equal (f 1 vec) [(0, 0)]
    equal (f 2 vec) [(0, 0), (1, 1)]
    equal (f 3 vec) [(0, 0), (1, 1), (2, 0)]

test_sig_op = do
    let f vec0 vec1 = unvec $ SignalBase.sig_op (+) (mkvec vec0) (mkvec vec1)
    equal (f [(0, 0), (2, 2), (4, 0)] [(0, 1)])
        [(0, 1), (2, 3), (4, 1)]
    equal (f [(0, 0), (2, 2), (4, 0)] [(1, 1), (3, 0)])
        [(0, 0), (1, 1), (2, 3), (3, 2), (4, 0)]

test_map_signal_accum = do
    let go accum x0 y0 x1 y1 = (accum+1, [(x0, y0), (x1, y1)])
        final accum (x, y) = [(x*10, y*10+accum)]
        f vec = unvec (SignalBase.map_signal_accum go final 0 (mkvec vec))
    equal (f []) []
    equal (f [(0, 0), (1, 1), (2, 2)])
        [ (0, 0), (0, 0)
        , (0, 0), (1, 1)
        , (1, 1), (2, 2)
        , (20, 23)
        ]

test_resample_to_list = do
    let f vec0 vec1 = SignalBase.resample_to_list (mkvec vec0) (mkvec vec1)
    equal (f [(1, 1), (2, 2)] []) [(1, 1, 0), (2, 2, 0)]
    equal (f [] [(1, 1), (2, 2)]) [(1, 0, 1), (2, 0, 2)]
    equal (f [(1, 1), (2, 2)] [(1, 3), (2, 4)])
        [(1, 1, 3), (2, 2, 4)]
    equal (f [(1, 1)] [(0, 2), (2, 4), (3, 6)])
        [(0, 0, 2), (1, 1, 2), (2, 1, 4), (3, 1, 6)]
