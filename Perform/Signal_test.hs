-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Signal_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Perform.Signal (signal, unsignal)
import Global


test_constant_val = do
    let f = Signal.constant_val . signal
    equal (f []) (Just 0)
    equal (f [(0, 0)]) (Just 0)
    equal (f [(3, 0)]) (Just 0)
    equal (f [(3, 2)]) Nothing
    equal (Signal.constant_val $ Signal.constant 1 <> signal [(3, 1)]) (Just 1)
    equal (Signal.constant_val $ Signal.shift 10 $ Signal.constant 1) (Just 1)

test_before = do
    let f x = Signal.before x . signal
    equal (f 2 [(2, 1), (2, 2)]) 0
    equal (f 3 [(2, 1), (2, 2)]) 2

test_at_linear = do
    let f vec x = Signal.at_linear x (signal vec)
    equal (map (f [(2, 2), (4, 0)]) (Seq.range 0 5 1)) [0, 0, 2, 1, 0, 0]
    equal (map (f [(2, 0), (4, 2)]) (Seq.range 0 5 1)) [0, 0, 0, 1, 2, 2]
    equal (f [(0, 2), (2, 0)] (-1)) 0
    equal (f [(-1, 2), (2, 0)] (-2)) 0

test_at_linear_extend = do
    let f vec x = Signal.at_linear_extend x (signal vec)
    equal (map (f []) (Seq.range (-2) 2 1)) [-2, -1, 0, 1, 2]
    equal (map (f [(0, 1)]) (Seq.range (-2) 2 1)) [-1, 0, 1, 2, 3]
    equal (map (f [(0, 0), (1, 1), (2, 3)]) (Seq.range (-2) 3 1))
        [-2, -1, 0, 1, 3, 5]
    equal (map (f [(2, 2), (4, 0)]) (Seq.range 0 5 1)) [4, 3, 2, 1, 0, -1]
    equal (map (f [(2, 0), (4, 2)]) (Seq.range 0 5 1)) [-2, -1, 0, 1, 2, 3]

test_inverse_at_extend = do
    let f sig y = Signal.inverse_at_extend y (signal sig)
    let trip sig ys = (map (roundtrip sig) ys, ys)
        roundtrip sig_ y = Signal.at_linear_extend
                (Signal.inverse_at_extend y sig) sig
            where sig = signal sig_
    equal (map (f []) [-1, 0, 1]) [-1, 0, 1]
    uncurry equal (trip [] [-1, 0, 1])
    equal (map (f [(1, 2)]) [1, 2, 3]) [0, 1, 2]
    uncurry equal (trip [(1, 2)] [1, 2, 3])

    let complicated = [(0, 0), (1, 2), (2, 2), (4, 4)]
    equal (map (f complicated) [-1 .. 5])
        [-0.5, 0, 0.5, 2, 3, 4, 5]
    uncurry equal (trip complicated [-1 .. 5])
    equal (f [(0, 0), (1, 0)] 1) RealTime.large

-- * transformation

test_scale = do
    let f = Signal.scale 0.5
    equal (map f [-1, -0.5, 0, 0.5, 1]) [0, 0.25, 0.5, 0.75, 1]
    let invert = Signal.scale 0.5 . Signal.scale_invert 0.5
    equal (map invert [-1, -0.5, 0, 0.5, 1]) [-1, -0.5, 0, 0.5, 1]

test_compose = do
    let f s1 s2 = unsignal $ Signal.compose (signal s1) (signal s2)
    -- They cancel each other out.
    equal (f [(0, 0), (10, 20)] [(0, 0), (1, 0.5), (2, 1)])
        [(0, 0), (1, 1), (2, 2)]
    let lin = [(0, 0), (1, 1), (2, 2), (3, 3), (100, 100)]
        slow = [(0, 0), (1, 2), (2, 4), (3, 6)]
    equal (f lin lin) lin
    equal (f lin slow) slow
    -- Since the signals are not the same length it looks funny at the end.
    -- But this should never be audible since a sub-block's warp shouldn't
    -- be longer than its parent's.
    equal (f slow lin) [(0, 0), (1, 2), (2, 4), (3, 6), (100, 6)]

    equal (f (segments (-2) [(1, 6)]) (segments 0 [(1, 4)]))
        [(0, 2), (1, 3), (2, 4), (3, 5), (4, 6)]

test_compose_hybrid = do
    let f start1 sig1 start2 sig2 = map snd $ unsignal $
            Signal.compose_hybrid (Signal.signal (segments start1 sig1))
                (Signal.signal (segments start2 sig2))

    -- 2 with 2 results in 4
    equal (f 0 [(2, 4)] 0 [(2, 2)]) [0, 4, 8]

    -- 2 with 0 results in 1
    equal (f 0 [(2, 4)] 0 [(0, 2)]) [0, 1, 2]

    -- 2 with 0, 1 results in 1, 2
    equal (f 0 [(2, 8)] 0 [(0, 2), (1, 2)]) [0, 1, 2, 4, 6]

    -- 2 with 2, 0, 1 results in 4, 1, 2
    equal (f 0 [(2, 8)] 0 [(2, 2), (0, 2), (1, 2)])
        [0, 4, 8, 9, 10, 12, 14]
    -- 0.5 with 2, 0, 1 results in 1, 1, 0.5
    equal (f 0 [(0.5, 14)] 0 [(2, 2), (0, 3), (1, 2)])
        [0, 1, 2, 3, 4, 5, 5.5, 6]

    -- 2 with 1, 0, 1, 0 results in [2, 1, 2, 1]
    equal (f 0 [(2, 8)] 0 [(1, 2), (0, 2), (1, 2), (0, 2)])
        [0, 2, 4, 5, 6, 8, 10, 11, 12]

    -- Offset
    equal (f (-2) [(1, 10)] 0 [(1, 2)]) [2, 3, 4]
    equal (f (-1) [(2, 10)] 0 [(1, 2), (0, 2)]) [2, 4, 6, 7, 8]
    equal (f (-1) [(2, 10)] 0 [(0, 2), (1, 2)]) [2, 3, 4, 6, 8]

segments :: Signal.X -> [(Signal.Y, Int)] -> [(Signal.X, Signal.Y)]
segments start = zip (Seq.range_ start 1) . scanl (+) 0
    . concatMap (uncurry (flip replicate))

test_integrate = do
    let f = unsignal . Signal.integrate 1 . signal
    equal (f [(0, 1), (3, 2)])
        [(0, 0), (1, 1), (2, 2), (3, 3)]
    equal (f [(1, 1), (3, 2)])
        [(0, 0), (1, 0), (2, 1), (3, 2)]
    equal (f [(0, 0), (1, 1), (2, 2), (4, 2)])
        [(0, 0), (1, 0), (2, 1), (3, 3), (4, 5)]
    equal (f [(0, 0), (1, -1), (2, -2), (3, -3)])
        [(0, 0), (1, 0), (2, -1), (3, -3)]

test_sig_add = do
    let f s1 s2 = unsignal $ Signal.sig_add (signal s1) (signal s2)
    equal (f [(0, 1)] [(1, 1)]) [(0, 1), (1, 2)]
    -- An identity signal means the other signal is returned directly.
    equal (f [(1, 2)] [(0, 0)]) [(1, 2)]

test_sig_subtract = do
    let f s1 s2 = unsignal $ Signal.sig_subtract (signal s1) (signal s2)
    equal (f [(0, 0)] [(0, 2)]) [(0, -2)]
    equal (f [(0, 1)] [(0, 2)]) [(0, -1)]

-- * comparison

test_pitches_share = do
    let sample0 sig = fromIntegral (floor y)
            where Just (_, y) = Seq.head sig
    let f start end sig0 sig1 = Signal.pitches_share False start end
            (sample0 sig0) (signal sig0) (sample0 sig1) (signal sig1)

    -- Different signals.
    equal (f 0 1 [(0, 0), (1, 1)] [(0, 1), (1, 0)]) False
    -- Separated by an integer can share.
    equal (f 0 1 [(0, 0), (1, 1)] [(0, 1), (1, 2)]) True
    -- Separated by 0 can't share.
    equal (f 0 1 [(0, 0), (1, 1)] [(0, 0), (1, 1)]) False
    equal (Signal.pitches_share False 1 2 1 (signal [(0, 1)]) 1
            (signal [(1, 1)]))
        False
    -- Except when one note is in decay.
    equal (Signal.pitches_share True 1 2 1 (signal [(0, 1)]) 1
            (signal [(1, 1)]))
        True

    -- Not an integral difference.
    equal (f 1 3 [(1, 60.01)] [(1, 61)]) False
    -- But this difference is inaudible.
    equal (f 1 3 [(1, 60.001)] [(1, 61)]) True

    -- Signals with different starting times.
    equal (f 1 3 [(1, 61), (10, 61)] [(0, 60), (10, 60)]) True

    -- one pitch changes but the other doesn't, so they can't share
    equal (f 0 3 [(0, 0), (1, 1)] [(0, 2)]) False
    equal (f 1 5 [(1, 74), (2, 76), (3, 74)] [(0, 48)]) False
