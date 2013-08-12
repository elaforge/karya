-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Signal_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Perform.Signal as Signal


signal = Signal.signal
unsignal = Signal.unsignal

test_is_constant = do
    let f = Signal.is_constant . signal
    equal (f []) True
    equal (f [(0, 0)]) True
    equal (f [(3, 0)]) True
    equal (f [(3, 2)]) False
    equal (f [(0, 1), (3, 1)]) True

test_at_linear = do
    let f vec x = Signal.at_linear x (signal vec)
    equal (map (f [(2, 2), (4, 0)]) (Seq.range 0 5 1)) [0, 0, 2, 1, 0, 0]
    equal (f [(0, 2), (2, 0)] (-1)) 0
    equal (f [(-1, 2), (2, 0)] (-2)) 0

-- * transformation

test_inverse_at = do
    let f sig pos = Signal.inverse_at pos (signal sig)
    equal (map (f [(0, 0), (2, 2)]) (Seq.range 0 3 1))
        [Just 0, Just 1, Just 2, Nothing]
    equal (map (f [(1, 1), (2, 2)]) (Seq.range 0 3 1))
        [Just 0, Just 1, Just 2, Nothing]
    -- Flat curve.
    equal (map (f [(1, 1), (2, 1), (3, 2)]) (Seq.range 0 3 1))
        [Just 0, Just 1, Just 3, Nothing]
    -- Vertical discontinuity.  Signals shouldn't have those but it doesn't
    -- hurt to handle them correctly anyway.
    equal (map (f [(1, 1), (1, 2), (2, 3)]) (Seq.range 0 4 1))
        [Just 0, Just 1, Just 1, Just 2, Nothing]

    equal (f [(0, 0), (1, 1)] 5) Nothing
    equal (f [(0, 0), (1, 1)] (-1)) (Just (-1))

test_compose = do
    let sig0 = signal [(0, 0), (10, 20)]
        sig1 = signal [(0, 0), (1, 0.5), (2, 1)]
    let f = Signal.compose
    -- They cancel each other out.
    equal (unsignal (f sig0 sig1)) [(0, 0), (1, 1), (2, 2)]
    let lin = signal [(0, 0), (1, 1), (2, 2), (3, 3), (100, 100)]
        slow = signal [(0, 0), (1, 2), (2, 4), (3, 6)]
    equal (f lin lin) lin
    equal (f lin slow) slow
    -- Since the signals are not the same length it looks funny at the end.
    -- But this should never be audible since a sub-block's warp shouldn't
    -- be longer than its parent's.
    equal (f slow lin) $
        signal [(0, 0), (1, 2), (2, 4), (3, 6), (100, 6)]

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

-- * comparison

test_pitches_share = do
    let sample0 sig = fromIntegral (floor y)
            where Just (_, y) = Signal.head sig
    let f start end sig0 sig1 = Signal.pitches_share False start end
            (sample0 sig0) sig0 (sample0 sig1) sig1

    -- Different signals.
    equal (f 0 1 (signal [(0, 0), (1, 1)]) (signal [(0, 1), (1, 0)])) False
    -- Separated by an integer can share.
    equal (f 0 1 (signal [(0, 0), (1, 1)]) (signal [(0, 1), (1, 2)])) True
    -- Separated by 0 can't share.
    equal (f 0 1 (signal [(0, 0), (1, 1)]) (signal [(0, 0), (1, 1)])) False
    equal (Signal.pitches_share False 1 2 1 (signal [(0, 1)]) 1
            (signal [(1, 1)]))
        False
    -- Except when one note is in decay.
    equal (Signal.pitches_share True 1 2 1 (signal [(0, 1)]) 1
            (signal [(1, 1)]))
        True

    -- Not an integral difference.
    equal (f 1 3 (signal [(1, 60.01)]) (signal [(1, 61)])) False
    -- But this difference is inaudible.
    equal (f 1 3 (signal [(1, 60.001)]) (signal [(1, 61)])) True

    -- Signals with different starting times.
    equal (f 1 3 (signal [(1, 61), (10, 61)]) (signal [(0, 60), (10, 60)]))
        True

    -- one pitch changes but the other doesn't, so they can't share
    equal (f 0 3 (signal [(0, 0), (1, 1)]) (signal [(0, 2)])) False
    equal (f 1 5 (signal [(1, 74), (2, 76), (3, 74)]) (signal [(0, 48)])) False
