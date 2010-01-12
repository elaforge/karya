module Perform.Signal_test where
import Util.Test
import qualified Util.Seq as Seq

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Signal as Signal


mksig = Signal.signal
unsig = Signal.unpack

-- * transformation

test_inverse_at = do
    let f sig ts = Signal.inverse_at (mksig sig) (Timestamp.seconds ts)
    equal (map (f [(0, 0), (2, 2)]) [0..3])
        [Just 0, Just 1, Just 2, Nothing]
    equal (map (f [(1, 1), (2, 2)]) [0..3])
        [Just 0, Just 1, Just 2, Nothing]
    -- Flat curve.
    equal (map (f [(1, 1), (2, 1), (3, 2)]) [0..3])
        [Just 0, Just 1, Just 3, Nothing]
    -- Vertical discontinuity.  Signals shouldn't have those but it doesn't
    -- hurt to handle them correctly anyway.
    equal (map (f [(1, 1), (1, 2), (2, 3)]) [0..4])
        [Just 0, Just 1, Just 1, Just 2, Nothing]

test_compose = do
    let f = mksig [(0, 0), (10, 20)]
        g = mksig [(0, 0), (1, 0.5), (2, 1)]
    -- They cancel each other out.
    equal (unsig (Signal.compose f g))
        [(0, 0), (1, 1), (2, 2)]
    let lin = mksig [(0, 0), (1, 1), (2, 2), (3, 3), (100, 100)]
        slow = mksig [(0, 0), (1, 2), (2, 4), (3, 6)]
    equal (Signal.compose lin lin) lin
    equal (Signal.compose lin slow) slow
    -- Since the signals are not the same length it looks funny at the end.
    -- But this should never be audible since a sub-block's warp shouldn't
    -- be longer than its parent's.
    equal (Signal.compose slow lin) $
        mksig [(0, 0), (1, 2), (2, 4), (3, 6), (100, 6)]

test_integrate = do
    let f sig = Seq.rdrop (length Signal._extra_samples - 1) $
            unsig $ Signal.integrate 1 sig
    equal (f (mksig [(0, 0), (1, 1), (2, 2), (4, 2)]))
        [(0, 0), (1, 0.5), (2, 2), (4, 6)]
    equal (f (mksig [(0, 0), (1, -1), (2, -2), (3, -3)]))
        [(0, 0), (1, -0.5), (2, -2), (3, -4.5)]

test_integrate_segment = do
    let f = Signal.integrate_segment 1 0
    equal (f 0 2 0 2) (0, [])
    equal (f 0 2 1 2) (2, [(0, 0)])
    equal (f 0 2 4 2) (8, [(0, 0)])

    equal (f 0 0 2 2) (2, [(0, 0), (1, 0.5)])
    equal (f 0 0 3 3) (4.5, [(0, 0), (1, 0.5), (2, 2)])
    equal (f 0 0 3 (-3)) (-4.5, [(0, 0), (1, -0.5), (2, -2)])

    -- with offset
    equal (f 0 2 4 0) (4, [(0, 0), (1, 1.75), (2, 3), (3, 3.75)])
    -- crossing 0
    equal (f 0 2 4 (-2)) (0, [(0, 0), (1, 1.5), (2, 2), (3, 1.5)])

-- * comparison

test_pitches_share = do
    let sig = mksig
    let f = Signal.pitches_share False

    -- Different signals.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(1, 1), (0, 0)])) False
    -- Separated by an integer.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 1), (1, 2)])) True
    equal (f 0 1 (sig [(0, 0), (2, 2)]) (sig [(0, 1), (1, 2)])) True
    equal (f 1 2 (sig [(0, 0), (2, 2)]) (sig [(0, 1), (10, 11)])) True
    -- Separated by 0 can't share.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 0), (1, 1)])) False
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 0), (2, 2)])) False
    -- Except when one note is in decay.
    equal (Signal.pitches_share True 1 2
            (sig [(0, 1), (4, 1)]) (sig [(1, 1), (5, 1)]))
        True

    -- Must be separated by an integer.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 0.5), (1, 1.5)])) False
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 1.5), (1, 2.5)])) False

    -- Signals with different starting times.
    equal (f 1 3 (sig [(1, 0), (1, 61), (10, 61)]) (sig [(0, 60), (10, 60)]))
        True
