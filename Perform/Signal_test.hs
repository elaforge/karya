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
    let sig0 = mksig [(0, 0), (10, 20)]
        sig1 = mksig [(0, 0), (1, 0.5), (2, 1)]
    let f = Signal.compose
    -- They cancel each other out.
    equal (unsig (f sig0 sig1)) [(0, 0), (1, 1), (2, 2)]
    let lin = mksig [(0, 0), (1, 1), (2, 2), (3, 3), (100, 100)]
        slow = mksig [(0, 0), (1, 2), (2, 4), (3, 6)]
    equal (f lin lin) lin
    equal (f lin slow) slow
    -- Since the signals are not the same length it looks funny at the end.
    -- But this should never be audible since a sub-block's warp shouldn't
    -- be longer than its parent's.
    equal (f slow lin) $
        mksig [(0, 0), (1, 2), (2, 4), (3, 6), (100, 6)]

test_integrate = do
        -- strip off the padding that integrate appends
    let f samples = take nxs $ unsig $ Signal.integrate 1 (mksig samples)
            where nxs = floor (fst (last samples)) + 1

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
