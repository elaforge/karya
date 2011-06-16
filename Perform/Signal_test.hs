module Perform.Signal_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Perform.Signal as Signal


mksig = Signal.signal
unsig = Signal.unsignal

-- * transformation

test_inverse_at = do
    let f sig pos_list = Signal.inverse_at pos_list (mksig sig)
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
    let f samples = unsig $ Signal.integrate 1 (mksig samples)
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
    let sample0 sig = fromIntegral (floor y)
            where Just (_, y) = Signal.first sig
    let f start end sig0 sig1 = Signal.pitches_share False start end
            (sample0 sig0) sig0 (sample0 sig1) sig1

    -- Different signals.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 1), (1, 0)])) False
    -- Separated by an integer can share.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 1), (1, 2)])) True
    -- Separated by 0 can't share.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 0), (1, 1)])) False
    equal (Signal.pitches_share False 1 2 1 (sig [(0, 1)]) 1 (sig [(1, 1)]))
        False
    -- Except when one note is in decay.
    equal (Signal.pitches_share True 1 2 1 (sig [(0, 1)]) 1 (sig [(1, 1)]))
        True

    -- Not an integral difference.
    equal (f 1 3 (sig [(1, 60.01)]) (sig [(1, 61)])) False
    -- But this difference is inaudible.
    equal (f 1 3 (sig [(1, 60.001)]) (sig [(1, 61)])) True

    -- Signals with different starting times.
    equal (f 1 3 (sig [(1, 61), (10, 61)]) (sig [(0, 60), (10, 60)])) True

    -- one pitch changes but the other doesn't, so they can't share
    equal (f 0 3 (sig [(0, 0), (1, 1)]) (sig [(0, 2)])) False
    equal (f 1 5 (sig [(1, 74), (2, 76), (3, 74)]) (sig [(0, 48)])) False

    -- Signals with starting times inside the range.
    equal (f 0 2 (sig [(0, 1)]) (sig [(0.08, 2)])) True
