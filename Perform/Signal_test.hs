module Perform.Signal_test where
import qualified Data.List as List

import Util.Test
import qualified Util.Seq as Seq

import Ui

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Signal as Signal
import Perform.Signal (Method(..))


tsig segs = Signal.track_signal (TrackPos 1) (map mkseg segs)
mkseg (pos, meth, val) = (TrackPos pos, meth, val)

-- Also tests 'at'.
test_track_signal = do
    let f = Signal.track_signal 1
    -- Make sure Set and Linear work as expected.
    equal (f [(0, Set, 1), (2, Linear, 2)])
        (Signal.signal [(0, 1), (2, 2)])
    equal (f [(0, Set, 1), (2, Set, 2)])
        (Signal.signal [(0, 1), (2, 1), (2, 2)])
    equal (f [(0, Set, 1), (2, Set, 2), (3, Linear, 0), (4, Set, 1)])
        (Signal.signal [(0, 1), (2, 1), (2, 2), (3, 0), (4, 0), (4, 1)])

    let range low high sig =
                map (\p -> Signal.at p (tsig sig)) (map TrackPos [low..high-1])

    equal (range 0 4 []) [0, 0, 0, 0]
    equal (range 0 4 [(2, Set, 1)]) [0, 0, 1, 1]
    equal (range 0 4 [(0, Set, 1), (3, Set, 1)]) [1, 1, 1, 1]

    equal (range 0 5 [(0, Set, 0), (4, Linear, 1)])
        [0, 0.25, 0.5, 0.75, 1]
    equal (range 0 6 [(0, Set, 0), (4, Linear, 1), (4, Set, 0)])
        [0, 0.25, 0.5, 0.75, 0, 0]

test_sample_function = do
    let f start end = Signal.sample_function
            (Signal.exp_function 2 1 2) (TrackPos 1)
            (TrackPos start) (TrackPos end)

    equal (f 0 0) []
    -- Doesn't include the end.
    equal (f 0 1) [(TrackPos 0, 1)]
    equal (f 0 2) [(TrackPos 0, 1), (TrackPos 1, 1.25)]
    equal (f 10 12) [(TrackPos 10, 1), (TrackPos 11, 1.25)]

-- * comparison

test_pitches_share = do
    let sig = Signal.signal
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

test_within = do
    let sig = Signal.signal [(1, 0), (1, 1), (4, 1)]
    let f = Signal.within
    equal (f 0 0 sig) (Signal.signal [])
    equal (f 0 1 sig) (Signal.signal [])
    equal (f 1 4 sig) (Signal.signal [(1, 1)])

test_resample = do
    -- TODO: test with coincident samples
    let f = Signal._resample (0, 0) (0, 0)

    equal (f [(1, 1), (2, 2)] []) [(1, 1, 0), (2, 2, 0)]
    equal (f [] [(1, 1), (2, 2)]) [(1, 0, 1), (2, 0, 2)]
    equal (f [(1, 1), (2, 2)] [(1, 3), (2, 4)]) [(1, 1, 3), (2, 2, 4)]
    equal (f [(1, 1)] [(0, 2), (2, 4), (3, 6)])
        [(0, 0, 2), (1, 1, 3), (2, 1, 4), (3, 1, 6)]
    equal (f [(0, 0), (4, 4)] [(1, 1), (2, 2), (3, 3)])
        [(0, 0, 0), (1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 3)]

-- * access

test_sample = do
    let sig = Signal.signal [(0, 0), (2, 2), (4, 2), (6, 0)]
    equal (Signal.sample 1 0 sig)
        [(0, 0), (1, 1), (2, 2), (5, 1), (6, 0)]
    equal (Signal.sample 1 2 sig)
        [(2, 2), (5, 1), (6, 0)]
    equal (Signal.sample 1 3 sig)
        [(3, 2), (5, 1), (6, 0)]

    let discont = Signal.signal [(0, 1), (2, 1), (2, 0)]
    equal (Signal.sample 1 0 discont)
        [(0, 1), (2, 0)]

-- * functions

-- ** signal ops

test_sig_add = do
    let f a b = Signal.unpack $
            Signal.sig_add (Signal.signal a) (Signal.signal b)
    equal (f [(0, 0), (2, 2), (4, 0)] [(0, 1)])
        [(0, 1), (2, 3), (4, 1)]
    equal (f [(0, 0), (2, 2), (4, 0)] [(1, 0), (1, 1), (3, 1), (3, 0)])
        [(0, 0), (1, 1), (1, 2), (2, 3), (3, 2), (3, 1), (4, 0)]

test_sig_max = do
    let f a b = Signal.unpack $
            Signal.sig_max (Signal.signal a) (Signal.signal b)
    let s = Signal.signal
    pprint (Signal.resample_to_list (s [(0, 0), (2, 2), (4, 0)]) (s [(0, 1)]))
    equal (f [(0, 0), (2, 2), (4, 0)] [(0, 1)])
        [(0, 1), (1, 1), (2, 2), (3, 1), (4, 1)]

-- ** special functions

test_inverse_at = do
    let mksig = Signal.signal
    let f sig ts = Signal.inverse_at (mksig sig) (Timestamp.seconds ts)

    equal (map (f [(0, 0), (2, 2)]) [0..3])
        [Just 0, Just 1, Just 2, Nothing]
    equal (map (f [(1, 1), (2, 2)]) [0..3])
        [Just 0, Just 1, Just 2, Nothing]
    -- Flat curve.
    equal (map (f [(1, 1), (2, 1), (3, 2)]) [0..3])
        [Just 0, Just 1, Just 3, Nothing]
    -- Vertical discontinuity.
    equal (map (f [(1, 1), (1, 2), (2, 3)]) [0..4])
        [Just 0, Just 1, Just 1, Just 2, Nothing]

test_compose = do
    let f = Signal.signal [(0, 0), (10, 20)]
        g = Signal.signal [(0, 0), (1, 0.5), (2, 1)]
    -- They cancel each other out.
    equal (Signal.unpack (Signal.compose f g))
        [(0, 0), (1, 1), (2, 2)]

test_integrate = do
    -- The last sample is the max_track_pos sentinel.
    let f sig = Seq.rdrop 1 $ Signal.unpack $ Signal.integrate (TrackPos 1) sig
    equal (f (tsig [(0, Set, 0), (2, Linear, 2), (4, Linear, 2)]))
        [(0, 0), (1, 0.5), (2, 2), (4, 6)]
    equal (f (tsig [(0, Set, 0), (3, Linear, -3)]))
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

test_shift_stretch = do
    let sig = Signal.signal [(0, 1), (1, 0)]

    equal (Signal.unpack (Signal.shift (TrackPos 2) sig))
        [(2, 1), (3, 0)]

    equal (Signal.unpack (Signal.stretch (TrackPos 1) sig))
        [(0, 1), (1, 0)]
    equal (Signal.unpack (Signal.stretch (TrackPos 2) sig))
        [(0, 1), (2, 0)]

test_find_samples = do
    let mksig = Signal.signal
    let f = Signal.find_samples
    let [zero, one, two] = map (\n -> (TrackPos n, n)) [0..2]
    let three = (TrackPos 3, 2)
    let sig = mksig [one, two]
    -- Fake up a zero segment that has some length.
    equal (f 0 (mksig [])) (zero, (TrackPos 1, 0))
    equal (f 0 sig) (zero, one)
    equal (f 1 sig) (one, two)
    equal (f 2 sig) (two, three)
    equal (f 3 sig) (two, three)

    -- Make sure the simultaneous samples are skipped.
    let sig2 = mksig [(0, 0), (1, 0), (1, 1)]
    equal (map (\n -> f n sig2) [0..2])
        [ ((0, 0), (1, 0))
        , ((1, 1), (2, 1))
        , ((1, 1), (2, 1))
        ]

test_clip_max = do
    let f = Signal.unpack . Signal.clip_max 1 . Signal.signal
    let below samples = check $ List.all ((<=1) . snd) (f samples)

    below [(0, 0), (0, 2), (4, 2)]
    below [(1, 0)]
    below []

    -- signal isn't shortened
    equal (f [(0, 0), (2, 2), (4, 2)]) [(0, 0), (1, 1), (4, 1)]

test_clip_min = do
    let f = Signal.unpack . Signal.clip_min 1 . Signal.signal
        above samples = check $ List.all ((>=1) . snd) (f samples)
    above [(0, 0), (0, 2), (4, 2)]
    above [(1, 0)]
    above []

test_truncate = do
    let sig = Signal.signal [(0, 0), (1, 1), (2, 0)]
    let f p = Signal.unpack . Signal.truncate p
    equal (f 0 sig) []
    equal (f 1 sig) [(0, 0)]
    equal (f 2 sig) [(0, 0), (1, 1)]
    equal (f 3 sig) [(0, 0), (1, 1), (2, 0)]
