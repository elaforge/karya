module Perform.Signal_test where
import qualified Data.List as List

import Util.Test

import Ui.Types

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Signal as Signal
import Perform.Signal (Method(..))


tsig segs = Signal.track_signal (TrackPos 1) (map mkseg segs)
mkseg (pos, meth, val) = (TrackPos pos, meth, val)

-- Also tests 'at'.
test_track_signal = do
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
    let f = Signal.pitches_share
    -- Different signals.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(1, 1), (0, 0)])) False
    -- Separated by an integer.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 1), (1, 2)])) True
    equal (f 0 1 (sig [(0, 0), (2, 2)]) (sig [(0, 1), (1, 2)])) True
    equal (f 1 2 (sig [(0, 0), (2, 2)]) (sig [(0, 1), (10, 11)])) True
    -- Separated by 0.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 0), (1, 1)])) False
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 0), (2, 2)])) False

    -- Must be separated by an integer.
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 0.5), (1, 1.5)])) False
    equal (f 0 1 (sig [(0, 0), (1, 1)]) (sig [(0, 1.5), (1, 2.5)])) False

test_resample_list = do
    let f = Signal.resample_list (,)
    equal (f [(1, 1), (2, 2)] [])
        [(1, (1, 0)), (2, (2, 0))]
    equal (f [(1, 1), (2, 2)] [(1, 2), (2, 1)])
        [(1, (1, 2)), (2, (2, 1))]

    let sig0 = [(1, 4), (4, 1), (6, 4)]
        sig1 = [(2, 2), (4, 4), (8, 2), (9, 3)]
        merged =
            [ (1, (4, 1)), (2, (3, 2)), (4, (1, 4)), (6, (4, 3))
            , (8, (4, 2)), (9, (4, 3))]
    equal (f sig0 sig1) merged
    equal (f sig1 sig0) [(x, (by, ay)) | (x, (ay, by)) <- merged]

-- * access

test_sample = do
    let sig = Signal.signal [(0, 0), (2, 2), (4, 2), (6, 0)]
    equal (Signal.sample (TrackPos 1) (TrackPos 0) sig)
        [(0, 0), (1, 1), (2, 2), (5, 1), (6, 0)]
    equal (Signal.sample (TrackPos 1) (TrackPos 2) sig)
        [(2, 2), (5, 1), (6, 0)]
    equal (Signal.sample (TrackPos 1) (TrackPos 3) sig)
        [(3, 2), (5, 1), (6, 0)]

    let discont = Signal.signal [(0, 1), (2, 1), (2, 0)]
    equal (Signal.sample (TrackPos 1) (TrackPos 0) discont)
        [(0, 1), (2, 0)]

-- * functions

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
    let f sig = Signal.unpack $ Signal.integrate (TrackPos 1) sig
    equal (f (tsig [(0, Set, 0), (3, Linear, 3)]))
        [(0, 0), (1, 1), (2, 3)]
    equal (f (tsig [(0, Set, 0), (3, Linear, -3)]))
        [(0, 0), (1, -1), (2, -3)]

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
    let f samples = check $ List.all ((<=1) . snd)
            (Signal.unpack (Signal.clip_max 1 (Signal.signal samples)))
    f [(0, 0), (0, 2), (4, 2)]
    f [(1, 0)]
    f []

test_clip_min = do
    let f samples = check $ List.all ((>=1) . snd)
            (Signal.unpack (Signal.clip_min 1 (Signal.signal samples)))
    f [(0, 0), (0, 2), (4, 2)]
    f [(1, 0)]
    f []
    -- print $ Signal.clip_min 1 (Signal.signal [(1, 0)])
