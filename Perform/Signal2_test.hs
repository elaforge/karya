module Perform.Signal2_test where

import Util.Test

import Ui.Types

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Signal2 as Signal
import Perform.Signal2 (Method(..))


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

test_integrate = do
    let f sig = Signal.unpack $ Signal.integrate (TrackPos 1) sig
    equal (f (tsig [(0, Set, 0), (3, Linear, 3)]))
        [(0, 0), (1, 1), (2, 3)]
    equal (f (tsig [(0, Set, 0), (3, Linear, -3)]))
        [(0, 0), (1, -1), (2, -3)]

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

test_clip = do
    let sig = Signal.signal [(0, 0), (2, 2), (4, 0)]
    print sig
    equal (Signal.unpack $ Signal.clip_max 1 sig)
        [(0, 0), (1, 1), (3, 1), (4, 0)]

test_clip2 = do
    let sig = Signal.signal [(0, 0), (0, 2), (4, 2)]
    print sig
    print $ Signal.clip_min 1 sig
