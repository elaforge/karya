module Perform.Signal_test where
import qualified Control.Arrow as Arrow
import qualified Data.List as List

import Util.Test

import Ui.Types
import qualified Perform.Signal as Signal
import Perform.Signal (Segment(..), Method(..), Sample(..))


test_interpolate = do
    let interpolate seg =
            map (Signal.interpolate seg (TrackPos 0) (TrackPos 4)) [0..4]
    equal (interpolate (SegLinear 2 2)) [2, 2, 2, 2, 2]
    equal (interpolate (SegLinear 0 4)) [0..4]
    equal (interpolate (SegLinear 4 0)) [4, 3, 2, 1, 0]
    equal (interpolate (SegLinear (-4) 0)) [-4, -3, -2, -1, 0]
    equal (interpolate (SegFunction "test" id)) [0, 0.25..1]

signal tsegs = Signal.signal
    [(TrackPos pos, meth, val1, val2) | (pos, meth, val1, val2) <- tsegs]

tsig = signal
    [ (0, Set, 1, 1), (2, Set, 1, 1), (4, Signal.Linear, 3, 8)
    , (6, Signal.Linear, 4, 10)
    ]
no_sig = signal []

test_at = do
    -- Notice that 'at' right on a jump gives the jumped-to val.
    equal (map (Signal.at tsig) (map TrackPos [0..7]))
        [1, 1, 1, 2, 8, 6, 10, 10]
    equal (Signal.at no_sig (TrackPos 2)) 0

un_pos (TrackPos p) = p

test_sample = do
    equal (map (Arrow.first un_pos) (Signal.sample srate tsig (TrackPos 0)))
        [(0, 1), (3, 2), (4, 8), (5, 6), (6, 10)]
    -- assert that 'at' all those points is equal to the samples

srate = TrackPos 1

test_linear_sample = do
    let sig = signal
            [ (0, Set, 1, 1)
            , (1, Set, 1, 1)
            , (5, Exp 2, 5, 1)
            , (8, Linear, 4, 4)
            ]
    equal (Signal.linear_sample srate sig)
        [ Sample (TrackPos 0) 1.0 (TrackPos 1) 1.0
        , Sample (TrackPos 1) 1.0 (TrackPos 2) 1.0
        , Sample (TrackPos 2) 1.25 (TrackPos 3) 1.25
        , Sample (TrackPos 3) 2.0 (TrackPos 4) 2.0
        , Sample (TrackPos 5) 1.0 (TrackPos 8) 4.0
        , Sample (TrackPos 8) 4.0 (TrackPos 9) 4.0
        ]

test_inverse = do
    let sig = signal
            [ (0, Set, 1, 1)
            , (2, Set, 1, 1)
            , (4, Linear, 3, 3)
            , (8, Exp 2, 7, 7)
            ]
        ps = Signal.inverse srate sig [0..6]
    plist $ Signal.linear_sample srate sig
    equal ps (map TrackPos [0, 0, 3, 4, 6, 8, 8])
    throws (exc_like "samples ended") $ Signal.inverse srate sig [8]

test_integrate = do
    let sig = signal
            [ (0, Set, 1, 1)
            , (3, Set, 4, 4)
            , (7, Set, 4, 4)
            , (10, Linear, 1, 1)
            , (12, Set, 1, 1)
            ]
    -- Signal with no width:
    equal (Signal.integrate srate (signal [(0, Set, 1, 1)]) [0..3]) [0..3]

    let ints = Signal.integrate srate sig (map TrackPos [0..13])
        diffs = zipWith (-) ints (0:ints)
    -- plist $ zip ints diffs
    equal diffs
        [ 0, 1, 1, 1, 4, 4, 4, 4
        , 3.5, 2.5, 1.5, 1, 1, 1
        ]

test_integrate_linear = do
    let int = Signal.integrate_linear
        asc = [0, 0.5, 2, 4.5, 8]
        desc = [0, 3.5, 6, 7.5, 8]

    -- positive
    equal (map (int (0, 0) (4, 4) ) [0..4]) asc
    equal (map (int (1, 1) (5, 5) ) [1..5]) (zipWith (+) [0..] asc)
    equal (map (int (1, 1) (5, 1) ) [1..5]) [0..4]

    equal (map (int (0, 4) (4, 0) ) [0..4]) desc
    equal (map (int (1, 5) (5, 1) ) [1..5]) (zipWith (+) [0..] desc)

    -- negative
    equal (map (int (0, -4) (4, 0) ) [0..4]) (map negate desc)
    equal (map (int (1, -5) (5, -1) ) [1..5])
        (zipWith (+) [0, -1..] (map negate desc))
    equal (map (int (1, -1) (5, -1) ) [1..5]) [0, -1.. -4]

    equal (map (int (0, 0) (4, -4) ) [0..4]) (map negate asc)
    equal (map (int (1, -1) (5, -5) ) [1..5])
        (zipWith (+) [0, -1..] (map negate asc))

    -- cross x axis
    equal (map (int (0, -1) (2, 1) ) [0..2]) [0, -0.5, 0]
    equal (map (int (0, 1) (2, -1) ) [0..2]) [0, 0.5, 0]
