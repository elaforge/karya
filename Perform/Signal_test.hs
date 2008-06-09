module Perform.Signal_test where

import Util.Test
import Util.Pretty

import Ui.Types
import qualified Perform.Timestamp as Timestamp
import Perform.Signal as Signal

import qualified Data.Map as Map


secs = Timestamp.to_track_pos . Timestamp.seconds

test_interpolate = do
    equal (map (interpolate Linear (secs 1, 1) (secs 2, 2))
            (map secs [1.0, 1.5 .. 2.0]))
        [1.0, 1.5, 2.0]

    equal (map (interpolate Linear (secs 1, 2) (secs 2, 1))
            (map secs [1.0, 1.5 .. 2.0]))
        [2.0, 1.5, 1.0]

test_at = do
    let sig = signal [(secs 1, Set, 1), (secs 2, Set, 2), (secs 3, Linear, 3)]
        samples = [0.75, 1 .. 3.25]
    equal (map (at sig) (map secs [1, 2, 3])) [1, 2, 3]
    equal (zip samples (map (at sig) (map secs samples)))
        [ (0.75, 0.0)
        , (1.0, 1.0), (1.25, 1.0), (1.5, 1.0), (1.75, 1.0)
        , (2.0, 2.0), (2.25, 2.25), (2.5, 2.5), (2.75, 2.75)
        , (3.0, 3.0), (3.25, 3.0)
        ]

test_sample = do
    let sig = signal [(secs 1, Set, 1), (secs 2, Set, 2), (secs 3, Linear, 3)]
        samples = [0.75, 1 .. 3.25]
    let res = sample sig (secs 0.75) (secs 3.25)
    check $ (secs 1, 1) `elem` res
    check $ (secs 2, 2) `elem` res
    print res

test_sample2 = do
    let sig = signal
            [(TrackPos 0, Set, 0), (TrackPos 200, Linear, 20)
            , (TrackPos 400, Linear, 60)]
    -- 1/1 up to 200, then 2/1 to 400, then stop
    equal (sample sig (TrackPos 0) (TrackPos 1000))
        [ (TrackPos 0, 0.0), (TrackPos 100, 10.0), (TrackPos 200, 20.0)
        , (TrackPos 300, 40.0), (TrackPos 400, 60.0)
        ]
    -- end point not included
    equal (sample sig (TrackPos 0) (TrackPos 200))
        [ (TrackPos 0, 0.0), (TrackPos 100, 10.0)
        ]
    equal (sample sig (TrackPos 0) (TrackPos 5))
        [(TrackPos 0, 0)]


test_sample_complex = do
    let sig = signal
            [ (secs 0, Set, 1)
            , (secs 2, Jump (Linear, 0.5), 0)
            , (secs 3, Exp 2, 0)
            ]
    let res = sample sig (secs 0) (secs 10)
    print sig
    print_samples res

test_sample_exp = do
    let sig = signal
            [ (secs 1, Set, 0), (secs 2, Exp (-2), 1)
            ]
    print_samples (sample sig (secs 0) (secs 10))

print_samples = mapM_ (putStrLn . show_sample)
show_sample (pos, val) = pretty_pos pos ++ ": " ++ show val

mksignal posvals = signal
    [(secs sec, Linear, val) | (sec, val) <- posvals]

pretty_pos = pretty . Timestamp.from_track_pos
