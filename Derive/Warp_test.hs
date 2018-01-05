-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Warp_test where
import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Util.Test

import qualified Derive.Warp as Warp
import Derive.Warp (shift, stretch)
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal2 as Signal
import Types


signal_identity :: Warp.Warp
signal_identity = Warp.from_signal $ Signal.from_pairs
    [(0, 0), (RealTime.large, RealTime.to_seconds RealTime.large)]

test_shift_stretch_linear = do
    let with :: CallStack.Stack => (Warp.Warp -> Warp.Warp) -> [RealTime]
            -> IO Bool
        with transform expected = do
            equal (map (Warp.warp (transform Warp.identity)) t03) expected
            uncurry equal (trip (transform Warp.identity))
            equal (map (Warp.warp (transform signal_identity)) t03) expected
            uncurry equal (trip (transform signal_identity))
    with id [0, 1, 2, 3]
    with (shift 2) [2, 3, 4, 5]
    with (stretch 2) [0, 2, 4, 6]
    with (shift 1 . stretch 2) [1, 3, 5, 7]
    with (stretch 2 . shift 1) [2, 4, 6, 8]
    with (shift 1 . stretch 2 . shift 1) [3, 5, 7, 9]
    with (stretch 2 . shift 1 . stretch 2) [2, 6, 10, 14]

test_shift_stretch_signal = do
    let with :: CallStack.Stack => (Warp.Warp -> Warp.Warp) -> [RealTime]
            -> IO Bool
        with transform expected = do
            let w = transform curve
            equal (map (Warp.warp w) t03) expected
            uncurry equal (trip w)
        curve = make [(0, 0), (1, 0.5), (2, 1), (3, 3), (4, 5), (5, 7), (6, 9)]
    with id [0, 0.5, 1, 3]
    with (shift 2) [2, 2.5, 3, 5]
    with (stretch 2) [0, 1, 2, 6]
    with (shift 1 . stretch 2) [1, 2, 3, 7]
    with (stretch 2 . shift 1) [2, 3, 4, 8]

test_compose = do
    let with :: CallStack.Stack => Warp.Warp -> Warp.Warp -> [RealTime]
            -> IO Bool
        with w1 w2 expected = do
        let w = Warp.compose w1 w2
        equal (map (Warp.warp w) t03) expected
        uncurry equal (trip w)
    let ident = Warp.identity
    let slow = make [(RealTime.seconds n, n*2) | n <- Seq.range 0 100 1]
    let curve = make $
            [(0, 0), (1, 0.5), (2, 1), (3, 3), (4, 5), (5, 7), (6, 9)]

    with ident ident [0, 1, 2, 3]
    with ident (shift 2 ident) [2, 3, 4, 5]
    with ident (shift 1 (stretch 2 ident)) [1, 3, 5, 7]

    with curve ident [0, 0.5, 1, 3]
    with curve (shift 2 ident) [1, 3, 5, 7]
    with curve (stretch 2 ident) [0, 1, 5, 9]

    with slow ident [0, 2, 4, 6]
    with slow (shift 1 ident) [2, 4, 6, 8]
    with slow (stretch 2 ident) [0, 4, 8, 12]
    with (stretch 2 ident) slow [0, 4, 8, 12]

    with slow slow [0, 4, 8, 12]
    with slow (stretch 2 slow) [0, 8, 16, 24]
    with slow (stretch 2 (shift 1 slow)) [4, 12, 20, 28]

-- * util

t03 :: [ScoreTime]
t03 = [0, 1, 2, 3]

trip :: Warp.Warp -> ([ScoreTime], [ScoreTime])
trip w = (t03, map (Warp.unwarp w . Warp.warp w) t03)

make :: [(RealTime, Signal.Y)] -> Warp.Warp
make = Warp.from_signal . Signal.from_pairs
