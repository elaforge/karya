-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Warp_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.Warp as Warp
import Derive.Warp (shift, stretch, place)
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal2 as Signal
import Types


test_shift_stretch_id = do
    let warps w = map (Warp.warp w)
    let ident = Warp.identity

    equal (warps ident t03) [0, 1, 2, 3]
    uncurry equal $ trip ident

    equal (warps (shift 2 ident) t03) [2, 3, 4, 5]
    uncurry equal $ trip (shift 2 ident)

    equal (warps (stretch 2 ident) t03) [0, 2, 4, 6]
    uncurry equal $ trip (stretch 2 ident)

    equal (warps (place 1 2 ident) t03) [1, 3, 5, 7]
    uncurry equal $ trip (place 1 2 ident)

    equal (warps (shift 1 (stretch 2 ident)) t03) [2, 4, 6, 8]
    uncurry equal $ trip (shift 1 (stretch 2 ident))

    equal (warps (stretch 2 (shift 1 ident)) t03) [1, 3, 5, 7]
    uncurry equal $ trip (stretch 2 (shift 1 ident))

test_shift_stretch_signal = do
    let warps w = map (Warp.warp w) t03

    -- Should be the same as the identity.
    equal (warps (make [(0, 0), (10, 10)])) [0, 1, 2, 3]
    uncurry equal $ trip (make [(0, 0), (10, 10)])

    let curve = make $
            [(0, 0), (1, 0.5), (2, 1), (3, 3), (4, 5), (5, 7), (6, 9)]
    equal (warps curve) [0, 0.5, 1, 3]
    equal (warps (shift 2 curve)) [1, 3, 5, 7]
    equal (warps (stretch 2 curve)) [0, 1, 5, 9]

test_compose = do
    let ident = Warp.identity
    let f w1 w2 = warps $ Warp.compose w1 w2

    equal (f ident ident) [0, 1, 2, 3]
    equal (f ident (shift 2 ident)) [2, 3, 4, 5]
    equal (f ident (shift 1 (stretch 2 ident))) [2, 4, 6, 8]

    let curve = make $
            [(0, 0), (1, 0.5), (2, 1), (3, 3), (4, 5), (5, 7), (6, 9)]
    equal (f curve ident) [0, 0.5, 1, 3]
    equal (f curve (shift 2 ident)) [1, 3, 5, 7]
    equal (f curve (stretch 2 ident)) [0, 1, 5, 9]

    let slow = make [(RealTime.seconds n, n*2) | n <- Seq.range 0 100 1]
    equal (f slow ident) [0, 2, 4, 6]
    equal (f slow (stretch 2 ident)) [0, 4, 8, 12]
    equal (f (stretch 2 ident) slow) [0, 4, 8, 12]

    equal (f slow slow) [0, 4, 8, 12]
    equal (f slow (stretch 2 slow)) [0, 8, 16, 24]
    equal (f slow (stretch 2 (shift 1 slow))) [4, 12, 20, 28]

-- * util

t03 :: [ScoreTime]
t03 = [0, 1, 2, 3]

trip :: Warp.Warp -> ([ScoreTime], [ScoreTime])
trip w = (t03, map (Warp.unwarp w . Warp.warp w) t03)

warps :: Warp.Warp -> [RealTime]
warps w = map (Warp.warp w) t03

make :: [(RealTime, Signal.Y)] -> Warp.Warp
make = Warp.from_signal . Signal.from_pairs
