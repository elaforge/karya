-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Score_test where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


test_move = do
    let event = Score.empty_event
            { Score.event_start = 2
            , Score.event_untransformed_controls =
                Map.fromList [("c", Score.untyped $ Signal.signal [(2, 2)])]
            , Score.event_untransformed_pitch =
                PitchSignal.signal [(2, Pitches.nn_pitch 42)]
            }
        signal = Score.untyped . Signal.signal
    let moved = Score.move (+2) event
    equal (Score.event_start moved) 4
    equal (Score.control_at 2 "c" moved) (Just (Score.untyped 0))
    equal (Score.control_at 4 "c" moved) (Just (Score.untyped 2))
    equal (Score.event_control "c" moved) (Just $ signal [(4, 2)])
    equal (Score.event_control "d" $
            Score.set_control "d" (signal [(0, 1)]) moved)
        (Just $ signal [(0, 1)])
    equal (Score.nn_at 2 moved) Nothing
    equal (Score.nn_at 4 moved) (Just 42)

test_compose_warp = do
    let f w1 w2 = [Score.warp_pos warp n | n <- Seq.range 0 3 1]
            where warp = Score.compose_warps w1 w2
        w = Score.id_warp
        shift n warp = warp { Score.warp_shift = n }
        stretch n warp = warp { Score.warp_stretch = n }
    equal (f w w) [0, 1, 2, 3]
    equal (f w (shift 2 w)) [2, 3, 4, 5]
    equal (f w (shift 2 (stretch 2 w))) [2, 4, 6, 8]

    let curve = Score.warp $ Signal.signal
            [(0, 0), (1, 0.5), (2, 1), (3, 3), (4, 5), (5, 7), (6, 9)]
    equal (f curve w) [0, 0.5, 1, 3]
    equal (f curve (shift 2 w)) [1, 3, 5, 7]
    equal (f curve (stretch 2 w)) [0, 1, 5, 9]

    let slow = Score.warp $ Signal.signal
            [(RealTime.seconds n, n*2) | n <- Seq.range 0 100 1]
    equal (f slow w) [0, 2, 4, 6]
    equal (f slow (stretch 2 w)) [0, 4, 8, 12]
    equal (f (stretch 2 w) slow) [0, 4, 8, 12]

    equal (f slow slow) [0, 4, 8, 12]
    equal (f slow (stretch 2 slow)) [0, 8, 16, 24]
    equal (f slow (shift 1 (stretch 2 slow))) [4, 12, 20, 28]

test_warp_to_signal = do
    let f warp = Seq.diff (==) (pos warp) (pos wsig)
            where
            pos w = [Score.warp_pos w p | p <- Seq.range 0 10 1]
            wsig = Score.warp $ Score.warp_to_signal warp
    let id_sig = Score.id_warp_signal
    equal (f Score.id_warp) []
    equal (f (Score.Warp id_sig 0 2)) []
    equal (f (Score.Warp id_sig 1 1)) []
    equal (f (Score.Warp id_sig 1 2)) []

test_warp_pos = do
    let f = Score.warp_pos
    let id_warp shift = Score.Warp Score.id_warp_signal shift 1
        linear = Signal.signal [(0, 0), (1, 1), (2, 2)]
        warp shift = Score.Warp linear shift 1

        repl = replicate 4
        combinations p =
            [ f (id_warp 0) p, f (id_warp (RealTime.score p)) 0
            , f (warp 0) p, f (warp (RealTime.score p)) 0
            ]

    -- Score.id_warp_signal should be the same as any other linear signal.
    equal (combinations 0) (repl 0)
    equal (combinations 1) (repl 1)
    -- The warp is extended past its bounds.
    equal (combinations 10) (repl 10)
    equal (combinations (-10)) (repl (-10))

test_warp_roundtrip = do
    let warp = Score.warp_pos
        unwarp = Score.unwarp_pos
    let srs w = unwarp w . warp w
        rsr w = warp w . unwarp w
        trip f xs = (map f xs, xs)
        warp1 = Score.warp (Signal.signal [(0, 0), (1, 1)])
        warp2 = Score.warp (Signal.signal [(0, 0), (1, 2)])
    uncurry equal (trip (srs warp1) [-1, 0, 1, 2])
    uncurry equal (trip (srs warp2) [-1, 0, 1, 2])
    uncurry equal (trip (rsr warp1) [-1, 0, 1, 2])
    uncurry equal (trip (rsr warp2) [-1, 0, 1, 2])
