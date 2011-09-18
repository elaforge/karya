module Derive.Score_test where
import qualified Util.Seq as Seq
import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Signal as Signal
import qualified Perform.RealTime as RealTime


test_compose_warp = do
    let f w1 w2 = [Score.warp_pos n warp | n <- [0..3]]
            where
            warp = Score.compose_warps w1 w2
        w = Score.id_warp
        shift n warp = warp { Score.warp_shift = n }
        stretch n warp = warp { Score.warp_stretch = n }

    equal (f w w) [0, 1, 2, 3]
    equal (f w (shift 2 w)) [2, 3, 4, 5]
    equal (f w (shift 2 (stretch 2 w))) [2, 4, 6, 8]

    let curve = Score.signal_to_warp $ Signal.signal
            [(0, 0), (1, 0.5), (2, 1), (3, 3), (4, 5), (5, 7), (6, 9)]
    equal (f curve w) [0, 0.5, 1, 3]
    equal (f curve (shift 2 w)) [1, 3, 5, 7]
    equal (f curve (stretch 2 w)) [0, 1, 5, 9]

    let slow = Score.signal_to_warp $ Signal.signal
            [(RealTime.seconds n, n*2) | n <- [0..100]]
    equal (f slow w) [0, 2, 4, 6]
    equal (f slow (stretch 2 w)) [0, 4, 8, 12]
    equal (f (stretch 2 w) slow) [0, 4, 8, 12]
    equal (f slow slow) [0, 4, 8, 12]
    equal (f slow (stretch 2 slow)) [0, 8, 16, 24]
    equal (f slow (shift 1 (stretch 2 slow))) [4, 12, 20, 28]


test_warp_to_signal = do
    let f warp = Seq.diff (==) (pos warp) (pos wsig)
            where
            pos w = [Score.warp_pos p w | p <- [0..10]]
            wsig = Score.signal_to_warp $ Score.warp_to_signal warp
    let w = Score.Warp
        id_sig = Score.id_warp_signal
    equal (f Score.id_warp) []
    equal (f (w id_sig 0 2)) []
    equal (f (w id_sig 1 1)) []
    equal (f (w id_sig 1 2)) []

test_safe_unwarp_pos = do
    let f ps warp = map (flip Score.safe_unwarp_pos warp) ps
        w = Score.Warp (Signal.signal [(0, 0), (2, 1)])
    equal (f [0, 1, 2, 3] (w 0 1)) [0, 2, 3, 4]
    equal (f [0, 1, 2, 3] (w 1 1)) [-1, 1, 2, 3]
    equal (f [0, 1, 2, 3] (w 0 2)) [0, 1, 3, 4]
    equal (map (flip Score.safe_unwarp_pos (Score.Warp Signal.empty 0 1))
            [0, 1, 2, 3])
        [0, 1, 2, 3]
