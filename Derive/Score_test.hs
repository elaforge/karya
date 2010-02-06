module Derive.Score_test where
import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Signal as Signal

mksig = Signal.signal
unsig = Signal.unsignal

test_warped_controls = do
    let c = Score.Control "c"
    let cs = Score.warped_controls [(c, mksig [(0, 1), (1, 2), (2, 0)])]
    let lookup controls p = Score.lookup_control p c controls

    equal (map (lookup cs) [0..3]) (map Just [1, 2, 0, 0])
    let cs2 = Score.modify_warps (Score.shift_warp 2) cs
    equal (map (lookup cs2) [0..5]) (map Just [1, 1, 1, 2, 0, 0])

    let cs2 = Score.modify_warps (Score.stretch_warp 2) cs
    equal (map (lookup cs2) [0..5]) (map Just [1, 1, 2, 2, 0, 0])

test_warp_control = do
    let f sig = unsig . Score.warp_control sig
    let sig = mksig [(0, 1), (1, 2), (2, 0)]

    -- score -> real, score 1 -> real 2
    equal (f sig (Score.stretch_warp 2 Score.id_warp))
        [(0, 1), (2, 2), (4, 0)]
    equal (f sig (Score.shift_warp 2 Score.id_warp))
        [(2, 1), (3, 2), (4, 0)]

test_warp_to_signal = do
    let sample = flip Signal.at_linear
    let f op = map (sample (Score.warp_to_signal (op Score.id_warp))) [0..4]
    equal (f id) [0..4]
    equal (f (Score.shift_warp 2)) [2, 3, 4, 5, 6]
    -- stretch 2 means twice as much score time maps to the same about of real
    -- time
    equal (f (Score.stretch_warp 2)) [0, 0.5, 1, 1.5, 2]
    equal (f (Score.shift_warp 2 . Score.stretch_warp 2)) [2, 2.5, 3, 3.5, 4]
    equal (f (Score.stretch_warp 2 . Score.shift_warp 2)) [1, 1.5, 2, 2.5, 3]
