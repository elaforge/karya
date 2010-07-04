module Derive.Score_test where
import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Signal as Signal

mksig = Signal.signal
unsig = Signal.unsignal

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
