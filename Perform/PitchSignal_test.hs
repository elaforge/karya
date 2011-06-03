module Perform.PitchSignal_test where
import Util.Control
import Util.Test

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


scale = Pitch.ScaleId "scale"
mksig = PitchSignal.signal scale . map (first RealTime.seconds)
unsig = PitchSignal.unsignal

test_at = do
    let sig = mksig
            [(0, (2, 0, 0)), (2, (2, 0, 1)), (4, (0, 4, 0)), (6, (0, 4, 1))]
        at f x = f (RealTime.seconds x) sig
    equal (map (PitchSignal.y_to_degree . at PitchSignal.at) [0..7])
        (map Pitch.Degree [2, 2, 0, 0, 0, 0, 4, 4])
    equal (map (at PitchSignal.at_linear) [0..7])
        [2, 1, 0, 0, 0, 2, 4, 4]

test_clip_min_max = do
    let sig = mksig [(1, (1, 5, 0.5))]
    let f v = unsig $ PitchSignal.clip_max v sig
    equal (map (f . Pitch.Degree) [0..6])
        (map (\d -> [(1, (1, 5, d))]) [0, 0, 0.25, 0.5, 0.5, 0.5, 0.5])

test_sig_add = do
    let conv = PitchSignal.to_nn (\(Pitch.Degree d) -> Just d)
    let f = PitchSignal.sig_add

    let s0 = mksig [(x, (4, 0, realToFrac (x/4))) | x <- [0..4]]
        s1 = mksig [(x, (0, 6, realToFrac (x/6))) | x <- [0..6]]
    equal (f s0 (mksig [(1, (1, 1, 0))]))
        (mksig [(0, (5, 1, 0)), (1, (5, 1, 0.25)), (2, (5, 1, 0.5)),
            (3, (5, 1, 0.75)), (4, (5, 1, 1))])
    equal (conv (f s0 (mksig [(1, (1, 1, 0))])))
        (Signal.signal [(0, 5), (1, 4), (2, 3), (3, 2), (4, 1)])
    equal (conv (f s0 s1))
        (Signal.signal [(0, 4), (1, 4), (2, 4), (3, 4), (4, 4), (5, 5), (6, 6)])
