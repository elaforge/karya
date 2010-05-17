module Perform.PitchSignal_test where

import Util.Test

import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


scale = Pitch.ScaleId "scale"
mksig = PitchSignal.signal scale
unsig = PitchSignal.unsignal

test_at = do
    let sig = mksig
            [(0, (2, 0, 0)), (2, (2, 0, 1)), (4, (0, 4, 0)), (6, (0, 4, 1))]
    equal [PitchSignal.y_to_degree (PitchSignal.at x sig) | x <- [0..7]]
        (map Pitch.Degree [2, 2, 0, 0, 0, 0, 4, 4])
    equal [PitchSignal.at_linear x sig | x <- [0..7]]
        [2, 1, 0, 0, 0, 2, 4, 4]

test_clip_min_max = do
    let sig = mksig [(1, (1, 5, 0.5))]
    let f v = unsig $ PitchSignal.clip_max v sig
    equal (map f (map Pitch.NoteNumber [0..6]))
        (map (\d -> [(1, (1, 5, d))]) [0, 0, 0.25, 0.5, 0.5, 0.5, 0.5])

test_sig_add = do
    let conv = PitchSignal.to_nn Twelve.scale
    let f = PitchSignal.sig_add

    let s0 = mksig [(x, (4, 0, realToFrac (x/4))) | x <- [0..4]]
        s1 = mksig [(x, (0, 6, realToFrac (x/6))) | x <- [0..6]]
    equal (f s0 (mksig [(1, (1, 1, 0))]))
        (mksig [(0, (4, 0, 0)), (1, (5, 1, 0.25)), (2, (5, 1, 0.5)),
            (3, (5, 1, 0.75)), (4, (5, 1, 1))])
    equal (conv (f s0 (mksig [(1, (1, 1, 0))])))
        (Signal.signal [(0, 4), (1, 4), (2, 3), (3, 2), (4, 1)])
    equal (conv (f s0 s1))
        (Signal.signal [(0, 4), (1, 4), (2, 4), (3, 4), (4, 4), (5, 5), (6, 6)])
