module Perform.PitchSignal_test where

import qualified Util.Num as Num
import Util.Test

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import Perform.SignalBase (Method(..))


scale = Pitch.ScaleId "scale"
mksig = PitchSignal.signal scale
unsig = PitchSignal.unpack

test_y_at = do
    -- y_at is not exported, but at_linear is the thing that calls it.
    let sig = PitchSignal.track_signal scale 1
            [(0, Set, 2), (2, Linear, 0), (4, Set, 0), (6, Linear, 4)]
    let reduce (from, to, at) = Num.scale from to at
    let res = (map (\x -> PitchSignal.at_linear x sig) [0..7])
    equal (map reduce res)
        [2, 1, 0, 0, 0, 2, 4, 4]

test_track_signal = do
    let sig = PitchSignal.signal scale
    let f = PitchSignal.track_signal scale 1

    equal (f [(0, Set, 2), (2, Linear, 0)])
        (sig [(0, (2, 2, 0)), (1, (2, 0, 0.5)), (2, (2, 0, 1))])
    equal (f [(1, Set, 2), (3, Set, 1)])
        (sig [(1, (2, 2, 0)), (3, (1, 1, 0))])

test_clip_min_max = do
    let sig = mksig [(1, (1, 5, 0.5))]
    let f v = unsig (PitchSignal.clip_max v sig)
    equal (map f (map Pitch.NoteNumber [0..6]))
        (map (\d -> [(1, (1, 5, d))]) [0, 0, 0.25, 0.5, 0.5, 0.5, 0.5])
