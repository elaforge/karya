module Derive.Call.Trill_test where
import Util.Test

import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Trill as Trill
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


test_absolute_trill = do
    let f = Trill.absolute_trill
    equal (transform (f (con 1) (con 2))) $
        Right [[(0, 60), (0.5, 61), (1, 60)]]
    equal (transform (f (con 1) (con 1.9))) $
        Right [[(0, 60)]]
    -- trill speed is in real time and not affected by stretch
    equal (transform (Derive.d_stretch 2 . f (con 1) (con 2))) $
        Right [[(0, 60), (0.5, 61), (1, 60), (1.5, 61), (2, 60)]]
    -- it still produces an integral number of cycles
    equal (transform (Derive.d_stretch 1.8 . f (con 1) (con 2))) $
        Right [[(0, 60), (0.5, 61), (1, 60)]]

    equal (transform (f (Signal.signal [(0, 1), (0.5, 2)]) (con 4))) $
        Right [[(0, 60), (0.25, 61), (0.5, 60), (0.75, 62), (1, 60)]]
    equal (transform (f (con 1) (Signal.signal [(0, 2), (0.5, 4)]))) $
        Right [[(0, 60), (0.5, 61), (0.75, 60)]]

test_score_trill = do
    let f = Trill.score_trill
    equal (transform (f 1 (con 1) (con 2))) $
        Right [[(0, 60), (0.5, 61), (1, 60)]]
    -- If the event was length 2 there should be 2 cycles
    equal (transform (f 2 (con 1) (con 2))) $
        Right [[(0, 60), (0.25, 61), (0.5, 60), (0.75, 61), (1, 60)]]
    -- trill speed affected by stretch
    equal (transform (Derive.d_stretch 2 . f 1 (con 1) (con 2))) $
        Right [[(0, 60), (1, 61), (2, 60)]]

transform = extract . CallTest.transform
extract = DeriveTest.extract_events_only
    (Signal.unsignal . PitchSignal.to_nn (Pitch.degree_to_nn Twelve.scale)
        . Score.event_pitch)
con = Signal.constant

-- * pitch calls

test_pitch_absolute_trill = do
    equal (CallTest.run_pitch [(0, "abs-trill (4e) 2 2"), (2.8, "4c")]) $
        Right (
            [(x, (64, 66, v)) | (x, v) <- zip [0, 0.5, 1, 1.5, 2] (cycle [0, 1])]
            ++ [(2.8, (60, 60, 0))])

test_default_relative_note = do
    equal (CallTest.run_with_scale "twelve"
            [(0, "abs-trill (4e) 2 2"), (1, "4c")])
        (Right [(0, (64, 64, 0)), (0.5, (66, 66, 0)), (1, (60, 60, 0))])
    equal (CallTest.run_with_scale "relative" [(0, "abs-trill 2 2"), (1, "8")])
        (Right [(0, (0, 0, 0)), (0.5, (2, 2, 0)), (1, (8, 8, 0))])
