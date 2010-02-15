module Derive.Call.Trill_test where
import Util.Test

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Call.Trill as Trill
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


test_absolute_trill = do
    let f = Trill.absolute_trill
    equal (run (f (con 1) (con 2))) $
        Right [[(0, 60), (0.5, 61), (1, 60)]]
    equal (run (f (con 1) (con 1.9))) $
        Right [[(0, 60)]]
    -- trill speed is in real time and not affected by stretch
    equal (run (Derive.d_stretch 2 . f (con 1) (con 2))) $
        Right [[(0, 60), (0.5, 61), (1, 60), (1.5, 61), (2, 60)]]
    equal (run (f (Signal.signal [(0, 1), (0.5, 2)]) (con 4))) $
        Right [[(0, 60), (0.25, 61), (0.5, 60), (0.75, 62), (1, 60)]]
    equal (run (f (con 1) (Signal.signal [(0, 2), (0.5, 4)]))) $
        Right [[(0, 60), (0.5, 61), (0.75, 60)]]

test_score_trill = do
    let f = Trill.score_trill
    equal (run (f 1 (con 1) (con 2))) $
        Right [[(0, 60), (0.5, 61), (1, 60)]]
    -- If the event was length 2 there should be 2 cycles
    equal (run (f 2 (con 1) (con 2))) $
        Right [[(0, 60), (0.25, 61), (0.5, 60), (0.75, 61), (1, 60)]]
    -- trill speed affected by stretch
    equal (run (Derive.d_stretch 2 . f 1 (con 1) (con 2))) $
        Right [[(0, 60), (1, 61), (2, 60)]]

run deriver = extract $ DeriveTest.derive_note $
    Derive.with_constant_pitch (Pitch.Degree 60) $
    deriver DeriveTest.d_note
con = Signal.constant

extract = DeriveTest.extract_events_only
    (Signal.unsignal . PitchSignal.to_nn Twelve.scale . Score.event_pitch)
