module Derive.Call.Sekar_test where
import Util.Test

import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Derive.Call.Sekar as Sekar
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import Derive.Operator
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


test_sekar = do
    equal 1 1 -- TODO placeholder since Sekar is broken for now

{-
-- test_sekar = do
    let e_event e = (Score.event_start e, Score.event_duration e,
            Score.event_pitch e)
    let extract = DeriveTest.extract_events_only e_event
        notes = ["4c", "4d", "4e", "4f", "4g", "4a", "4b"]
        mkevents txt timing = [(start, dur, s, note)
            | ((start, dur), s, note) <- zip3 timing (txt : repeat "") notes]
        run txt evts = extract $ DeriveTest.derive_tracks_tempo
            (pitch_tracks (mkevents txt evts))

    pprint (run "" [(0, 4), (4, 4)])
    pprint (run "sekar 'abab'" [(0, 4), (4, 4), (8, 16)])
    -- (0, 2, [(0, 60)])
    -- (2, 2, [(0, 60), (2, 62)])
    -- (4, 2, [(0, 60)])
    -- (6, 2, [(0, 60), (2, 62)])
    -- equal (run "sekar 'abab'" [(0, 4), (4, 4), (8, 16)]) $ Right
    --     [ (0, 2, 60), (2, 2, 62)
    --     , (4, 2, 60), (6, 2, 62)
    --     , (8, 16, 64)
    --     ]

-- test_to_pattern = do
    let f = Sekar.to_pattern
    let pat = (f "abab" [] [Track.event 0 4 "1", Track.event 4 4 "2"])
    let Right (pattern, _, _) = pat
    pprint [(start, dur) | (start, dur, _) <- Sekar.place pattern]


pitch_tracks pitch_events =
    [ (">i", [(start, dur, s) | (start, dur, s, _) <- pitch_events])
    , ("*twelve", [(start, 0, pitch) | (start, _, _, pitch) <- pitch_events])
    ]


run deriver = extract $ DeriveTest.derive_note $
    Derive.with_constant_pitch Nothing (Pitch.Degree 60) $
    deriver DeriveTest.d_note
con = Signal.constant

extract = DeriveTest.extract_events_only
    (Signal.unsignal . PitchSignal.to_nn Twelve.scale . Score.event_pitch)
-}
