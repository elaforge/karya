module Derive.Call.Echo_test where

import Util.Test

import qualified Derive.DeriveTest as DeriveTest
import qualified Util.Log as Log


test_delay = do
    let extract = DeriveTest.extract_events_only DeriveTest.e_event
    let run title pref tracks = extract $ DeriveTest.derive_tracks_tempo
            ((title, [(0, 1, pref ++ "--1"), (1, 1, pref ++ "--2")]) : tracks)

    let pref = "delay %delay | "
    equal (run ">i" pref [("delay", [(0, 0, "1"), (1, 0, "2")])]) $
        Right [(1, 1, pref ++ "--1"), (3, 1, pref ++ "--2")]
    equal (run ">i | delay 2" "" []) $
        Right [(2, 1, "--1"), (3, 1, "--2")]
    equal (run ">i | delay %delay,2" "" []) $
        Right [(2, 1, "--1"), (3, 1, "--2")]
    let pref = "delay %delay | "
    equal (run ">i" pref [("delay", [(0, 0, "1"), (1, 0, "2")])]) $
        Right [(1, 1, pref ++ "--1"), (3, 1, pref ++ "--2")]
    equal (run ">i | delay %delay,1 | delay %delay,1" "" []) $
        Right [(2, 1, "--1"), (3, 1, "--2")]

test_echo = do
    let (logs, convert_warns, perf_warns, _score_events, mmsgs) =
            derive_midi ("echo 2", [(0, 1, "--1"), (1, 1, "--2")])
                [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]

    -- The MIDI test is probably enough.
    equal (logs, convert_warns, perf_warns) ([], [], [])
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100), (2000, 60, 40), (3000, 62, 40)]

test_event_echo = do
    let (logs, convert_warns, perf_warns, _score_events, mmsgs) =
            derive_midi ("e-echo 2", [(0, 1, "--1"), (1, 1, "--2")])
                [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]
    equal (logs, convert_warns, perf_warns) ([], [], [])
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100), (2000, 60, 40), (3000, 62, 40)]

    let (logs, convert_warns, perf_warns, _score_events, mmsgs) =
            derive_midi ("e-echo %edelay", [(0, 1, "--1"), (1, 1, "--2")])
                [("*twelve", [(0, 0, "4c"), (1, 0, "4d")]),
                    ("edelay", [(0, 0, "2"), (1, 0, "4")]),
                    ("echo-times", [(0, 0, "1"), (1, 0, "2")])]
    equal (logs, convert_warns, perf_warns) ([], [], [])
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100),
            (2000, 60, 40), (5000, 62, 40), (9000, 62, 16)]

derive_midi (title, events) tracks =
    (map Log.msg_string logs, convert_warns, perf_warns, score_events, mmsgs)
    where
    derive title = DeriveTest.derive_tracks_tempo
        ((title, events) : tracks)
    result = derive (DeriveTest.default_inst_title ++ " | " ++ title)
    (score_events, logs) = DeriveTest.e_val_right result
    (_perf_events, convert_warns, mmsgs, perf_warns) =
        DeriveTest.perform DeriveTest.default_midi_config score_events
