module Derive.Call.Echo_test where

import Util.Test

import qualified Derive.DeriveTest as DeriveTest


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
    let derive title tracks = DeriveTest.derive_tracks_tempo
                ((title, [(0, 1, "--1"), (1, 1, "--2")]) : tracks)
    let result = derive (DeriveTest.default_inst_title ++ " | echo 2")
            [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]
    let (events, logs) = DeriveTest.e_val_right result
    let (_perf_events, convert_warns, mmsgs, perf_warns) =
            DeriveTest.perform DeriveTest.default_inst_config events

    -- The MIDI test is probably enough.
    equal logs []
    equal convert_warns []
    equal perf_warns []
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100), (2000, 60, 40), (3000, 62, 40)]
