module Derive.Call.Echo_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_delay = do
    let extract = DeriveTest.extract_events DeriveTest.e_event
    let run title pref tracks = extract $ DeriveTest.derive_tracks_tempo
            ((title, [(0, 1, pref ++ "--1"), (1, 1, pref ++ "--2")]) : tracks)

    let pref = "delay %delay | "
    equal (run ">i" pref [("delay", [(0, 0, "1"), (1, 0, "2")])]) $
        [(1, 1, pref ++ "--1"), (3, 1, pref ++ "--2")]
    equal (run ">i | delay 2" "" []) $
        [(2, 1, "--1"), (3, 1, "--2")]
    equal (run ">i | delay %delay,2" "" []) $
        [(2, 1, "--1"), (3, 1, "--2")]
    let pref = "delay %delay | "
    equal (run ">i" pref [("delay", [(0, 0, "1"), (1, 0, "2")])]) $
        [(1, 1, pref ++ "--1"), (3, 1, pref ++ "--2")]
    equal (run ">i | delay %delay,1 | delay %delay,1" "" []) $
        [(2, 1, "--1"), (3, 1, "--2")]

test_echo = do
    let (mmsgs, logs) = perform ("echo 2", [(0, 1, "--1"), (1, 1, "--2")])
            [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]
    equal logs []
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100), (2000, 60, 40), (3000, 62, 40)]

test_event_echo = do
    let (mmsgs, logs) = perform ("e-echo 2", [(0, 1, "--1"), (1, 1, "--2")])
            [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]
    equal logs []
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100), (2000, 60, 40), (3000, 62, 40)]

    let (mmsgs, logs) = perform
            ("e-echo %edelay", [(0, 1, "--1"), (1, 1, "--2")])
                [("*twelve", [(0, 0, "4c"), (1, 0, "4d")]),
                    ("edelay", [(0, 0, "2"), (1, 0, "4")]),
                    ("echo-times", [(0, 0, "1"), (1, 0, "2")])]
    equal logs []
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100),
            (2000, 60, 40), (5000, 62, 40), (9000, 62, 16)]

-- perform :: (String, [(Double, Double, String)]) -> [Ui.UiTest.TrackSpec]
--      -> ([(Integer, Midi.Midi.Message)], [String])
perform (title, events) tracks = DeriveTest.perform_block
    ((DeriveTest.default_inst_title ++ " | " ++ title, events) : tracks)
