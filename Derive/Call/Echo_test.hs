module Derive.Call.Echo_test where
import Util.Test
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_delay = do
    let run title pref tracks = DeriveTest.extract_events DeriveTest.e_event $
            DeriveTest.derive_tracks (tracks ++ [event])
            where
            event = (title, [(0, 1, pref ++ "--1"), (1, 1, pref ++ "--2")])

    -- delay notes with a delay signal
    let pref = "d %delay | "
    equal (run ">i" pref [("delay", [(0, 0, "1"), (1, 0, "2")])]) $
        [(1, 1, pref ++ "--1"), (3, 1, pref ++ "--2")]
    equal (run ">i | d 2" "" []) $
        [(2, 1, "--1"), (3, 1, "--2")]
    equal (run ">i | d %delay,2" "" []) $
        [(2, 1, "--1"), (3, 1, "--2")]
    let pref = "d %delay | "
    equal (run ">i" pref [("delay", [(0, 0, "1"), (1, 0, "2")])]) $
        [(1, 1, pref ++ "--1"), (3, 1, pref ++ "--2")]
    equal (run ">i | d %delay,1s | d %delay,1s" "" []) $
        [(2, 1, "--1"), (3, 1, "--2")]

test_delay_inverted = do
    let run text = extract $ DeriveTest.derive_tracks
            [ ("tempo", [(0, 0, "2")])
            , (">i", [(2, 2, text)])
            , ("*twelve", [(0, 0, "4c"), (2, 0, "4d")])
            ]
        extract = DeriveTest.extract_events DeriveTest.e_note2
    equal (run "d 2s |") [(2, 1, "4d")]
    equal (run "d .1r |") [(1.1, 1.0, "4d")]

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

perform :: (String, [(Double, Double, String)]) -> [UiTest.TrackSpec]
     -> ([(Integer, Midi.Message)], [String])
perform (title, events) tracks = DeriveTest.perform_block
    (tracks ++ [(DeriveTest.default_inst_title ++ " | " ++ title, events)])
