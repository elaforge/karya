module Derive.Call.Trill_test where
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Trill as Trill
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Signal as Signal



test_note_trill = do
    let run notes pitches = extract $ DeriveTest.derive_tracks
            [(">", notes), ("*", pitches)]
        extract = DeriveTest.extract DeriveTest.e_note2
    equal (run [(0, 3, "tr 1 1")] [(0, 0, "4c")])
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c")], [])
    equal (run [(0, 3, "tr 1 1")] [(0, 0, "4a"), (2, 0, "i (4b)")])
        ([(0, 1, "4a"), (1, 1, "4b 50"), (2, 1, "4b")], [])

test_tremolo = do
    let run tempo notes = extract $ DeriveTest.derive_tracks
            [("tempo", [(0, 0, tempo)]), (">", notes), ("*", [(0, 0, "4c")])]
        extract = DeriveTest.extract DeriveTest.e_note2
    -- RealTime
    equal (run "1" [(0, 2, "trem 1s")]) ([(0, 1, "4c"), (1, 1, "4c")], [])
    equal (run "2" [(0, 4, "trem 1s")]) ([(0, 1, "4c"), (1, 1, "4c")], [])
    equal (run "1" [(2, 2, "trem 1s")]) ([(2, 1, "4c"), (3, 1, "4c")], [])
    equal (run "1" [(0, 2.5, "trem 1s")]) ([(0, 1, "4c"), (1, 1.5, "4c")], [])
    -- ScoreTime
    equal (run "1" [(0, 2, "trem 1t")]) ([(0, 1, "4c"), (1, 1, "4c")], [])
    equal (run "1" [(2, 2, "trem 1t")]) ([(2, 1, "4c"), (3, 1, "4c")], [])
    equal (run "2" [(0, 2, "trem 1t")]) ([(0, 0.5, "4c"), (0.5, 0.5, "4c")], [])

test_tremolo_transformer = do
    let run notes = extract $ DeriveTest.derive_tracks $
            [ (">", notes)
            , ("*", [(s, 0, p) | ((s, _, _), p)
                <- zip notes (cycle ["4a", "4b", "4c"])])
            ]
        extract = DeriveTest.extract_events DeriveTest.e_note2
    equal (run [(0, 2, ""), (2, 2, "trem 1 |"), (4, 2, "")])
        [ (0, 2, "4a")
        , (2, 1, "4b"), (3, 1, "4b")
        , (4, 2, "4c")
        ]
    equal (run [(0, 2, ""), (3, 3, "trem 1 |"), (6, 1, "")])
        [ (0, 2, "4a")
        , (3, 1, "4b"), (4, 1, "4b"), (5, 1, "4b")
        , (6, 1, "4c")
        ]
    -- Tremolo on a 0 dur note extends to the next event.
    equal (run [(0, 2, ""), (2, 0, "trem 1 |"), (4, 2, "")])
        [ (0, 2, "4a")
        , (2, 0, "4b"), (3, 0, "4b")
        , (4, 2, "4c")
        ]

-- * pitch calls

test_trill = do
    let run text = extract $ DeriveTest.derive_tracks
            [(">", [(0, 3, "")]), ("*", [(0, 0, text), (3, 0, "--")])]
        extract = DeriveTest.extract DeriveTest.e_pitch
    -- -- Defaults to diatonic.
    equal (run "tr (4c) 1 1")
        ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run "tr (4c) 1c 1")
        ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run "tr (4c) 1d 1")
        ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run "tr (4c) -1d 1")
        ([[(0, 60), (1, 59), (2, 60)]], [])

    let run_neighbor suffix val = extract $ DeriveTest.derive_tracks
            [ (">", [(0, 3, "")])
            , ("trill-neighbor" ++ suffix, [(0, 0, val)])
            , ("*", [(0, 0, "tr (4c) _ 1"), (3, 0, "--")])
            ]
    equal (run_neighbor "" "1") ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run_neighbor "" "-2") ([[(0, 60), (1, 57), (2, 60)]], [])
    equal (run_neighbor ":d" "1") ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run_neighbor ":d" "-2") ([[(0, 60), (1, 57), (2, 60)]], [])
    equal (run_neighbor ":c" "1") ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run_neighbor ":c" "-2") ([[(0, 60), (1, 58), (2, 60)]], [])

    let run_speed suffix = extract $ DeriveTest.derive_tracks
            [ ("tempo", [(0, 0, "2")])
            , (">", [(0, 3, "")])
            , ("trill-speed" ++ suffix, [(0, 0, "2")])
            , ("*", [(0, 0, "tr (4c) 1"), (3, 0, "--")])
            ]
        trill xs = [zip xs (cycle [60, 62])]
    equal (run_speed "") (trill [0, 0.5, 1], [])
    equal (run_speed ":s") (trill [0, 0.5, 1], [])
    equal (run_speed ":t") (trill [0, 0.25, 0.5, 0.75, 1, 1.25, 1.5], [])
    equal (run_speed ":d") ([[]],
        ["Error: expected time type for %trill-speed,14s but got Diatonic"])

test_moving_trill = do
    -- Ensure a diatonic trill on a moving base note remains correct.
    let run tracks = extract $ DeriveTest.derive_tracks $
            (">", [(0, 6, "")]) : tracks
        extract = DeriveTest.extract DeriveTest.e_pitch
    -- Trill transitions from 2 semitones to 1 semitone.
    equal (run
        [ ("*", [(0, 0, "4a"), (4, 0, "i (4b)")])
        , ("t-diatonic", [(0, 0, "tr 1 1"), (6, 0, "--")])
        ])
        ([[(0, 69), (1, 71.25), (2, 70), (3, 71.75), (4, 71), (5, 72),
            (6, 71)]], [])
    equal (run
        [ ("*", [(0, 0, "4a"), (4, 0, "i (4b)")])
        , ("t-chromatic", [(0, 0, "tr 1 1"), (6, 0, "--")])
        ])
        ([[(0, 69), (1, 70.5), (2, 70), (3, 71.5), (4, 71), (5, 72),
            (6, 71)]], [])

test_real_trill = do
    let f = Trill.real_trill Trill.UnisonFirst (0, 1)
        run = extract . DeriveTest.run State.empty
        extract = DeriveTest.extract_run Signal.unsignal
    equal (run $ f (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0)]
    equal (run $ f (con (-1)) (con 2)) $
        Right [(0, 0), (0.5, -1), (1, 0)]
    -- Cycles must be integral.
    equal (run $ f (con 1) (con 1.9)) $
        Right [(0, 0)]

    -- Trill speed is in real time and not affected by stretch.
    equal (run $ Derive.d_stretch 2 $ f (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1), (2, 0)]

    -- It still produces an integral number of cycles.
    equal (run $ Derive.d_stretch 1.8 $ f (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0)]

    -- Changing depth signal.
    equal (run $ f (Signal.signal [(0, 1), (0.5, 2)]) (con 4)) $
        Right [(0, 0), (0.25, 1), (0.5, 0), (0.75, 2), (1, 0)]
    equal (run $ f (con 1) (Signal.signal [(0, 2), (0.5, 4)])) $
        Right [(0, 0), (0.5, 1), (0.75, 0)]

test_score_trill = do
    let f dur = Trill.score_trill Trill.UnisonFirst (0, dur)
        run = extract . DeriveTest.run State.empty
        extract = DeriveTest.extract_run Signal.unsignal
    equal (run $ f 1 (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0)]

    -- If the event was length 2 there should be 2 cycles
    equal (run $ f 2 (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1), (2, 0)]

    -- Ignores a bit of extra times since cycles must be integral.
    equal (run $ f 2.5 (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1), (2, 0)]

    -- Trill speed affected by stretch.
    equal (run $ Derive.d_stretch 2 $ f 1 (con 1) (con 2)) $
        Right [(0, 0), (1, 1), (2, 0)]

test_pitch_trill = do
    equal (CallTest.run_pitch [(0, "tr (4e) 2 2"), (2.8, "4c")]) $
        zip [0, 0.5, 1, 1.5, 2] (cycle [64, 67]) ++ [(2.8, 60)]


-- * control calls

test_control_trill = do
    let run tempo text = extract $ DeriveTest.derive_tracks
            [ ("tempo", [(0, 0, show tempo)])
            , (">", [(0, 3, "")])
            , ("cont", [(0, 0, text), (3, 0, "--")])
            ]
        extract = DeriveTest.extract (DeriveTest.e_control "cont")
        trill xs = zip xs (cycle [0, 1])
    equal (run 1 "tr 1 1") ([trill [0, 1, 2]], [])
    -- Defaults to RealTime, but stretches with ScoreTime if asked.
    equal (run 0.5 "tr 1 1") ([trill [0, 1, 2, 3, 4, 5, 6]], [])
    equal (run 0.5 "tr 1 1s") ([trill [0, 1, 2, 3, 4, 5, 6]], [])
    equal (run 0.5 "tr 1 1t") ([trill [0, 2, 4]], [])
    equal (run 1 "tr 1 1d")
        ([[(0, 0)]], ["Error: expected time type for 1d but got Diatonic"])

-- * util

con = Signal.constant
