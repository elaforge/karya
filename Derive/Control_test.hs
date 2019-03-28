-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Control_test where
import qualified Data.Map as Map

import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Sub as Sub
import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.NN as NN
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


derive :: (Score.Event -> a) -> UiTest.TrackSpec -> ([a], [Text])
derive extract track = DeriveTest.extract extract $
    DeriveTest.derive_tracks "" [(">", [(0, 8, "")]), track]

test_control_track = do
    let run = derive (DeriveTest.e_control "cont")
    let events = [(0, 0, "1"), (1, 0, "2")]

    -- various failures
    let (val, logs) = run ("*bork *bork *bork", events)
    equal val []
    strings_like logs ["track title: parse error"]

    strings_like (snd (run ("cont", [(0, 0, "abc"), (1, 0, "def")])))
        ["not found: abc", "not found: def"]
    equal (run ("cont", events)) ([[(0, 1), (1, 1), (1, 2)]], [])

test_back_to_back_controls = do
    let run events = DeriveTest.extract (DeriveTest.e_control "c") $
            DeriveTest.derive_tracks ""
                [(">", [(0, 2, ""), (2, 2, "")]), ("c", events)]
    equal (run [(0, 0, "0"), (2, 0, "i 2")]) ([[(0, 0), (2, 2)], [(2, 2)]], [])
    equal (run [(0, 0, "0"), (2, -0, "i 2"), (2, 0, "4")])
        ([[(0, 0), (2, 2)], [(2, 4)]], [])

test_back_to_back_pitches = do
    let run events = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks ""
                [(">", [(0, 2, ""), (2, 2, "")]), ("*", events)]
    equal (run [(0, 0, "4c"), (2, 0, "i (4d)")])
        ([[(0, NN.c4), (2, NN.d4)], [(2, NN.d4)]], [])
    equal (run [(0, 0, "4c"), (2, -0, "i (4d)"), (2, 0, "4f")])
        ([[(0, NN.c4), (2, NN.d4)], [(2, NN.f4)]], [])

test_hex = do
    let run events = derive (DeriveTest.e_control "cont") ("cont", events)
    equal (run [(0, 0, "`0x`ff"), (1, 0, "`0x`33"), (2, 0, "`0x`00")])
        ([[(0, 1), (1, 1), (1, 0.2), (2, 0.2), (2, 0)]], [])

test_track_expression = do
    let run = derive (DeriveTest.e_control "cont")
    equal (run ("cont", [(0, 0, "0"), (4, 0, "i 1")]))
        ([[(0, 0), (4, 1)]], [])
    equal (run ("cont | sh .5", [(0, 0, "0"), (4, 0, "i 1")]))
        ([[(0, 0), (2, 0), (2, 0.5), (4, 0.5), (4, 1)]], [])

    let run_pitch = derive DeriveTest.e_nns
    equal (run_pitch ("* | sh .5", [(0, 0, "4c"), (4, 0, "i (4d)")]))
        ([[(0, NN.c4), (2, NN.c4), (2, NN.cs4), (4, NN.cs4), (4, NN.d4)]], [])

test_derive_control = do
    let ex (sig, logs) = (Signal.to_pairs sig, map DeriveTest.show_log logs)
    let run events = DeriveTest.extract_run ex $
            DeriveTest.run Ui.empty $ Derive.with_default_imported $
            Control.derive_control False (mktrack 10 events) id
    equal (run [(0, 0, "1"), (1, 0, "2")])
        (Right ([(0, 1), (1, 1), (1, 2)], []))
    equal (run [(0, 0, "1"), (2, 0, "i 2")])
        (Right ([(0, 1), (2, 2)], []))
    equal (run [(0, 0, "1"), (2, 0, "i 2"), (4, 0, "i 1")])
        (Right ([(0, 1), (2, 2), (4, 1)], []))

    -- evaluation continues after an error
    equal (run [(0, 0, "1"), (1, 0, "def")])
        (Right ([(0, 1)],
            ["Error: control generator not found: def"]))
    equal (run [(0, 0, "1"), (1, 0, "def"), (2, 0, "i 2")])
        (Right ([(0, 1), (2, 2)], ["Error: control generator not found: def"]))

mktrack :: ScoreTime -> [UiTest.EventSpec] -> TrackTree.Track
mktrack events_end events = TrackTree.make_track ">" evts events_end
    where evts = Events.from_list (map UiTest.make_event events)

test_pitch_track = do
    let run = derive DeriveTest.e_nns

    let (val, logs) = run ("*no-scale", [(0, 0, "1"), (1, 0, "2")])
    equal val []
    strings_like logs ["get_scale: unknown scale: no-scale"]

    let (val, logs) = run ("*twelve", [(0, 0, "1"), (1, 0, "2")])
    equal val [[]]
    strings_like logs ["not found: 1", "not found: 2"]
    let (val, logs) = run
            ("*twelve", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4hc")])
    equal val [[(0, 60), (1, 60), (1, 62)]]
    strings_like logs ["not found: 4hc"]
    equal (run ("*twelve", [(0, 0, "4c"), (1, 0, "4d")]))
        ([[(0, 60), (1, 60), (1, 62)]], [])
    equal (run ("*twelve", [(0, 0, "4c"), (2, 0, "i (4d)")]))
        ([[(0, 60), (2, 62)]], [])

test_control_merge = do
    let run suf add_suf = extract $ DeriveTest.derive_tracks ""
            [ (">", [(0, 5, "")])
            , ("*", [(0, 0, "4c")])
            , ("cont" <> suf, [(0, 0, "0"), (2, 0, "i 2"), (4, 0, "i 0")])
            , ("cont" <> add_suf <> " add", [(0, 0, "1")])
            ]
        extract = DeriveTest.extract $
            (\(ScoreT.Typed typ sig) -> (typ, map (at sig) [0..5]))
                . (Map.! "cont") . Score.event_controls
        at sig t = Signal.at (RealTime.seconds t) sig
    equal (run "" "") ([(ScoreT.Untyped, [1, 2, 3, 2, 1, 1])], [])
    -- No type on the relative signal means it gets the absolute signal's
    -- type.
    equal (run ":d" "") ([(ScoreT.Diatonic, [1, 2, 3, 2, 1, 1])], [])
    -- And vice versa.
    equal (run "" ":d") ([(ScoreT.Diatonic, [1, 2, 3, 2, 1, 1])], [])
    -- If they both have types, the absolute signal wins.
    equal (run ":c" ":d") ([(ScoreT.Chromatic, [1, 2, 3, 2, 1, 1])], [])

    -- Putting relative and absolute in the wrong order is ok since addition
    -- is a monoid.
    let run2 c1 v1 c2 v2 = extract $ DeriveTest.derive_tracks ""
            [ (">", [(0, 10, "")])
            , (c1, [(0, 0, v1)])
            , (c2, [(0, 0, v2)])
            ]
        extract = DeriveTest.extract $ DeriveTest.e_control "cont"
    equal (run2 "cont add" "1" "cont add" "1") ([[(0, 2)]], [])
    -- Default is multiply, set replaces.
    equal (run2 "cont" ".5" "cont" ".5") ([[(0, 0.25)]], [])
    equal (run2 "cont" ".5" "cont set" ".5") ([[(0, 0.5)]], [])

test_default_merge = do
    let run control = DeriveTest.extract (DeriveTest.e_control control) $
            DeriveTest.derive_tracks ""
                [ (">", [(0, 4, "")])
                , (c, [(0, 0, ".5")])
                , (c, [(0, 0, ".5")])
                ]
                where c = ScoreT.control_name control
    equal (run "dyn") ([[(0, 0.25)]], [])
    equal (run "t-dia") ([[(0, 1)]], [])

test_trim_signal = do
    let run = DeriveTest.extract DeriveTest.e_nns
            . DeriveTest.derive_tracks_setup
                (CallTest.with_note_generator "g" c_note) ""
        c_note = CallTest.generator $ Sub.inverting $ \_args -> Call.note
    equal (run [(">", [(0, 4, "g")]), ("*", [(0, 0, "4c"), (1, 0, "4d")])])
        ([[(0, NN.c4), (1, NN.c4), (1, NN.d4)]], [])
    equal (run
            [ (">", [(0, 4, "g"), (4, 4, "g")])
            , ("*", [(0, 0, "4c"), (4, 0, "4d")])
            ])
        ([[(0, NN.c4), (4, NN.c4)], [(4, NN.d4)]], [])
    equal (run
            [ (">", [(4, -4, "g"), (4, 4, "g")])
            , ("*", [(0, 0, "4c"), (4, 0, "4d")])
            ])
        ([[(4, NN.d4)], [(4, NN.d4)]], [])
        -- c4 is now not included in (4, -4) because of the usual selection
        -- range rules.

-- * track signal

test_track_signal_transpose = do
    let run title = e_tsig_sigs $ DeriveTest.derive_tracks_setup setup title $
            UiTest.note_track [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")]
        setup = DeriveTest.with_tsig_tracknums [2]
    equal (run "") [[(0, 60), (1, 60), (1, 62), (2, 62), (2, 64)]]
    -- Make sure the transposition doesn't mess up the signal.  This tests
    -- that PSignal.apply_controls doesn't create pitch signal starting at 0
    -- where the transpose control starts.
    equal (run "%t-oct=1") [[(0, 72), (1, 72), (1, 74), (2, 74), (2, 76)]]

test_stash_signal = do
    -- make sure that TrackSignals are recorded when control tracks are derived
    let itrack = (">i", [])
        ctrack = ("cont", [(0, 0, "1"), (1, 0, "0")])
        csig = [(0, 1), (1, 1), (1, 0)]
    let run tracks = e_tsigs $ DeriveTest.derive_tracks_setup
            (DeriveTest.with_tsig_tracknums [1 .. length tracks]) "" tracks
    let tsig samples p x = (samples, p, x)

    equal (run [ctrack, itrack]) [(csig, 0, 1)]
    -- Constant tempo stretches track sig.
    -- Tempo track itself is unstretched.
    equal (run [("tempo", [(0, 0, "2")]), ctrack, itrack]) $
        [ tsig [(0, 2)] 0 1
        , tsig [(0, 1), (0.5, 1), (0.5, 0)] 0 0.5
        ]

    -- But a complicated tempo makes it unwarp so output is still in RealTime.
    equal (run [("tempo", [(0, 0, "2"), (4, 0, "i 1")]), ctrack, itrack])
        [ tsig [(0, 2), (4, 1)] 0 1
        , tsig [(0, 1), (1, 1), (1, 0)] 0 1
        ]

    -- pitch tracks work too
    let ptrack = ("*", [(0, 0, "4c"), (1, 0, "4d")])
    equal (run [ptrack, itrack]) [([(0, 60), (1, 60), (1, 62)], 0, 1)]

    -- Subtracks should be rendered, and their fragments merged together.
    equal (run [(">", [(0, 3, "")]), ctrack]) [(csig, 0, 1)]
    equal (run [(">", [(0, 1, ""), (1, 1, "")]), ctrack]) [(csig, 0, 1)]
    equal (run [(">", [(0, 1, ""), (1, 1, ""), (2, 1, "")]), ctrack])
        [(csig, 0, 1)]

test_stash_signal_duplicate_samples = do
    let run tracks = e_tsigs $ DeriveTest.derive_tracks_setup
            (DeriveTest.with_tsig_tracknums [1 .. length tracks]) "" tracks
    equal (run
        [ (">", [(0, 1, ""), (1, 1, "d .5 |"), (2, 1, "")])
        , ("dyn", [(0, 0, ".5")])
        ])
        [([(0, 0.5)], 0, 1)]

test_signal_fragments = do
    let run tsig_tracks = e_tsigs . DeriveTest.derive_tracks_setup
            (DeriveTest.with_tsigs tsig_tracks) ""
    equal (run [UiTest.mk_tid 2]
            [ (">", [(0, 1, ""), (1, 1, "")])
            , ("dyn", [(0, 0, ".25"), (1, 0, ".5"), (2, 0, ".75")])
            ])
        [([(0, 0.25), (1, 0.25), (1, 0.5), (2, 0.5), (2, 0.75)], 0, 1)]
    equal (run [UiTest.mk_tid 2]
            [ (">", [(0, 4, ""), (4, 4, "")])
            , ("dyn", [(0, 0, "0"), (8, 0, "i 1")])
            ])
        [([(0, 0), (8, 1)], 0, 1)]

test_stash_signal_default_tempo = do
    -- Signal is stretched by the default tempo.
    let r = e_tsigs $ DeriveTest.derive_tracks_setup
            (DeriveTest.with_tsig_tracknums [1] <> set_tempo) ""
            [("*", [(0, 0, "4c"), (10, 0, "4d"), (20, 0, "4c")])]
        set_tempo = DeriveTest.with_ui $ Ui.config#Ui.default_#Ui.tempo #= 2
    equal r [([(0, 60), (5, 60), (5, 62), (10, 62), (10, 60)], 0, 0.5)]

e_tsig_sigs :: Derive.Result -> [[(RealTime, Signal.Y)]]
e_tsig_sigs = map (\(sig, _, _) -> sig) . e_tsigs

e_tsigs :: Derive.Result -> [([(RealTime, Signal.Y)], ScoreTime, ScoreTime)]
    -- ^ for each track, (signal, shift, stretch)
e_tsigs = map snd . e_tsig_tracks

e_tsig_tracks :: Derive.Result
    -> [((BlockId, TrackId), ([(RealTime, Signal.Y)], ScoreTime, ScoreTime))]
e_tsig_tracks = map (second extract) . Map.toList . Derive.r_track_signals
    where
    extract (Track.TrackSignal sig shift stretch) =
        (Signal.to_pairs sig, shift, stretch)
