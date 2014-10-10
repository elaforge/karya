-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Control_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.UiTest as UiTest

import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


do_derive :: (Score.Event -> a) -> UiTest.TrackSpec -> ([a], [String])
do_derive extract track = DeriveTest.extract extract $
    DeriveTest.derive_tracks "" [(">", [(0, 8, "")]), track]

test_control_track = do
    let derive = do_derive (DeriveTest.e_control "cont")
    let events = [(0, 0, "1"), (1, 0, "2")]

    -- various failures
    let (val, logs) = derive ("*bork *bork *bork", events)
    equal val []
    strings_like logs ["track title: control track must be one of"]

    strings_like (snd (derive ("cont", [(0, 0, "abc"), (1, 0, "def")])))
        ["not found: abc", "not found: def"]
    equal (derive ("cont", events)) ([[(0, 1), (1, 2)]], [])

test_hex = do
    let derive events =
            do_derive (DeriveTest.e_control "cont") ("cont", events)
    equal (derive [(0, 0, "`0x`ff"), (1, 0, "`0x`33"), (2, 0, "`0x`00")])
        ([[(0, 1), (1, 0.2), (2, 0)]], [])

test_track_expression = do
    let derive = do_derive (DeriveTest.e_control "cont")
    equal (derive ("cont", [(0, 0, "0"), (4, 0, "i 1")]))
        ([[(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]], [])
    equal (derive ("cont | srate = 2", [(0, 0, "0"), (4, 0, "i 1")]))
        ([[(0, 0), (2, 0.5), (4, 1)]], [])

    let derive_pitch = do_derive DeriveTest.e_nns
    equal (derive_pitch ("*twelve | srate = 2",
            [(0, 0, "4c"), (4, 0, "i (4d)")]))
        ([[(0, 60), (2, 61), (4, 62)]], [])

test_derive_control = do
    let ex (sig, logs) = (Signal.unsignal sig, map DeriveTest.show_log logs)
    let derive events = DeriveTest.extract_run ex $
            DeriveTest.run State.empty $ Derive.with_default_imported $
            Control.derive_control False (mktrack 10 events) id
    equal (derive [(0, 0, "1"), (1, 0, "2")])
        (Right ([(0, 1), (1, 2)], []))
    equal (derive [(0, 0, "1"), (2, 0, "i 2")])
        (Right ([(0, 1), (1, 1.5), (2, 2)], []))
    equal (derive [(0, 0, "1"), (2, 0, "i 2"), (4, 0, "i 1")])
        (Right ([(0, 1), (1, 1.5), (2, 2), (3, 1.5), (4, 1)], []))

    -- evaluation continues after an error
    equal (derive [(0, 0, "1"), (1, 0, "def")])
        (Right ([(0, 1)],
            ["Error: control generator not found: def"]))
    equal (derive [(0, 0, "1"), (1, 0, "def"), (2, 0, "i 2")])
        (Right ([(0, 1), (1, 1.5), (2, 2)],
            ["Error: control generator not found: def"]))

mktrack :: ScoreTime -> [UiTest.EventSpec] -> TrackTree.Track
mktrack events_end events = TrackTree.make_track ">" evts events_end
    where evts = Events.from_list (map UiTest.make_event events)

test_pitch_track = do
    let derive = do_derive DeriveTest.e_nns

    let (val, logs) = derive ("*no-scale", [(0, 0, "1"), (1, 0, "2")])
    equal val []
    strings_like logs ["get_scale: unknown \\*no-scale"]

    let (val, logs) = derive ("*twelve", [(0, 0, "1"), (1, 0, "2")])
    equal val [[]]
    strings_like logs ["not found: 1", "not found: 2"]
    let (val, logs) = derive
            ("*twelve", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4hc")])
    equal val [[(0, 60), (1, 62)]]
    strings_like logs ["not found: 4hc"]
    equal (derive ("*twelve", [(0, 0, "4c"), (1, 0, "4d")]))
        ([[(0, 60), (1, 62)]], [])
    equal (derive ("*twelve", [(0, 0, "4c"), (2, 0, "i (4d)")]))
        ([[(0, 60), (1, 61), (2, 62)]], [])

test_relative_control = do
    let run suf add_suf = extract $ DeriveTest.derive_tracks ""
            [ (">", [(0, 5, "")])
            , ("*", [(0, 0, "4c")])
            , ("cont" ++ suf, [(0, 0, "0"), (2, 0, "i 2"), (4, 0, "i 0")])
            , ("add cont" ++ add_suf, [(0, 0, "1")])
            ]
        extract = DeriveTest.extract $
            (\(Score.Typed typ sig) -> (typ, map (at sig) [0..5]))
                . (Map.! "cont") . Score.event_transformed_controls
        at sig t = Signal.at (RealTime.seconds t) sig
    equal (run "" "") ([(Score.Untyped, [1, 2, 3, 2, 1, 1])], [])
    -- No type on the relative signal means it gets the absolute signal's
    -- type.
    equal (run ":d" "") ([(Score.Diatonic, [1, 2, 3, 2, 1, 1])], [])
    -- And vice versa.
    equal (run "" ":d") ([(Score.Diatonic, [1, 2, 3, 2, 1, 1])], [])
    -- If they both have types, the absolute signal wins.
    equal (run ":c" ":d") ([(Score.Chromatic, [1, 2, 3, 2, 1, 1])], [])

    -- Putting relative and absolute in the wrong order is ok since addition
    -- is a monoid.
    let run2 c1 v1 c2 v2 = extract $ DeriveTest.derive_tracks ""
            [ (">", [(0, 10, "")])
            , (c1, [(0, 0, v1)])
            , (c2, [(0, 0, v2)])
            ]
        extract = DeriveTest.extract $ DeriveTest.e_control "cont"
    equal (run2 "add cont" "1" "add cont" "1") ([[(0, 2)]], [])
    -- Default is multiply, set replaces.
    equal (run2 "cont" ".5" "cont" ".5") ([[(0, 0.25)]], [])
    equal (run2 "cont" ".5" "set cont" ".5") ([[(0, 0.5)]], [])

test_default_merge = do
    let run control = DeriveTest.extract (DeriveTest.e_control
            (Score.control (txt control))) $ DeriveTest.derive_tracks ""
                [ (">", [(0, 4, "")])
                , (control, [(0, 0, ".5")])
                , (control, [(0, 0, ".5")])
                ]
    equal (run "dyn") ([[(0, 0.25)]], [])
    equal (run "t-dia") ([[(0, 1)]], [])

test_stash_signal = do
    -- make sure that TrackSignals are recorded when control tracks are derived
    let itrack = (">i", [])
        ctrack = ("cont", [(0, 0, "1"), (1, 0, "0")])
        csig = [(0, 1), (1, 0)]
    let run tracks = e_tsigs $ DeriveTest.derive_tracks_with_ui id
            (DeriveTest.with_tsig_tracknums [1 .. length tracks]) "" tracks
    let tsig samples p x = (samples, p, x)

    equal (run [ctrack, itrack]) [(csig, 0, 1)]
    -- Constant tempo stretches track sig.
    -- Tempo track itself is unstretched.
    -- Extra sample at the end of the tempo track due to the
    -- 'Tempo.extend_signal' hack.
    let end = RealTime.score UiTest.default_block_end
    equal (run [("tempo", [(0, 0, "2")]), ctrack, itrack]) $
        [ tsig [(0, 2), (end, 2)] 0 1
        , tsig [(0, 1), (0.5, 0)] 0 0.5
        ]

    -- But a complicated tempo makes it unwarp so output is still in RealTime.
    equal (run [("tempo", [(0, 0, "2"), (4, 0, "i 1")]), ctrack, itrack])
        [ tsig [(0, 2), (1, 1.75), (2, 1.5), (3, 1.25), (4, 1), (end, 1)] 0 1
        , tsig [(0, 1), (1, 0)] 0 1
        ]

    -- pitch tracks work too
    let ptrack = ("*", [(0, 0, "4c"), (1, 0, "4d")])
        psig = [(0, 60), (1, 62)]
    equal (run [ptrack, itrack]) [(psig, 0, 1)]

    -- Subtracks should be rendered, and their fragments merged together.
    equal (run [(">", [(0, 3, "")]), ctrack]) [(csig, 0, 1)]
    equal (run [(">", [(0, 1, ""), (1, 1, "")]), ctrack]) [(csig, 0, 1)]
    equal (run [(">", [(0, 1, ""), (1, 1, ""), (2, 1, "")]), ctrack])
        [(csig, 0, 1)]

test_signal_fragments = do
    let run tsig_tracks = e_tsigs . DeriveTest.derive_tracks_with_ui id
            (DeriveTest.with_tsigs tsig_tracks) ""
    equal (run [UiTest.mk_tid 2]
            [ (">", [(0, 1, ""), (1, 1, "")])
            , ("dyn", [(0, 0, ".25"), (1, 0, ".5"), (2, 0, ".75")])
            ])
        [([(0, 0.25), (1, 0.5), (2, 0.75)], 0, 1)]

test_stash_signal_default_tempo = do
    -- Signal is stretched by the default tempo.
    let r = e_tsigs $ DeriveTest.derive_tracks_with_ui id
            (DeriveTest.with_tsig_tracknums [1] . set_tempo) ""
            [("*", [(0, 0, "4c"), (10, 0, "4d"), (20, 0, "4c")])]
        set_tempo = State.config#State.default_#State.tempo #= 2
    equal r [([(0, 60), (5, 62), (10, 60)], 0, 0.5)]

e_tsigs :: Derive.Result -> [([(RealTime, Signal.Y)], ScoreTime, ScoreTime)]
e_tsigs = map snd . e_tsig_tracks

e_tsig_tracks :: Derive.Result
    -> [((BlockId, TrackId), ([(RealTime, Signal.Y)], ScoreTime, ScoreTime))]
e_tsig_tracks = map (second extract) . Map.toList . Derive.r_track_signals
    where
    extract (Track.TrackSignal sig shift stretch) =
        (Signal.unsignal sig, shift, stretch)
