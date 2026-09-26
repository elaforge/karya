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
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


derive :: (Score.Event -> a) -> UiTest.TrackSpec -> ([a], [Text])
derive extract track = DeriveTest.extract extract $
    DeriveTest.deriveTracks "" [(">", [(0, 8, "")]), track]

test_control_track :: Test
test_control_track = do
    let run = derive (DeriveTest.eControl "cont")
    let events = [(0, 0, "1"), (1, 0, "2")]

    -- various failures
    let (val, logs) = run ("*bork *bork *bork", events)
    equal val []
    stringsLike logs ["track title: parse error"]

    stringsLike (snd (run ("cont", [(0, 0, "abc"), (1, 0, "def")])))
        ["not found: abc", "not found: def"]
    equal (run ("cont", events)) ([[(0, 1), (1, 1), (1, 2)]], [])

test_back_to_back_controls :: Test
test_back_to_back_controls = do
    let run events = DeriveTest.extract (DeriveTest.eControl "c") $
            DeriveTest.deriveTracks ""
                [(">", [(0, 2, ""), (2, 2, "")]), ("c", events)]
    equal (run [(0, 0, "0"), (2, 0, "i 2")]) ([[(0, 0), (2, 2)], [(2, 2)]], [])
    equal (run [(0, 0, "0"), (2, -0, "i 2"), (2, 0, "4")])
        ([[(0, 0), (2, 2)], [(2, 4)]], [])

test_back_to_back_pitches :: Test
test_back_to_back_pitches = do
    let run events = DeriveTest.extract DeriveTest.eNns $
            DeriveTest.deriveTracks ""
                [(">", [(0, 2, ""), (2, 2, "")]), ("*", events)]
    equal (run [(0, 0, "4c"), (2, 0, "i (4d)")])
        ([[(0, NN.c4), (2, NN.d4)], [(2, NN.d4)]], [])
    equal (run [(0, 0, "4c"), (2, -0, "i (4d)"), (2, 0, "4f")])
        ([[(0, NN.c4), (2, NN.d4)], [(2, NN.f4)]], [])

test_hex :: Test
test_hex = do
    let run events = derive (DeriveTest.eControl "cont") ("cont", events)
    equal (run [(0, 0, "`0x`ff"), (1, 0, "`0x`33"), (2, 0, "`0x`00")])
        ([[(0, 1), (1, 1), (1, 0.2), (2, 0.2), (2, 0)]], [])

test_track_expression :: Test
test_track_expression = do
    let run = derive (DeriveTest.eControl "cont")
    equal (run ("cont", [(0, 0, "0"), (4, 0, "i 1")]))
        ([[(0, 0), (4, 1)]], [])
    equal (run ("cont | sh .5", [(0, 0, "0"), (4, 0, "i 1")]))
        ([[(0, 0), (2, 0), (2, 0.5), (4, 0.5), (4, 1)]], [])

    let runPitch = derive DeriveTest.eNns
    equal (runPitch ("* | sh .5", [(0, 0, "4c"), (4, 0, "i (4d)")]))
        ([[(0, NN.c4), (2, NN.c4), (2, NN.cs4), (4, NN.cs4), (4, NN.d4)]], [])

test_derive_control :: Test
test_derive_control = do
    let ex (sig, logs) = (Signal.toPairs sig, map DeriveTest.showLog logs)
    let run events = DeriveTest.extractRun ex $
            DeriveTest.run Ui.empty $ Derive.withDefaultImported $
            Control.deriveControl False (mktrack 10 events) id
    equal (run [(0, 0, "1"), (1, 0, "2")])
        (Right ([(0, 1), (1, 1), (1, 2)], []))
    equal (run [(0, 0, "1"), (2, 0, "i 2")])
        (Right ([(0, 1), (2, 2)], []))
    equal (run [(0, 0, "1"), (2, 0, "i 2"), (4, 0, "i 1")])
        (Right ([(0, 1), (2, 2), (4, 1)], []))

    -- evaluation continues after an error
    equal (run [(0, 0, "1"), (1, 0, "def")])
        (Right ([(0, 1)], ["control generator not found: def"]))
    equal (run [(0, 0, "1"), (1, 0, "def"), (2, 0, "i 2")])
        (Right ([(0, 1), (2, 2)], ["control generator not found: def"]))

mktrack :: ScoreTime -> [UiTest.EventSpec] -> TrackTree.Track
mktrack eventsEnd events = TrackTree.makeTrack ">" evts eventsEnd
    where evts = Events.fromList (map UiTest.makeEvent events)

test_pitch_track :: Test
test_pitch_track = do
    let run = derive DeriveTest.eNns

    let (val, logs) = run ("*no-scale", [(0, 0, "1"), (1, 0, "2")])
    equal val []
    stringsLike logs ["get_scale: unknown scale: no-scale"]

    let (val, logs) = run ("*twelve", [(0, 0, "1"), (1, 0, "2")])
    equal val [[]]
    stringsLike logs ["not found: 1", "not found: 2"]
    let (val, logs) = run
            ("*twelve", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4hc")])
    equal val [[(0, 60), (1, 60), (1, 62)]]
    stringsLike logs ["not found: 4hc"]
    equal (run ("*twelve", [(0, 0, "4c"), (1, 0, "4d")]))
        ([[(0, 60), (1, 60), (1, 62)]], [])
    equal (run ("*twelve", [(0, 0, "4c"), (2, 0, "i (4d)")]))
        ([[(0, 60), (2, 62)]], [])

test_control_merge :: Test
test_control_merge = do
    let run suf addSuf = extract $ DeriveTest.deriveTracks ""
            [ (">", [(0, 5, "")])
            , ("*", [(0, 0, "4c")])
            , ("cont" <> suf, [(0, 0, "0"), (2, 0, "i 2"), (4, 0, "i 0")])
            , ("cont" <> addSuf <> " add", [(0, 0, "1")])
            ]
        extract = DeriveTest.extract $
            (\(ScoreT.Typed typ sig) -> (typ, map (at sig) [0..5]))
                . (Map.! "cont") . Score.eventControls
        at sig = Signal.at sig . RealTime.seconds
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
    let run2 c1 v1 c2 v2 = extract $ DeriveTest.deriveTracks ""
            [ (">", [(0, 10, "")])
            , (c1, [(0, 0, v1)])
            , (c2, [(0, 0, v2)])
            ]
        extract = DeriveTest.extract $ DeriveTest.eControl "cont"
    equal (run2 "cont add" "1" "cont add" "1") ([[(0, 2)]], [])
    -- Default is multiply, set replaces.
    equal (run2 "cont" ".5" "cont" ".5") ([[(0, 0.25)]], [])
    equal (run2 "cont" ".5" "cont set" ".5") ([[(0, 0.5)]], [])

test_default_merge :: Test
test_default_merge = do
    let run control = DeriveTest.extract (DeriveTest.eControl control) $
            DeriveTest.deriveTracks ""
                [ (">", [(0, 4, "")])
                , (c, [(0, 0, ".5")])
                , (c, [(0, 0, ".5")])
                ]
                where c = ScoreT.controlName control
    equal (run "dyn") ([[(0, 0.25)]], [])
    equal (run "t-dia") ([[(0, 1)]], [])

test_trim_signal :: Test
test_trim_signal = do
    let run = DeriveTest.extract DeriveTest.eNns
            . DeriveTest.deriveTracksSetup
                (CallTest.withNoteGenerator "g" cNote) ""
        cNote = CallTest.generator $ Sub.inverting $ \_args -> Call.note
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

test_track_signal_transpose :: Test
test_track_signal_transpose = do
    let run title = eTsigSigs $ DeriveTest.deriveTracksSetup setup title $
            UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")]
        setup = DeriveTest.withTsigTracknums [2]
    equal (run "") [[(0, 60), (1, 60), (1, 62), (2, 62), (2, 64)]]
    -- Make sure the transposition doesn't mess up the signal.  This tests
    -- that PSignal.apply_controls doesn't create pitch signal starting at 0
    -- where the transpose control starts.
    equal (run "t-oct=1") [[(0, 72), (1, 72), (1, 74), (2, 74), (2, 76)]]

test_stash_signal :: Test
test_stash_signal = do
    -- make sure that TrackSignals are recorded when control tracks are derived
    let itrack = (">i", [])
        ctrack = ("cont", [(0, 0, "1"), (1, 0, "0")])
        csig = [(0, 1), (1, 1), (1, 0)]
    let run tracks = eTsigs $ DeriveTest.deriveTracksSetup
            (DeriveTest.withTsigTracknums [1 .. length tracks]) "" tracks
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

test_stash_signal_duplicate_samples :: Test
test_stash_signal_duplicate_samples = do
    let run tracks = eTsigs $ DeriveTest.deriveTracksSetup
            (DeriveTest.withTsigTracknums [1 .. length tracks]) "" tracks
    equal (run
        [ (">", [(0, 1, ""), (1, 1, "d .5 |"), (2, 1, "")])
        , ("dyn", [(0, 0, ".5")])
        ])
        [([(0, 0.5)], 0, 1)]

test_signal_fragments :: Test
test_signal_fragments = do
    let run tsigTracks = eTsigs . DeriveTest.deriveTracksSetup
            (DeriveTest.withTsigs tsigTracks) ""
    equal (run [UiTest.mkTid 2]
            [ (">", [(0, 1, ""), (1, 1, "")])
            , ("dyn", [(0, 0, ".25"), (1, 0, ".5"), (2, 0, ".75")])
            ])
        [([(0, 0.25), (1, 0.25), (1, 0.5), (2, 0.5), (2, 0.75)], 0, 1)]
    equal (run [UiTest.mkTid 2]
            [ (">", [(0, 4, ""), (4, 4, "")])
            , ("dyn", [(0, 0, "0"), (8, 0, "i 1")])
            ])
        [([(0, 0), (8, 1)], 0, 1)]

test_stash_signal_default_tempo :: Test
test_stash_signal_default_tempo = do
    -- Signal is stretched by the default tempo.
    let r = eTsigs $ DeriveTest.deriveTracksSetup
            (DeriveTest.withTsigTracknums [1] <> setTempo) ""
            [("*", [(0, 0, "4c"), (10, 0, "4d"), (20, 0, "4c")])]
        setTempo = DeriveTest.withUi $
            Ui.config#UiConfig.default_#UiConfig.tempo #= 2
    equal r [([(0, 60), (5, 60), (5, 62), (10, 62), (10, 60)], 0, 0.5)]

eTsigSigs :: Derive.Result -> [[(RealTime, Signal.Y)]]
eTsigSigs = map (\(sig, _, _) -> sig) . eTsigs

eTsigs :: Derive.Result -> [([(RealTime, Signal.Y)], ScoreTime, ScoreTime)]
    -- ^ for each track, (signal, shift, stretch)
eTsigs = map snd . eTsigTracks

eTsigTracks :: Derive.Result
    -> [((BlockId, TrackId), ([(RealTime, Signal.Y)], ScoreTime, ScoreTime))]
eTsigTracks = map (second extract) . Map.toList . Derive.rTrackSignals
    where
    extract (Track.TrackSignal sig shift stretch) =
        (Signal.toPairs sig, shift, stretch)
