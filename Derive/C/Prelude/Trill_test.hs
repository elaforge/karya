-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Trill_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Perform.NN as NN
import qualified Perform.Signal as Signal

import Global


test_note_trill = do
    let run tempo notes pitches = DeriveTest.extract extract $ derive_tracks
            [("tempo", [(0, 0, showt tempo)]), (">", notes), ("*", pitches)]
        extract = DeriveTest.e_note
    equal (run 1 [(0, 3, "tr 1 1")] [(0, 0, "4c")])
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c")], [])
    equal (run 1 [(0, 3, "tr 2 1")] [(0, 0, "4a"), (2, 0, "i (4b)")])
        ([(0, 1, "4a"), (1, 1, "5d"), (2, 1, "4b")], [])
    -- Without the eta argument to 'integral_cycles', I wind up with a super
    -- short note that fell right below the "full cycle" threshold.
    let (notes, logs) = run 1.75
            [(7, 1, "tr"), (8, 1, "")] [(7, 0, "5b"), (8, 0, "3c")]
    equal logs []
    check ("dur > 0.05 " <> pretty notes) $ all (>0.05) [d | (_, d, _) <- notes]

    -- TODO this is doable but not implemented
    -- equal (run 1 [(0, 3, "tr (4db) 1")] [(0, 0, "4c")])
    --     ([(0, 1, "4c"), (1, 1, "4db"), (2, 1, "4c")], [])

    let run_nn = DeriveTest.extract e_nn . derive_tracks . UiTest.note_track
        e_nn e = (Score.event_start e, Score.initial_nn e)
    equal (run_nn [(0, 3, "tr 1c 1 -- 4c")])
        ([(0, Just NN.c4), (1, Just NN.cs4), (2, Just NN.c4)], [])
    equal (run_nn [(0, 3, "tr (4db) 1 -- 4c")])
        ([(0, Just NN.c4), (1, Just NN.cs4), (2, Just NN.c4)], [])

test_note_trill_ly = do
    let run = LilypondTest.measures ["repeat"]
            . LilypondTest.derive_tracks . UiTest.note_track
    equal (run [(0, 4, "tr -- 3c")]) (Right "c1 \\trill", [])
    equal (run [(0, 2, "tr -- 3c")]) (Right "c2 \\trill r2", [])
    equal (run [(0, 4, "tr 1c -- 3c")]) (Right "c1 ^\\trFlat", [])
    equal (run [(0, 4, "tr 7c -- 3c")])
        (Right "\\repeat tremolo 16 { c32( g32) }", [])
    -- Split across measures.
    equal (run [(0, 8, "tr 7c -- 3c")])
        (Right "\\repeat tremolo 16 { c32( g32) }\
            \ | \\repeat tremolo 16 { c32( g32) }", [])
    equal (run [(0, 4, "tr 4 -- 3c")])
        (Right "\\repeat tremolo 16 { c32( g32) }", [])
    equal (run [(0, 4, "tr (3gb) -- 3c")])
        (Right "\\repeat tremolo 16 { c32( gf32) }", [])

    -- Attributes propagate properly.
    equal (run [(0, 2, "+pizz -- 3c"), (2, 4, "tr 4 -- 3c"), (6, 2, "4f")])
        (Right "c2 ^\"pizz.\" \\repeat tremolo 8 { c32( ^\"arco\" g32) }\
            \ | \\repeat tremolo 8 { c32( g32) } f'2", [])
    equal (run [(0, 2, "+pizz -- 3c"), (2, 4, "+pizz | tr 4 -- 3c"),
            (6, 2, "4f")])
        (Right "c2 ^\"pizz.\" \\repeat tremolo 8 { c32( g32) }\
            \ | \\repeat tremolo 8 { c32( g32) } f'2 ^\"arco\"", [])

test_note_trill_ly_code = do
    -- Code attaches to the notes inside the tremolo.
    let run = LilypondTest.measures ["f", "repeat"]
            . LilypondTest.derive_tracks . concatMap UiTest.note_track
    equal (run [[(0, 0, "dyn f --")], [(0, 4, "tr 4 -- 3c")]])
        (Right "\\repeat tremolo 16 { c32( \\f g32) }", [])
    equal (run [[(4, 0, "dyn f --")], [(0, 8, "tr 4 -- 3c")]])
        (Right "\\repeat tremolo 16 { c32( g32) }\
            \ | \\repeat tremolo 16 { c32( \\f g32) }", [])

test_note_trill_ly_style = do
    let run = LilypondTest.measures
                [ "pitchedTrill", "startTrillSpan", "stopTrillSpan", "repeat"
                , "trill"
                ]
            . LilypondTest.derive_tracks . UiTest.note_track
    equal (run [(0, 6, "tr-style=span | tr -- 3c")])
        ( Right "\\pitchedTrill c1~ \\startTrillSpan d | c2 \\stopTrillSpan r2"
        , []
        )
    equal (run [(0, 6, "tr-style=tremolo | tr -- 3c")])
        (Right "\\repeat tremolo 16 { c32( d32) }\
            \ | \\repeat tremolo 8 { c32( d32) } r2", [])
    equal (run [(0, 6, "tr-style=tr | tr -- 3c")])
        (Right "c1~ \\trill | c2 \\trill r2", [])
    equal (run [(0, 4, "key=c-min | trill-style=tr | tr (3e) -- 3d")])
        (Right "d1 ^\\trNatural", [])

    -- with accidentals
    -- Also 1c figures out d flat instead of c#.
    equal (run [(0, 4, "tr-style=span | tr 1c -- 3c")])
        ( Right "\\pitchedTrill c1 \\startTrillSpan df \\stopTrillSpan"
        , []
        )
    equal (run [(0, 6, "tr-style=tr | tr 1c -- 3c")])
        (Right "c1~ ^\\trFlat | c2 ^\\trFlat r2", [])

test_attr_trill = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_setup with ""
            . UiTest.note_track
        with = CallTest.with_note_generator "tr"
            (Trill.c_note_trill True Nothing Nothing)
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    equal (run [(0, 1, "tr -- 4c")]) ([((0, 1, "4c"), "+trill+whole")], [])
    equal (run [(0, 1, "tr 1c -- 4c")]) ([((0, 1, "4c"), "+half+trill")], [])
    equal (run [(0, 1, "tr -- 4e")]) ([((0, 1, "4e"), "+half+trill")], [])
    equal (run [(0, 2, "tr 3c 1 -- 4e")])
        ([((0, 1, "4e"), "+trill"), ((1, 1, "4g"), "+trill")], [])

test_tremolo = do
    let run tempo notes = extract $ derive_tracks
            [("tempo", [(0, 0, tempo)]), (">", notes), ("*", [(0, 0, "4c")])]
        extract = DeriveTest.extract DeriveTest.e_note
    -- RealTime
    equal (run "1" [(0, 2, "trem 1s")]) ([(0, 1, "4c"), (1, 1, "4c")], [])
    equal (run "2" [(0, 4, "trem 1s")]) ([(0, 1, "4c"), (1, 1, "4c")], [])
    equal (run "1" [(2, 2, "trem 1s")]) ([(2, 1, "4c"), (3, 1, "4c")], [])
    equal (run "1" [(0, 2.5, "trem 1s")]) ([(0, 1, "4c"), (1, 1.5, "4c")], [])
    -- ScoreTime
    equal (run "1" [(0, 2, "trem 1t")]) ([(0, 1, "4c"), (1, 1, "4c")], [])
    equal (run "1" [(2, 2, "trem 1t")]) ([(2, 1, "4c"), (3, 1, "4c")], [])
    equal (run "2" [(0, 2, "trem 1t")]) ([(0, 0.5, "4c"), (0.5, 0.5, "4c")], [])
    -- hold
    equal (run "1" [(0, 4, "hold=2 | trem 1s")])
        ([(0, 2, "4c"), (2, 1, "4c"), (3, 1, "4c")], [])
    equal (run "1" [(0, 4, "hold=3 | trem 1s")])
        ([(0, 3, "4c"), (3, 1, "4c")], [])
    equal (run "1" [(0, 4, "hold=4 | trem 1s")])
        ([(0, 4, "4c")], [])

test_full_notes = do
    let f = Trill.full_notes
    equal (f 2.5 [0, 1, 2]) [0, 1, 2.5]
    equal (f 2 [0, 1, 2]) [0, 1, 2]
    equal (f 0.5 [0]) [0, 0.5]
    equal (f 0.5 [1]) []

test_tremolo_transformer = do
    let run notes = extract $ derive_tracks $
            [ (">", notes)
            , ("*", [(s, 0, p) | ((s, _, _), p)
                <- zip notes (cycle ["4a", "4b", "4c"])])
            ]
        extract = DeriveTest.extract_events DeriveTest.e_note
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

test_chord_tremolo = do
    let run dur notes1 notes2 = DeriveTest.extract DeriveTest.e_pitch $
            DeriveTest.derive_tracks_setup skel "" $
                (">", [(0, dur, "trem 1s")])
                : concatMap UiTest.note_track [notes1, notes2]
        skel = DeriveTest.with_skel [(1, 2), (1, 4), (2, 3), (4, 5)]
    equal (run 2 [(0, 2, "4c")] [(0, 2, "4d")])
        (["4c", "4d"], [])
    equal (run 4 [(0, 4, "4c")] [(0, 2, "4d")])
        (["4c", "4d", "4c", "4c"], [])
    equal (run 4 [(0, 2, "4c")] [(0, 2, "4d")])
        (["4c", "4d"], [])
    equal (run 6 [(0, 6, "4c")] [(0, 2, "4d"), (4, 2, "4e")])
        (["4c", "4d", "4c", "4c", "4e", "4c"], [])

test_chord_tremolo_function = do
    let f dur = map ex_event . Trill.chord_tremolo (Seq.range 0 dur 1)
            . map (map mkevent)
        mkevent (s, d, n) = Sub.Event s d (n :: Char)
        ex_event (Sub.Event s d n) = (s, d, n)
    equal (f 3 []) []
    equal (f 6 [[(0, 4, 'a')], [(0, 4, 'b')]])
        [(0, 1, 'a'), (1, 1, 'b'), (2, 1, 'a'), (3, 1, 'b')]

-- * pitch calls

test_trill = do
    let run text = extract $ derive_tracks
            [(">", [(0, 3, "")]), ("*", [(0, 0, text), (3, 0, "--|")])]
        extract = DeriveTest.extract DeriveTest.e_nns
    -- Defaults to diatonic.
    equal (run "tr (4c) 1 1")
        ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run "tr (4c) 1c 1")
        ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run "tr (4c) 1d 1")
        ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run "tr (4c) -1d 1")
        ([[(0, 60), (1, 59), (2, 60)]], [])

    let run_neighbor suffix val = extract $ derive_tracks
            [ (">", [(0, 3, "")])
            , ("tr-neighbor" <> suffix, [(0, 0, val)])
            , ("*", [(0, 0, "tr (4c) _ 1"), (3, 0, "--|")])
            ]
    equal (run_neighbor "" "1") ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run_neighbor "" "-2") ([[(0, 60), (1, 57), (2, 60)]], [])
    equal (run_neighbor ":d" "1") ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run_neighbor ":d" "-2") ([[(0, 60), (1, 57), (2, 60)]], [])
    equal (run_neighbor ":c" "1") ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run_neighbor ":c" "-2") ([[(0, 60), (1, 58), (2, 60)]], [])

    let run_speed suffix = extract $ derive_tracks
            [ ("tempo", [(0, 0, "2")])
            , (">", [(0, 3, "")])
            , ("tr-speed" <> suffix, [(0, 0, "2")])
            , ("*", [(0, 0, "tr (4c) 1"), (3, 0, "--|")])
            ]
        trill xs = [zip xs (cycle [60, 62])]
    -- 3 at tempo 2 means the trill should end at 1.5.
    equal (run_speed "") (trill [0, 0.5, 1], [])
    equal (run_speed ":s") (trill [0, 0.5, 1], [])
    equal (run_speed ":t") (trill [0, 0.25, 0.5, 0.75, 1, 1.25], [])
    equal (run_speed ":d") ([[]],
        ["Error: expected time type for %tr-speed,14s but got Diatonic"])

test_trill_start_end = do
    let run ex text = DeriveTest.extract ex $ derive_tracks
            [(">", [(0, 3, "")]), ("*", [(0, 0, text), (3, 0, "--|")])]
        nns = map snd . DeriveTest.e_nns
    equal (run nns "tr (4c) 1 1") ([[60, 62, 60]], [])
    equal (run nns "tr-start = high | tr (4c) 1 1") ([[62, 60, 62]], [])
    -- Default is ignored for high and low variants.
    equal (run nns "tr-mode = low | tr^ (4c) 1 1") ([[62, 60, 62]], [])
    equal (run nns "tr-mode = high | tr_ (4c) 1 1") ([[60, 62, 60]], [])

    -- Start, end, and adjust.
    equal (run DeriveTest.e_nns "tr-adjust = shorten | tr^_ (4c) 1 1")
        ([[(0, 62), (1, 60)]], [])
    equal (run DeriveTest.e_nns "tr-adjust = stretch | tr^_ (4c) 1 1")
        ([[(0, 62), (3, 60)]], [])

test_trill_hold = do
    let run text = DeriveTest.extract DeriveTest.e_nns $ derive_tracks
            [(">", [(0, 3, "")]), ("*", [(0, 0, text), (3, 0, "--|")])]
    equal (run "hold = 1 | tr (4c) 1 1") ([[(0, 60), (2, 62)]], [])
    -- Still respects start and end restrictions.
    equal (run "hold = 1 | tr__ (4c) 1 1") ([[(0, 60)]], [])
    equal (run "hold = 4 | tr (4c) 1 1") ([[(0, 60)]], [])

test_moving_trill = do
    -- Ensure a diatonic trill on a moving base note remains correct.
    let run tracks = extract $ derive_tracks $
            (">", [(0, 6, "")]) : tracks
        extract = DeriveTest.extract DeriveTest.e_nns
    -- Trill transitions from 2 semitones to 1 semitone.
    equal (run
        [ ("*", [(0, 0, "4a"), (4, 0, "i (4b)")])
        , ("t-dia", [(0, 0, "tr 1 1"), (6, 0, "--|")])
        ])
        ([[(0, 69), (1, 71.25), (2, 70), (3, 71.75), (4, 71), (5, 72)]], [])
    equal (run
        [ ("*", [(0, 0, "4a"), (4, 0, "i (4b)")])
        , ("t-chrom", [(0, 0, "tr 1 1"), (6, 0, "--|")])
        ])
        ([[(0, 69), (1, 70.5), (2, 70), (3, 71.5), (4, 71), (5, 72)]], [])

test_real_trill = do
    let f neighbor speed = fst <$> Trill.get_trill_control (0, 1)
            Nothing Nothing Trill.Shorten (BaseTypes.RealDuration 0)
            (mkcontrol Score.Chromatic neighbor) (mkcontrol Score.Real speed)
        run = DeriveTest.extract_run id . DeriveTest.run Ui.empty
        cnst = Signal.constant
    equal (run $ f (cnst 1) (cnst 2)) $ Right [(0, 0), (0.5, 1)]
    equal (run $ f (cnst (-1)) (cnst 2)) $ Right [(0, 0), (0.5, -1)]

    -- Trill speed is in real time and not affected by stretch.
    equal (run $ Derive.stretch 2 $ f (cnst 1) (cnst 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1)]

    -- Changing depth signal.
    equal (run $ f (Signal.from_pairs [(0, 1), (0.5, 1), (0.5, 2)]) (cnst 4)) $
        Right [(0, 0), (0.25, 1), (0.5, 0), (0.75, 2)]
    equal (run $ f (cnst 1) (Signal.from_pairs [(0, 2), (0.5, 4)])) $
        Right [(0, 0), (0.5, 1), (0.75, 0)]

test_score_trill = do
    let f dur neighbor speed = fst <$> Trill.get_trill_control (0, dur)
            Nothing Nothing Trill.Shorten (BaseTypes.RealDuration 0)
            (mkcontrol Score.Chromatic neighbor) (mkcontrol Score.Score speed)
        run = DeriveTest.extract_run id . DeriveTest.run Ui.empty
        cnst = Signal.constant
    equal (run $ f 1 (cnst 1) (cnst 2)) $
        Right [(0, 0), (0.5, 1)]

    -- If the event was length 2 there should be 2 cycles
    equal (run $ f 2 (cnst 1) (cnst 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1)]

    -- Trill speed affected by stretch.
    equal (run $ Derive.stretch 2 $ f 1 (cnst 1) (cnst 2)) $
        Right [(0, 0), (1, 1)]

mkcontrol :: Score.Type -> Signal.Control -> BaseTypes.ControlRef
mkcontrol typ = BaseTypes.ControlSignal . Score.Typed typ

test_pitch_trill = do
    equal (CallTest.run_pitch "" [(0, "tr (4e) 2 2"), (2.8, "4c")])
        [ (0, NN.e4), (0.5, NN.e4)
        , (0.5, NN.g4), (1, NN.g4)
        , (1, NN.e4), (1.5, NN.e4)
        , (1.5, NN.g4), (2, NN.g4)
        , (2, NN.e4)
        , (2.8, NN.e4), (2.8, NN.c4)
        ]

test_xcut_pitch = do
    let f tracks = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks_linear "" $
            (">", [(0, 4, "")]) : tracks
    equal (f
            [ ("* #xcut1", [(0, 0, "4c")])
            , ("* #xcut2", [(0, 0, "5c")])
            , ("*", [(0, 0, "xcut _ _ 1"), (4, 0, "--|")])
            ])
        ([[(0, 60), (1, 72), (2, 60), (3, 72)]], [])
    equal (f [("*", [(0, 0, "xcut (4c) (5c) 1"), (4, 0, "--|")])])
        ([[(0, 60), (1, 72), (2, 60), (3, 72)]], [])


-- * control calls

test_control_trill = do
    let run tempo text = extract $ derive_tracks
            [ ("tempo", [(0, 0, showt tempo)])
            , (">", [(0, 3, "")])
            , ("cont", [(0, 0, text), (3, 0, "--|")])
            ]
        extract = DeriveTest.extract (DeriveTest.e_control "cont")
        trill xs = zip xs (cycle [0, 1])
    equal (run 1 "tr 1 1") ([trill [0, 1, 2]], [])
    -- Defaults to RealTime, but stretches with ScoreTime if asked.
    equal (run 0.5 "tr 1 1") ([trill [0, 1, 2, 3, 4, 5]], [])
    equal (run 0.5 "tr 1 1s") ([trill [0, 1, 2, 3, 4, 5]], [])
    equal (run 0.5 "tr 1 1t") ([trill [0, 2, 4]], [])
    strings_like (snd (run 1 "tr 1 1d"))
        ["expected time type for 1d but got Diatonic"]

test_xcut_control = do
    let f hold val1 val2 = Signal.to_pairs
            . Trill.xcut_control hold (Signal.from_pairs val1)
                (Signal.from_pairs val2)
    let sig1 = [(0, 0.25), (1, 0.5), (2, 0.75), (3, 1)]
        sig2 = [(0, 0)]
    equal (f False sig1 sig2 [1, 2]) [(1, 0.5), (2, 0.75), (2, 0)]
    -- You get the rest of the signal after the last transition.
    equal (f False sig2 sig1 [1, 2]) [(1, 0), (2, 0), (2, 0.75), (3, 1)]
    -- Hold a changing signal.
    equal (f True sig1 sig2 [0, 2]) [(0, 0.25), (2, 0.25), (2, 0)]

    equal (f False [(0, 0)] [(0, 1)] [0, 1, 2, 3, 4])
        [ (0, 0), (1, 0), (1, 1), (2, 1)
        , (2, 0), (3, 0), (3, 1), (4, 1), (4, 0)
        ]

test_saw = do
    let run = CallTest.run_control
    equal (run [(0, "saw .5"), (4, "--|")]) [(0, 1), (2, 0), (2, 1), (4, 0)]
    equal (run [(0, "saw .25 .5 .75"), (4, "--|")]) [(0, 0.5), (4, 0.75)]

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = DeriveTest.derive_tracks "import europe"
