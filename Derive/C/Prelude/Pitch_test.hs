-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Pitch_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Global
import Types


test_set_and_val_to_pitch = do
    -- This also tests Derive.Deriver.Lib.val_to_pitch, but they should have
    -- the same behaviour.
    let run modify notes pitches = DeriveTest.extract extract $
            DeriveTest.derive_tracks ""
                [ (">", [(s, d, "") | (s, d) <- notes])
                , ("*", [(s, 0, modify p) | (s, p) <- pitches])
                ]
        extract = DeriveTest.e_nns_literal
    forM_ [id, \p -> "set (" <> p <> ")"] $ \modify -> do
        equal (run modify [(0, 1), (1, 1)] [(0, "4c"), (1, "4d")])
            ([[(0, NN.c4), (1, NN.c4)], [(1, NN.d4)]], [])
        -- A set emits an explicit discontinuity.
        equal (run modify [(0, 2)] [(0, "4c"), (1, "4d")])
            ([[(0, NN.c4), (1, NN.c4), (1, NN.d4)]], [])

test_set = do
    equal (run [(0, "set (4c)")]) [(0, NN.c4)]
    equal (run [(0, "4c"), (2, "set (4c) | i (4d)")])
        [(0, NN.c4), (2, NN.d4), (2, NN.c4)]

test_multiply = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks ""
            . UiTest.note_track
        extract e = (DeriveTest.e_nns_rounded e, DeriveTest.e_pitch e)
    equal (run [(1, 1, "4c")]) ([([(1, NN.c4)], "4c")], [])
    equal (run [(1, 1, "* (4c) 2")]) ([([(1, NN.c5)], "4c")], [])
    equal (run [(1, 1, "* (4c) P5")]) ([([(1, NN.g4 + 0.02)], "4c")], [])

test_interpolated_transpose = do
    -- An even interpolation on an un-equal tempered scale should remain even
    -- after transposition.
    let scale = DeriveTest.mkscale "test" [("A", 1), ("B", 3), ("C", 4)]
    let run title = extract $ DeriveTest.derive_tracks_setup
            (DeriveTest.with_scale scale) ""
            [ (title, [(0, 5, "")])
            , ("*test", [(0, 0, "A"), (4, 0, "i (B)")])
            ]
        extract = head . DeriveTest.extract_events DeriveTest.e_nns_literal
    equal (run ">") [(0, 1), (4, 3)]
    equal (run "> | %t-chrom = 1") [(0, 3), (4, 4)]

test_transpose_out_of_range = do
    equal (run_with_title ">" "twelve" [(0, "4c")])
        ([(0, 60)], [])
    equal (run_with_title "> | %t-chrom = 10" "twelve" [(0, "4c")])
        ([(0, 70)], [])
    equal (run_with_title "> | %t-chrom = -10" "twelve" [(0, "4c")])
        ([(0, 50)], [])
    let (sig, errs) = run_with_title "> | %t-chrom = 200" "twelve" [(0, "4c")]
    equal sig []
    strings_like errs ["260nn is out of range"]
    where
    run_with_title inst_title pitch_title pitches =
        head $ DeriveTest.extract_events DeriveTest.e_nns_errors $
            DeriveTest.derive_tracks ""
                [ (inst_title, [(0, 5, "")])
                , ("*" <> pitch_title, [(x, 0, n) | (x, n) <- pitches])
                ]

test_neighbor = do
    equal (CallTest.run_pitch "" [(0, "n (4c) 1 2")]) [(0, 61), (2, 60)]
    -- Both chromatic and diatonic literals.
    equal (CallTest.run_pitch "" [(0, "n (4c) 1c 1")]) [(0, 61), (1, 60)]
    equal (CallTest.run_pitch "" [(0, "n (4c) 1d 1")]) [(0, 62), (1, 60)]

    -- It defaults to RealTime.
    equal (run_tempo 2 [(0, "n (4c) 1d 1")]) [(0, 62), (1, 60)]
    -- Except when explicitly set to ScoreTime.
    equal (run_tempo 2 [(0, "n (4c) 1d 1t")]) [(0, 62), (0.5, 60)]

test_approach = do
    equal (run [(0, "4c"), (10, "a 2s"), (20, "4d")])
        [(0, NN.c4), (10, NN.c4), (12, NN.d4), (20, NN.d4)]

    let run = DeriveTest.extract_events DeriveTest.e_nns_literal
            . DeriveTest.derive_tracks "" . UiTest.note_spec
    equal (run ("", [(0, 10, "4c"), (10, 10, "a 2s"), (20, 10, "4d")], []))
        [ [(0, NN.c4), (10, NN.c4)]
        , [(10, NN.c4), (12, NN.d4), (20, NN.d4)]
        , [(20, NN.d4)]
        ]

test_linear = do
    equal (run [(0, "4c"), (2, "i (4d)")]) [(0, NN.c4), (2, NN.d4)]

test_porta = do
    equal (run [(0, "4c"), (2, "porta-place=1 | p (4d) 2s")])
        [(0, NN.c4), (2, NN.c4), (4, NN.d4)]
    equal (run [(0, "4c"), (2, "porta-place=0 | p (4d) 1s")])
        [(0, NN.c4), (1, NN.c4), (2, NN.d4)]
    equal (run [(0, "4c"),
            (2, "porta-place=1 | curve=(cf-expon 2) | p (4d) 2s")])
        [(0, NN.c4), (2, NN.c4), (3, 60.5), (4, 62)]

test_linear_next = do
    equal (run [(0, "4c"), (4, "i> (4d)"), (6, "4c")])
        [(0, NN.c4), (4, NN.c4), (6, NN.d4), (6, NN.c4)]
    -- A number is interpreted as a transposition of the previous pitch.
    equal (run [(0, "4c"), (4, "i> 4"), (6, "4c")])
        [(0, NN.c4), (4, NN.c4), (6, NN.c4 + 4), (6, NN.c4)]
    equal (run [(0, "4c"), (2, "i> (>)"), (4, "4d")])
        [(0, NN.c4), (2, NN.c4), (4, NN.d4)]
    -- Test with slicing.
    let run3 = DeriveTest.extract_events DeriveTest.e_nns_literal
            . DeriveTest.derive_tracks "" . UiTest.note_track
    equal (run3 [(0, 1, "4c"), (1, 1, "i> (4d)"), (3, 1, "4e")])
        [[(0, NN.c4), (1, NN.c4)], [(1, NN.c4), (3, NN.d4)], [(3, NN.e4)]]

run :: [(ScoreTime, Text)] -> [(RealTime, Pitch.NoteNumber)]
run = CallTest.run_pitch2 ""

run_tempo :: Int -> [(ScoreTime, Text)] -> [(RealTime, Pitch.NoteNumber)]
run_tempo tempo pitches = extract $ run_ tempo pitches []
    where extract = head . DeriveTest.extract_events DeriveTest.e_nns_literal

run_ :: Int -> [(ScoreTime, Text)] -> [UiTest.TrackSpec] -> Derive.Result
run_ tempo pitches tracks = DeriveTest.derive_tracks "" $
    [ ("tempo", [(0, 0, showt tempo)])
    , (">", [(0, 10, "")])
    , ("*", [(start, 0, text) | (start, text) <- pitches])
    ] ++ tracks
