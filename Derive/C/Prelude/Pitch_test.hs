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


test_set_and_val_to_pitch :: Test
test_set_and_val_to_pitch = do
    -- This also tests Derive.Deriver.Lib.val_to_pitch, but they should have
    -- the same behaviour.
    let run modify notes pitches = DeriveTest.extract extract $
            DeriveTest.deriveTracks ""
                [ (">", [(s, d, "") | (s, d) <- notes])
                , ("*", [(s, 0, modify p) | (s, p) <- pitches])
                ]
        extract = DeriveTest.eNns
    forM_ [id, \p -> "set (" <> p <> ")"] $ \modify -> do
        equal (run modify [(0, 1), (1, 1)] [(0, "4c"), (1, "4d")])
            ([[(0, NN.c4), (1, NN.c4)], [(1, NN.d4)]], [])
        -- A set emits an explicit discontinuity.
        equal (run modify [(0, 2)] [(0, "4c"), (1, "4d")])
            ([[(0, NN.c4), (1, NN.c4), (1, NN.d4)]], [])

test_set :: Test
test_set = do
    equal (run [(0, "set (4c)")]) [(0, NN.c4)]
    equal (run [(0, "4c"), (2, "set (4c) | i (4d)")])
        [(0, NN.c4), (2, NN.d4), (2, NN.c4)]

test_set_or_move :: Test
test_set_or_move = do
    equal (run [(0, "set-or-move (4c) 2"), (2, "set-or-move (4d) 2")])
        [(0, NN.c4), (2, NN.c4), (4, NN.d4)]

test_multiply :: Test
test_multiply = do
    let run = DeriveTest.extract extract . DeriveTest.deriveTracks ""
            . UiTest.noteTrack
        extract e = (DeriveTest.eNnsRounded e, DeriveTest.ePitch e)
    equal (run [(1, 1, "4c")]) ([([(1, NN.c4)], "4c")], [])
    equal (run [(1, 1, "* (4c) 2")]) ([([(1, NN.c5)], "4c")], [])
    equal (run [(1, 1, "* (4c) P5")]) ([([(1, NN.g4 + 0.02)], "4c")], [])

test_interpolated_transpose :: Test
test_interpolated_transpose = do
    -- An even interpolation on an un-equal tempered scale should remain even
    -- after transposition.
    let scale = DeriveTest.mkscale "test" [("A", 1), ("B", 3), ("C", 4)]
    let run title = extract $ DeriveTest.deriveTracksSetup
            (DeriveTest.withScale scale) ""
            [ (title, [(0, 5, "")])
            , ("*test", [(0, 0, "A"), (4, 0, "i (B)")])
            ]
        extract = head . DeriveTest.extractEvents DeriveTest.eNns
    equal (run ">") [(0, 1), (4, 3)]
    equal (run "> | t-chrom = 1") [(0, 3), (4, 4)]

test_transpose_out_of_range :: Test
test_transpose_out_of_range = do
    equal (runWithTitle ">" "twelve" [(0, "4c")])
        ([(0, 60)], [])
    equal (runWithTitle "> | t-chrom = 10" "twelve" [(0, "4c")])
        ([(0, 70)], [])
    equal (runWithTitle "> | t-chrom = -10" "twelve" [(0, "4c")])
        ([(0, 50)], [])
    let (sig, errs) = runWithTitle "> | t-chrom = 200" "twelve" [(0, "4c")]
    equal sig []
    stringsLike errs ["260nn out of range"]
    where
    runWithTitle instTitle pitchTitle pitches =
        head $ DeriveTest.extractEvents DeriveTest.eNnsErrors $
            DeriveTest.deriveTracks ""
                [ (instTitle, [(0, 5, "")])
                , ("*" <> pitchTitle, [(x, 0, n) | (x, n) <- pitches])
                ]

test_neighbor :: Test
test_neighbor = do
    equal (CallTest.runPitch "" [(0, "n (4c) 1 2")]) [(0, 61), (2, 60)]
    -- Both chromatic and diatonic literals.
    equal (CallTest.runPitch "" [(0, "n (4c) 1c 1")]) [(0, 61), (1, 60)]
    equal (CallTest.runPitch "" [(0, "n (4c) 1d 1")]) [(0, 62), (1, 60)]

    -- It defaults to RealTime.
    equal (runTempo 2 [(0, "n (4c) 1d 1")]) [(0, 62), (1, 60)]
    -- Except when explicitly set to ScoreTime.
    equal (runTempo 2 [(0, "n (4c) 1d 1t")]) [(0, 62), (0.5, 60)]

test_approach :: Test
test_approach = do
    equal (run [(0, "4c"), (10, "a 2s"), (20, "4d")])
        [(0, NN.c4), (10, NN.c4), (12, NN.d4), (20, NN.d4)]

    let run = DeriveTest.extractEvents DeriveTest.eNns
            . DeriveTest.deriveTracks "" . UiTest.noteSpec
    equal (run ("", [(0, 10, "4c"), (10, 10, "a 2s"), (20, 10, "4d")], []))
        [ [(0, NN.c4), (10, NN.c4)]
        , [(10, NN.c4), (12, NN.d4), (20, NN.d4)]
        , [(20, NN.d4)]
        ]

test_linear :: Test
test_linear = do
    equal (run [(0, "4c"), (2, "i (4d)")]) [(0, NN.c4), (2, NN.d4)]

test_porta :: Test
test_porta = do
    equal (run [(0, "4c"), (2, "porta-place=1 | p (4d) 2s")])
        [(0, NN.c4), (2, NN.c4), (4, NN.d4)]
    equal (run [(0, "4c"), (2, "porta-place=0 | p (4d) 1s")])
        [(0, NN.c4), (1, NN.c4), (2, NN.d4)]
    equal (run [(0, "4c"),
            (2, "porta-place=1 | curve=(curve-expon 2) | p (4d) 2s")])
        [(0, NN.c4), (2, NN.c4), (3, 60.5), (4, 62)]

test_linear_next :: Test
test_linear_next = do
    equal (run [(0, "4c"), (4, "i> (4d)"), (6, "4c")])
        [(0, NN.c4), (4, NN.c4), (6, NN.d4), (6, NN.c4)]
    -- A number is interpreted as a transposition of the previous pitch.
    equal (run [(0, "4c"), (4, "i> 4"), (6, "4c")])
        [(0, NN.c4), (4, NN.c4), (6, NN.c4 + 4), (6, NN.c4)]
    equal (run [(0, "4c"), (2, "i> (>)"), (4, "4d")])
        [(0, NN.c4), (2, NN.c4), (4, NN.d4)]
    -- Test with slicing.
    let run3 = DeriveTest.extractEvents DeriveTest.eNns
            . DeriveTest.deriveTracks "" . UiTest.noteTrack
    equal (run3 [(0, 1, "4c"), (1, 1, "i> (4d)"), (3, 1, "4e")])
        [[(0, NN.c4), (1, NN.c4)], [(1, NN.c4), (3, NN.d4)], [(3, NN.e4)]]

run :: [(ScoreTime, Text)] -> [(RealTime, Pitch.NoteNumber)]
run = CallTest.runPitch ""

runTempo :: Int -> [(ScoreTime, Text)] -> [(RealTime, Pitch.NoteNumber)]
runTempo tempo pitches = extract $ run_ tempo pitches []
    where extract = head . DeriveTest.extractEvents DeriveTest.eNns

run_ :: Int -> [(ScoreTime, Text)] -> [UiTest.TrackSpec] -> Derive.Result
run_ tempo pitches tracks = DeriveTest.deriveTracks "" $
    [ ("tempo", [(0, 0, showt tempo)])
    , (">", [(0, 10, "")])
    , ("*", [(start, 0, text) | (start, text) <- pitches])
    ] ++ tracks
