-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.NoteTransformer_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackWarp as TrackWarp

import Types


test_sequence = do
    let run start dur call = run_blocks $ make_subs start dur call
    equal (run 0 2 "sequence sub-cd") ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (run 0 1 "sequence sub-cd") ([(0, 0.5, "4c"), (0.5, 0.5, "4d")], [])
    equal (run 0 3 "sequence sub-cd sub-e")
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")], [])
    equal (run 0 3 "sequence \"(%t-dia=1 | sub-e) sub-cd")
        ([(0, 1, "4f"), (1, 1, "4c"), (2, 1, "4d")], [])
    -- Duration is the sum of callees, so I can sequence sequences.
    equal (run 0 4 "sequence \"(sequence sub-e sub-e) sub-cd")
        ([(0, 1, "4e"), (1, 1, "4e"), (2, 1, "4c"), (3, 1, "4d")], [])

    -- Notes stretch with event duration.
    equal (run 2 1 "sequence sub-cd") ([(2, 0.5, "4c"), (2.5, 0.5, "4d")], [])
    equal (run 2 6 "sequence sub-cd sub-e")
        ([(2, 2, "4c"), (4, 2, "4d"), (6, 2, "4e")], [])

test_sequence_realtime = do
    let run = run_blocks
    equal (run $ make_subs 0 2 "sequence-rt sub-cd")
        ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (run $ make_subs 0 3 "sequence-rt sub-cd sub-e")
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")], [])
    let tempo_subs start dur call =
            [ ("top", [(">", [(start, dur, call)])])
            , ("sub-cd=ruler", ("tempo", [(0, 0, ".5")])
                : UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
            , ("sub-e=ruler", UiTest.note_track [(0, 1, "4e")])
            ]
    equal (run $ tempo_subs 0 5 "sequence-rt sub-cd sub-e")
        ([(0, 2, "4c"), (2, 2, "4d"), (4, 1, "4e")], [])
    -- Translate and stretch.
    equal (run $ tempo_subs 2 5 "sequence-rt sub-cd sub-e")
        ([(2, 2, "4c"), (4, 2, "4d"), (6, 1, "4e")], [])
    equal (run $ tempo_subs 2 10 "sequence-rt sub-cd sub-e")
        ([(2, 4, "4c"), (6, 4, "4d"), (10, 2, "4e")], [])
    equal (run $ tempo_subs 2 2.5 "sequence-rt sub-cd sub-e")
        ([(2, 1, "4c"), (3, 1, "4d"), (4, 0.5, "4e")], [])

    equal (run $ tempo_subs 2 6 "sequence \"(sequence-rt sub-cd sub-e) sub-e")
        ([(2, 2, "4c"), (4, 2, "4d"), (6, 1, "4e"), (7, 1, "4e")], [])

test_parallel = do
    let run start dur call = run_blocks $ make_subs start dur call
    equal (run 0 2 "parallel sub-cd") ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (run 0 2 "parallel sub-cd sub-e")
        ([(0, 1, "4c"), (0, 1, "4e"), (1, 1, "4d")], [])
    equal (run 0 4 "parallel sub-cd sub-e")
        ([(0, 2, "4c"), (0, 2, "4e"), (2, 2, "4d")], [])

run_blocks :: [UiTest.BlockSpec] -> ([(RealTime, RealTime, String)], [String])
run_blocks = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_blocks

make_subs :: ScoreTime -> ScoreTime -> String -> [UiTest.BlockSpec]
make_subs start dur call =
    [ ("top", [(">", [(start, dur, call)])])
    , ("sub-cd=ruler", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
    , ("sub-e=ruler", UiTest.note_track [(0, 1, "4e")])
    ]

test_multiple = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks ""
        extract e = (Score.event_start e, DeriveTest.e_instrument e)
    equal (run [("> | multiple \"(inst=i1) \"(inst=i2)", [(0, 1, "")])])
        ([(0, "i1"), (0, "i2")], [])

test_track_warps = do
    let run notes = extract $ DeriveTest.derive_blocks
            [ ("top", [(">", notes)])
            , ("sub=ruler", UiTest.regular_notes 4)
            ]
        extract = map e_track_warp . Derive.r_track_warps
        e_track_warp tw =
            (TrackWarp.tw_block tw, TrackWarp.tw_start tw, TrackWarp.tw_end tw)
        top = UiTest.bid "top"
        sub = UiTest.bid "sub"

    equal (run [(0, 4, "sub")]) [(sub, 0, 4), (top, 0, 4)]
    equal (run [(0, 2, "clip | sub")]) [(sub, 0, 2), (top, 0, 2)]
    equal (run [(1, 2, "clip | sub")]) [(sub, 1, 3), (top, 0, 3)]
    equal (run [(0, 2, "clip | sub"), (4, 2, "clip | sub")])
        [(sub, 0, 2), (sub, 4, 6), (top, 0, 6)]

    equal (run [(0, 2, "Clip | sub")]) [(sub, 0, 2), (top, 0, 2)]
    equal (run [(1, 2, "Clip | sub")]) [(sub, 1, 3), (top, 0, 3)]

    equal (run [(0, 7, "loop | sub")]) [(sub, 0, 4), (sub, 4, 7), (top, 0, 7)]
    equal (run [(2, 4, "tile | sub")]) [(sub, 2, 4), (sub, 4, 6), (top, 0, 6)]
    equal (run [(2, 4, "repeat 2 | sub")])
        [(sub, 2, 4), (sub, 4, 6), (top, 0, 6)]

test_clip = do
    let run top = run_sub DeriveTest.e_start_dur [(">", top)]
            [(">", [(0, 1, ""), (1, 1, "")])]
    -- make sure out of range notes are clipped
    equal (run [(0, 1, "clip | sub")]) ([(0, 1)], [])

    -- sub goes *4/2 + 1 ==> [(1, 2), (3, 2)]
    -- I want            ==> [(1, 1), (2, 1)]
    -- so (-1) (*0.5)
    equal (run [(1, 4, "clip | sub")]) ([(1, 1), (2, 1)], [])

    -- sub goes *1.5/2 + 1 ==> [(1, 0.75), (1.75, 0.75)]
    -- I want              ==> [(1, 1), (2, 1)]
    -- so (-1) (* 1/.75)
    -- notes that overlap the end are shortened
    equal (run [(1, 1.5, "clip | sub")]) ([(1, 1), (2, 0.5)], [])

    -- A note at the end is clipped.
    equal (run_sub DeriveTest.e_start_dur
            [(">", [(0, 1, "clip | sub")])] [(">", [(0, 0, ""), (1, 0, "")])])
        ([(0, 0)], [])

    -- clip works even when it's not directly a block call.
    equal (run [(1, 1.5, "^b=sub | clip | b")]) ([(1, 1), (2, 0.5)], [])

test_clip_start = do
    let run = run_sub DeriveTest.e_note
    -- Aligned to the end.
    equal (run [(">", [(0, 2, "Clip | sub")])] (UiTest.regular_notes 1))
        ([(1, 1, "3c")], [])
    -- Get the last two notes.
    equal (run [(">", [(0, 2, "Clip | sub")])] (UiTest.regular_notes 3))
        ([(0, 1, "3d"), (1, 1, "3e")], [])

test_loop = do
    let run = run_sub DeriveTest.e_start_dur
    equal (run [(">", [(0, 4, "loop | sub")])] [(">", [(0, 1, "")])])
        ([(0, 1), (1, 1), (2, 1), (3, 1)], [])
    -- Cuts off the last event.
    let sub = [(">", [(0, 1, ""), (1, 3, "")])]
    equal (run [(">", [(0, 5, "loop | sub")])] sub)
        ([(0, 1), (1, 3), (4, 1)], [])

test_tile = do
    let run top = run_sub Score.event_start [(">", top)]
            [(">", [(0, 1, ""), (1, 3, "")])]
    -- If it starts at 0, it's just like 'loop'.
    equal (run [(0, 5, "tile | sub")]) ([0, 1, 4], [])
    equal (run [(1, 5, "tile | sub")]) ([1, 4, 5], [])
    equal (run [(9, 5, "tile | sub")]) ([9, 12, 13], [])

test_repeat = do
    let run top = run_sub DeriveTest.e_note [(">", top)]
            (UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
    equal (run [(0, 4, "repeat 2 | sub")])
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c"), (3, 1, "4d")], [])
    equal (run [(0, 6, "repeat 3 | sub")])
        ([ (0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c"), (3, 1, "4d")
         , (4, 1, "4c"), (5, 1, "4d")
         ], [])
    equal (run [(2, 4, "repeat 2 | sub")])
        ([(2, 1, "4c"), (3, 1, "4d"), (4, 1, "4c"), (5, 1, "4d")], [])

run_sub :: (Score.Event -> a) -> [UiTest.TrackSpec] -> [UiTest.TrackSpec]
    -> ([a], [String])
run_sub extract top sub = DeriveTest.extract extract $ DeriveTest.derive_blocks
    [("top", top), ("sub=ruler", sub)]
