-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Note_test where
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Note as Note
import qualified Derive.DeriveTest as DeriveTest


test_start_controls = do
    let run = DeriveTest.extract DeriveTest.e_start_dur
            . DeriveTest.derive_tracks
    let min_dur = Note.min_duration
    let start_s sig (s, d) = [("> | %start-s = " ++ sig, [(s, d, "")])]
    equal (run (start_s "0" (0, 1))) ([(0, 1)], [])
    equal (run (start_s "1" (0, 0))) ([(1, 0)], [])
    equal (run (start_s "1" (1, -1))) ([(2, -2)], [])
    equal (run (start_s "-.5" (1, -1))) ([(0.5, -0.5)], [])
    equal (run (start_s "-1" (1, -1))) ([(min_dur, -min_dur)], [])
    equal (run (start_s "-1" (0, 1))) ([(-1, 2)], [])
    equal (run (start_s "1" (0, 1))) ([(1 - min_dur, min_dur)], [])

    let event title = [(title, [(0, 1, "")])]
    equal (run $ ("tempo", [(0, 0, "2")]) : event "> | start-t = 1")
        ([(0, 0.5)], [])

    -- Randomization.
    let events title n = [(title, [(t, 1, "") | t <- Seq.range' 0 n 1])]
    equal (run $ events ">" 2) ([(0, 1), (1, 1)], [])
    check $ (run $ events "> | %start-s = (cf-rnd -1 1)" 2)
        /= ([(0, 1), (1, 1)], [])

    -- The pitch moves with the note.
    let runp = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_tracks
    let tracks n =
            [ ("> | %start-s = -1", [(n, 1, "")])
            , ("*", [(n, 0, "4c")])
            ]
    equal (runp (tracks 1)) ([(0, 2, "4c")], [])
    equal (runp (tracks 2)) ([(1, 2, "4c")], [])

test_orphan_notes = do
    -- Slice out orphans that aren't covered by a parent event.
    -- Also tested in 'Derive.Slice_test.test_slice_notes_orphans'.
    -- This is analogous to track level orphans, which are tested in
    -- "Ui.Call.BlockUtil_test".
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    equal (run
        [ (">i", [(0, 2, "a = b")])
        , (">", [])
        , (">", [(0, 1, ""), (1, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d")])
        ])
        ([((0, 1, "4c"), "+"), ((1, 1, "4d"), "+")], [])
    equal (run
        [ (">i", [(0, 2, "a = b")])
        , (">", [(0, 1, "+a")])
        , (">", [(0, 1, ""), (1, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d")])
        ])
        ([((0, 1, "4c"), "+a"), ((1, 1, "4d"), "+")], [])

test_arrival_notes = do
    let run notes = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ (">", notes)
            , ("*", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e"), (3, 0, "4f")])
            ]
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    equal (run [(1, -1, ""), (2, -1, "")])
        ([((1, 1, "4d"), "+"), ((2, -1, "4e"), "+arrival-note")], [])

test_arrival_notes_postproc = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_blocks_with_ui id
                (DeriveTest.with_transform "arrival-note 2")

    -- Arrival notes carry across block boundaries.
    equal (run
            [ ("b1", [(">", [(1, -1, "b2"), (2, -1, "b2")])])
            , ("b2=ruler", UiTest.note_track [(1, -1, "4c"), (2, -1, "4d")])
            ])
        ([(0.5, 0.5, "4c"), (1, 0.5, "4d")
        , (1.5, 0.5, "4c"), (2, 2, "4d")
        ], [])

    equal (run
            [ ("b1",
                [ (">", [(1, -1, "b2"), (2, -1, "b2")])
                , ("*", [(1, 0, "4c"), (2, 0, "5c")])
                ])
            , ("b2=ruler",
                [ (">", [(1, -1, ""), (2, -1, "")])
                , ("t-chromatic", [(1, 0, "1"), (2, 0, "0")])
                ])
            ])
        ([(0.5, 0.5, "4c#"), (1, 0.5, "4c")
        , (1.5, 0.5, "5c#"), (2, 2, "5c")
        ], [])
