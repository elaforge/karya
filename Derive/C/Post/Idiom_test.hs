-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Idiom_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_pizz_arp = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks "pizz-arp .5"
            . concatMap UiTest.note_track

    equal (run [[(0, 1, "+pizz -- 4c")], [(0, 1, "+pizz -- 4d")]])
        ([(0, 1, "4c"), (0.5, 1, "4d")], [])
    equal (run [[(0, 1, "4c")], [(0, 1, "+pizz -- 4d"), (1, 1, "+pizz -- 4e")]])
        ([(0, 1, "4c"), (0, 1, "4d"), (1, 1, "4e")], [])

test_avoid_overlap = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks "avoid-overlap .5"
            . UiTest.note_track
    equal (run [(0, 1, "4c"), (1, 1, "4d")])
        ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (run [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4d"), (3, 1, "4c")])
        ([(0, 1, "4c"), (1, 0.5, "4d"), (2, 1, "4d"), (3, 1, "4c")], [])
    equal (run [(0, 0.25, "4c"), (0.25, 0.25, "4c")])
        ([(0, Note.min_duration, "4c"), (0.25, 0.25, "4c")], [])

test_extend_duration = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks "extend-duration (list +a +b) 2"
            . UiTest.note_track
    equal (run [(0, 1, "+a -- 4c")]) ([(0, 2, "4c")], [])
    equal (run [(0, 1, "+b+z -- 4c")]) ([(0, 2, "4c")], [])
    equal (run [(0, 1, "+z -- 4c")]) ([(0, 1, "4c")], [])
    -- Won't shorten a duration.
    equal (run [(0, 4, "+a -- 4c")]) ([(0, 4, "4c")], [])
    -- Overlap is ok.
    equal (run [(0, 1, "+a -- 4c"), (1, 1, "+a -- 4d")])
        ([(0, 2, "4c"), (1, 2, "4d")], [])
    -- Unless it has the same pitch.
    equal (run [(0, 1, "+a -- 4c"), (1.5, 1, "+a -- 4c")])
        ([(0, 1.45, "4c"), (1.5, 2, "4c")], [])

test_apply_attributes = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks "apply-attributes"
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run
            [ ("attr-a-b", [(0, 0, "0"), (1, 0, "1"), (2, 0, "0")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            ])
        ([(0, "+"), (1, "+a+b"), (2, "+")], [])
