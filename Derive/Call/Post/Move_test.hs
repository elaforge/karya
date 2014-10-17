-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Post.Move_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Note as Note
import qualified Derive.DeriveTest as DeriveTest


test_infer_duration = do
    let run = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_blocks
        top = ("top -- infer-duration 2",
            [(">", [(0, 2, "sub")]), (">", [(2, 2, "sub")])])
        sub notes = ("sub=ruler", UiTest.note_track notes)
    equal (run [top, sub [(1, 1, "4c"), (2, 0, "4d")]])
        ([(1, 1, "4c"), (2, 1, "4d"), (3, 1, "4c"), (4, 2, "4d")], [])
    -- First note is cancelled out.
    equal (run [top, sub [(0, 1, "4c"), (1, 1, "4d"), (2, 0, "4e")]])
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (3, 1, "4d"), (4, 2, "4e")],
            [])
    -- The inferred note takes the duration of the replaced one.
    equal (run [top, sub [(0, 1.5, "4c"), (2, 0, "4d")]])
        ([(0, 1.5, "4c"), (2, 1.5, "4d"), (4, 2, "4d")], [])

    -- A zero duration block doesn't have its duration changed, since otherwise
    -- I can't write single note calls for e.g. percussion.
    equal (run [top, sub [(0, 0, "4c")]]) ([(0, 0, "4c"), (2, 0, "4c")], [])

test_infer_duration_controls = do
    -- A zero duration note at the end of a block gets controls from right
    -- before.
    let run extract = DeriveTest.extract extract . DeriveTest.derive_blocks
        top = "top -- infer-duration 1"

    -- sub---| sub---|
    -- 4c 4d 4e 4d 4e
    -- "  "  "  4e 4f
    equal (run DeriveTest.e_pitch
            [ (top,
                [ ("t-dia", [(0, 0, "0"), (2, 0, "1")])
                , (">", [(0, 2, "sub"), (2, 2, "sub")])
                ])
            , ("sub=ruler", UiTest.note_track
                [(0, 1, "4c"), (1, 1, "4d"), (2, 0, "4e")])
            ])
        (["4c", "4d", "4e", "4e", "4f"], [])

    -- The inferred note takes the controls of the replaced note, except the
    -- first sample, which comes from the previous block.
    equal (run (DeriveTest.e_control "c")
            [ (top, [(">", [(0, 2, "sub"), (2, 2, "sub")])])
            , ("sub=ruler",
                [ ("c", [(0, 0, "1"), (1, 0, "2"), (2, 0, "3")])
                , (">", [(0, 2, ""), (2, 0, "")])
                ])
            ])
        ([[(0, 1), (1, 2), (2, 3)], [(2, 3), (3, 2), (4, 3)], [(4, 3)]], [])

test_apply_start_offset = do
    let run = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_blocks
        top = "top -- apply-start-offset"
    equal (run [(top, UiTest.note_track [(1, 1, "%start-s=1 | -- 4c")])])
        ([(2, Note.min_duration, "4c")], [])
    equal (run
            [ (top, UiTest.note_track [(2, 2, "%start-s=-1 | sub -- 4c")])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ])
        ([(1, 3, "4c")], [])
    equal (run [(top, ("tempo", [(0, 0, "2")])
            : UiTest.note_track [(2, 2, "%start-t=1 | -- 4c")])])
        ([(1.5, 0.5, "4c")], [])
