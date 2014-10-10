-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Post.Move_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Note as Note
import qualified Derive.DeriveTest as DeriveTest


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
