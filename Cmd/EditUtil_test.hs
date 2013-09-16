-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.EditUtil_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.EditUtil as EditUtil
import qualified Derive.Scale.Wayang as Wayang
import qualified Perform.Pitch as Pitch


test_modify_text_key = do
    let f c = EditUtil.modify_text_key [] (EditUtil.Key c)
    equal (f 'c' "a") (Just "ac")
    equal (f ' ' "a") (Just "a ")
    equal (f ' ' "") (Just "")

test_modify_text_note = do
    let f n = EditUtil.modify_text_note (Pitch.Note n)
    equal (f "abc" "") (Just "(abc)")
    equal (f "abc" "a") (Just "a (abc)")

test_backspace = do
    let f = EditUtil.backspace
    equal (f "") Nothing
    equal (f "a") (Just "")

test_input_to_note = do
    let run cmd = CmdTest.extract id $ CmdTest.run_sel 1 note_tracks cmd
        note_tracks = [(">i", [(0, 1, "")]), ("*", [(0, 0, "4d")])]

    -- Fake up a Performance that overrides the default *twelve.
    let set_env = CmdTest.set_scale UiTest.default_block_id
            UiTest.default_block_id (UiTest.mk_tid 2) Wayang.scale_id
    let f = EditUtil.input_to_note
    let input = Pitch.Input Pitch.PianoKbd Pitch.middle_c 0
    equal (run (f input))
        (Right (Just (Pitch.Note "4c"), []))
    equal (run (set_env >> f input))
        (Right (Just (Pitch.Note "4i"), []))
