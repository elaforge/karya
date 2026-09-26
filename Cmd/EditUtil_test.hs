-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.EditUtil_test where
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.EditUtil as EditUtil
import qualified Derive.Scale.Wayang as Wayang
import qualified Perform.Pitch as Pitch
import qualified Ui.UiTest as UiTest

import           Util.Test


test_modify_text_key :: Test
test_modify_text_key = do
    let f c = EditUtil.modifyTextKey [] (EditUtil.Key c)
    equal (f 'c' "a") (Just "ac")
    equal (f ' ' "a") (Just "a ")
    equal (f ' ' "") (Just "")

test_backspace :: Test
test_backspace = do
    let f = EditUtil.backspace
    equal (f "") Nothing
    equal (f "a") Nothing
    equal (f "ab") (Just "a")

test_input_to_note :: Test
test_input_to_note = do
    let run cmd = CmdTest.extract id $ CmdTest.runSel 1 noteTracks cmd
        noteTracks = [(">i", [(0, 1, "")]), ("*", [(0, 0, "4d")])]

    -- Fake up a Performance that overrides the default *twelve.
    let setEnv = CmdTest.setScale UiTest.defaultBlockId
            UiTest.defaultBlockId (UiTest.mkTid 2) Wayang.scaleId
    let f = EditUtil.inputToNote
    let input = Pitch.Input Pitch.PianoKbd Pitch.middleC 0
    equal (run (f input)) (Right (Just "4c", []))
    equal (run (setEnv >> f input)) (Right (Just "4i", []))
