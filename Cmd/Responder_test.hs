-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Responder_test where
import qualified Data.Vector as Vector

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ResponderTest as ResponderTest

import qualified Derive.Score as Score
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Util.Test


-- TODO Do some full-cycle tests.

test_modify_tempo :: Test
test_modify_tempo = do
    let ustate = UiTest.exec Ui.empty $ do
            UiTest.mkblockView (UiTest.defaultBlockName,
                [ ("tempo", [(0, 0, "1")])
                , (">i1", [(0, 1, ""), (1, 1, "")])
                ])
            CmdTest.setPointSel 1 0
            Ui.modifyRuler UiTest.defaultRulerId $ const $ Right $
                UiTest.mkruler44 16 1
    let cstate = ResponderTest.mkCmdState ustate (UiTest.defaultViewId)
    results <- ResponderTest.thread False (ustate, cstate)
        -- Delete the tempo and set it to 2.
        (CmdTest.keypresses [Key.Escape, Key.Backspace, Key.Char '2'])
    -- Icky.  If I don't pick exactly the right result it will hang fover
    -- because it's waiting on the loopback to emit a DeriveStatus.  I could
    -- fix it by simulating loopback more accurately.
    --
    -- 2 msgs for each key, key down and key up.  I want the '2' key down.
    let result = results !! 4
    (_, perf) <- ResponderTest.resultPerf result
    equal (map Score.eventStart $ Vector.toList $ Cmd.perfEvents perf)
        [0, 0.5]

test_modify_middle_tempo :: Test
test_modify_middle_tempo = do
    let states = ResponderTest.mkstates
            [("tempo", [(0, 0, "1")]), (">i1", [(0, 1, ""), (1, 1, "")])]
    res <- ResponderTest.respondCmd states $ UiTest.insertEvent 1 (1, 0, "2")
    (_, perf) <- ResponderTest.resultPerf res
    equal (map Score.eventDuration $ Vector.toList $ Cmd.perfEvents perf)
        [1, 0.5]
