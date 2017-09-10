-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Responder_test where
import qualified Data.Vector as Vector

import Util.Test
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters

import qualified Derive.Score as Score


-- TODO Do some full-cycle tests.

test_modify_tempo = do
    let ustate = UiTest.exec Ui.empty $ do
            UiTest.mkblock_view (UiTest.default_block_name,
                [ ("tempo", [(0, 0, "1")])
                , (">i1", [(0, 1, ""), (1, 1, "")])
                ])
            CmdTest.set_point_sel 1 0
            rid <- Create.ruler "meter44" $
                Meter.fit_ruler Meter.default_config 16
                    (replicate 4 Meters.m44_4)
            ruler <- Ui.get_ruler rid
            Ui.modify_ruler UiTest.default_ruler_id (const (Right ruler))
    let cstate = ResponderTest.mk_cmd_state ustate (UiTest.default_view_id)
    results <- ResponderTest.thread False (ustate, cstate)
        -- Delete the tempo and set it to 2.
        (CmdTest.keypresses [Key.Escape, Key.Backspace, Key.Char '2'])
    -- Icky.  If I don't pick exactly the right result it will hang fover
    -- because it's waiting on the loopback to emit a DeriveStatus.  I could
    -- fix it by simulating loopback more accurately.
    --
    -- 2 msgs for each key, key down and key up.  I want the '2' key down.
    let result = results !! 4
    (_, perf) <- ResponderTest.result_perf result
    equal (map Score.event_start $ Vector.toList $ Cmd.perf_events perf)
        [0, 0.5]

test_modify_middle_tempo = do
    let states = ResponderTest.mkstates
            [("tempo", [(0, 0, "1")]), (">i1", [(0, 1, ""), (1, 1, "")])]
    res <- ResponderTest.respond_cmd states $ UiTest.insert_event 1 (1, 0, "2")
    (_, perf) <- ResponderTest.result_perf res
    equal (map Score.event_duration $ Vector.toList $ Cmd.perf_events perf)
        [1, 0.5]
