module Cmd.Responder_test where
import Util.Test
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.Meter as Meter
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.RulerUtil as RulerUtil

import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score


-- TODO Do some full-cycle tests.

test_modify_tempo = do
    let ustate = UiTest.exec State.empty $ do
            UiTest.mkblock_view (UiTest.default_block_name,
                [ ("tempo", [(0, 0, "1")])
                , (">i", [(0, 1, ""), (1, 1, "")])
                ])
            CmdTest.set_point_sel 1 0
            rid <- Create.ruler "meter44" $
                RulerUtil.meter_ruler 16 (replicate 4 Meter.m44)
            ruler <- State.get_ruler rid
            State.modify_ruler UiTest.default_ruler_id (const ruler)
    let cstate = ResponderTest.mk_cmd_state ustate (UiTest.default_view_id)
    results <- ResponderTest.thread (ustate, cstate)
        (CmdTest.keypresses [Key.Escape, Key.Backspace, Key.Char '2'])
    -- Icky.  If I don't pick exactly the right result it will hang fover
    -- because it's waiting on the loopback to emit a DeriveStatus.  I could
    -- fix it by simulating loopback more accurately.
    let result = results !! 4
    (_, perf) <- ResponderTest.result_perf result
    equal (map Score.event_start $ LEvent.events_of $ Cmd.perf_events perf)
        [0, 0.5]

test_modify_middle_tempo = do
    let states = ResponderTest.mkstates
            [("tempo", [(0, 0, "1")]), (">i", [(0, 1, ""), (1, 1, "")])]
    res <- ResponderTest.respond_cmd states $ UiTest.insert_event 1 (1, 0, "2")
    (_, perf) <- ResponderTest.result_perf res
    equal (map Score.event_duration $ LEvent.events_of $ Cmd.perf_events perf)
        [1, 0.5]
