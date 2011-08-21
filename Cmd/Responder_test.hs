module Cmd.Responder_test where
import Util.Test
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.ResponderTest as ResponderTest


-- TODO Do some full-cycle tests.

modify_tempo = do
    let ustate = UiTest.exec State.empty $ do
            UiTest.mkstate_view UiTest.default_block_name
                [ ("tempo", [(0, 0, "1")])
                , (">i", [(0, 1, ""), (1, 1, "")])
                ]
            CmdTest.set_point_sel 1 0
            (rid, _) <- Create.ruler "meter_44"
                (MakeRuler.ruler [MakeRuler.meter_ruler (1/16) MakeRuler.m44])
            ruler <- State.get_ruler rid
            State.modify_ruler UiTest.default_ruler_id (const ruler)
    let cstate = ResponderTest.mk_cmd_state (UiTest.default_view_id)
    results <- ResponderTest.thread (ustate, cstate)
        (CmdTest.keypresses [Key.Escape, Key.Backspace, Key.Char '2'])
    let result = results !! 4
    (_, perf) <- ResponderTest.result_perf result
    -- TODO actually test stuff
    pprint (Cmd.perf_events perf)
