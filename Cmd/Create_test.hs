module Cmd.Create_test where
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create


test_track_ruler = do
    let f tracknum = Create.track_ruler UiTest.default_block_id
            State.no_ruler tracknum 20
        run track_specs cmd = CmdTest.trace_logs $ CmdTest.e_tracks $
            CmdTest.run_tracks track_specs cmd
    equal (run [] (f 0)) $ Right [("", [])]
    equal (run [] (f 10)) $ Right [("", [])]
    equal (run [("1", [])] (f 10)) $ Right [("1", []), ("", [])]
    equal (run [("1", [])] (f 0)) $ Right [("", []), ("1", [])]

-- test_track = do
    -- TODO actually test the ruler stuff too
