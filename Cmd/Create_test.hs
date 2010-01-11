module Cmd.Create_test where
import Util.Test

import qualified Ui.Types as Types
import qualified Ui.State as State

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Cmd as Cmd

import qualified Cmd.Create as Create



mksel = Types.point_selection
mkkey = CmdTest.make_key True

cstate = Cmd.empty_state

test_track_ruler = do
    let f tracknum = Create.track_ruler CmdTest.default_block_id State.no_ruler
            tracknum 20
        run track_specs cmd = CmdTest.e_tracks $
            CmdTest.run_tracks track_specs cmd

    equal (run [] (f 0)) $ Right [("", [])]
    equal (run [] (f 10)) $ Right [("", [])]
    equal (run [("1", [])] (f 10)) $ Right [("1", []), ("", [])]
    equal (run [("1", [])] (f 0)) $ Right [("", []), ("1", [])]

-- test_track = do
    -- TODO actually test the ruler stuff too
