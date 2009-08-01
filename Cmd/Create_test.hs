module Cmd.Create_test where
import qualified Data.Map as Map
import Util.Test
import qualified Util.Log as Log

import qualified Ui.Block as Block
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.TestSetup as TestSetup

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd

import qualified Derive.Twelve as Twelve

import qualified Cmd.Create as Create
import Util.Debug



mksel = Block.point_selection
mkkey = CmdTest.make_key True

cstate = Cmd.empty_state

test_track_ruler = do
    let f tracknum = Create.track_ruler CmdTest.default_block_id State.no_ruler
            tracknum 20
        run track_specs cmd = tracks
            where (Right (_, tracks, [])) = CmdTest.run_tracks track_specs cmd

    equal (run [] (f 0)) [("", [])]
    equal (run [] (f 10)) [("", [])]
    equal (run [("1", [])] (f 10)) [("1", []), ("", [])]
    equal (run [("1", [])] (f 0)) [("", []), ("1", [])]

-- test_track = do
    -- TODO actually test the ruler stuff too
