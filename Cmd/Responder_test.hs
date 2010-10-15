module Cmd.Responder_test where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
-- import qualified Data.Map as Map
-- import qualified System.IO as IO
import qualified System.CPUTime as CPUTime

import qualified Midi.CoreMidi as CoreMidi
import qualified Midi.Midi as Midi

import qualified Util.Log as Log
import Util.Test

-- import qualified Ui.Key as Key
import qualified Ui.State as State
-- import qualified Ui.Update as Update
import qualified Ui.UiTest as UiTest

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Lang as Lang
import qualified Cmd.Responder as Responder

import qualified Perform.Transport as Transport

import qualified App.StaticConfig as StaticConfig


{-
-- test_keydown = do
    let (_, ustate) = UiTest.run_mkview []
    let cstate = CmdTest.default_cmd_state
    let extract ([((res, cstate), _)], _) =
            (res2, Map.keys (Cmd.state_keys_down cstate))
            where
            res2 = case res of
                Left err -> Just (show err)
                _ -> Nothing
    let run msg = fmap extract (run_msgs ustate cstate [msg])
    -- Make sure keydown works even when the cmd throws.
    -- This is brittle because it relies on the definition of ' '.
    io_equal (run (CmdTest.make_key True (Key.KeyChar ' ')))
        (Just "StateError: player thread not running",
            [Cmd.KeyMod (Key.KeyChar ' ')])
-}
