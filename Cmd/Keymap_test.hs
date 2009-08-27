module Cmd.Keymap_test where
import qualified Data.Map as Map

import qualified Util.Log as Log
import Util.Test

import qualified Ui.Key as Key
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Keymap as Keymap


test_make_cmd_map = do
    let (_, errors) = Keymap.make_cmd_map binds
    strings_like errors
        ["cmds overlap.* KeySpec .*Char '1'.*: 1, KeySpec .*Char '1'.*: 12"]

test_make_cmd = do
    let (cmd_map, _) = Keymap.make_cmd_map binds
    let cmd = Keymap.make_cmd cmd_map
    let run mods char = CmdTest.extract_logs $
            CmdTest.run State.empty cstate (cmd (CmdTest.key_down char))
            where
            cstate = Cmd.empty_state { Cmd.state_keys_down = state_mods }
            state_mods = Map.fromList [(Cmd.KeyMod m, Cmd.KeyMod m) | m <- mods]
        no_run = Right (Just [])
        did_run cname cmdlog =
            Right (Just ["running command " ++ show cname, cmdlog])
    equal (run [] 'a') no_run
    equal (run [] '1') (did_run "12" "cmd1")
    equal (run [] '2') (did_run "2" "cmd2")
    equal (run [] '3') no_run

    equal (run [Key.ShiftL] '1') no_run
    equal (run [Key.ShiftL] '3') (did_run "s-3" "cmd1")
    equal (run [Key.ShiftR] '3') (did_run "s-3" "cmd1")
    equal (run [Key.MetaL] '1') (did_run "c-1" "cmd1")
    equal (run [Key.MetaL, Key.ShiftR] '1') (did_run "cs-1" "cmd1")

cmd1, cmd2 :: Cmd.CmdId
cmd1 = Log.notice "cmd1" >> return Cmd.Done
cmd2 = Log.notice "cmd2" >> return Cmd.Done

binds = concat
    [ Keymap.bind_key (Key.KeyChar '1') "1" cmd1
    , Keymap.bind_key (Key.KeyChar '1') "12" cmd1
    , Keymap.bind_key (Key.KeyChar '2') "2" cmd2
    , Keymap.bind_mod [Keymap.Shift] (Key.KeyChar '3') "s-3" cmd1
    , Keymap.bind_mod [Keymap.PrimaryCommand] (Key.KeyChar '1') "c-1" cmd1
    , Keymap.bind_mod [Keymap.PrimaryCommand, Keymap.Shift]
        (Key.KeyChar '1') "cs-1" cmd1
    ]
