module Cmd.Keymap_test where
import qualified Data.Map as Map

import qualified Util.Log as Log
import Util.Test
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg

import qualified Derive.DeriveTest as DeriveTest


test_make_cmd_map = do
    let (_, errors) = Keymap.make_cmd_map binds
    strings_like errors
        ["cmds overlap* KeySpec *Char '1'*: 1, KeySpec *Char '1'*: 12"]

test_make_cmd = do
    let (cmd_map, _) = Keymap.make_cmd_map binds
    let cmd = Keymap.make_cmd cmd_map
    let extract = map DeriveTest.show_log . CmdTest.result_logs
    let run_cmd mods msg = CmdTest.run State.empty cstate (cmd msg)
            where
            cstate = Cmd.empty_state { Cmd.state_keys_down = state_mods }
            state_mods = Map.fromList [(m, m) | m <- mods ++ extra_mods]
            -- A click or drag implies that mouse button must be down.
            extra_mods = case fmap UiMsg.mouse_state (Msg.mouse msg) of
                Just (UiMsg.MouseDown b) -> [Cmd.MouseMod b Nothing]
                Just (UiMsg.MouseDrag b) -> [Cmd.MouseMod b Nothing]
                _ -> []
        no_run = []
        did_run cname cmdlog = ["running command " ++ show cname, cmdlog]
        aborted = Right (Nothing, [])
    let run mods msg = extract (run_cmd mods msg)
    let run_char mods char = run (map Cmd.KeyMod mods) (CmdTest.key_down char)
    -- pprint $ zip (Map.keys cmd_map)
    --     (map (\(Keymap.CmdSpec name _) -> name) (Map.elems cmd_map))
    equal (run_char [] 'a') no_run
    equal (run_char [] '1') (did_run "12" "cmd1") -- last cmd wins
    equal (run_char [] '2') (did_run "2" "cmd2")
    equal (run_char [] '3') no_run

    equal (run_char [Key.Shift] '1') no_run
    equal (run_char [Key.Shift] '3') (did_run "s-3" "cmd1")
    equal (run_char [Key.Meta] '1') (did_run "c-1" "cmd1")
    equal (run_char [Key.Meta, Key.Shift] '1') (did_run "cs-1" "cmd1")

    -- key up aborts
    equal (CmdTest.extract id (run_cmd [] (CmdTest.key_up '1'))) aborted

    -- mouse chording and dragging
    equal (run [] (CmdTest.mouse True 2)) no_run
    equal (run [Cmd.MouseMod 1 Nothing] (CmdTest.mouse True 2))
        (did_run "chord-12" "cmd1")
    equal (CmdTest.extract id (run_cmd [Cmd.MouseMod 1 Nothing]
            (CmdTest.mouse False 2)))
        aborted
    -- bind_drag binds both the click and the drag
    equal (run [Cmd.MouseMod 3 Nothing] (CmdTest.mouse True 3))
        (did_run "drag-3" "cmd1")
    equal (run [Cmd.MouseMod 3 Nothing] (CmdTest.drag 3))
        (did_run "drag-3" "cmd1")

cmd1, cmd2 :: Cmd.CmdId ()
cmd1 = Log.notice "cmd1"
cmd2 = Log.notice "cmd2"

binds :: [Keymap.Binding Cmd.CmdId]
binds = concat
    [ Keymap.bind_key (Key.KeyChar '1') "1" cmd1
    , Keymap.bind_key (Key.KeyChar '1') "12" cmd1
    , Keymap.bind_key (Key.KeyChar '2') "2" cmd2
    , Keymap.bind_mod [Keymap.Shift] (Key.KeyChar '3') "s-3" cmd1
    , Keymap.bind_mod [Keymap.PrimaryCommand] (Key.KeyChar '1') "c-1" cmd1
    , Keymap.bind_mod [Keymap.PrimaryCommand, Keymap.Shift]
        (Key.KeyChar '1') "cs-1" cmd1
    , Keymap.bind_click [Keymap.Mouse 1] 2 1 "chord-12" (const cmd1)
    , Keymap.bind_drag [] 3 "drag-3" (const cmd1)
    ]
