-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Keymap_test where
import qualified Data.Map as Map

import qualified Util.Log as Log
import Util.Test
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg

import qualified Derive.DeriveTest as DeriveTest
import Global


test_make_cmd_map = do
    let (_, errors) = Keymap.make_cmd_map binds
    strings_like (map untxt errors) ["cmds overlap* [1: 1, 1: 12]"]

test_make_cmd = do
    let (cmd_map, _) = Keymap.make_cmd_map binds
    let cmd = Keymap.make_cmd cmd_map
    let no_run = []
        did_run cname cmdlog = ["running command " <> showt cname, cmdlog]
        aborted = Right (Nothing, [])
    let run mods msg = extract_logs (run_cmd cmd mods msg)
    let run_char mods char = run (map Cmd.KeyMod mods) (CmdTest.key_down char)
    -- pprint $ zip (Map.keys cmd_map)
    --     (map (\(Keymap.CmdSpec name _) -> name) (Map.elems cmd_map))
    equal (run_char [] 'a') no_run
    equal (run_char [] '1') (did_run "12" "cmd1") -- last cmd wins
    equal (run_char [] '2') (did_run "2" "cmd2")
    equal (run_char [] '3') no_run

    equal (run_char [Key.Shift] '3') (did_run "s-3" "cmd1")
    -- The control key varies by platform.
    let Just (control:_) = lookup Keymap.PrimaryCommand Keymap.simple_mod_map
    equal (run_char [control] '1') (did_run "c-1" "cmd1")
    equal (run_char [control, Key.Shift] '1') (did_run "cs-1" "cmd1")

    -- key up aborts
    equal (CmdTest.extract id (run_cmd cmd [] (CmdTest.key_up '1'))) aborted

    -- mouse chording and dragging
    equal (run [] (CmdTest.mouse True 2 0 0)) no_run
    equal (run [Cmd.MouseMod 1 Nothing] (CmdTest.mouse True 2 0 0))
        (did_run "chord-12" "cmd1")
    -- mouse release is not bound
    equal (CmdTest.extract id (run_cmd cmd [Cmd.MouseMod 1 Nothing]
            (CmdTest.mouse False 2 0 0)))
        (Right (Just Cmd.Continue, []))
    -- bind_drag binds both the click and the drag
    equal (run [Cmd.MouseMod 3 Nothing] (CmdTest.mouse True 3 0 0))
        (did_run "drag-3" "cmd1")
    equal (run [Cmd.MouseMod 3 Nothing] (CmdTest.drag 3 0 0))
        (did_run "drag-3" "cmd1")

test_key_repeat = do
    let (cmd_map, _) = Keymap.make_cmd_map $ concat
            [ Keymap.plain_char '1' "1" cmd1
            , Keymap.bind_repeatable [] (Key.Char '2') "2" cmd2
            ]
    let cmd = Keymap.make_cmd cmd_map
    let run repeat char = extract_logs $ run_cmd cmd []
            (CmdTest.make_key
                (if repeat then UiMsg.KeyRepeat else UiMsg.KeyDown)
                (Key.Char char))
    equal (run False '1') ["running command \"1\"", "cmd1"]
    equal (run True '1') []
    equal (run False '2') ["running command \"2\"", "cmd2"]
    equal (run True '2') ["running command \"2\"", "cmd2"]


extract_logs :: CmdTest.Result val -> [Text]
extract_logs = map DeriveTest.show_log . CmdTest.result_logs

run_cmd :: (Msg.Msg -> Cmd.CmdId a) -> [Cmd.Modifier] -> Msg.Msg
    -> CmdTest.Result a
run_cmd cmd mods msg = CmdTest.run Ui.empty cstate (cmd msg)
    where
    cstate = CmdTest.default_cmd_state { Cmd.state_keys_down = state_mods }
    state_mods = Map.fromList [(m, m) | m <- mods ++ extra_mods]
    -- A click or drag implies that mouse button must be down.
    extra_mods = case fmap UiMsg.mouse_state (Msg.mouse msg) of
        Just (UiMsg.MouseDown b) -> [Cmd.MouseMod b Nothing]
        Just (UiMsg.MouseDrag b) -> [Cmd.MouseMod b Nothing]
        _ -> []

cmd1, cmd2 :: Cmd.CmdId ()
cmd1 = Log.notice "cmd1"
cmd2 = Log.notice "cmd2"

binds :: [Keymap.Binding Cmd.CmdId]
binds = concat
    [ Keymap.plain_char '1' "1" cmd1
    , Keymap.plain_char '1' "12" cmd1
    , Keymap.plain_char '2' "2" cmd2
    , Keymap.bind_key [Keymap.Shift] (Key.Char '3') "s-3" cmd1
    , Keymap.bind_key [Keymap.PrimaryCommand] (Key.Char '1') "c-1" cmd1
    , Keymap.bind_key [Keymap.Shift, Keymap.PrimaryCommand] (Key.Char '1')
        "cs-1" cmd1
    , Keymap.bind_click [Keymap.Mouse 1] 2 Keymap.OnTrack 1 "chord-12"
        (const cmd1)
    , Keymap.bind_drag [] 3 Keymap.OnTrack "drag-3" (const cmd1)
    ]
