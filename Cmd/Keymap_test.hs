-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Keymap_test where
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg

import qualified Derive.DeriveTest as DeriveTest
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg

import           Global
import           Util.Test


test_make_keymap :: Test
test_make_keymap = do
    let (_, errors) = Keymap.makeKeymap binds
    stringsLike (map untxt errors) ["cmds overlap* [1: 1, 1: 12]"]

test_make_cmd :: Test
test_make_cmd = do
    let (keymap, _) = Keymap.makeKeymap binds
    let cmd = Cmd.call $ Cmd.Keymap keymap
    let noRun = []
        didRun cname cmdlog = ["running command: " <> cname, cmdlog]
        aborted = Right (Nothing, [])
    let run mods msg = extractLogs (runCmd cmd mods msg)
    let runChar mods char = run (map Cmd.KeyMod mods) (CmdTest.keyDown char)
    -- pprint $ zip (Map.keys keymap)
    --     (map (\(Keymap.CmdSpec name _) -> name) (Map.elems keymap))
    equal (runChar [] 'a') noRun
    equal (runChar [] '1') (didRun "12" "cmd1") -- last cmd wins
    equal (runChar [] '2') (didRun "2" "cmd2")
    equal (runChar [] '3') noRun

    equal (runChar [Key.Shift] '3') (didRun "s-3" "cmd1")
    -- The control key varies by platform.
    let Just control = Map.lookup Keymap.PrimaryCommand Keymap.simpleModMap
    equal (runChar [control] '1') (didRun "c-1" "cmd1")
    equal (runChar [control, Key.Shift] '1') (didRun "cs-1" "cmd1")

    -- key up aborts
    equal (CmdTest.extract id (runCmd cmd [] (CmdTest.keyUp '1'))) aborted

    -- mouse chording and dragging
    equal (run [] (CmdTest.mouse True 2 0 0)) noRun
    equal (run [Cmd.MouseMod 1 Nothing] (CmdTest.mouse True 2 0 0))
        (didRun "chord-12" "cmd1")
    -- mouse release is not bound
    equal (CmdTest.extract id (runCmd cmd [Cmd.MouseMod 1 Nothing]
            (CmdTest.mouse False 2 0 0)))
        (Right (Just Cmd.Continue, []))
    -- bind_drag binds both the click and the drag
    equal (run [Cmd.MouseMod 3 Nothing] (CmdTest.mouse True 3 0 0))
        (didRun "drag-3" "cmd1")
    equal (run [Cmd.MouseMod 3 Nothing] (CmdTest.drag 3 0 0))
        (didRun "drag-3" "cmd1")

test_key_repeat :: Test
test_key_repeat = do
    let (keymap, _) = Keymap.makeKeymap $ concat
            [ Keymap.plainChar '1' "1" cmd1
            , Keymap.bindRepeatable [] (Key.Char '2') "2" cmd2
            ]
    let cmd = Cmd.call $ Cmd.Keymap keymap
    let run repeat char = extractLogs $ runCmd cmd []
            (CmdTest.makeKey
                (if repeat then UiMsg.KeyRepeat else UiMsg.KeyDown)
                (Key.Char char))
    equal (run False '1') ["running command: 1", "cmd1"]
    equal (run True '1') []
    equal (run False '2') ["running command: 2", "cmd2"]
    equal (run True '2') ["running command: 2", "cmd2"]


extractLogs :: CmdTest.Result val -> [Text]
extractLogs = map DeriveTest.showLog . CmdTest.resultLogs

runCmd :: (Msg.Msg -> Cmd.CmdId a) -> [Cmd.Modifier] -> Msg.Msg
    -> CmdTest.Result a
runCmd cmd mods msg = CmdTest.run Ui.empty cstate (cmd msg)
    where
    cstate = CmdTest.defaultCmdState { Cmd.stateKeysDown = stateMods }
    stateMods = Map.fromList [(m, m) | m <- mods ++ extraMods]
    -- A click or drag implies that mouse button must be down.
    extraMods = case fmap UiMsg.mouseState (Msg.mouse msg) of
        Just (UiMsg.MouseDown b) -> [Cmd.MouseMod b Nothing]
        Just (UiMsg.MouseDrag b) -> [Cmd.MouseMod b Nothing]
        _ -> []

cmd1, cmd2 :: Cmd.CmdId ()
cmd1 = Log.notice "cmd1"
cmd2 = Log.notice "cmd2"

binds :: [Keymap.Binding Cmd.CmdId]
binds = concat
    [ Keymap.plainChar '1' "1" cmd1
    , Keymap.plainChar '1' "12" cmd1
    , Keymap.plainChar '2' "2" cmd2
    , Keymap.bindKey [Keymap.Shift] (Key.Char '3') "s-3" cmd1
    , Keymap.bindKey [Keymap.PrimaryCommand] (Key.Char '1') "c-1" cmd1
    , Keymap.bindKey [Keymap.Shift, Keymap.PrimaryCommand] (Key.Char '1')
        "cs-1" cmd1
    , Keymap.bindClick [Keymap.Mouse 1] 2 Cmd.OnTrack 1 "chord-12"
        (const cmd1)
    , Keymap.bindDrag [] 3 Cmd.OnTrack "drag-3" (const cmd1)
    ]
