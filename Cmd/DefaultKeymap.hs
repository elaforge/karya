module Cmd.DefaultKeymap where

import qualified Ui.Key as Key
import qualified Ui.Block as Block

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Movement as Movement


default_keymap :: Cmd.Cmd
default_keymap = Keymap.make_cmd (movement ++ block)


-- | SelNum of the insertion point.
insert_selnum :: Block.SelNum
insert_selnum = 0

-- | SelNum of the playback position indicator.
playback_selnum :: Block.SelNum
playback_selnum = 4

-- Move the insert selection around.
movement =
    [ (spec [] (Keymap.UiKey Key.Down), cmdm "advance selection" $
        Movement.step_selection insert_selnum Movement.Advance)
    , (spec [] (Keymap.UiKey Key.Up), cmdm "rewind selection" $
        Movement.step_selection insert_selnum Movement.Rewind)
    ]

block =
    [
    ]

-- TODO: should take [[Modifier]] and produce a mapping for each
-- wait and see what's actually useful
spec = Keymap.KeySpec
cmdm :: String -> Cmd.CmdM -> Keymap.CmdSpec
cmdm name cmd = Keymap.CmdSpec name (ignore_msg cmd)


-- ** util

-- | Cmds that don't actually need the msg can use this.
ignore_msg :: Cmd.CmdM -> Cmd.Cmd
ignore_msg = const
