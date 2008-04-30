module Cmd.DefaultKeymap where

import qualified Ui.Key as Key
import qualified Ui.Block as Block

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Selection as Selection

default_cmds :: [Cmd.Cmd]
default_cmds =
    [ Selection.cmd_mouse_selection 1 insert_selnum
    , Keymap.make_cmd selection
    ]


-- | SelNum of the insertion point.
insert_selnum :: Block.SelNum
insert_selnum = 0

-- | SelNum of the playback position indicator.
playback_selnum :: Block.SelNum
playback_selnum = 4

-- Move the insert selection around.
selection =
    [ single Key.Down "advance selection" $
        Selection.cmdm_step_selection insert_selnum Selection.Advance
    , single Key.Up "rewind selection" $
        Selection.cmdm_step_selection insert_selnum Selection.Rewind
    , single Key.Right "shift selection right" $
        Selection.cmdm_shift_selection insert_selnum 1
    , single Key.Left "shift selection left" $
        Selection.cmdm_shift_selection insert_selnum (-1)
    ]

-- TODO: should take [[Modifier]] and produce a mapping for each
-- wait and see what's actually useful
spec = Keymap.KeySpec
single key desc cmd = (spec [] (Keymap.UiKey key), cmdm desc cmd)
cmdm :: String -> Cmd.CmdM -> Keymap.CmdSpec
cmdm name cmd = Keymap.CmdSpec name (ignore_msg cmd)


-- ** util

-- | Cmds that don't actually need the msg can use this.
ignore_msg :: Cmd.CmdM -> Cmd.Cmd
ignore_msg = const
