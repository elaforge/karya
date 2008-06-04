{- The default keyboard layout.

For kbd entry, you only get midi thru when you're in insert mode, which means
you can't try out a key without entering a note.  It's probably useful to
do thru with insert off, but then midi thru cmds take up most of the kbd even
in cmd mode.  Possible solutions:

- Do kbd midi thru anyway, since it doesn't particularly hurt to play a note
when running some cmd.

- Leave the kbd for for notes, and use control or command for all kbd commands.
This isn't too appealing because kbd note entry may be turned off most of the
time (when a midi kbd is available).

TODO I should also be careful how I use the modifiers.  If I must use both
control and command, I should be consistent about their meaning.

-}
module Cmd.DefaultKeymap where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config

import qualified Ui.Key as Key

import Cmd.Types
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (bind_key, bind_kmod)

import qualified Cmd.Selection as Selection
import qualified Cmd.Edit as Edit
import qualified Cmd.Save as Save
import qualified Cmd.Play as Play
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Transport as Transport


default_cmds :: [Cmd.Cmd]
default_cmds =
    [ Selection.cmd_mouse_selection 1 Config.insert_selnum
    , Keymap.make_cmd (misc_bindings ++ selection_bindings ++ edit_bindings)
    ]

cmd_io_keymap :: Transport.Info -> Msg.Msg -> Cmd.CmdIO
cmd_io_keymap player_info = Keymap.make_cmd (io_bindings player_info)

io_bindings :: Transport.Info -> [Keymap.Binding IO]
io_bindings player_info =
    [ bind_kmod [Key.MetaL] (Key.KeyChar 's') "save" cmd_save
    , bind_kmod [Key.MetaL] (Key.KeyChar 'l') "load" cmd_load
    , bind_key Key.Enter "play block" (Play.cmd_play_block player_info)
    , bind_key (Key.KeyChar ' ') "stop play" Play.cmd_stop
    ]

cmd_save, cmd_load :: Cmd.CmdIO
cmd_save = Save.cmd_save Nothing >> return Cmd.Done
cmd_load = Save.cmd_load Nothing >> return Cmd.Done

misc_bindings =
    [ bind_kmod [Key.MetaL] (Key.KeyChar '\'') "quit" Cmd.cmd_quit
    ]

repeating key desc cmd = [bind_key key desc cmd, bind_kmod [key] key desc cmd]
selection_bindings =
    repeating Key.Down "advance selection"
        (Selection.cmd_step_selection Config.insert_selnum TimeStep.Advance)
    ++ repeating Key.Up "rewind selection"
        (Selection.cmd_step_selection Config.insert_selnum TimeStep.Rewind)
    ++ repeating Key.Right "shift selection right"
        (Selection.cmd_shift_selection Config.insert_selnum 1)
    ++ repeating Key.Left "shift selection left"
        (Selection.cmd_shift_selection Config.insert_selnum (-1))

edit_bindings =
    [ bind_key Key.Escape "toggle edit mode" Edit.cmd_toggle_edit
    , bind_key Key.Backspace "remove event"
        (Edit.cmd_remove_events >> Selection.cmd_advance_insert)

    , bind_key (Key.KeyChar '1') "step rank 1" (Edit.cmd_meter_step 1)
    , bind_key (Key.KeyChar '2') "step rank 2" (Edit.cmd_meter_step 2)
    , bind_key (Key.KeyChar '3') "step rank 3" (Edit.cmd_meter_step 3)
    , bind_key (Key.KeyChar '4') "step rank 4" (Edit.cmd_meter_step 4)

    , bind_key (Key.KeyChar '-') "octave -1" (Edit.cmd_modify_octave (+ (-1)))
    , bind_key (Key.KeyChar '=') "octave +1" (Edit.cmd_modify_octave (+1))
    ]
