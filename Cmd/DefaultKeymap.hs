{- The default keyboard layout.

TODO:
Most of the keyboard is taken up by note entry when you're on an instrument
track, but this isn't too useful when you are using a MIDI keyboard.  I could
have switchable keymaps and a state to turn kbd entry on or off, but it would
be confusing to move all the commands around.  Unless I do something consistent
like say that it's ^X with keymap on, and just X with keymap off.

TODO I should also be careful how I use the modifiers.  If I must use both
control and command, I should be consistent about their meaning.  If I do
the above scheme, command is for uncommon and possibly dangerous things like
save, load, undo, redo, etc., and control would be for all the single key
commands that are displaced by kbd note entry: set step, transpose selection,
modify note length, ...

-}
module Cmd.DefaultKeymap where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config

import qualified Ui.Key as Key

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (bind_key, bind_kmod)

import qualified Cmd.Selection as Selection
import qualified Cmd.Edit as Edit
import qualified Cmd.Save as Save
import qualified Cmd.Play as Play
import qualified Cmd.View as View
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Transport as Transport


default_cmds :: [Cmd.Cmd]
default_cmds =
    [ Selection.cmd_mouse_selection 1 Config.insert_selnum
    , Keymap.make_cmd (misc_bindings ++ selection_bindings
        ++ view_config_bindings ++ edit_bindings)
    ]

cmd_io_keymap :: Transport.Info -> Msg.Msg -> Cmd.CmdIO
cmd_io_keymap player_info = Keymap.make_cmd (io_bindings player_info)

io_bindings :: Transport.Info -> [Keymap.Binding IO]
io_bindings player_info =
    [ command (Key.KeyChar 's') "save" cmd_save
    , command (Key.KeyChar 'l') "load" cmd_load
    , bind_key Key.Enter "play block" (Play.cmd_play_block player_info)
    , bind_key (Key.KeyChar ' ') "stop play" Play.cmd_stop
    ]

cmd_save, cmd_load :: Cmd.CmdIO
cmd_save = Save.cmd_save Nothing >> return Cmd.Done
cmd_load = Save.cmd_load Nothing >> return Cmd.Done

misc_bindings =
    [ bind_kmod [Key.MetaL] (Key.KeyChar '\'') "quit" Cmd.cmd_quit
    ]

-- This is command on a mac, I might have to generalize this for other
-- platforms.
command = bind_kmod [Key.MetaL]

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

view_config_bindings =
    [ bind_key (Key.KeyChar '[') "zoom out *0.8"
        (View.cmd_zoom_around_insert (*0.8))
    , bind_key (Key.KeyChar ']') "zoom in *1.25"
        (View.cmd_zoom_around_insert (*1.25))
    ]

edit_bindings =
    [ bind_key Key.Escape "toggle edit mode" Edit.cmd_toggle_edit
    , bind_key Key.Backspace "remove event"
        (Edit.cmd_remove_events >> Selection.cmd_advance_insert)

    , command (Key.KeyChar '0') "step rank 0" (Edit.cmd_meter_step 0)
    , command (Key.KeyChar '1') "step rank 1" (Edit.cmd_meter_step 1)
    , command (Key.KeyChar '2') "step rank 2" (Edit.cmd_meter_step 2)
    , command (Key.KeyChar '3') "step rank 3" (Edit.cmd_meter_step 3)
    , command (Key.KeyChar '4') "step rank 4" (Edit.cmd_meter_step 4)

    , bind_key (Key.KeyChar '-') "octave -1" (Edit.cmd_modify_octave (+ (-1)))
    , bind_key (Key.KeyChar '=') "octave +1" (Edit.cmd_modify_octave (+1))
    ]
