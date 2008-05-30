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

import qualified Cmd.Selection as Selection
import qualified Cmd.Edit as Edit
import qualified Cmd.Save as Save
import qualified Cmd.Play as Play

import qualified Midi.Midi as Midi -- for default_addr hack

import qualified Perform.Transport as Transport


default_cmds :: Cmd.State -> [Cmd.Cmd]
default_cmds state =
    Edit.cmd_midi_thru
    : (if Cmd.state_edit_mode state
        then [cmd_midi_entry, cmd_kbd_note_entry] else [])
    ++ [ Selection.cmd_mouse_selection 1 Config.insert_selnum
    , Keymap.make_cmd (misc_bindings ++ selection_bindings ++ edit_bindings)
    ]

cmd_io_keymap :: Transport.Info -> Cmd.CmdIO
cmd_io_keymap player_info = Keymap.make_cmd (io_bindings player_info)

io_bindings :: Transport.Info -> [Keymap.Binding IO]
io_bindings player_info =
    [ kbd_kmod_cmd [Key.MetaL] (Key.KeyChar 's') "save" cmd_save
    , kbd_kmod_cmd [Key.MetaL] (Key.KeyChar 'l') "load" cmd_load
    , kbd_cmd Key.Enter "play block" (Play.cmd_play_block player_info)
    , kbd_cmd (Key.KeyChar ' ') "stop play" Play.cmd_stop
    ]

cmd_save, cmd_load :: Cmd.CmdT IO Cmd.Status
cmd_save = Save.cmd_save Nothing >> return Cmd.Done
cmd_load = Save.cmd_load Nothing >> return Cmd.Done

cmd_kbd_note_entry :: Cmd.Cmd
cmd_kbd_note_entry = Keymap.make_cmd kbd_note_entry
cmd_midi_entry msg = Edit.cmd_insert_midi_note msg >> cmd_advance_insert

misc_bindings =
    [ kbd_cmd (Key.KeyChar '=') "quit" Cmd.cmd_quit
    ]

selection_bindings =
    [ kbd_cmd Key.Down "advance selection" $
        Selection.cmd_step_selection Config.insert_selnum Advance
    , kbd_cmd Key.Up "rewind selection" $
        Selection.cmd_step_selection Config.insert_selnum Rewind
    , kbd_cmd Key.Right "shift selection right" $
        Selection.cmd_shift_selection Config.insert_selnum 1
    , kbd_cmd Key.Left "shift selection left" $
        Selection.cmd_shift_selection Config.insert_selnum (-1)
    ]

cmd_advance_insert = Selection.cmd_step_selection Config.insert_selnum Advance

edit_bindings =
    [ bind_key Key.Escape "toggle edit mode" Edit.cmd_toggle_edit
    , bind_key Key.Backspace "remove event"
        (Edit.cmd_remove_events >> Selection.cmd_advance_insert)

    , bind_key (Key.KeyChar '1') "step rank 1" (Edit.cmd_meter_step 1)
    , bind_key (Key.KeyChar '2') "step rank 2" (Edit.cmd_meter_step 2)
    , bind_key (Key.KeyChar '3') "step rank 3" (Edit.cmd_meter_step 3)
    , bind_key (Key.KeyChar '4') "step rank 4" (Edit.cmd_meter_step 4)
    ]
