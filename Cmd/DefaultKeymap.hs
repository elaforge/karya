{- The default keyboard layout.

    TODO I should also be careful how I use the modifiers.  If I must use both
    control and command, I should be consistent about their meaning.  If I do
    the above scheme, command is for uncommon and possibly dangerous things
    like save, load, undo, redo, etc., and control would be for all the single
    key commands that are displaced by kbd note entry: set step, transpose
    selection, modify note length, ...
-}
module Cmd.DefaultKeymap where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config

import qualified Ui.Key as Key

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (bind_key, bind_kmod)

import qualified Cmd.Clip as Clip
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Play as Play
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.View as View


default_cmds :: [Cmd.Cmd]
default_cmds =
    [ Selection.cmd_mouse_selection 1 Config.insert_selnum
    , Keymap.make_cmd (misc_bindings ++ selection_bindings
        ++ view_config_bindings ++ edit_bindings ++ create_bindings
        ++ clip_bindings)
    ]

cmd_io_keymap :: Play.PlayInfo -> Msg.Msg -> Cmd.CmdIO
cmd_io_keymap play_info = Keymap.make_cmd (io_bindings play_info)

io_bindings :: Play.PlayInfo -> [Keymap.Binding IO]
io_bindings play_info =
    [ bind_kmod [Key.MetaL, Key.ShiftL] (Key.KeyChar 's') "save" cmd_save
    , bind_kmod [Key.MetaL, Key.ShiftL] (Key.KeyChar 'l') "load" cmd_load

    -- player
    , bind_key Key.Enter "play block" (Play.cmd_play_focused play_info)
    , bind_kmod [Key.ShiftL] Key.Enter "play from insert"
        (Play.cmd_play_from_insert play_info)
    , bind_key (Key.KeyChar ' ') "stop play" Play.cmd_stop
    ]

cmd_save, cmd_load :: Cmd.CmdIO
cmd_save = Save.get_save_file >>= Save.cmd_save >> return Cmd.Done
cmd_load = Save.get_save_file >>= Save.cmd_load >> return Cmd.Done

done = (>> return Cmd.Done)

misc_bindings =
    [ bind_kmod [Key.MetaL] (Key.KeyChar '\'') "quit" Cmd.cmd_quit
    ]

-- This is command on a mac, I might have to generalize this for other
-- platforms.
command = bind_kmod [Key.MetaL]

repeating mods key desc cmd =
    [bind_kmod mods key desc cmd, bind_kmod (key:mods) key desc cmd]

selection_bindings = concat
    [ repeating [] Key.Down "advance selection"
        (Selection.cmd_step_selection selnum TimeStep.Advance False)
    , repeating [Key.ShiftL] Key.Down "extend advance selection"
        (Selection.cmd_step_selection selnum TimeStep.Advance True)

    , repeating [] Key.Up "rewind selection"
        (Selection.cmd_step_selection selnum TimeStep.Rewind False)
    , repeating [Key.ShiftL] Key.Up "extend rewind selection"
        (Selection.cmd_step_selection selnum TimeStep.Rewind True)

    , repeating [] Key.Right "shift selection right"
        (Selection.cmd_shift_selection selnum 1 False)
    , repeating [Key.ShiftL] Key.Right "extend shift selection right"
        (Selection.cmd_shift_selection selnum 1 True)

    , repeating [] Key.Left "shift selection left"
        (Selection.cmd_shift_selection selnum (-1) False)
    , repeating [Key.ShiftL] Key.Left "extend shift selection left"
        (Selection.cmd_shift_selection selnum (-1) True)
    ]
    where selnum = Config.insert_selnum

view_config_bindings =
    [ bind_key (Key.KeyChar '[') "zoom out *0.8"
        (View.cmd_zoom_around_insert (*0.8))
    , bind_key (Key.KeyChar ']') "zoom in *1.25"
        (View.cmd_zoom_around_insert (*1.25))
    ]

edit_bindings =
    [ bind_key Key.Escape "toggle kbd entry" Edit.cmd_toggle_kbd_entry
    , bind_kmod [Key.ShiftL] Key.Escape "toggle midi entry"
        Edit.cmd_toggle_midi_entry
    , bind_key Key.Backspace "remove event"
        (Edit.cmd_remove_selected >> Selection.cmd_advance_insert)

    , command (Key.KeyChar '0') "step rank 0" (Edit.cmd_meter_step 0)
    , command (Key.KeyChar '1') "step rank 1" (Edit.cmd_meter_step 1)
    , command (Key.KeyChar '2') "step rank 2" (Edit.cmd_meter_step 2)
    , command (Key.KeyChar '3') "step rank 3" (Edit.cmd_meter_step 3)
    , command (Key.KeyChar '4') "step rank 4" (Edit.cmd_meter_step 4)

    , bind_key (Key.KeyChar '-') "octave -1" (Edit.cmd_modify_octave (+ (-1)))
    , bind_key (Key.KeyChar '=') "octave +1" (Edit.cmd_modify_octave (+1))

    -- TODO These should probably go in the note track bindings.
    , command (Key.KeyChar 's') "set dur" (done Edit.cmd_set_duration)
    , command (Key.KeyChar '.') "dur * 1.5" (done (Edit.cmd_modify_dur (*1.5)))
    , command (Key.KeyChar ',') "dur / 1.5" (done (Edit.cmd_modify_dur (/1.5)))
    ]

create_bindings =
    [ command (Key.KeyChar 't') "append track"
        (done Create.insert_track_after_selection)
    , command (Key.KeyChar 'd') "remove track"
        (done Create.remove_selected_tracks)
    ]

clip_bindings =
    [ command (Key.KeyChar 'c') "copy selection" Clip.cmd_copy_selection
    , command (Key.KeyChar 'x') "cut selection" Clip.cmd_cut_selection
    , command (Key.KeyChar 'v') "paste selection" Clip.cmd_paste_overwrite
    , command (Key.KeyChar 'm') "merge selection" Clip.cmd_paste_merge
    , bind_kmod [Key.MetaL, Key.ShiftL] (Key.KeyChar 'v') "insert selection"
        Clip.cmd_paste_insert
    ]
