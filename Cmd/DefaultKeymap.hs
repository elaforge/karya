{- | The default keyboard layout.

    I group commands by their function:

    There should be deriver independent generic editing, and then deriver
    specific shortcuts built on that.  So generic level looks like:

    -1 State level operations, like load, save, undo.  Set timestep mode, set
    edit mode.  Play, stop.  Move the selection.

    0 Modify text independent attributes, like start position and duration.
    This includes copy and paste, remove, modify duration, nudge, ...

    1 Create event with length as per timestep, then type to edit it.  Uses
    alphanum keys.

    Deriver specific commands are enabled in Schema.schema_cmds:

    2 Modify events depending on track type, and hence requiring a deriver that
    uses skeletons: transpose, modify vals, ...

    2.5 Send midi thru, which is remapped according to the nearest note track
    and its active scale.

    3 Edit the method part of an event.  Takes alphanum keys.

    4 Edit the value part.  For numeric values, this is alphanum keys.  For enum
    values (like notes) I'll want an adjustable key->text mapping.  Notes
    take KeyNum -> Scale -> String -> String, while enums take Instrument ->
    Controller -> String -> String.  A scale is like a special case of an enum.
    TODO Enums are not implemented yet, and scales are hardcoded.  The scale
    should be configured globally, as an insert mode, and enum vals are stored
    in the instrument.

    5 Edit the call part.  Takes alphanum keys.

    Since a fair number of these want full alphanum keys, they have to be
    organized with a system of modes.  -1 and 0 are always in scope.  They may
    be shadowed by other modes, but generally globally available commands
    should not be.

    1 is raw edit mode.  It may be useful for entering deriver commands, like
    scale, random seed, or something else.

    2 is active in command mode, shouldn't overlap with -1 and 0.

    3 is active in method edit mode, which should be easily accessible from val
    edit mode.

    4 is active in val edit mode.  On controller tracks, it takes alphanum or
    midi controller, and on note tracks it only takes midi notes (so I can
    still use 0 commands).  If I turn off the number typing in favor of midi
    controller, or make it so it doesn't shadow useful keys (numbers, dot,
    minus), it's more convenient.

    5 is active in call edit mode, which can be via an escape combo.

    Furthermore, there is kbd entry mode, which should behave exactly like
    a midi kbd entry.

    -1 and 0 commands generally get a command- variant, so you can run them
    even when the alphanums are taken over.

    All the edit modes are exclusive.  There is a set of -1 level commands to
    toggle between edit modes and no edit (command mode).  Toggling kbd note
    entry is orthogonal to the rest: cmd-shift-esc, and puts a K in the edit
    box.

    TODO then there's midi recording:

    In record mode, the block is played while recording midi msgs and their
    timestamps, which are later passed to the integrator to convert into
    events.
-}
-- TODO rename this GlobalKeymap?
module Cmd.DefaultKeymap where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config

import qualified Ui.Key as Key

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap

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
io_bindings play_info = concat
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

-- | Most command keys are mapped to both a plain keystroke and command-key
-- (this should presumably becoume control-key on linux).  That way the command
-- can still be invoked when an edit mode has taken over the alphanumeric keys.
command key desc cmd =
    [ Keymap.bind_key key desc cmd
    , Keymap.bind_kmod [Key.MetaL] key desc cmd
    , Keymap.bind_kmod [Key.MetaR] key desc cmd
    ]

bind_key key desc cmd = [Keymap.bind_key key desc cmd]
bind_kmod mods key desc cmd = [Keymap.bind_kmod mods key desc cmd]

-- | But some commands are too dangerous to get a plain keystroke version.
command_only key desc cmd = bind_kmod [Key.MetaL] key desc cmd

-- | Normally commands won't be re-invoked by key repeat, but sometimes it's
-- useful.
repeating mods key desc cmd =
    [ Keymap.bind_kmod mods key desc cmd
    , Keymap.bind_kmod (key:mods) key desc cmd ]

misc_bindings = command_only (Key.KeyChar '\'') "quit" Cmd.cmd_quit

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

-- All the edit modes are exclusive.  Go to raw edit: cmd-esc, go to call
-- edit: shift-esc, go to val edit or turn of edit: esc.  Toggle val and
-- method edit: tab.

view_config_bindings = concat
    [ command (Key.KeyChar '[') "zoom out *0.8"
        (View.cmd_zoom_around_insert (*0.8))
    , command (Key.KeyChar ']') "zoom in *1.25"
        (View.cmd_zoom_around_insert (*1.25))
    ]

edit_bindings = concat
    [ bind_key Key.Escape "toggle edit" Edit.cmd_toggle_val_edit
    , bind_kmod [Key.MetaL] Key.Escape "toggle raw edit"
        Edit.cmd_toggle_raw_edit
    , bind_kmod [Key.ShiftL] Key.Escape "toggle call edit"
        Edit.cmd_toggle_call_edit
    , bind_kmod [] Key.Tab "toggle method edit" Edit.cmd_toggle_method_edit

    , bind_kmod [Key.ShiftL, Key.MetaL] Key.Escape "toggle kbd entry mode"
        Edit.cmd_toggle_kbd_entry

    , command Key.Backspace "remove event"
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

create_bindings = concat
    [ command (Key.KeyChar 't') "append track"
        (done Create.insert_track_after_selection)
    , command (Key.KeyChar 'd') "remove track"
        (done Create.remove_selected_tracks)
    ]

clip_bindings = concat
    [ command_only (Key.KeyChar 'c') "copy selection" Clip.cmd_copy_selection
    , command_only (Key.KeyChar 'x') "cut selection" Clip.cmd_cut_selection
    , command_only (Key.KeyChar 'v') "paste selection" Clip.cmd_paste_overwrite
    , command_only (Key.KeyChar 'm') "merge selection" Clip.cmd_paste_merge
    , bind_kmod [Key.MetaL, Key.ShiftL] (Key.KeyChar 'v') "insert selection"
        Clip.cmd_paste_insert
    ]
