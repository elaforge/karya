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
module Cmd.GlobalKeymap where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config

import qualified Ui.Key as Key

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (bind_key, bind_char, bind_mod, bind_click, bind_drag)
import Cmd.Keymap (SimpleMod(..))

import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Clip as Clip
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Play as Play
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.View as View

import qualified Perform.Transport as Transport


global_cmds :: [Cmd.Cmd]
global_cmds = [Keymap.make_cmd cmd_map]

io_cmds :: Transport.Info -> [Msg.Msg -> Cmd.CmdIO]
io_cmds transport_info =
    [ Keymap.make_cmd io_cmd_map
    , Keymap.make_cmd (player_bindings transport_info)
    ]

-- * io cmds

(io_cmd_map, io_cmd_map_errors) = Keymap.make_cmd_map $ concat
    [ bind_mod [Shift, PrimaryCommand] (Key.KeyChar 's') "save" cmd_save
    , bind_mod [Shift, PrimaryCommand] (Key.KeyChar 'l') "load" cmd_load
    , bind_char ' ' "stop play" Play.cmd_stop
    ]

cmd_save, cmd_load :: Cmd.CmdIO
cmd_save = Save.get_save_file >>= Save.cmd_save >> return Cmd.Done
cmd_load = Save.get_save_file >>= Save.cmd_load >> return Cmd.Done

-- | This is unfortunate.  In order to construct the cmd map only once, I
-- want it to be a CAF.  However, these Cmds take an argument, which means
-- I need to either have the CmdMap map to Cmds that take an argument, or
-- recreate the map on each call.  Since there are only two commands, I opt
-- for the latter.
player_bindings :: Transport.Info -> Keymap.CmdMap IO
player_bindings transport_info = fst $ Keymap.make_cmd_map $ concat
    [ bind_key Key.Enter "play block" (Play.cmd_play_focused transport_info)
    , bind_mod [Shift] Key.Enter "play from insert"
        (Play.cmd_play_from_insert transport_info)
    ]

-- * pure cmds

(cmd_map, cmd_map_errors) = Keymap.make_cmd_map $
    misc_bindings ++ selection_bindings ++ view_config_bindings
    ++ block_config_bindings ++ edit_bindings ++ create_bindings
    ++ clip_bindings

misc_bindings = command_only '\'' "quit" Cmd.cmd_quit

selection_bindings = concat
    [ bind_drag [] Config.mouse_select "snap drag selection"
        (Selection.cmd_snap_selection Config.mouse_select Config.insert_selnum)
    , bind_drag [PrimaryCommand] Config.mouse_select "free drag selection"
        (Selection.cmd_mouse_selection Config.mouse_select Config.insert_selnum)

    , bind_mod [] Key.Down "advance selection"
        (Selection.cmd_step_selection selnum TimeStep.Advance False)
    , bind_mod [Shift] Key.Down "extend advance selection"
        (Selection.cmd_step_selection selnum TimeStep.Advance True)

    , bind_mod [] Key.Up "rewind selection"
        (Selection.cmd_step_selection selnum TimeStep.Rewind False)
    , bind_mod [Shift] Key.Up "extend rewind selection"
        (Selection.cmd_step_selection selnum TimeStep.Rewind True)

    , bind_mod [] Key.Right "shift selection right"
        (Selection.cmd_shift_selection selnum 1 False)
    , bind_mod [Shift] Key.Right "extend shift selection right"
        (Selection.cmd_shift_selection selnum 1 True)

    , bind_mod [] Key.Left "shift selection left"
        (Selection.cmd_shift_selection selnum (-1) False)
    , bind_mod [Shift] Key.Left "extend shift selection left"
        (Selection.cmd_shift_selection selnum (-1) True)
    ]
    where selnum = Config.insert_selnum

view_config_bindings = concat
    [ command_char '[' "zoom out *0.8" (View.cmd_zoom_around_insert (*0.8))
    , command_char ']' "zoom in *1.25" (View.cmd_zoom_around_insert (*1.25))
    ]

block_config_bindings = concat
    [ bind_click [Shift, PrimaryCommand] Config.mouse_select 0
        "toggle skeleton edge" BlockConfig.cmd_toggle_edge
    ]

-- delete = remove events and move following events back
-- remove = just remove events
edit_bindings = concat
    [ bind_key Key.Escape "toggle val edit" Edit.cmd_toggle_val_edit
    , bind_mod [PrimaryCommand] Key.Escape "toggle raw edit"
        Edit.cmd_toggle_raw_edit
    , bind_mod [] Key.Tab "toggle method edit" Edit.cmd_toggle_method_edit

    , bind_mod [Shift] Key.Escape "toggle kbd entry mode"
        Edit.cmd_toggle_kbd_entry

    -- Unlike other event editing commands, you don't have to be in insert mode
    -- to remove events.  Maybe I'll change that later.
    -- , command Key.Backspace "remove event" Edit.cmd_remove_selected
    , bind_mod [Shift] Key.Backspace "delete selection"
        (done Edit.cmd_delete_selection)
    , bind_mod [Shift] (Key.KeyChar '=') "insert selection"
        (done Edit.cmd_insert_selection)

    , command_char 'u' "undo" (done Edit.undo)
    , command_char 'r' "redo" (done Edit.redo)

    , command_char '0' "step rank 0" (Edit.cmd_meter_step 0)
    , command_char '1' "step rank 1" (Edit.cmd_meter_step 1)
    , command_char '2' "step rank 2" (Edit.cmd_meter_step 2)
    , command_char '3' "step rank 3" (Edit.cmd_meter_step 3)
    , command_char '4' "step rank 4" (Edit.cmd_meter_step 4)

    , bind_char '-' "octave -1" (Edit.cmd_modify_octave (+ (-1)))
    , bind_char '=' "octave +1" (Edit.cmd_modify_octave (+1))

    -- TODO These should probably go in the note track bindings.
    , command_char 's' "set dur" (done Edit.cmd_set_duration)
    , command_char '.' "dur * 1.5" (done (Edit.cmd_modify_dur (*1.5)))
    , command_char ',' "dur / 1.5" (done (Edit.cmd_modify_dur (/1.5)))
    ]

create_bindings = concat
    [ command_char 't' "append track" (done Create.insert_track_after_selection)
    , command_char 'd' "remove track" (done Create.remove_selected_tracks)
    ]

clip_bindings = concat
    [ command_only 'c' "copy selection" Clip.cmd_copy_selection
    , command_only 'x' "cut selection" Clip.cmd_cut_selection
    , command_only 'v' "paste selection" Clip.cmd_paste_overwrite
    , command_only 'm' "merge selection" Clip.cmd_paste_merge
    , bind_mod [Shift, PrimaryCommand] (Key.KeyChar 'v') "insert selection"
        Clip.cmd_paste_insert
    ]

-- * util

done = (>> return Cmd.Done)
msg_done cmd msg = done (cmd msg)

-- | Most command keys are mapped to both a plain keystroke and command key.
-- This is a little unusual, but it means the command can still be invoked when
-- an edit mode has taken over the alphanumeric keys.
command key desc cmd =
    bind_key key desc cmd ++ bind_mod [PrimaryCommand] key desc cmd
command_char char = command (Key.KeyChar char)

-- | But some commands are too dangerous to get a plain keystroke version.
command_only char = bind_mod [PrimaryCommand] (Key.KeyChar char)
