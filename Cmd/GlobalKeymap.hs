{- | The default keyboard layout.

    I group cmds by their function.  The cmds of with higher numbers will
    shadow the cmds with lower numbers.

    There should be deriver independent generic editing, and then deriver
    specific shortcuts built on that.  Global cmds also generally have
    a command-key version, so you can invoke them even when kbd entry is on.

    Global generic cmds:

    0. State level operations, like load, save, undo.  Set timestep mode, set
    edit mode.  Play, stop.  Move the selection.

    1. Modify text independent attributes, like start position and duration.
    This includes copy and paste, remove, modify duration, nudge, ...

    Track-specific cmds depend on the schema and are looked up in
    'Derive.Schema.schema_cmds':

    2. Track specific event operations, possibly affecting all events in the
    selection: transposition, etc.  These are active regardless of EditMode,
    and are found in Cmd.*TrackKeymap.

    3. Track specific event editing, which modifies a single event.
    E.g., input notes and note text, pitches in a certain scale, controller
    values, etc.  This is enabled by edit mode, and will shadow printable keys,
    depending on the track type and edit mode.  Found in Cmd.*Track.

    4. Kbd entry, if on, will hijack the letter keys and turn them into
    NoteOns.

    TODO then there's midi recording:

    In record mode, the block is played while recording midi msgs and their
    timestamps, which are later passed to the integrator to convert into
    events.
-}
module Cmd.GlobalKeymap where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config

import qualified Ui.Block as Block
import qualified Ui.Key as Key

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (bind_key, bind_char, bind_mod, bind_click, bind_drag,
    command, command_char, command_only)
import Cmd.Keymap (SimpleMod(..))

import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Clip as Clip
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Play as Play
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.ViewConfig as ViewConfig

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

cmd_save, cmd_load :: Cmd.CmdT IO ()
cmd_save = Save.get_save_file >>= Save.cmd_save
cmd_load = Save.get_save_file >>= Save.cmd_load

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
    quit_bindings ++ selection_bindings ++ view_config_bindings
    ++ block_config_bindings ++ edit_bindings ++ create_bindings
    ++ clip_bindings

-- Quit is special because it's the only Cmd that returns Cmd.Quit.
-- See how annoying it is to make a keymap by hand?
quit_bindings = [(kspec, cspec) | kspec <- kspecs]
    where
    kspecs = [Keymap.key_spec mods (Keymap.Key (Key.KeyChar '\''))
        | mods <- Keymap.expand_mods [PrimaryCommand]]
    cspec = Keymap.cspec "quit" (const Cmd.cmd_quit)

selection_bindings = concat
    [ bind_drag [] Config.mouse_select "snap drag selection"
        (Selection.cmd_snap_selection Config.mouse_select Config.insert_selnum)
    , bind_drag [PrimaryCommand] Config.mouse_select "free drag selection"
        (Selection.cmd_mouse_selection Config.mouse_select Config.insert_selnum)
    -- TODO extend selection with drag + shift
    -- can I use shift+secondary or something to subtract the selection?

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
    [ command_char '[' "zoom out *0.8"
        (ViewConfig.cmd_zoom_around_insert (*0.8))
    , command_char ']' "zoom in *1.25"
        (ViewConfig.cmd_zoom_around_insert (*1.25))
    ]

block_config_bindings = concat
    [ bind_click [Shift, PrimaryCommand] Config.mouse_select 0
        "toggle skeleton edge" BlockConfig.cmd_toggle_edge
    , bind_mod [Shift] (Key.KeyChar 'm') "toggle mute"
        (BlockConfig.cmd_toggle_flag Block.Mute)
    , bind_mod [Shift] (Key.KeyChar 's') "toggle solo"
        (BlockConfig.cmd_toggle_flag Block.Solo)
    ]

-- delete = remove events and move following events back
-- clear = just remove events
edit_bindings = concat
    [ bind_key Key.Escape "toggle val edit" Edit.cmd_toggle_val_edit
    , bind_mod [PrimaryCommand] Key.Escape "toggle raw edit"
        Edit.cmd_toggle_raw_edit
    , bind_mod [] Key.Tab "toggle method edit" Edit.cmd_toggle_method_edit

    , bind_mod [Shift] Key.Escape "toggle kbd entry mode"
        Edit.cmd_toggle_kbd_entry

    -- Unlike other event editing commands, you don't have to be in insert mode
    -- to remove events.  Maybe I'll change that later.
    , command Key.Backspace "clear selected" Edit.cmd_clear_selected

    , bind_mod [PrimaryCommand] Key.Down "insert time" Edit.cmd_insert_time
    , bind_mod [PrimaryCommand] Key.Up "delete time" Edit.cmd_delete_time

    , command_char 'u' "undo" Edit.undo
    , command_char 'r' "redo" Edit.redo

    -- The convention from MakeRuler is: 0 = block, 1 = block section,
    -- 2 = whole, 3 = quarter, 4 = 16th, etc.  Since it goes to /4 after
    -- rank 2, I use a skip to keep the note divisions binary.
    , command_char '0' "step rank 0+0" (step_rank 0 0) -- block
    , command_char '9' "step rank 1+0" (step_rank 1 0) -- block section
    , command_char '1' "step rank 2+0" (step_rank 2 0) -- whole
    , command_char '2' "step rank 3+1" (step_rank 3 1) -- half
    , command_char '3' "step rank 3+0" (step_rank 3 0) -- 4th
    , command_char '4' "step rank 4+1" (step_rank 4 1) -- 8th
    , command_char '5' "step rank 4+0" (step_rank 4 0) -- 16th
    , command_char '6' "step rank 5+1" (step_rank 5 1) -- 32nd
    , command_char '7' "step_rank 6+0" (step_rank 6 0) -- 64th

    , bind_char '-' "octave -1" (Edit.cmd_modify_octave (+ (-1)))
    , bind_char '=' "octave +1" (Edit.cmd_modify_octave (+1))
    ]
    where
    step_rank rank skips = Edit.cmd_meter_step (TimeStep.MatchRank rank skips)

create_bindings = concat
    [ command_only 't' "append track" Create.insert_track_after_selection
    , command_only 'd' "delete track" Create.remove_selected_tracks
    ]

clip_bindings = concat
    [ command_only 'c' "copy selection" Clip.cmd_copy_selection
    , command_only 'x' "cut selection" Clip.cmd_cut_selection
    , command_only 'v' "paste selection" Clip.cmd_paste_overwrite
    -- Makes as much sense as xcv I suppose.
    , command_only 'b' "merge selection" Clip.cmd_paste_merge
    , bind_mod [Shift, PrimaryCommand] (Key.KeyChar 'v') "insert selection"
        Clip.cmd_paste_insert
    ]
