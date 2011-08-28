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
    E.g., input notes and note text, pitches in a certain scale, control
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

import Util.Control
import qualified Ui.Block as Block
import qualified Ui.Key as Key
import qualified Ui.State as State

import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap
       (bind_key, bind_char, bind_mod, bind_repeatable, bind_click,
        bind_drag, command, command_char, command_only, SimpleMod(..))
import qualified Cmd.Msg as Msg
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Play as Play
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Undo as Undo
import qualified Cmd.ViewConfig as ViewConfig

import qualified Perform.Transport as Transport
import qualified App.Config as Config


pure_cmds :: [Cmd.Cmd]
pure_cmds = [Keymap.make_cmd (fst (Keymap.make_cmd_map pure_bindings))]

io_cmds :: Transport.Info -> [Msg.Msg -> Cmd.CmdIO]
io_cmds transport_info =
    [ Keymap.make_cmd (fst (Keymap.make_cmd_map io_bindings))
    -- player_bindings wants an arg, but I want to run 'make_cmd_map' as a CAF
    -- so I can easily print the errors once on startup, and also be assured
    -- that the map is only built once.  Otherwise I think I have to thread
    -- io_cmds through the responder state which is a bit annoying.
    , Keymap.make_cmd $ fst $ Keymap.make_cmd_map $
        player_bindings transport_info
    ]

-- | 'all_cmd_map' is not useful for actual cmds since the cmds themselves
-- have been stripped, but it's still useful to find keymap collisions and
-- print a global keymap.
all_cmd_map :: Keymap.CmdMap (Cmd.CmdT Identity.Identity)
cmd_map_errors  :: [String]
(all_cmd_map, cmd_map_errors) =
    -- Pure cmds bind before IO cmds since they are extendable.
    Keymap.make_cmd_map (pure_bindings ++ map strip io_bindings)
    where
    strip = second $ \(Keymap.CmdSpec name _) ->
        Keymap.CmdSpec name (const (return Cmd.Done))

-- * io cmds

io_bindings :: [Keymap.Binding (Cmd.CmdT IO)]
io_bindings = concat [file_bindings, quit_bindings]

file_bindings :: [Keymap.Binding (Cmd.CmdT IO)]
file_bindings = concat
    [ command_only 'S' "save" (Save.cmd_save =<< Save.get_save_file)
    , command_only 'L' "load" (Save.cmd_load =<< Save.get_save_file)
    ]

-- | This is unfortunate.  In order to construct the cmd map only once, I want
-- it to be a CAF.  However, these Cmds take an argument, which means I need to
-- either have the CmdMap map to Cmds that take an argument, or recreate the
-- map on each call.  Since there are not many cmds, I opt for the latter.
player_bindings :: Transport.Info -> [Keymap.Binding (Cmd.CmdT IO)]
player_bindings transport_info = concat
    [ bind_key Key.Enter "play block" (Play.cmd_play_focused transport_info)
    , bind_mod [Shift] Key.Enter "play from insert"
        (Play.cmd_play_from_insert transport_info)
    , bind_mod [PrimaryCommand] Key.Enter "play from previous step"
        (Play.cmd_play_from_previous_step transport_info)
    , bind_mod [Shift, PrimaryCommand] Key.Enter "play from previous root step"
        (Play.cmd_play_from_previous_root_step transport_info)
    , bind_char ' ' "stop" Play.cmd_context_stop
    ]

-- | Quit is special because it's the only Cmd that returns Cmd.Quit.
-- See how annoying it is to make a keymap by hand?
quit_bindings :: [Keymap.Binding (Cmd.CmdT IO)]
quit_bindings = [(kspec, cspec) | kspec <- kspecs]
    where
    kspecs = [Keymap.key_spec mods key
        | mods <- Keymap.expand_mods key [PrimaryCommand]]
    key = Keymap.Key False (Key.Char '\'')
    cspec = Keymap.cspec "quit" $ const (Play.cmd_stop >> return Cmd.Quit)

-- * pure cmds

pure_bindings :: [Keymap.Binding (Cmd.CmdT Identity.Identity)]
pure_bindings = concat
    [ mouse_bindings, selection_bindings, step_play_bindings
    , view_config_bindings, block_config_bindings, edit_state_bindings
    , event_bindings, pitch_bindings, create_bindings, clip_bindings
    ]

-- | I bind the mouse by device rather than function, since I can't detect
-- overlaps as easily for mouse bindings.
mouse_bindings :: (Cmd.M m) => [Keymap.Binding m]
mouse_bindings = concat
    [ bind_drag [] btn Keymap.OnTrack "snap drag selection"
        (Selection.cmd_snap_selection btn Config.insert_selnum False)
    , bind_drag [Shift] btn Keymap.OnTrack "snap drag selection"
        (Selection.cmd_snap_selection btn Config.insert_selnum True)
    , bind_drag [PrimaryCommand] btn Keymap.OnTrack "drag selection"
        (Selection.cmd_mouse_selection btn Config.insert_selnum False)
    , bind_drag [Shift, PrimaryCommand] btn Keymap.OnTrack "extend selection"
        (Selection.cmd_mouse_selection btn Config.insert_selnum True)

    , bind_click [] btn Keymap.OnTrack 2 "open block"
        (const BlockConfig.cmd_open_block)

    -- TODO without a track_drag equivalent for skeleton clicks, this
    -- will interfere with the OnTrack bind_drag when you drag into the
    -- track.
    , bind_drag [] btn Keymap.OnSkeleton "select track"
        (Selection.cmd_select_track btn Config.insert_selnum)
    , bind_click [PrimaryCommand] btn Keymap.OnSkeleton 1
        "toggle skeleton edge" BlockConfig.cmd_toggle_edge
    , bind_click [Shift] btn Keymap.OnSkeleton 1 "move tracks"
        BlockConfig.cmd_move_tracks

    , bind_click [Shift] btn Keymap.OnDivider 1 "expand collapsed"
        BlockConfig.cmd_expand_track
    ]
    where
    btn = Config.mouse_select

selection_bindings :: (Cmd.M m) => [Keymap.Binding m]
selection_bindings = concat
    [ bind_repeatable [] Key.Down "advance selection"
        (Selection.cmd_step_selection selnum TimeStep.Advance False)
    , bind_repeatable [Shift] Key.Down "extend advance selection"
        (Selection.cmd_step_selection selnum TimeStep.Advance True)

    , bind_repeatable [] Key.Up "rewind selection"
        (Selection.cmd_step_selection selnum TimeStep.Rewind False)
    , bind_repeatable [Shift] Key.Up "extend rewind selection"
        (Selection.cmd_step_selection selnum TimeStep.Rewind True)

    , bind_repeatable [] Key.Right "shift selection right"
        (Selection.cmd_shift_selection selnum 1 False)
    , bind_repeatable [Shift] Key.Right "extend shift selection right"
        (Selection.cmd_shift_selection selnum 1 True)

    , bind_repeatable [] Key.Left "shift selection left"
        (Selection.cmd_shift_selection selnum (-1) False)
    , bind_repeatable [Shift] Key.Left "extend shift selection left"
        (Selection.cmd_shift_selection selnum (-1) True)

    , bind_mod [PrimaryCommand] (Key.Char 'a') "select track / all"
        (Selection.cmd_track_all selnum)
    ]
    where selnum = Config.insert_selnum

step_play_bindings :: (Cmd.M m) => [Keymap.Binding m]
step_play_bindings = concat
    [ bind_repeatable [PrimaryCommand] Key.Down
        "step play advance" (StepPlay.cmd_set_or_advance False)
    , bind_repeatable [Shift, PrimaryCommand] Key.Down
        "step play tracks advance" (StepPlay.cmd_set_or_advance True)
    , bind_repeatable [PrimaryCommand] Key.Up
        "step play rewind" StepPlay.cmd_rewind
    , bind_repeatable [Shift, PrimaryCommand] Key.Up
        "step play rewind" StepPlay.cmd_rewind
    , bind_mod [PrimaryCommand] Key.Right
        "step play here" (StepPlay.cmd_here False)
    , bind_mod [Shift, PrimaryCommand] Key.Right
        "step play tracks here" (StepPlay.cmd_here True)
    ]

view_config_bindings :: (Cmd.M m) => [Keymap.Binding m]
view_config_bindings = concat
    [ command_char '[' "zoom out *0.8"
        (ViewConfig.cmd_zoom_around_insert (*0.8))
    , command_char ']' "zoom in *1.25"
        (ViewConfig.cmd_zoom_around_insert (*1.25))
    , command_char '\\' "zoom to fit"
        (ViewConfig.zoom_to_fit =<< Cmd.get_focused_view)
    , command_char  'R' "resize to fit"
        (ViewConfig.resize_to_fit =<< Cmd.get_focused_view)
    ]

block_config_bindings :: (Cmd.M m) => [Keymap.Binding m]
block_config_bindings = concat
    [ bind_char 'M' "toggle mute" (BlockConfig.cmd_toggle_flag Block.Mute)
    , bind_char 'S' "toggle solo" (BlockConfig.cmd_toggle_flag Block.Solo)
    , command_only 'C' "toggle collapse"
        (BlockConfig.cmd_toggle_flag Block.Collapse)
    , command_only 'M' "toggle merge all"
        (BlockConfig.toggle_merge_all =<< Cmd.get_focused_block)
    ]

-- | Global bindings for edit type things.
edit_state_bindings :: (Cmd.M m) => [Keymap.Binding m]
edit_state_bindings = concat
    [ bind_key Key.Escape "toggle val edit" Edit.cmd_toggle_val_edit
    , bind_mod [PrimaryCommand] Key.Escape "toggle raw edit"
        Edit.cmd_toggle_raw_edit
    , bind_mod [] Key.Tab "toggle method edit" Edit.cmd_toggle_method_edit

    , bind_mod [Shift] Key.Escape "toggle kbd entry mode"
        Edit.cmd_toggle_kbd_entry

    , command_char 'u' "undo" Undo.undo
    , command_char 'r' "redo" Undo.redo

    -- The convention from MakeRuler is: 0 = block, 1 = block section,
    -- 2 = whole, 3 = quarter, 4 = 16th, etc.  Since it goes to /4 after
    -- rank 2, I use a skip to keep the note divisions binary.
    , command_char '0' "step rank 0+0" (step_rank 0 0) -- block
    , command_char '1' "step rank 1+0" (step_rank 1 0) -- block section
    , command_char '2' "step rank 2+0" (step_rank 2 0) -- whole
    , command_char '3' "step rank 3+1" (step_rank 3 1) -- half
    , command_char '4' "step rank 3+0" (step_rank 3 0) -- 4th
    , command_char '5' "step rank 4+1" (step_rank 4 1) -- 8th
    , command_char '6' "step rank 4+0" (step_rank 4 0) -- 16th
    , command_char '7' "step rank 5+1" (step_rank 5 1) -- 32nd
    , command_char '8' "step_rank 6+0" (step_rank 6 0) -- 64th
    , command_char '9' "step_rank 7+0" (step_rank 7 0) -- 256th
    , command_only '`' "toggle step mode" Edit.toggle_mark_step

    , command_only '~' "invert step" Edit.cmd_invert_step_direction

    , bind_char '-' "octave -1" (Edit.cmd_modify_octave (subtract 1))
    , bind_char '=' "octave +1" (Edit.cmd_modify_octave (+1))
    ]
    where
    meter = TimeStep.NamedMarklists ["meter"]
    step_rank rank skips = Edit.set_step_rank
        (TimeStep.time_step skips (TimeStep.AbsoluteMark meter rank))
        rank skips

-- delete = remove events and move following events back
-- clear = just remove events

event_bindings :: (Cmd.M m) => [Keymap.Binding m]
event_bindings = concat
    -- J = move previous event down, K = move next event up.
    [ command_char 'J' "move event forward" Edit.cmd_move_event_forward
    , command_char 'K' "move event back" Edit.cmd_move_event_back
    , command_char 'j' "insert time" Edit.cmd_insert_time
    , command_char 'k' "delete time" Edit.cmd_delete_time
    -- Unlike other event editing commands, you don't have to be in insert
    -- mode to remove events.  Maybe I'll change that later.
    , command Key.Backspace "clear selected" Edit.cmd_clear_selected
    , command_char 'o' "join events" Edit.cmd_join_events

    , bind_char 's' "set dur" Edit.cmd_set_duration
    , bind_char 'z' "toggle zero-dur" Edit.cmd_toggle_zero_duration
    , bind_char 'b' "set beginning" Edit.cmd_set_beginning

    , bind_char '_' "insert track end" Edit.cmd_insert_track_end
    , bind_char '!' "insert recent 1" (Edit.cmd_insert_recent 1)
    , bind_char '@' "insert recent 2" (Edit.cmd_insert_recent 2)
    , bind_char '#' "insert recent 3" (Edit.cmd_insert_recent 3)
    , bind_char '$' "insert recent 4" (Edit.cmd_insert_recent 4)
    ]

-- | Bindings which work on pitch tracks.  The reason this is global rather
-- than in pitch track keymaps is that it's handy to select multiple tracks
-- and have the cmd automatically skip non pitch tracks.
pitch_bindings :: (Cmd.M m) => [Keymap.Binding m]
pitch_bindings = concat
    -- These are named after the vi commands for up and down, but they don't
    -- feel right.
    [ command_char 'y' "transpose up degree"
        (PitchTrack.transpose_selection 0 1)
    , command_char 'e' "transpose down degree"
        (PitchTrack.transpose_selection 0 (-1))
    , command_char 'Y' "transpose up octave"
        (PitchTrack.transpose_selection 1 0)
    , command_char 'E' "transpose down octave"
        (PitchTrack.transpose_selection (-1) 0)
    ]

create_bindings :: (Cmd.M m) => [Keymap.Binding m]
create_bindings = concat
    [ command_only 't' "insert track" Create.insert_track
    , command_only 'T' "splice track below" Create.splice_below
    , command_only 'H' "splice track above" Create.splice_above
    , command_only 'd' "delete tracks" Create.destroy_selected_tracks

    , command_only 'n' "create view"
        (Create.fitted_view =<< Cmd.get_focused_block)
    -- For the moment, never destroy blocks when closing the view.
    , command_only 'w' "destroy view"
        (State.destroy_view =<< Cmd.get_focused_view)
    , command_only 'W' "destroy block"
        (Create.destroy_block =<< Cmd.get_focused_block)
    , command_only 'b' "create block" (Create.block_from_template False)
    , command_only 'B' "create block template"
        (Create.block_from_template True)
    ]

clip_bindings :: (Cmd.M m) => [Keymap.Binding m]
clip_bindings = concat
    [ command_only 'c' "copy selection" Clip.cmd_copy_selection
    , command_only 'x' "cut selection" Clip.cmd_cut_selection
    , command_only 'v' "paste selection" Clip.cmd_paste_overwrite
    , command_only 'V' "insert selection" Clip.cmd_paste_insert
    , command_only 'i' "merge selection" Clip.cmd_paste_merge
    ]
