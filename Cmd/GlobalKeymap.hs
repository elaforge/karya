-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The default keyboard layout.

    I group cmds by their function.  Overlapping keys will be detected and show
    up in the log as a warning on startup.

    Some global cmds also have a command-key version, so you can invoke them
    even when kbd entry is on.

    Track-specific cmds are bound in 'Cmd.Track'.

    Global generic cmds:

    0. State level operations, like load, save, undo.  Set timestep mode, set
    edit mode.  Play, stop.  Move the selection.

    1. Modify text independent attributes, like start position and duration.
    This includes copy and paste, remove, modify duration, nudge, ...

    Track-specific cmds are looked up in "Cmd.Track".

    2. Track specific event operations, possibly affecting all events in the
    selection: transposition, etc.  These are active regardless of EditMode,
    and are found in Cmd.*TrackKeymap.

    3. Track specific event editing, which modifies a single event.
    E.g., input notes and note text, pitches in a certain scale, control
    values, etc.  This is enabled by edit mode, and will shadow printable keys,
    depending on the track type and edit mode.  Found in Cmd.*Track.

    4. Kbd entry, if on, will hijack the letter keys and turn them into
    NoteOns.

    Then there's midi recording:  (TODO which is theoretical since it's not
    implemented)

    In record mode, the block is played while recording midi msgs and their
    timestamps, which are later passed to the integrator to convert into
    events.
-}
module Cmd.GlobalKeymap (
    pure_keymap, io_keymap, all_keymap, all_keymap_errors
) where
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Factor as Factor
import qualified Cmd.Keymap as Keymap
import           Cmd.Keymap
    (SimpleMod(..), bind_click, bind_drag, bind_key, bind_key_status,
     bind_repeatable, command_char, secondary_char, plain_char, plain_key,
     shift_char)
import qualified Cmd.PhysicalKey as PhysicalKey
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Play as Play
import qualified Cmd.RulerCmd as RulerCmd
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Track as Track
import qualified Cmd.Undo as Undo
import qualified Cmd.ViewConfig as ViewConfig
import qualified Cmd.Views as Views

import qualified Derive.Scale as Scale
import qualified Ui.Block as Block
import qualified Ui.Key as Key
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ui as Ui

import           Global


pure_keymap :: Cmd.Handler Cmd.CmdId
pure_keymap = Cmd.Keymap $ fst $ Keymap.make_keymap pure_bindings

io_keymap :: Cmd.Handler (Cmd.CmdT IO)
io_keymap = Cmd.Keymap $ fst $ Keymap.make_keymap io_bindings

-- | This is not useful for execution since the cmds themselves have been
-- stripped of their code, but it's still useful to find keymap collisions and
-- print a global keymap.  They're stripped to make them all the same type, so
-- they can all go into the same Keymap, so collision detection and
-- documentation doesn't have to care about 'pure_keymap' vs 'io_keymap'.
all_keymap :: Cmd.Keymap Cmd.CmdId
all_keymap_errors :: [Text]
(all_keymap, all_keymap_errors) =
    Keymap.make_keymap (pure_bindings ++ map strip io_bindings)
    where
    strip = second $ \(Cmd.NamedCmd name _) ->
        Cmd.NamedCmd name (const (return Cmd.Done))

-- * io cmds

-- | Cmds that use IO.  This should be a limited to the small set of cmds that
-- need it.
io_bindings :: [Keymap.Binding (Cmd.CmdT IO)]
io_bindings = concat
    [ file_bindings, undo_bindings, quit_bindings
    -- This actually belongs in 'play_bindings', but needs to be in IO, because
    -- stopping involves possibly killing threads and stopping audio.
    , bind_key_status [] (Key.Char ' ') "stop or play" stop_or_play
    ]

-- Previously a second space would contract the selection, but it didn't seem
-- that useful.
-- context_stop :: Cmd.CmdT IO ()
-- context_stop = unlessM Play.cmd_context_stop (Selection.to_point True)

stop_or_play :: Cmd.CmdT IO Cmd.Status
stop_or_play = Play.cmd_context_stop >>= \case
    True -> return Cmd.Done
    False -> Cmd.gets (Cmd.state_previous_play . Cmd.state_play) >>= \case
        Nothing -> Cmd.Play <$> Play.root_top
        Just (Cmd.PlayCmd _ cmd) -> Cmd.Play <$> Cmd.lift_id cmd

file_bindings :: [Keymap.Binding (Cmd.CmdT IO)]
file_bindings = concat
    [ command_char 'S' "save" $ Cmd.gets Cmd.state_save_file >>= \case
        Just (_, Cmd.SaveRepo repo) -> do
            -- Even when using git, write a standalone state as a safeguard.
            Save.write_current_state $
                Save.state_path_for_repo (Path.to_path repo)
            Save.save_git_as (Path.to_path repo)
        _ -> Save.save_state
    ]

undo_bindings :: [Keymap.Binding (Cmd.CmdT IO)]
undo_bindings = concat
    [ command_char 'u' "undo" Undo.undo
    , command_char 'r' "redo" Undo.redo
    ]

-- | Quit is special because it's the only Cmd that returns Cmd.Quit.
-- See how annoying it is to make a keymap by hand?
quit_bindings :: [Keymap.Binding (Cmd.CmdT IO)]
quit_bindings = concat
    [ bind_key_status [PrimaryCommand] q "soft quit" Save.soft_quit
    , bind_key_status [Shift, PrimaryCommand] q "hard quit" Save.hard_quit
    ]
    where q = Key.Char (PhysicalKey.physical_key 'q')

-- * pure cmds

-- | Cmds that don't use IO.  Exported from the module for the responder.
pure_bindings :: [Keymap.Binding (Cmd.CmdT Identity.Identity)]
pure_bindings = concat
    [ play_bindings, mouse_bindings, selection_bindings, step_play_bindings
    , view_config_bindings, block_config_bindings, edit_state_bindings
    , event_bindings, pitch_bindings, create_bindings, clip_bindings
    -- TODO use this to catch the focus bug
    , bind_key [SecondaryCommand] (Key.Char 'a') "debug" $
        Cmd.modify $ \st -> st { Cmd.state_debug = not (Cmd.state_debug st) }
    ]

play_bindings :: [Keymap.Binding (Cmd.CmdT Identity.Identity)]
play_bindings = concat
    -- The pattern is that the modifiers select where to start playing, and
    -- the key says whether it's the local block or from the root block.
    [ bind block local "play local block" Play.local_block
    , bind sel local "play or loop local selection" Play.local_selection
    -- See if playing from the top is more useful than the previous step.
    , bind prev local "play local from top of window" Play.local_top
    , bind block root "play root from start" Play.root_block
    , bind sel root "play or loop root from local selection" Play.root_selection
    -- It plays from the selection on the root, instead of the local one.
    -- This breaks the modifier+key pattern, but it's useful to manually set
    -- a play starting point on the root.
    , bind (block ++ sel) root "play root from root selection"
        Play.root_from_root_selection
    , bind prev root "play root from top of window" Play.root_top
    ]
    where
    bind smods key desc cmd = bind_key_status smods key desc $
        Cmd.Play <$> (cmd <* Play.set_previous_play desc cmd)
    block = [PrimaryCommand]
    sel = [Shift]
    prev = []
    local = Key.Enter
    root = Key.Char (PhysicalKey.physical_key '\'')

-- | I bind the mouse by device rather than function, since I can't detect
-- overlaps as easily for mouse bindings.
mouse_bindings :: Cmd.M m => [Keymap.Binding m]
mouse_bindings = concat
    [ bind_drag [] btn Cmd.OnTrack "snap drag selection"
        (Selection.cmd_snap_selection btn False)
    , bind_drag [Shift] btn Cmd.OnTrack "snap drag selection"
        (Selection.cmd_snap_selection btn True)
    , bind_drag [PrimaryCommand] btn Cmd.OnTrack "drag selection"
        (Selection.cmd_mouse_selection btn False)
    , bind_drag [Shift, PrimaryCommand] btn Cmd.OnTrack "extend selection"
        (Selection.cmd_mouse_selection btn True)
    , Keymap.bind_release [] btn Cmd.OnTrack "mouse release"
        (const Selection.record_history)

    , bind_click [] btn Cmd.OnTrack 2 "open block"
        (const (BlockConfig.cmd_open_block False))
    , bind_click [PrimaryCommand] btn Cmd.OnTrack 2 "open block"
        (const (BlockConfig.cmd_open_block True))

    -- TODO without a track_drag equivalent for skeleton clicks, this
    -- will interfere with the OnTrack bind_drag when you drag into the
    -- track.
    , bind_drag [] btn Cmd.OnSkeleton "select track"
        (Selection.cmd_select_track btn)
    , bind_click [] btn Cmd.OnSkeleton 2 "add block title"
        BlockConfig.cmd_add_block_title
    , bind_click [PrimaryCommand] btn Cmd.OnSkeleton 1
        "toggle skeleton edge" BlockConfig.cmd_toggle_edge
    , bind_click [Shift] btn Cmd.OnSkeleton 1 "move tracks"
        BlockConfig.cmd_move_tracks
    , bind_click [SecondaryCommand] btn Cmd.OnSkeleton 1 "toggle mute"
        BlockConfig.cmd_mute_or_unsolo
    , bind_click [SecondaryCommand] btn Cmd.OnSkeleton 2 "toggle solo"
        BlockConfig.cmd_set_solo

    , bind_click [Shift] btn Cmd.OnDivider 1 "expand collapsed"
        BlockConfig.cmd_expand_track
    ]
    where
    btn = Config.mouse_select

selection_bindings :: Cmd.M m => [Keymap.Binding m]
selection_bindings = concat
    [ bind_repeatable [] Key.Down "advance selection" $
        Selection.step TimeStep.Advance move
    , bind_repeatable [Shift] Key.Down "extend advance selection" $
        Selection.step TimeStep.Advance Selection.Extend

    , bind_repeatable [] Key.Up "rewind selection" $
        Selection.step TimeStep.Rewind move
    , bind_repeatable [Shift] Key.Up "extend rewind selection" $
        Selection.step TimeStep.Rewind Selection.Extend

    , bind_repeatable [] Key.Right "shift selection right" $
        Selection.shift True move 1
    , bind_repeatable [Shift] Key.Right "extend selection right" $
        Selection.shift True Selection.Extend 1

    , bind_repeatable [] Key.Left "shift selection left" $
        Selection.shift True move (-1)
    , bind_repeatable [Shift] Key.Left "extend selection left" $
        Selection.shift True Selection.Extend (-1)

    , repeatable_char 'h' "move selection left" $
        Selection.shift True move (-1)
    , repeatable_char 'H' "move selection left" $
        Selection.shift True Selection.Extend (-1)
    , repeatable_char 'l' "move selection right" $
        Selection.shift True move 1
    , repeatable_char 'L' "move selection right" $
        Selection.shift True Selection.Extend 1
    , repeatable_char 'j' "move selection advance" $
        Selection.step TimeStep.Advance move
    , repeatable_char 'J' "move selection advance" $
        Selection.step TimeStep.Advance Selection.Extend
    , repeatable_char 'k' "move selection rewind" $
        Selection.step TimeStep.Rewind move
    , repeatable_char 'K' "move selection rewind" $
        Selection.step TimeStep.Rewind Selection.Extend
    -- Mnemonic: next, previous.
    , repeatable_char 'n' "move to next note track" $
        Selection.jump_to_track move =<< Cmd.abort_unless
            =<< Selection.find_note_track Selection.R False
    , repeatable_char 'p' "move to previous note track" $
        Selection.jump_to_track move =<< Cmd.abort_unless
            =<< Selection.find_note_track Selection.L False
    , plain_char 'N' "expand to next note track" $
        Selection.jump_to_track Selection.Extend =<< Cmd.abort_unless
            =<< Selection.find_note_track Selection.R True
    , plain_char 'P' "expand to previous note track" $
        Selection.jump_to_track Selection.Extend =<< Cmd.abort_unless
            =<< Selection.find_note_track Selection.L True

    , repeatable_char 'w' "move to next event" $
        Selection.step_with 1 move =<< Track.event_and_note_step
    , repeatable_char 'W' "extend to next event" $
        Selection.step_with 1 Selection.Extend =<< Track.event_and_note_step
    , repeatable_char 'b' "move to previous event" $
        Selection.step_with (-1) move =<< Track.event_and_note_step
    , repeatable_char 'B' "extend to previous event" $
        Selection.step_with (-1) Selection.Extend =<< Track.event_and_note_step

    , bind_key [PrimaryCommand] (Key.Char 'a') "select track / all"
        Selection.cmd_track_all
    , bind_key [PrimaryCommand] (Key.Char 'A') "toggle extend tracks"
        Selection.cmd_toggle_extend_tracks

    , bind_key [Shift] (Key.Char ' ') "selection start to point"
        (Selection.to_point False)
    ]
    where
    move = Selection.default_move
    repeatable_char c = bind_repeatable [] (Key.Char c)

step_play_bindings :: Cmd.M m => [Keymap.Binding m]
step_play_bindings = concat
    [ bind_repeatable [PrimaryCommand] Key.Down
        "step play advance" (StepPlay.cmd_set_or_advance False)
    , bind_repeatable [Shift, PrimaryCommand] Key.Down
        "step play tracks advance" (StepPlay.cmd_set_or_advance True)
    , bind_repeatable [PrimaryCommand] Key.Up
        "step play rewind" StepPlay.cmd_rewind
    , bind_repeatable [Shift, PrimaryCommand] Key.Up
        "step play rewind" StepPlay.cmd_rewind
    , bind_key [PrimaryCommand] Key.Right
        "step play here" (StepPlay.cmd_here False)
    , bind_key [Shift, PrimaryCommand] Key.Right
        "step play tracks here" (StepPlay.cmd_here True)
    ]

view_config_bindings :: Cmd.M m => [Keymap.Binding m]
view_config_bindings = concat
    [ plain_char '[' "zoom out step" (ViewConfig.zoom_by_rank TimeStep.Rewind)
    , plain_char ']' "zoom in step" (ViewConfig.zoom_by_rank TimeStep.Advance)
    , plain_char '{' "zoom out *0.8" (ViewConfig.cmd_zoom_around_insert (*0.8))
    , plain_char '}' "zoom in *1.25" (ViewConfig.cmd_zoom_around_insert (*1.25))
    -- undo and redo for selection, but I never use it
    , secondary_char '[' "previous selection"
        (Selection.previous_selection True)
    , secondary_char ']' "next selection" (Selection.next_selection True)

    -- TODO experimental
    , command_char 'q' "set suggested track widths"
        (ViewConfig.set_suggested_track_widths =<< Cmd.get_focused_view)

    , plain_char '\\' "zoom to ruler or selection"
        ViewConfig.zoom_to_ruler_or_selection
    , command_char 'R' "resize to fit"
        (Views.resize_to_fit False =<< Cmd.get_focused_view)
    , command_char 'L' "horizontal tile" ViewConfig.horizontal_tile
    , command_char '\\' "maximize and zoom"
        (Views.maximize_and_zoom =<< Cmd.get_focused_view)

    -- Unfortunately cmd-` is taken by an edit state bind, and they all live
    -- there so it would create an inconsistency to move just one.
    , secondary_char '`' "cycle focus forward" (ViewConfig.cycle_focus True)
    , bind_key [SecondaryCommand, Shift] (Key.Char '`') "cycle focus backward"
        (ViewConfig.cycle_focus False)
    , secondary_char 'H' "block focus left" $
        ViewConfig.move_focus ViewConfig.West
    , secondary_char 'J' "block focus down" $
        ViewConfig.move_focus ViewConfig.South
    , secondary_char 'K' "block focus up" $
        ViewConfig.move_focus ViewConfig.North
    , secondary_char 'L' "block focus right" $
        ViewConfig.move_focus ViewConfig.East
    , secondary_char 'W' "close view" $
        Ui.destroy_view =<< Cmd.get_focused_view

    , secondary_char 'f' "scroll advance" $ ViewConfig.scroll_pages 0.75
    , secondary_char 'b' "scroll rewind" $ ViewConfig.scroll_pages (-0.75)
    , bind_key [] Key.Home "scroll home" ViewConfig.scroll_to_home
    , bind_key [] Key.End "scroll end" ViewConfig.scroll_to_end
    , bind_key [] Key.PageDown "scroll advance" $ ViewConfig.scroll_pages 0.75
    , bind_key [] Key.PageUp "scroll rewind" $ ViewConfig.scroll_pages (-0.75)
    ]

block_config_bindings :: Cmd.M m => [Keymap.Binding m]
block_config_bindings = concat
    [ plain_char 'M' "toggle mute" (BlockConfig.cmd_toggle_flag Block.Mute)
    , plain_char 'S' "toggle solo" (BlockConfig.cmd_toggle_flag Block.Solo)
    , plain_char 'D' "toggle disable"
        (BlockConfig.cmd_toggle_flag Block.Disable)
    , plain_char 'C' "toggle collapse"
        (BlockConfig.cmd_toggle_flag Block.Collapse)
    , command_char 'C' "collapse empty"
        (BlockConfig.toggle_collapse_empty =<< Cmd.get_focused_block)
    , command_char 'M' "toggle merge all"
        (BlockConfig.toggle_merge_all =<< Cmd.get_focused_block)
    , command_char 'm' "toggle merged" BlockConfig.toggle_merge_selected

    -- The idea is cmd-; works in kbd mode.
    , command_char ';' "add ruler section" RulerCmd.local_add_section
    , plain_char ':' "clip block" BlockConfig.clip
    ]

-- | Modify global edit state.
edit_state_bindings :: Cmd.M m => [Keymap.Binding m]
edit_state_bindings = concat
    [ plain_key Key.Escape "toggle val edit" Edit.cmd_toggle_val_edit
    , command_char '[' "toggle val edit" Edit.cmd_toggle_val_edit
    , bind_key [] Key.Tab "toggle method edit" Edit.cmd_toggle_method_edit
    , command_char ']' "toggle kbd entry mode" Edit.cmd_toggle_kbd_entry
    , command_char '{' "toogle val edit and kbd entry"
        Edit.cmd_toggle_val_edit_kbd_entry

    , command_char '0' "set event step" $ Edit.set_step TimeStep.event_step
    , uncurry (command_char '1') (step_rank Meter.Section)
    , uncurry (command_char '2') (step_rank Meter.W)
    , uncurry (command_char '3') (step_rank Meter.H)
    , uncurry (command_char '4') (step_rank Meter.Q)
    , uncurry (command_char '5') (step_rank Meter.E)
    , uncurry (command_char '6') (step_rank Meter.S)
    , uncurry (command_char '7') (step_rank Meter.T32)
    , uncurry (command_char '8') (step_rank Meter.T64)
    , shift_char '=' "toggle duration" Edit.toggle_note_duration

    , bind_key [PrimaryCommand] (Key.Char '`') "toggle absolute/relative step"
        Edit.toggle_absolute_relative_step
    , bind_key [PrimaryCommand, Shift] (Key.Char '`')
        "toggle input note orientation"
        (Edit.cmd_toggle_note_orientation *> Selection.update_orientation)
    , plain_char '`' "toggle advance" Edit.toggle_advance
    , shift_char '`' "toggle chord" Edit.toggle_chord

    , plain_char '-' "octave -1" (Edit.cmd_modify_octave (subtract 1))
    , plain_char '=' "octave +1" (Edit.cmd_modify_octave (+1))
    , command_char '-' "play speed / 9/8"
        (Play.modify_play_multiplier (/ (9/8)))
    , command_char '=' "play speed * 9/8"
        (Play.modify_play_multiplier (* (9/8)))
    ]
    where
    step_rank rank =
        ( "set step: " <> TimeStep.show_time_step step
        , Edit.set_step_rank step rank
        )
        where step = TimeStep.rank rank

-- delete = remove events and move following events back
-- clear = just remove events

-- | Modify events.
event_bindings :: Cmd.M m => [Keymap.Binding m]
event_bindings = concat
    -- J = move previous event to cursor, K = move next event to cursor.
    [ command_char 'J' "move event forward" Edit.cmd_move_event_forward
    , command_char 'K' "move event backward" Edit.cmd_move_event_backward
    , command_char 'j' "insert time" Edit.cmd_insert_time
    , command_char 'k' "delete time" Edit.cmd_delete_time
    -- Unlike other event editing commands, you don't have to be in insert
    -- mode to remove events.  Maybe I'll change that later.
    , plain_key Key.Backspace "clear selected" Edit.cmd_clear_and_advance
    , command_char 'o' "join events" Edit.cmd_join_events
    , command_char 'O' "split events" Edit.cmd_split_events

    -- This used to be cmd-s but 'a' is unmapped in kbd entry and I often want
    -- to adjust note durations.
    , plain_char 'a' "set dur" Edit.cmd_set_duration
    , command_char 'z' "set call duration" Edit.cmd_set_call_duration
    , command_char 'Z' "toggle zero-dur" Edit.cmd_toggle_zero_timestep
    , command_char 'g' "set start" Edit.cmd_set_start
    , command_char 'G' "invert orientation" Edit.cmd_invert_orientation

    , bind_repeatable [] (Key.Char '.') "run last action"
        (Edit.run_action_at '.')
    , shift_char '1' "run action 1" (Edit.run_action_at '1')
    , shift_char '2' "run action 2" (Edit.run_action_at '2')
    , shift_char '3' "run action 3" (Edit.run_action_at '3')
    , shift_char '4' "run action 4" (Edit.run_action_at '4')
    , shift_char '5' "cycle enharmonic"
        (PitchTrack.pitches PitchTrack.cycle_enharmonics)
    , shift_command '1' "record in slot 1" (Edit.save_last_action_to '1')
    , shift_command '2' "record in slot 2" (Edit.save_last_action_to '2')
    , shift_command '3' "record in slot 3" (Edit.save_last_action_to '3')
    , shift_command '4' "record in slot 4" (Edit.save_last_action_to '4')

    -- modify event text

    -- There are placed to avoid conflict with 'Cmd.state_kbd_entry' for either
    -- pitched instruments or specialized keymaps like drums.
    , bind_key_status [] (Key.Char (PhysicalKey.physical_key '/'))
        "append text" Edit.append_text
    , bind_key_status [] (Key.Char (PhysicalKey.physical_key '?'))
        "replace last call" Edit.replace_last_call
    , bind_key_status [] (Key.Char 'i') "prepend text" Edit.prepend_text
    , bind_key_status [] (Key.Char 'I') "replace first call"
        Edit.replace_first_call

    , bind_key [SecondaryCommand] (Key.Char 'c')
        "toggle commented" Edit.cmd_toggle_commented
    , command_char ',' "strip transformer" Edit.strip_transformer
    ]
    where
    shift_command = bind_key [Shift, PrimaryCommand] . Key.Char

-- | Bindings which work on pitch tracks.  The reason this is global rather
-- than in pitch track keymaps is that it's handy to select multiple tracks
-- and have the cmd automatically skip non pitch tracks.  This only really
-- applies to pitch tracks though, because they are often collapsed and edited
-- from their note track.
pitch_bindings :: Cmd.M m => [Keymap.Binding m]
pitch_bindings = concat
    -- These are named after the vi commands for up and down, but they don't
    -- feel right.
    [ command_char 'y' "transpose up chromatic degree" $
        PitchTrack.transpose_selection Scale.Chromatic 0 1
    , command_char 'e' "transpose down chromatic degree" $
        PitchTrack.transpose_selection Scale.Chromatic 0 (-1)
    , bind_key [SecondaryCommand] (Key.Char 'y')
        "transpose up diatonic degree" $
        PitchTrack.transpose_selection Scale.Diatonic 0 1
    , bind_key [SecondaryCommand] (Key.Char 'e')
        "transpose down diatonic degree" $
        PitchTrack.transpose_selection Scale.Diatonic 0 (-1)
    , command_char 'Y' "transpose up octave" $
        PitchTrack.transpose_selection Scale.Chromatic 1 0
    , command_char 'E' "transpose down octave" $
        PitchTrack.transpose_selection Scale.Chromatic (-1) 0
    ]

create_bindings :: Cmd.M m => [Keymap.Binding m]
create_bindings = concat
    [ command_char 'n' "insert track right" (void Create.insert_track_right)
    , command_char 't' "splice track below" (void Create.splice_below)
    , command_char 'T' "insert branch" Create.insert_branch
    , command_char 'h' "splice track above" (void Create.splice_above)
    , command_char 'H' "splice track above ancestors"
        (void Create.splice_above_ancestors)
    , command_char 'd' "delete tracks" Create.destroy_selected_tracks

    , command_char 'N' "create view"
        (void $ Create.view =<< Cmd.get_focused_block)
    , command_char 'b' "create block"
        (void $ Create.view =<< Create.block =<< Ui.block_ruler
            =<< Cmd.get_focused_block)
    , command_char 'B' "create block from template or selection"
        Factor.block_from_template
    ]

clip_bindings :: Cmd.M m => [Keymap.Binding m]
clip_bindings = concat
    [ command_char 'c' "copy selection" Clip.cmd_copy_selection
    , command_char 'x' "cut selection" Clip.cmd_cut_selection
    , command_char 'v' "paste selection" Clip.cmd_paste_overwrite
    , command_char 'V' "insert selection" Clip.cmd_paste_insert
    , command_char 'i' "merge selection" Clip.cmd_paste_merge
    ]
