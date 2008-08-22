{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and controller or note track
    commands, go in their own modules.
-}
module Cmd.Edit where
import Control.Monad
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Selection as Selection

import qualified App.Config as Config


-- | Change the edit mode, changing the color of the edit_box as a reminder.
cmd_toggle_kbd_entry :: Cmd.CmdId
cmd_toggle_kbd_entry = modify_edit_mode $ \m -> case m of
    Cmd.EditKbd -> Cmd.NoEdit
    _ -> Cmd.EditKbd

cmd_toggle_midi_entry :: Cmd.CmdId
cmd_toggle_midi_entry = modify_edit_mode $ \m -> case m of
    Cmd.EditMidi -> Cmd.NoEdit
    _ -> Cmd.EditMidi

modify_edit_mode f = do
    Cmd.modify_state $ \st ->
        st { Cmd.state_edit_mode = f (Cmd.state_edit_mode st) }
    sync_edit_box_status
    return Cmd.Done

sync_edit_box_status :: (Monad m) => Cmd.CmdT m ()
sync_edit_box_status = do
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    block_ids <- fmap (Map.keys . State.state_blocks) State.get
    mapM_ (flip State.set_edit_box (edit_color edit_mode)) block_ids

edit_color Cmd.NoEdit = Config.box_color
edit_color Cmd.EditMidi = Config.midi_entry_color
edit_color Cmd.EditKbd = Config.kbd_entry_color

-- * universal event cmds

-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note.
-- TODO control tracks that are covered by the selection should be skipped
cmd_set_duration :: (Monad m) => Cmd.CmdT m ()
cmd_set_duration = modify_events $ \_start end (pos, event) ->
    (pos, event { Event.event_duration = end - pos })

cmd_modify_dur :: (Monad m) => (TrackPos -> TrackPos) -> Cmd.CmdT m ()
cmd_modify_dur f = modify_events $ \_ _ (pos, evt) ->
    (pos, evt { Event.event_duration = f (Event.event_duration evt) })

-- | Modify previous event if the selection is a point, and all events under
-- the selection if it's a range.
modify_events :: (Monad m) =>
    (TrackPos -> TrackPos -> Track.PosEvent -> Track.PosEvent) -> Cmd.CmdT m ()
modify_events f = do
    (start, end, track_events) <-
        Selection.selected_events True Config.insert_selnum
    forM_ track_events $ \(track_id, pos_events) -> do
        let pos_events2 = map (f start end) pos_events
        if start == end then State.remove_event track_id start
            else State.remove_events track_id start end
        State.insert_events track_id pos_events2

-- | If the insertion selection is a point, remove any event under it.  If it's
-- a range, remove all events within its half-open extent.
cmd_remove_selected :: (Monad m) => Cmd.CmdT m Cmd.Status
cmd_remove_selected = do
    (track_ids, sel) <- Selection.selected_tracks Config.insert_selnum
    let (start, end) = Block.sel_range sel
    forM_ track_ids $ \track_id -> if start == end
        then State.remove_event track_id start
        else State.remove_events track_id start end
    return Cmd.Done

cmd_meter_step :: Int -> Cmd.CmdId
cmd_meter_step rank = do
    let step = (TimeStep.UntilMark
            (TimeStep.NamedMarklists ["meter"]) (TimeStep.MatchRank rank))
    Cmd.modify_state $ \st -> st { Cmd.state_step = step }
    sync_step_status
    return Cmd.Done

sync_step_status :: (Monad m) => Cmd.CmdT m ()
sync_step_status = do
    step <- fmap Cmd.state_step Cmd.get_state
    Cmd.set_status status_step (Just (show_step step))

show_step (TimeStep.Absolute pos) = "abs:" ++ show pos
show_step (TimeStep.UntilMark mlists match) =
    "until:" ++ show_marklists mlists ++ "/" ++ show_match match
show_step (TimeStep.MarkDistance mlists match) =
    "dist:" ++ show_marklists mlists ++ "/" ++ show_match match

show_match (TimeStep.MatchRank rank) = "r" ++ show rank

show_marklists TimeStep.AllMarklists = "all"
show_marklists (TimeStep.NamedMarklists mlists) = Seq.join "," mlists

cmd_modify_octave :: (Monad m) => (Int -> Int) -> Cmd.CmdM m
cmd_modify_octave f = do
    Cmd.modify_state $ \st -> st
        { Cmd.state_kbd_entry_octave = f (Cmd.state_kbd_entry_octave st) }
    sync_octave_status
    return Cmd.Done

sync_octave_status :: (Monad m) => Cmd.CmdT m ()
sync_octave_status = do
    octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    Cmd.set_status status_octave (Just (show octave))

status_octave = "8ve"
status_step = "step"
