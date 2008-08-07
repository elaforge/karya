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


-- | Turn edit mode on and off, changing the color of the edit_box as
-- a reminder.
cmd_toggle_edit :: Cmd.CmdId
cmd_toggle_edit = do
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    Cmd.modify_state $ \st -> st { Cmd.state_edit_mode = not edit_mode }
    sync_edit_box_status
    return Cmd.Done

sync_edit_box_status :: (Monad m) => Cmd.CmdT m ()
sync_edit_box_status = do
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    block_ids <- fmap (Map.keys . State.state_blocks) State.get
    mapM_ (flip State.set_edit_box (edit_color edit_mode)) block_ids

edit_color True = Config.edit_color
edit_color False = Config.box_color

-- * universal event cmds

-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note.  Of course, this should only be in scope in
-- note tracks.
-- TODO make sure that's true
cmd_extend_events :: (Monad m) => Cmd.CmdT m ()
cmd_extend_events = do
    (track_ids, sel) <- Selection.selected_tracks Config.insert_selnum
    let (start, end) = Block.sel_range sel
    forM_ track_ids $ \track_id ->
        State.modify_event_range track_id (trans_legato end) start end

trans_legato :: TrackPos -> State.EventTransformer
trans_legato sel_end _prev next (pos, event) = [(pos, extended)]
    where
    dur = case next of
        ((next_pos, _):_) -> (min sel_end next_pos) - pos
        [] -> Event.event_duration event
    extended = event { Event.event_duration = dur }

-- | An event overlapping the selection's point will be clipped to end there.
cmd_clip_event :: (Monad m) => Cmd.CmdT m ()
cmd_clip_event = do
    (pos, _, track_id) <- Selection.get_insert_pos
    track <- State.get_track track_id
    (event_pos, event) <- Cmd.require $
        Track.event_overlapping (Track.track_events track) pos
    let max_dur = pos - event_pos
    let clipped = event
            { Event.event_duration = min max_dur (Event.event_duration event) }
    State.insert_events track_id [(event_pos, clipped)]

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
