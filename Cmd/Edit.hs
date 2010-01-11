{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and control or note track
    commands, go in their own modules.
-}
module Cmd.Edit where
import Control.Monad
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Pitch as Pitch

import qualified App.Config as Config


cmd_toggle_raw_edit, cmd_toggle_val_edit,
    cmd_toggle_method_edit, cmd_toggle_kbd_entry :: (Monad m) => Cmd.CmdT m ()

cmd_toggle_raw_edit = modify_edit_mode $ \m -> case m of
    Cmd.RawEdit -> Cmd.NoEdit
    _ -> Cmd.RawEdit

-- | Unlike the other toggle commands, val edit, being the \"default\" toggle,
-- always turns other modes off.  So you can't switch directly from some other
-- kind of edit to val edit.
cmd_toggle_val_edit = modify_edit_mode $ \m -> case m of
    Cmd.NoEdit -> Cmd.ValEdit
    _ -> Cmd.NoEdit

cmd_toggle_method_edit = modify_edit_mode $ \m -> case m of
    Cmd.MethodEdit -> Cmd.ValEdit
    Cmd.ValEdit -> Cmd.MethodEdit
    _ -> m

-- | Turn on kbd entry mode, putting a K in the edit box as a reminder.  This
-- is orthogonal to the previous edit modes.
cmd_toggle_kbd_entry = do
    Cmd.modify_state $ \st ->
        st { Cmd.state_kbd_entry = not (Cmd.state_kbd_entry st) }
    sync_edit_box_status

-- ** util

modify_edit_mode :: (Monad m) => (Cmd.EditMode -> Cmd.EditMode) -> Cmd.CmdT m ()
modify_edit_mode f = do
    Cmd.modify_state $ \st ->
        st { Cmd.state_edit_mode = f (Cmd.state_edit_mode st) }
    sync_edit_box_status

sync_edit_box_status :: (Monad m) => Cmd.CmdT m ()
sync_edit_box_status = do
    edit_mode <- Cmd.gets Cmd.state_edit_mode
    kbd_entry <- Cmd.gets Cmd.state_kbd_entry
    let c = if kbd_entry then 'K' else ' '
    Cmd.set_edit_box (edit_color edit_mode) c

edit_color :: Cmd.EditMode -> Color
edit_color mode = case mode of
    Cmd.NoEdit -> Config.box_color
    Cmd.RawEdit -> Config.raw_edit_color
    Cmd.ValEdit -> Config.val_edit_color
    Cmd.MethodEdit -> Config.method_edit_color

-- * universal event cmds

-- | Insert an event at the current insert pos.
insert_event :: (Monad m) => String -> TrackPos -> Cmd.CmdT m ()
insert_event text dur = do
    (_, _, track_id, pos) <- Selection.get_insert
    State.insert_events track_id [(pos, Event.event text dur)]

-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note, whichever is shorter.
--
-- To make it easy to create legato, if a point selection matches an event,
-- modify the previous event instead of setting the current one to 0 duration.
cmd_set_duration :: (Monad m) => Cmd.CmdT m ()
cmd_set_duration = do
    (_, sel) <- Selection.get
    ModifyEvents.modify_events $ \pos event ->
        Event.set_duration (snd (Types.sel_range sel) - pos) event

-- | Modify event durations by applying a function to them.  0 durations
-- are passed through, so you can't accidentally give control events duration.
cmd_modify_dur :: (Monad m) => (TrackPos -> TrackPos) -> Cmd.CmdT m ()
cmd_modify_dur f = ModifyEvents.modify_events $ \_ evt ->
    Event.set_duration (apply (Event.event_duration evt)) evt
    where apply dur = if dur == TrackPos 0 then dur else f dur

-- | If there is a following event, delete it and extend this one to its end.
cmd_join_events :: (Monad m) => Cmd.CmdT m ()
cmd_join_events = mapM_ process =<< Selection.events_around
    where
        -- If I only selected one, join with the next.  Otherwise, join
        -- selected events.
    process (track_id, _, (_, [evt1], (evt2:_))) = join track_id evt1 evt2
    process (track_id, _, (_, evts@(_ : _ : _), _)) =
        join track_id (head evts) (last evts)
    process _ = return ()
    join track_id (pos1, evt1) (pos2, evt2) =
        -- Yes, this deletes any "backwards" events in the middle, but that
        -- should be ok.
        case (Event.is_negative evt1, Event.is_negative evt2) of
            (False, False) -> do
                let end = Track.event_end (pos2, evt2)
                State.remove_events track_id pos1 end
                State.insert_event track_id pos1
                    (Event.set_duration (end - pos1) evt1)
            (True, True) -> do
                let start = Track.event_end (pos1, evt1)
                State.remove_events track_id start pos2
                State.insert_event track_id pos2
                    (Event.set_duration (start - pos2) evt2)
            _ -> return () -- no sensible way to join these

-- | Insert empty space at the beginning of the selection for the length of
-- the selection, pushing subsequent events forwards.  If the selection is
-- a point, insert one timestep.
cmd_insert_time :: (Monad m) => Cmd.CmdT m ()
cmd_insert_time = do
    (tracknums, track_ids, start, end) <- Selection.merged_tracks
    (start, end) <- expand_range tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        track <- State.get_track track_id
        case Track.split_at_before start (Track.track_events track) of
            (_, []) -> return ()
            (_, evts@((pos, _):_)) -> do
                track_end <- State.track_end track_id
                State.remove_events track_id (min pos start) track_end
                State.insert_sorted_events track_id
                    (map (stretch start end) evts)

-- TODO both stretch and shrink could be faster by just mapping the shift
-- once the overlapping events are done, but it's probably not worth it.
stretch :: TrackPos -> TrackPos -> Track.PosEvent -> Track.PosEvent
stretch start end event@(pos, evt)
    | Event.is_positive evt = stretchp
    | otherwise = stretchm
    where
    shift = end - start
    stretchp
        | pos < start && Track.event_end event <= start = event
        | pos < start = (pos, Event.modify_duration (+shift) evt)
        | otherwise = (pos + shift, evt)
    stretchm
        | pos <= start = event
        | Track.event_end event < start =
            (pos + shift, Event.modify_duration (subtract shift) evt)
        | otherwise = (pos + shift, evt)

-- | Remove the notes under the selection, and move everything else back.  If
-- the selection is a point, delete one timestep.
cmd_delete_time :: (Monad m) => Cmd.CmdT m ()
cmd_delete_time = do
    (tracknums, track_ids, start, end) <- Selection.merged_tracks
    (start, end) <- expand_range tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        track <- State.get_track track_id
        case Track.split_at_before start (Track.track_events track) of
            (_, []) -> return ()
            (_, evts@((pos, _):_)) -> do
                track_end <- State.track_end track_id
                State.remove_events track_id (min pos start) track_end
                State.insert_sorted_events track_id
                    (Seq.map_maybe (shrink start end) evts)

shrink :: TrackPos -> TrackPos -> Track.PosEvent -> Maybe Track.PosEvent
shrink start end event@(pos, evt)
    | Event.is_positive evt = shrinkp
    | otherwise = shrinkm
    where
    shift = end - start
    shrinkp
        | pos < start && Track.event_end event <= start = Just event
        | pos < start =
            Just (pos, Event.modify_duration
                (subtract (min (Track.event_end event - start) shift)) evt)
        | pos < end = Nothing
        | otherwise = Just (pos - shift, evt)
    shrinkm
        | pos <= start = Just event
        | pos <= end = Nothing
        | Track.event_end event < end =
            Just (pos - shift, Event.modify_duration
                (+ min shift (end - Track.event_end event)) evt)
        | otherwise = Just (pos - shift, evt)

clip_until :: TrackPos -> [Track.PosEvent] -> [Track.PosEvent]
clip_until until events = Seq.map_maybe f events
    where
    f pos_evt@(pos, evt)
        | Track.event_end pos_evt <= until = Nothing
        | pos >= until = Just pos_evt
        | otherwise = Just $ (until,
            Event.set_duration (Event.event_duration evt - (until-pos)) evt)

-- | If the range is a point, then expand it to one timestep.
expand_range :: (Monad m) => [TrackNum] -> TrackPos -> TrackPos
    -> Cmd.CmdT m (TrackPos, TrackPos)
expand_range (tracknum:_) start end
    | start == end = do
        block_id <- Cmd.get_focused_block
        step <- Cmd.get_current_step
        pos <- TimeStep.step_from step TimeStep.Advance block_id tracknum end
        return (start, maybe end id pos)
    | otherwise = return (start, end)
expand_range [] start end = return (start, end)

-- | If the insertion selection is a point, clear any event under it.  If it's
-- a range, clear all events within its half-open extent.
cmd_clear_selected :: (Monad m) => Cmd.CmdT m ()
cmd_clear_selected = do
    (_, track_ids, start, end) <- Selection.merged_tracks
    forM_ track_ids $ \track_id -> if start == end
        then State.remove_event track_id start
        else State.remove_events track_id start end

cmd_meter_step :: (Monad m) => TimeStep.MarkMatch -> Cmd.CmdT m ()
cmd_meter_step match = do
    let step = TimeStep.UntilMark (TimeStep.NamedMarklists ["meter"]) match
    Cmd.modify_state $ \st -> st { Cmd.state_step = step }
    sync_step_status

cmd_invert_step_direction :: (Monad m) => Cmd.CmdT m ()
cmd_invert_step_direction = do
    Cmd.modify_state $ \st -> st { Cmd.state_note_direction =
        invert (Cmd.state_note_direction st) }
    sync_step_status
    where
    invert TimeStep.Advance = TimeStep.Rewind
    invert TimeStep.Rewind = TimeStep.Advance

sync_step_status :: (Monad m) => Cmd.CmdT m ()
sync_step_status = do
    st <- Cmd.get_state
    Cmd.set_global_status "step" $
        show_step (Cmd.state_step st) (Cmd.state_note_direction st)

show_step :: TimeStep.TimeStep -> TimeStep.Direction -> String
show_step step direction = dir_s : case step of
    TimeStep.Absolute pos -> "abs:" ++ show pos
    TimeStep.UntilMark mlists match ->
        "until:" ++ show_marklists mlists ++ "/" ++ show_match match
    TimeStep.MarkDistance mlists match ->
        "dist:" ++ show_marklists mlists ++ "/" ++ show_match match
    where
    dir_s = case direction of
        TimeStep.Advance -> '+'
        TimeStep.Rewind -> '-'

show_match :: TimeStep.MarkMatch -> String
show_match (TimeStep.MatchRank rank skips) =
    "r" ++ show rank ++ "+" ++ show skips

show_marklists TimeStep.AllMarklists = "all"
show_marklists (TimeStep.NamedMarklists mlists) = Seq.join "," mlists

cmd_modify_octave :: (Monad m) => (Pitch.Octave -> Pitch.Octave)
    -> Cmd.CmdT m ()
cmd_modify_octave f = do
    Cmd.modify_state $ \st -> st
        { Cmd.state_kbd_entry_octave = f (Cmd.state_kbd_entry_octave st) }
    sync_octave_status

sync_octave_status :: (Monad m) => Cmd.CmdT m ()
sync_octave_status = do
    octave <- Cmd.gets Cmd.state_kbd_entry_octave
    -- This is technically global state and doesn't belong in the block's
    -- status line, but I'm used to looking for it there, so put it in both
    -- places.
    Cmd.set_status "8ve" (Just (show octave))
    Cmd.set_global_status "8ve" (show octave)

-- * undo / redo

-- TODO if I want to remember the name of the state from the undo, I'll have
-- to stick it in the history: (past, future, now_name)

undo :: (Monad m) => Cmd.CmdT m ()
undo = do
    (past, future) <- Cmd.gets Cmd.state_history
    now <- State.get
    case past of
        (prev:rest) -> do
            Cmd.modify_state $ \st -> st
                { Cmd.state_history = (rest, Cmd.HistoryEntry "" now : future)
                , Cmd.state_skip_history_record = True }
            State.put (Cmd.hist_state prev)
            State.modify $ \st -> merge_undo_states st (Cmd.hist_state prev)
            initialize_state
        [] -> Log.notice "no past to undo"

redo :: (Monad m) => Cmd.CmdT m ()
redo = do
    (past, future) <- Cmd.gets Cmd.state_history
    now <- State.get
    case future of
        (next:rest) -> do
            Cmd.modify_state $ \st -> st
                { Cmd.state_history = (Cmd.HistoryEntry "" now : past, rest)
                , Cmd.state_skip_history_record = True }
            State.modify $ \st -> merge_undo_states st (Cmd.hist_state next)
            initialize_state
        [] -> Log.notice "no future to redo"

-- | There are certain parts of the state that I don't want to undo, so inherit
-- them from the old state: Block and view configs.
merge_undo_states :: State.State -> State.State -> State.State
merge_undo_states old new = new {
    State.state_project = State.state_project old
    , State.state_project_dir = State.state_project_dir old
    , State.state_views = Map.mapWithKey
        (merge_view (State.state_views old)) (State.state_views new)
    , State.state_blocks = Map.mapWithKey
        (merge_block (State.state_blocks old)) (State.state_blocks new)
    , State.state_midi_config = State.state_midi_config old
    }

hist_status :: (Monad m) => Cmd.CmdT m ()
hist_status = do
    (past, future) <- Cmd.gets Cmd.state_history
    Log.debug $ "past length: " ++ show (length past)
        ++ ", future length: " ++ show (length future)

merge_view :: Map.Map ViewId Block.View -> ViewId -> Block.View -> Block.View
merge_view old_views view_id new = case Map.lookup view_id old_views of
    Nothing -> new
    Just old -> new { Block.view_config = Block.view_config old }

merge_block :: Map.Map BlockId Block.Block
    -> BlockId -> Block.Block -> Block.Block
merge_block old_blocks block_id new = case Map.lookup block_id old_blocks of
    Nothing -> new
    Just old -> new { Block.block_config = Block.block_config old }


-- | Sync UI state up with Cmd state and schedule UI updates.
initialize_state :: (Monad m) => Cmd.CmdT m ()
initialize_state = do
    -- TODO these scattered sync functions are kinda grody.  Isn't there a
    -- better way to keep track of state that needs to be synced?  Or avoid
    -- doing it in the first place?
    sync_edit_box_status
    sync_octave_status
    sync_step_status
    sync_global_status
    mapM_ Selection.sync_selection_status =<< State.get_all_view_ids
    mapM_ Cmd.sync_zoom_status =<< State.get_all_view_ids
    -- Emit track updates for all tracks, since I don't know where events have
    -- changed.
    State.update_all_tracks


-- | Sync global status with the current state.  Should be invoked whenever
-- said global state changes.
--
-- TODO Except it won't be when you use State directly.  Solutions are: have
-- Cmd.set_namespace etc. and don't forget to call them, move logging to State,
-- or modify the UiStateMonad instance so it logs in Cmd.
sync_global_status :: (Monad m) => Cmd.CmdT m ()
sync_global_status = do
    st <- State.get
    Cmd.set_global_status "namespace" (State.state_project st)
    let (Pitch.ScaleId scale) = State.state_project_scale st
    Cmd.set_global_status "scale" scale
