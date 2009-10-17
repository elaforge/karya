{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and controller or note track
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
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Selection as Selection

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

modify_edit_mode f = do
    Cmd.modify_state $ \st ->
        st { Cmd.state_edit_mode = f (Cmd.state_edit_mode st) }
    sync_edit_box_status

sync_edit_box_status :: (Monad m) => Cmd.CmdT m ()
sync_edit_box_status = do
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    kbd_entry <- fmap Cmd.state_kbd_entry Cmd.get_state
    block_ids <- State.get_all_block_ids
    let c = if kbd_entry then 'K' else ' '
    forM_ block_ids $ \bid -> State.set_edit_box bid (edit_color edit_mode) c

edit_color mode = case mode of
    Cmd.NoEdit -> Config.box_color
    Cmd.RawEdit -> Config.raw_edit_color
    Cmd.ValEdit -> Config.val_edit_color
    Cmd.MethodEdit -> Config.method_edit_color

-- * universal event cmds

-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note.
cmd_set_duration :: (Monad m) => Cmd.CmdT m ()
cmd_set_duration = modify_events $ \_ end (pos, event) ->
    (pos, event { Event.event_duration = end - pos })

-- | If there is a following event, delete it and extend this one to its end.
cmd_join_events :: (Monad m) => Cmd.CmdT m ()
cmd_join_events = mapM_ process =<< Selection.events_around
    where
    process (track_id, ((pos1, evt1):_), Just (pos2, evt2)) = do
        let end = Track.event_end (pos2, evt2)
        State.remove_events track_id pos1 end
        State.insert_events track_id
            [(pos1, evt1 { Event.event_duration = end - pos1 })]
    process _ = return ()

-- | Insert empty space at the beginning of the selection for the length of
-- the selection, pushing subsequent events forwards.  If the selection is
-- a point, insert one timestep.
cmd_insert_time :: (Monad m) => Cmd.CmdT m ()
cmd_insert_time = do
    (tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- expand_range tracknums start end
    when (start /= end) $ forM_ track_ids $ \track_id -> do
        -- lengthen an overlapping event
        modify_overlapping track_id start $ \pos evt ->
            let dur = Event.event_duration evt
            in Just $ if pos == start then evt
                else evt { Event.event_duration = dur + (end-start) }
        move_track_events start (end-start) track_id

-- | Remove the notes under the selection, and move everything else back.  If
-- the selection is a point, delete one timestep.
cmd_delete_time :: (Monad m) => Cmd.CmdT m ()
cmd_delete_time = do
    (tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- expand_range tracknums start end
    when (start /= end) $ forM_ track_ids $ \track_id -> do
        -- shorten an overlapping event
        modify_overlapping track_id start $ \pos evt ->
            let shorten = min end (Track.event_end (pos, evt)) - start
                dur = Event.event_duration evt
            in Just $ evt { Event.event_duration = dur - shorten }
        deleted <- State.get_events track_id start end
        State.remove_events track_id start end
        move_track_events start (-(end-start)) track_id
        -- Reinsert the deleted events, if they only partially overlapped the
        -- deleted area.
        State.insert_events track_id $
            map (shift (start-end)) (clip_until end deleted)
    where shift val (pos, evt) = (pos + val, evt)

clip_until :: TrackPos -> [Track.PosEvent] -> [Track.PosEvent]
clip_until until events = Seq.map_maybe f events
    where
    f pos_evt@(pos, evt)
        | Track.event_end pos_evt <= until = Nothing
        | pos >= until = Just pos_evt
        | otherwise = Just $ (until, evt
            { Event.event_duration = Event.event_duration evt - (until-pos) })

modify_overlapping :: (Monad m) => TrackId -> TrackPos
    -> (TrackPos -> Event.Event -> Maybe Event.Event) -> Cmd.CmdT m ()
modify_overlapping track_id pos f = do
    track <- State.get_track track_id
    case Track.event_before pos (Track.track_events track) of
        Just (evt_pos, evt) | Track.event_end (evt_pos, evt) > pos -> do
            State.map_events track_id evt_pos evt_pos modify
        _ -> return ()
    where
    modify [(pos, evt)] = maybe [] ((:[]) . ((,) pos)) (f pos evt)
    modify _ = []

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

-- | Move everything at or after @start@ by @shift@.
move_track_events :: (State.UiStateMonad m) =>
    TrackPos -> TrackPos -> TrackId -> m ()
move_track_events start shift track_id = do
    track <- State.get_track track_id
    let shifted = move_events start shift (Track.track_events track)
    State.set_events track_id shifted

-- | All events starting at a point to the end are shifted by the given amount.
move_events :: TrackPos -> TrackPos -> Track.TrackEvents -> Track.TrackEvents
move_events start shift events = merged
    where
    -- If the last event has 0 duration, the selection will not include it.
    -- Ick.  Maybe I need a less error-prone way to say "select until the end
    -- of the track"?
    end = Track.time_end events + TrackPos 1
    shifted = map (\(pos, evt) -> (pos+shift, evt))
        (Track.events_after start events)
    merged = Track.insert_events shifted (Track.remove_events start end events)

-- | Modify event durations by applying a function to them.  0 durations
-- are passed through, so you can't accidentally give control events duration.
cmd_modify_dur :: (Monad m) => (TrackPos -> TrackPos) -> Cmd.CmdT m ()
cmd_modify_dur f = modify_events $ \_ _ (pos, evt) ->
    (pos, evt { Event.event_duration = apply (Event.event_duration evt) })
    where apply dur = if dur == TrackPos 0 then dur else f dur

-- | Modify previous event if the selection is a point, and all events under
-- the selection if it's a range.
modify_events :: (Monad m) =>
    (TrackPos -> TrackPos -> Track.PosEvent -> Track.PosEvent) -> Cmd.CmdT m ()
modify_events f = do
    (start, end, track_events) <- Selection.events True
    forM_ track_events $ \(track_id, pos_events) -> do
        let pos_events2 = map (f start end) pos_events
        if start == end then State.remove_event track_id start
            else State.remove_events track_id start end
        State.insert_events track_id pos_events2

-- | If the insertion selection is a point, clear any event under it.  If it's
-- a range, clear all events within its half-open extent.
cmd_clear_selected :: (Monad m) => Cmd.CmdT m ()
cmd_clear_selected = do
    (_, track_ids, start, end) <- Selection.tracks
    forM_ track_ids $ \track_id -> if start == end
        then State.remove_event track_id start
        else State.remove_events track_id start end

cmd_meter_step :: (Monad m) => TimeStep.MarkMatch -> Cmd.CmdT m ()
cmd_meter_step match = do
    let step = TimeStep.UntilMark (TimeStep.NamedMarklists ["meter"]) match
    Cmd.modify_state $ \st -> st { Cmd.state_step = step }
    sync_step_status

sync_step_status :: (Monad m) => Cmd.CmdT m ()
sync_step_status = do
    step <- fmap Cmd.state_step Cmd.get_state
    Cmd.set_global_status "step" (show_step step)

show_step (TimeStep.Absolute pos) = "abs:" ++ show pos
show_step (TimeStep.UntilMark mlists match) =
    "until:" ++ show_marklists mlists ++ "/" ++ show_match match
show_step (TimeStep.MarkDistance mlists match) =
    "dist:" ++ show_marklists mlists ++ "/" ++ show_match match

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
    octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
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
    (past, future) <- fmap Cmd.state_history Cmd.get_state
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
    (past, future) <- fmap Cmd.state_history Cmd.get_state
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
    (past, future) <- fmap Cmd.state_history Cmd.get_state
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

clear_history :: (Monad m) => Cmd.CmdT m ()
clear_history = Cmd.modify_state $ \st -> st { Cmd.state_history = ([], []) }


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
