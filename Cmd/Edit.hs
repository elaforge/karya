{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and controller or note track
    commands, go in their own modules.
-}
module Cmd.Edit where
import Control.Monad
import qualified Data.Map as Map

import qualified Util.Log as Log
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


cmd_toggle_raw_edit, cmd_toggle_val_edit, cmd_toggle_call_edit,
    cmd_toggle_method_edit, cmd_toggle_kbd_entry :: Cmd.CmdId

cmd_toggle_raw_edit = modify_edit_mode $ \m -> case m of
    Just Cmd.RawEdit -> Nothing
    _ -> Just Cmd.RawEdit

-- | Unlike the other toggle commands, val edit, being the \"default\" toggle,
-- always turns other modes off.  So you can't switch directly from some other
-- kind of edit to val edit.
cmd_toggle_val_edit = modify_edit_mode $ \m -> case m of
    Nothing -> Just Cmd.ValEdit
    Just _ -> Nothing

cmd_toggle_call_edit = modify_edit_mode $ \m -> case m of
    Just Cmd.CallEdit -> Nothing
    _ -> Just Cmd.CallEdit

cmd_toggle_method_edit = modify_edit_mode $ \m -> case m of
    Just Cmd.MethodEdit -> Just Cmd.ValEdit
    Just Cmd.ValEdit -> Just Cmd.MethodEdit
    _ -> m

-- | Turn on kbd entry mode, putting a K in the edit box as a reminder.  This
-- is orthogonal to the previous edit modes.
cmd_toggle_kbd_entry = do
    Cmd.modify_state $ \st ->
        st { Cmd.state_kbd_entry = not (Cmd.state_kbd_entry st) }
    sync_edit_box_status
    return Cmd.Done

-- ** util

modify_edit_mode f = do
    Cmd.modify_state $ \st ->
        st { Cmd.state_edit_mode = f (Cmd.state_edit_mode st) }
    sync_edit_box_status
    return Cmd.Done

sync_edit_box_status :: (Monad m) => Cmd.CmdT m ()
sync_edit_box_status = do
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    kbd_entry <- fmap Cmd.state_kbd_entry Cmd.get_state
    block_ids <- State.get_all_block_ids
    let c = if kbd_entry then 'K' else ' '
    forM_ block_ids $ \bid -> State.set_edit_box bid (edit_color edit_mode) c

edit_color mode = case mode of
    Nothing -> Config.box_color
    Just Cmd.RawEdit -> Config.raw_edit_color
    Just Cmd.ValEdit -> Config.val_edit_color
    Just Cmd.CallEdit -> Config.call_edit_color
    Just Cmd.MethodEdit -> Config.method_edit_color

-- * universal event cmds

-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note.
-- TODO control tracks that are covered by the selection should be skipped
-- TODO do this by ignoring dur 0 events
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

merge_view :: Map.Map Block.ViewId Block.View
    -> Block.ViewId -> Block.View -> Block.View
merge_view old_views view_id new = case Map.lookup view_id old_views of
    Nothing -> new
    Just old -> new { Block.view_config = Block.view_config old }

merge_block :: Map.Map Block.BlockId Block.Block
    -> Block.BlockId -> Block.Block -> Block.Block
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
    mapM_ Selection.sync_selection_status =<< State.get_all_view_ids
    mapM_ Cmd.sync_zoom_status =<< State.get_all_view_ids
    -- Emit track updates for all tracks, since I don't know where events have
    -- changed.
    State.update_all_tracks
