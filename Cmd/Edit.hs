{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and control or note track
    commands, go in their own modules.
-}
module Cmd.Edit where
import Control.Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import Ui
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Internal as Internal
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Pitch as Pitch
import qualified App.Config as Config


cmd_toggle_raw_edit, cmd_toggle_val_edit,
    cmd_toggle_method_edit, cmd_toggle_kbd_entry :: (Cmd.M m) => m ()

cmd_toggle_raw_edit = do
    whenM ((== Cmd.RawEdit) <$> get_mode) record_recent_note
    modify_edit_mode $ \m -> case m of
        Cmd.RawEdit -> Cmd.NoEdit
        _ -> Cmd.RawEdit

-- | Unlike the other toggle commands, val edit, being the \"default\" toggle,
-- always turns other modes off.  So you can't switch directly from some other
-- kind of edit to val edit.
cmd_toggle_val_edit = do
    whenM ((== Cmd.RawEdit) <$> get_mode) record_recent_note
    modify_edit_mode $ \m -> case m of
        Cmd.NoEdit -> Cmd.ValEdit
        _ -> Cmd.NoEdit

cmd_toggle_method_edit = modify_edit_mode $ \m -> case m of
    Cmd.MethodEdit -> Cmd.ValEdit
    Cmd.ValEdit -> Cmd.MethodEdit
    _ -> m

get_mode :: (Cmd.M m) => m Cmd.EditMode
get_mode = Cmd.gets (Cmd.state_edit_mode . Cmd.state_edit)

-- | Turn on kbd entry mode, putting a K in the edit box as a reminder.  This
-- is orthogonal to the previous edit modes.
cmd_toggle_kbd_entry = do
    Cmd.modify_edit_state $ \st ->
        st { Cmd.state_kbd_entry = not (Cmd.state_kbd_entry st) }
    sync_edit_box_status

-- ** util

modify_edit_mode :: (Cmd.M m) => (Cmd.EditMode -> Cmd.EditMode) -> m ()
modify_edit_mode f = do
    Cmd.modify_edit_state $ \st ->
        st { Cmd.state_edit_mode = f (Cmd.state_edit_mode st) }
    sync_edit_box_status

sync_edit_box_status :: (Cmd.M m) => m ()
sync_edit_box_status = do
    edit_mode <- Cmd.gets (Cmd.state_edit_mode . Cmd.state_edit)
    kbd_entry <- Cmd.gets (Cmd.state_kbd_entry . Cmd.state_edit)
    let c = if kbd_entry then 'K' else ' '
    Cmd.set_edit_box (edit_color edit_mode) c

edit_color :: Cmd.EditMode -> Color.Color
edit_color mode = case mode of
    Cmd.NoEdit -> Config.box_color
    Cmd.RawEdit -> Config.raw_edit_color
    Cmd.ValEdit -> Config.val_edit_color
    Cmd.MethodEdit -> Config.method_edit_color

-- * universal event cmds

-- | Insert an event at the current insert pos.
insert_event :: (Cmd.M m) => String -> ScoreTime -> m ()
insert_event text dur = do
    (_, _, track_id, pos) <- Selection.get_insert
    State.insert_events track_id [(pos, Event.event text dur)]

-- | This can be used to extend the length of a block so when it is subderived
-- it has the right length.
--
-- If it's more convenient, I could remove any existing "--" events before
-- inserting the new one.
cmd_insert_track_end :: (Cmd.M m) => m ()
cmd_insert_track_end = insert_event "--" 0

-- | Different from insert/delete time since it only modifies one event.
-- Move back the next event, or move down the previous event.  If the
-- selection is non-zero, the event's duration will be modified to the
-- selection.
cmd_move_event_forward :: (Cmd.M m) => m ()
cmd_move_event_forward = move_event $ \pos events ->
    case Events.split pos events of
        (_, (epos, _) : _) | pos == epos -> Nothing
        (prev : _, _) -> Just prev
        _ -> Nothing

cmd_move_event_back :: (Cmd.M m) => m ()
cmd_move_event_back = move_event $ \pos events ->
    case Seq.head (Events.at_after pos events) of
        Just (epos, _) | epos == pos -> Nothing
        Just next -> Just next
        Nothing -> Nothing

move_event :: (Cmd.M m) =>
    (ScoreTime -> Events.Events -> Maybe Events.PosEvent) -> m ()
move_event get_event = do
    (_, track_ids, start, end) <- Selection.tracks
    let pos = Selection.point_pos start end
        dur = abs (end - start)
    forM_ track_ids $ \track_id -> do
        events <- Track.track_events <$> State.get_track track_id
        when_just (get_event pos events) $ \(old_pos, event) -> do
            State.remove_event track_id old_pos
            State.insert_event track_id pos $
                if dur == 0 then event else set_dur dur event


-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note, whichever is shorter.
--
-- If the selection is on an event, the previous one is extended instead.
-- This is more useful than reducing the event to 0, which has its own cmd
-- anyway.
cmd_set_duration :: (Cmd.M m) => m ()
cmd_set_duration = do
    (_, sel) <- Selection.get
    (if Types.sel_is_point sel then set_prev_dur else set_sel_dur)
        (snd (Types.sel_range sel))
    where
    set_sel_dur sel_pos = ModifyEvents.modify_pos_events $ \pos event ->
        if Event.event_duration event /= 0
            then Event.set_duration (sel_pos - pos) event
            else event
    set_prev_dur sel_pos = do
        -- Wow it's a lot of work as soon as it's not the standard selection.
        (_, track_ids, _, _) <- Selection.tracks
        forM_ track_ids $ \track_id -> do
            prev <- Seq.head . fst . Events.split sel_pos . Track.track_events
                <$> State.get_track track_id
            when_just prev $ \(pos, event) ->
                State.insert_event track_id pos
                    (Event.set_duration (sel_pos - pos) event)

-- | Toggle duration between zero and non-zero.
--
-- If the event is non-zero, then make it zero.  Otherwise, set its end to the
-- cursor.  Unless the cursor is on the event start, and then extend it by
-- a timestep.
cmd_toggle_zero_duration :: (Cmd.M m) => m ()
cmd_toggle_zero_duration = do
    (_, sel) <- Selection.get
    let point = Selection.point sel
    (_, end) <- expand_range [Selection.point_track sel] point point
    ModifyEvents.modify_pos_events $ toggle end point
    where
    toggle end point pos event
        | Event.event_duration event /= 0 = Event.set_duration 0 event
        | pos == point = Event.set_duration (end - pos) event
        | otherwise = Event.set_duration (point - pos) event

-- | Move only the beginning of an event.  As is usual for zero duration
-- events, their duration will not be changed so this is equivalent to a move.
--
-- Other cmds are biased towards the previous event, i.e. they affect the
-- previous event if nothing is selected.  This cmd is a little different:
-- if it overlaps with an event, it will affect that one.  Otherwise, it
-- affects the next positive event or the previous negative event.  The idea
-- is that it's not very useful to clip an event to 0 by moving it past its
-- end, so let's not do that.
--
-- Unlike 'cmd_set_duration', I can't think of a way for this to make sense
-- with a non-point selection, so it uses the point position.
--
-- TODO for zero duration events, this is equivalent to 'cmd_move_event_back'.
-- I'm not totally happy about the overlap, is there a more orthogonal
-- organization?
cmd_set_beginning :: (Cmd.M m) => m ()
cmd_set_beginning = do
    (_, sel) <- Selection.get
    (_, track_ids, _, _) <- Selection.tracks
    let pos = Selection.point sel
    forM_ track_ids $ \track_id -> do
        (pre, post) <- Events.split pos . Track.track_events <$>
            State.get_track track_id
        let set = set_beginning track_id pos
        case (Seq.head pre, Seq.head post) of
            (Just prev, _) | Events.overlaps pos prev -> set prev
            (_, Just next) | Events.overlaps pos next -> set next
            (_, Just next) | Events.positive next -> set next
            (Just prev, _) | Events.negative prev -> set prev
            _ -> return ()
    where
    set_beginning track_id start (pos, event) = do
        let end = (if Event.is_positive event then max else min)
                (Events.end (pos, event)) start
            dur = if Event.event_duration event == 0 then 0 else end - start
        State.remove_event track_id pos
        State.insert_event track_id start (Event.set_duration dur event)

-- | Modify event durations by applying a function to them.  0 durations
-- are passed through, so you can't accidentally give control events duration.
cmd_modify_dur :: (Cmd.M m) => (ScoreTime -> ScoreTime) -> m ()
cmd_modify_dur f = ModifyEvents.modify_events $ \evt ->
    Event.set_duration (apply (Event.event_duration evt)) evt
    where apply dur = if dur == 0 then dur else f dur

-- | If there is a following event, delete it and extend this one to its end.
--
-- Since 0 dur events are neven lengthened, joining control events simply
-- deletes the later ones.
cmd_join_events :: (Cmd.M m) => m ()
cmd_join_events = mapM_ process =<< Selection.events_around
    where
        -- If I only selected one, join with the next.  Otherwise, join
        -- selected events.
    process (track_id, _, (_, [evt1], evt2:_)) = join track_id evt1 evt2
    process (track_id, _, (_, evts@(_ : _ : _), _)) =
        join track_id (head evts) (last evts)
    process _ = return ()
    join track_id (pos1, evt1) (pos2, evt2) =
        -- Yes, this deletes any "backwards" events in the middle, but that
        -- should be ok.
        case (Event.is_negative evt1, Event.is_negative evt2) of
            (False, False) -> do
                let end = Events.end (pos2, evt2)
                State.remove_events track_id pos1 end
                -- If evt2 is zero dur, the above half-open range won't get it.
                State.remove_event track_id pos2
                State.insert_event track_id pos1 (set_dur (end - pos1) evt1)
            (True, True) -> do
                let start = Events.end (pos1, evt1)
                State.remove_events track_id start pos2
                State.remove_event track_id pos2
                State.insert_event track_id pos2 (set_dur (start - pos2) evt2)
            _ -> return () -- no sensible way to join these

-- | Zero dur events are never lengthened.
set_dur :: ScoreTime -> Event.Event -> Event.Event
set_dur dur evt
    | Event.event_duration evt == 0 = evt
    | otherwise = Event.set_duration dur evt

-- | Insert empty space at the beginning of the selection for the length of
-- the selection, pushing subsequent events forwards.  If the selection is
-- a point, insert one timestep.
cmd_insert_time :: (Cmd.M m) => m ()
cmd_insert_time = do
    (tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- expand_range tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        track <- State.get_track track_id
        case Events.split_at_before start (Track.track_events track) of
            (_, []) -> return ()
            (_, evts@((pos, _):_)) -> do
                track_end <- State.track_end track_id
                -- +1 to get final event if it's 0 dur, see move_events
                State.remove_events track_id (min pos start) (track_end + 1)
                State.insert_sorted_events track_id
                    (map (insert_time start end) evts)

-- | Modify the event to insert time from @start@ to @end@, lengthening
-- it if @start@ falls within the event's duration.
--
-- TODO both insert_time and delete_time could be faster by just mapping the
-- shift once the overlapping events are done, but it's probably not worth it.
insert_time :: ScoreTime -> ScoreTime -> Events.PosEvent -> Events.PosEvent
insert_time start end event@(pos, evt)
    | Event.is_positive evt = insertp
    | otherwise = insertn
    where
    shift = end - start
    insertp
        | pos < start && Events.end event <= start = event
        | pos < start = (pos, Event.modify_duration (+shift) evt)
        | otherwise = (pos + shift, evt)
    insertn
        | pos <= start = event
        | Events.end event < start =
            (pos + shift, Event.modify_duration (subtract shift) evt)
        | otherwise = (pos + shift, evt)

-- | Remove the notes under the selection, and move everything else back.  If
-- the selection is a point, delete one timestep.
cmd_delete_time :: (Cmd.M m) => m ()
cmd_delete_time = do
    (tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- expand_range tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        track <- State.get_track track_id
        case Events.split_at_before start (Track.track_events track) of
            (_, []) -> return ()
            (_, evts@((pos, _):_)) -> do
                track_end <- State.track_end track_id
                -- +1 to get final event if it's 0 dur, see move_events
                State.remove_events track_id (min pos start) (track_end + 1)
                State.insert_sorted_events track_id
                    (Maybe.mapMaybe (delete_time start end) evts)

-- | Modify the event to delete the time from @start@ to @end@, shortening it
-- if @start@ falls within the event's duration.
delete_time :: ScoreTime -> ScoreTime -> Events.PosEvent
    -> Maybe Events.PosEvent
delete_time start end event@(pos, evt)
    | Event.is_positive evt = deletep
    | otherwise = deleten
    where
    shift = end - start
    deletep
        | pos < start && Events.end event <= start = Just event
        | pos < start =
            Just (pos, Event.modify_duration
                (subtract (min (Events.end event - start) shift)) evt)
        | pos < end = Nothing
        | otherwise = Just (pos - shift, evt)
    deleten
        | pos <= start = Just event
        | pos <= end = Nothing
        | Events.end event < end =
            Just (pos - shift, Event.modify_duration
                (+ min shift (end - Events.end event)) evt)
        | otherwise = Just (pos - shift, evt)

-- | If the range is a point, then expand it to one timestep.
expand_range :: (Cmd.M m) => [TrackNum] -> ScoreTime -> ScoreTime
    -> m (ScoreTime, ScoreTime)
expand_range (tracknum:_) start end
    | start == end = do
        block_id <- Cmd.get_focused_block
        step <- Cmd.get_current_step
        pos <- TimeStep.advance step block_id tracknum end
        return (start, Maybe.fromMaybe end pos)
    | otherwise = return (start, end)
expand_range [] start end = return (start, end)

-- | If the insertion selection is a point, clear any event under it.  If it's
-- a range, clear all events within its half-open extent.
cmd_clear_selected :: (Cmd.M m) => m ()
cmd_clear_selected = do
    (_, track_ids, start, end) <- Selection.tracks
    forM_ track_ids $ \track_id -> if start == end
        then State.remove_event track_id start
        else State.remove_events track_id start end

-- | If the TimeStep is AbsoluteMark or RelativeMark, set its rank and skips
-- to the given ones.  Otherwise, set it to the deflt.
set_step_rank :: (Cmd.M m) => TimeStep.TimeStep
    -> TimeStep.Rank -> TimeStep.Skip -> m ()
set_step_rank deflt rank skip = do
    Cmd.modify_edit_state $ \st -> st { Cmd.state_time_step =
        set (TimeStep.to_list (Cmd.state_time_step st)) }
    sync_step_status
    where
    set [(TimeStep.AbsoluteMark names _, _)] =
        TimeStep.time_step skip (TimeStep.AbsoluteMark names rank)
    set [(TimeStep.RelativeMark names _, _)] =
        TimeStep.time_step skip (TimeStep.RelativeMark names rank)
    set _ = deflt

-- | Toggle between absolute and relative mark step.
toggle_mark_step :: (Cmd.M m) => m ()
toggle_mark_step = do
    Cmd.modify_edit_state $ \st ->
        st { Cmd.state_time_step = toggle (Cmd.state_time_step st) }
    sync_step_status
    where
    toggle step = case TimeStep.to_list step of
        [(TimeStep.AbsoluteMark names rank, skip)] ->
            TimeStep.time_step skip (TimeStep.RelativeMark names rank)
        [(TimeStep.RelativeMark names rank, skip)] ->
            TimeStep.time_step skip (TimeStep.AbsoluteMark names rank)
        _ -> step

set_step :: (Cmd.M m) => TimeStep.TimeStep -> m ()
set_step step = do
    Cmd.modify_edit_state $ \st -> st { Cmd.state_time_step = step }
    sync_step_status

cmd_invert_step_direction :: (Cmd.M m) => m ()
cmd_invert_step_direction = do
    Cmd.modify_edit_state $ \st -> st { Cmd.state_note_direction =
        invert (Cmd.state_note_direction st) }
    sync_step_status
    where
    invert TimeStep.Advance = TimeStep.Rewind
    invert TimeStep.Rewind = TimeStep.Advance

sync_step_status :: (Cmd.M m) => m ()
sync_step_status = do
    st <- Cmd.gets Cmd.state_edit
    let status = TimeStep.show_step (Just (Cmd.state_note_direction st))
            (Cmd.state_time_step st)
    Cmd.set_global_status "step" status
    Cmd.set_status "step" (Just status)

cmd_modify_octave :: (Cmd.M m) => (Pitch.Octave -> Pitch.Octave) -> m ()
cmd_modify_octave f = do
    Cmd.modify_edit_state $ \st -> st
        { Cmd.state_kbd_entry_octave = f (Cmd.state_kbd_entry_octave st) }
    sync_octave_status

sync_octave_status :: (Cmd.M m) => m ()
sync_octave_status = do
    octave <- Cmd.gets (Cmd.state_kbd_entry_octave . Cmd.state_edit)
    -- This is technically global state and doesn't belong in the block's
    -- status line, but I'm used to looking for it there, so put it in both
    -- places.
    Cmd.set_status "8ve" (Just (show octave))
    Cmd.set_global_status "8ve" (show octave)


-- * recent note

cmd_insert_recent :: (Cmd.M m) => Int -> m ()
cmd_insert_recent num = do
    recent <- Cmd.gets (Cmd.state_recent_notes . Cmd.state_edit)
    insert_recent =<< Cmd.require (lookup num recent)

insert_recent :: (Cmd.M m) => Cmd.RecentNote -> m ()
insert_recent (Cmd.RecentNote text is_zero) =
    EditUtil.modify_event is_zero True (const (Just text, True))
insert_recent (Cmd.RecentTransform text) = do
    pos <- EditUtil.get_sel_pos
    EditUtil.modify_event_at pos True False $ \s ->
        (Just (text ++ " |" ++ (if null s then "" else " " ++ s)), False)

-- | Try to record the current event in the LIFO recent note queue, as
-- documented in 'Cmd.state_recent_notes'.
record_recent_note :: (Cmd.M m) => m ()
record_recent_note = do
    (_, _, track_id, pos) <- Selection.get_insert
    maybe_event <- State.get_event track_id pos
    when_just (recent_note =<< snd <$> maybe_event) $ \note -> do
        Cmd.modify_edit_state $ \st -> st { Cmd.state_recent_notes =
            record_recent note (Cmd.state_recent_notes st) }
        sync_recent

recent_note :: Event.Event -> Maybe Cmd.RecentNote
recent_note event
    | null post = let note = Seq.strip pre
        in if null note then Nothing
            else Just $ Cmd.RecentNote note (Event.event_duration event == 0)
    | otherwise = let trans = Seq.strip pre
        in if null trans then Nothing else Just (Cmd.RecentTransform trans)
    where (pre, post) = break (=='|') (Seq.strip (Event.event_string event))

record_recent :: Cmd.RecentNote -> [(Int, Cmd.RecentNote)]
    -> [(Int, Cmd.RecentNote)]
record_recent note recent0 = (key, note) : recent
    where
    recent = take (max_recent - 1) (filter ((/=note) . snd) recent0)
    key = Maybe.fromMaybe (length recent) $
        (fst <$> List.find ((==note) . snd) recent0)
        `mplus`
        Seq.head (filter (`notElem` (map fst recent)) [1..max_recent])
    max_recent = 4

sync_recent :: (Cmd.M m) => m ()
sync_recent = do
    recent <- Cmd.gets (Cmd.state_recent_notes . Cmd.state_edit)
    Cmd.set_global_status "recent" $
        Seq.join ", " (map show_recent (Seq.sort_on fst recent))
    where
    show_recent (num, note) = show num ++ ": " ++ case note of
        Cmd.RecentNote s _ -> s
        Cmd.RecentTransform s -> s ++ "|"


-- * sync

-- | Sync UI state up with Cmd state and schedule UI updates.
initialize_state :: (Cmd.M m) => m ()
initialize_state = do
    -- TODO these scattered sync functions are kinda grody.  Isn't there a
    -- better way to keep track of state that needs to be synced?  Or avoid
    -- doing it in the first place?
    sync_edit_box_status
    sync_octave_status
    sync_step_status
    sync_global_status
    mapM_ Selection.sync_selection_status =<< State.get_all_view_ids
    mapM_ Internal.sync_zoom_status =<< State.get_all_view_ids
    -- Emit track updates for all tracks, since I don't know where events have
    -- changed.
    State.update_all_tracks


-- | Sync global status with the current state.  Should be invoked whenever
-- said global state changes.
--
-- TODO Except it won't be when you use State directly.  Solutions are: have
-- Cmd.set_project etc. and don't forget to call them, move logging to State,
-- or modify the UiStateMonad instance so it logs in Cmd.
sync_global_status :: (Cmd.M m) => m ()
sync_global_status = do
    config <- State.get_config id
    Cmd.set_global_status "proj" (State.config_namespace config)
    let (Pitch.ScaleId scale) =
            State.default_scale (State.config_default config)
    Cmd.set_global_status "scale" scale
