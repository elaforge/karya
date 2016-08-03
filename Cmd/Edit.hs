-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and control or note track
    commands, go in their own modules.
-}
module Cmd.Edit where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Derive as Derive
import qualified Perform.Pitch as Pitch
import Global
import Types


-- * global editing state

-- | Unlike the other toggle commands, val edit, being the \"default\" toggle,
-- always turns other modes off.  So you can't switch directly from some other
-- kind of edit to val edit.
cmd_toggle_val_edit :: Cmd.M m => m ()
cmd_toggle_val_edit = modify_edit_mode $ \m -> case m of
    Cmd.NoEdit -> Cmd.ValEdit
    _ -> Cmd.NoEdit

cmd_toggle_method_edit :: Cmd.M m => m ()
cmd_toggle_method_edit = modify_edit_mode $ \m -> case m of
    Cmd.MethodEdit -> Cmd.ValEdit
    Cmd.ValEdit -> Cmd.MethodEdit
    _ -> m

get_mode :: Cmd.M m => m Cmd.EditMode
get_mode = Cmd.gets (Cmd.state_edit_mode . Cmd.state_edit)

-- | Toggle kbd entry mode, putting a K in the edit box as a reminder.  This is
-- orthogonal to the previous edit modes.
cmd_toggle_kbd_entry :: Cmd.M m => m ()
cmd_toggle_kbd_entry = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_kbd_entry = not (Cmd.state_kbd_entry st) }

-- | If the TimeStep is AbsoluteMark or RelativeMark, set its rank.  Otherwise,
-- set it to the deflt.  This means the marklist names are sticky, so if you
-- set it manually the default bindings won't mess it up.
set_step_rank :: Cmd.M m => TimeStep.TimeStep -> Ruler.Rank -> m ()
set_step_rank deflt rank = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_time_step =
        set (TimeStep.to_list (Cmd.state_time_step st)) }
    where
    set [TimeStep.AbsoluteMark names _] =
        TimeStep.time_step (TimeStep.AbsoluteMark names rank)
    set [TimeStep.RelativeMark names _] =
        TimeStep.time_step (TimeStep.RelativeMark names rank)
    set _ = deflt

-- | Toggle between absolute and relative mark step.
toggle_absolute_relative_step :: Cmd.M m => m ()
toggle_absolute_relative_step = Cmd.modify_edit_state $ \st ->
        st { Cmd.state_time_step = toggle (Cmd.state_time_step st) }
    where
    toggle step = case TimeStep.to_list step of
        [TimeStep.AbsoluteMark names rank] ->
            TimeStep.time_step (TimeStep.RelativeMark names rank)
        [TimeStep.RelativeMark names rank] ->
            TimeStep.time_step (TimeStep.AbsoluteMark names rank)
        _ -> step

set_step :: Cmd.M m => TimeStep.TimeStep -> m ()
set_step step = Cmd.modify_edit_state $ \st -> st { Cmd.state_time_step = step }

cmd_toggle_note_orientation :: Cmd.M m => m ()
cmd_toggle_note_orientation = Cmd.modify_edit_state $ \st -> st
    { Cmd.state_note_orientation = Event.invert (Cmd.state_note_orientation st)
    }

cmd_modify_octave :: Cmd.M m => (Pitch.Octave -> Pitch.Octave) -> m ()
cmd_modify_octave f = Cmd.modify_edit_state $ \st -> st
    { Cmd.state_kbd_entry_octave = f (Cmd.state_kbd_entry_octave st) }

-- ** util

modify_edit_mode :: Cmd.M m => (Cmd.EditMode -> Cmd.EditMode) -> m ()
modify_edit_mode f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_edit_mode = f (Cmd.state_edit_mode st) }

toggle_advance :: Cmd.M m => m ()
toggle_advance = modify_advance not

modify_advance :: Cmd.M m => (Bool -> Bool) -> m ()
modify_advance f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_advance = f (Cmd.state_advance st) }

toggle_chord :: Cmd.M m => m ()
toggle_chord = modify_chord not

modify_chord :: Cmd.M m => (Bool -> Bool) -> m ()
modify_chord f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_chord = f (Cmd.state_chord st) }

toggle_record_velocity :: Cmd.CmdL ()
toggle_record_velocity = modify_record_velocity not

modify_record_velocity :: (Bool -> Bool) -> Cmd.CmdL ()
modify_record_velocity f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_record_velocity = f (Cmd.state_record_velocity st) }

-- * event start and duration

-- | Insert an event at the current insert pos.
insert_event :: Cmd.M m => Text -> ScoreTime -> m ()
insert_event text dur = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    State.insert_block_events block_id track_id [Event.event pos dur text]

-- | Different from 'cmd_insert_time' and 'cmd_delete_time' since it only
-- modifies one event.  Move back the next event, or move forward the previous
-- event.  If the selection is non-zero, the event's duration will be modified
-- to the selection.
cmd_move_event_forward :: Cmd.M m => m ()
cmd_move_event_forward = move_event $ \pos events ->
    case Events.split pos events of
        (_, next : _) | Event.start next == pos -> Nothing
        (prev : _, _) -> Just prev
        _ -> Nothing

cmd_move_event_backward :: Cmd.M m => m ()
cmd_move_event_backward = move_event $ \pos events ->
    case Events.at_after pos events of
        next : _
            | Event.start next == pos -> Nothing
            | otherwise -> Just next
        [] -> Nothing

move_event :: Cmd.M m =>
    (ScoreTime -> Events.Events -> Maybe Event.Event) -> m ()
move_event modify = do
    (block_id, _, track_ids, pos, _) <- Selection.tracks
    forM_ track_ids $ \track_id -> do
        events <- Track.track_events <$> State.get_track track_id
        whenJust (modify pos events) $ \event -> do
            State.remove_event track_id (Event.start event)
            State.insert_block_events block_id track_id
                [Event.move_to pos event]


-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note, whichever is shorter.
--
-- If the selection is on an event, the previous or next one (depending on
-- 'Event.Orientation') is extended instead.  This is more useful than reducing
-- the event to 0, which has its own cmd anyway.
cmd_set_duration :: Cmd.M m => m ()
cmd_set_duration = modify_event_near_point modify
    where
    modify (start, end) event
        | Event.duration event == 0 = event
        | Event.is_negative event = Event.set_start start event
        | otherwise = set_dur (end - Event.start event) event

-- | Similar to 'ModifyEvents.event', but if the selection is a point, modify
-- the previous or next event, depending on if it's positive or negative.
modify_event_near_point :: Cmd.M m =>
    ((ScoreTime, ScoreTime) -> Event.Event -> Event.Event) -> m ()
modify_event_near_point modify = do
    (_, sel) <- Selection.get
    if Sel.is_point sel
        then prev_or_next (Sel.start_pos sel)
        else selection (Sel.range sel)
    where
    selection range = ModifyEvents.selection_expanded $ ModifyEvents.events $
        return . Events.clip_negative_events . map (modify range)
    -- TODO Should I make this ModifyEvents.prev_or_next?
    prev_or_next pos = do
        (block_id, tracknums, track_ids, _, _) <- Selection.tracks
        forM_ (zip tracknums track_ids) $ \(tracknum, track_id) ->
            unlessM (State.track_collapsed block_id tracknum) $
                track_point pos track_id
    track_point pos track_id = do
        events <- Track.track_events <$> State.get_track track_id
        whenJust (event_around pos events) $ \event -> do
            State.remove_event track_id (Event.start event)
            State.insert_event track_id $ modify (pos, pos) event

event_around :: TrackTime -> Events.Events -> Maybe Event.Event
event_around pos events = case Events.split pos events of
    (pre:_, _) | Event.overlaps pos pre && pos /= Event.trigger pre -> Just pre
    -- |--->    |---> => take pre
    (pre:_, posts) | positive pre && maybe True positive (Seq.head posts) ->
        Just pre
    -- <---|    <---| => take post
    (pres, post:_) | negative post && maybe True negative (Seq.head pres) ->
        Just post
    -- |--->    <---| => take closer, favor pre
    (pre:_, post:_) | positive pre && negative post ->
        Just $ Seq.min_on (abs . subtract pos . Event.trigger) pre post
    -- <---|    |---> => in the middle, do nothing
    _ -> Nothing
    where
    positive = Event.is_positive
    negative = Event.is_negative

{- | Toggle duration between zero and non-zero.

    If the event is non-zero, then make it zero.  Otherwise, set its end to the
    cursor.  Unless the cursor is on the event start, and then extend it by
    a timestep.

    Previously I would \"zero\" negative duration to the 'Event.trigger', like
    'cmd_set_duration'.  But because this is actually moving the start time, it
    will delete a next event that starts where this one ends, which is
    confusing.

    Also I previously used the same event selection strategy as
    'cmd_set_duration', which avoided the awkward point selection on zero-dur
    event case, but it turned out to be unintuitive to use in practice, because
    to toggle an event I'd have to put the selection on the next event.
-}
cmd_toggle_zero_timestep :: Cmd.M m => m ()
cmd_toggle_zero_timestep = do
    (_, sel) <- Selection.get
    ModifyEvents.selection_expanded $ \block_id track_id events -> do
        tracknum <- State.get_tracknum_of block_id track_id
        let (start, end) = Sel.range sel
        Just <$> mapM (toggle_zero_timestep block_id tracknum start end) events

toggle_zero_timestep :: Cmd.M m => BlockId -> TrackNum
    -> TrackTime -> TrackTime -> Event.Event -> m Event.Event
toggle_zero_timestep block_id tracknum start end event
    | Event.duration event /= 0 = return $ Event.set_duration 0 event
    | start == end && start == Event.trigger event = do
        step <- Cmd.get_current_step
        maybe_pos <- TimeStep.step_from 1 step block_id tracknum start
        case maybe_pos of
            Nothing -> return event
            Just pos -> return $ Event.set_end pos event
    | end > Event.start event = return $ Event.set_end end event
    | otherwise = return event

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
-- TODO for zero duration events, this is equivalent to
-- 'cmd_move_event_backward'.  I'm not totally happy about the overlap, is
-- there a more orthogonal organization?
cmd_set_start :: Cmd.M m => m ()
cmd_set_start = do
    (_, sel) <- Selection.get
    (_, _, track_ids, _, _) <- Selection.tracks
    let pos = Selection.point sel
    forM_ track_ids $ \track_id -> do
        (pre, post) <- Events.split pos . Track.track_events <$>
            State.get_track track_id
        let set = set_beginning track_id pos
        case (pre, post) of
            (prev:_, _) | Event.overlaps pos prev -> set prev
            (_, next:_) | Event.overlaps pos next -> set next
            (_, next:_) | Event.is_positive next -> set next
            (prev:_, _) | Event.is_negative prev -> set prev
            _ -> return ()
    where
    set_beginning track_id start event = do
        let end = (if Event.is_positive event then max else min)
                (Event.end event) start
            dur = if Event.duration event == 0 then 0 else end - start
        State.remove_event track_id (Event.start event)
        State.insert_event track_id $ place start dur event

-- | Modify event durations by applying a function to them.  0 durations
-- are passed through, so you can't accidentally give control events duration.
modify_dur :: Cmd.M m => (ScoreTime -> ScoreTime) -> m ()
modify_dur f = ModifyEvents.selection_expanded $ ModifyEvents.event $ \evt ->
    Event.set_duration (apply (Event.duration evt)) evt
    where apply dur = if dur == 0 then dur else f dur

-- | If there is a following event, delete it and extend this one to its end.
--
-- Since 0 dur events are never lengthened, joining control events simply
-- deletes the later ones.
cmd_join_events :: Cmd.M m => m ()
cmd_join_events = mapM_ process =<< Selection.events_around
    where
    -- If I only selected one, join with the next.  Otherwise, join select
    -- first to last.
    process (track_id, (_, [evt1], evt2:_)) = join track_id evt1 evt2
    process (track_id, (_, events@(_ : _ : _), _)) =
        join track_id (head events) (last events)
    process _ = return ()
    join track_id evt1 evt2 =
        -- Yes, this deletes any "backwards" events in the middle, but that
        -- should be ok.
        case (Event.is_positive evt1, Event.is_positive evt2) of
            (True, True) -> do
                remove_range track_id evt1 evt2
                State.insert_event track_id $
                    set_dur (Event.end evt2 - Event.start evt1) evt1
            (False, False) -> do
                remove_range track_id evt1 evt2
                State.insert_event track_id $ if Event.duration evt2 == 0
                    then evt2
                    else Event.place (Event.start evt1)
                        (Event.end evt2 - Event.start evt1) evt2
            _ -> return () -- no sensible way to join these
    remove_range track_id evt1 evt2 = do
        State.remove_event_range track_id (Event.start evt1) (Event.end evt2)
        -- If evt2 is zero dur, the above half-open range won't get it.
        when (Event.duration evt2 == 0) $
            State.remove_event track_id (Event.start evt2)


-- | Split the events under the cursor.
cmd_split_events :: Cmd.M m => m ()
cmd_split_events = do
    (_, _, _, p) <- Selection.get_insert
    ModifyEvents.overlapping $ ModifyEvents.events $
        return . concatMap (split p)
    where
    split p event
        | not (Event.overlaps p event) || p == Event.trigger event = [event]
        | otherwise =
            [ Event.set_duration (p - Event.start event) event
            , Event.place p (Event.end event - p) event
            ]

-- | The same as 'Event.set_duration' except don't modify a zero dur event.
set_dur :: TrackTime -> Event.Event -> Event.Event
set_dur dur evt
    | Event.duration evt == 0 = evt
    | otherwise = Event.set_duration dur evt

place :: TrackTime -> TrackTime -> Event.Event -> Event.Event
place start dur = Event.move_to start . set_dur dur

-- | Insert empty space at the beginning of the selection for the length of
-- the selection, pushing subsequent events forwards.  If the selection is
-- a point, insert one timestep.
cmd_insert_time :: Cmd.M m => m ()
cmd_insert_time = do
    (block_id, tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- point_to_timestep tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        track <- State.get_track track_id
        case Events.split_at_before start (Track.track_events track) of
            (_, []) -> return ()
            (_, events@(event:_)) -> do
                remove_events_from track_id (min (Event.start event) start)
                State.insert_block_events block_id track_id $
                    map (insert_event_time start (end-start)) events

-- | Modify the event to insert time, lengthening it if the start time falls
-- within the event's duration.
insert_event_time :: TrackTime -> TrackTime -> Event.Event -> Event.Event
insert_event_time start shift event
    | Event.start event >= start = Event.move (+shift) event
    | Event.end event > start = Event.modify_duration (+shift) event
    | otherwise = event

-- | Remove the notes under the selection, and move everything else back.  If
-- the selection is a point, delete one timestep.
cmd_delete_time :: Cmd.M m => m ()
cmd_delete_time = do
    (block_id, tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- point_to_timestep tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id ->
        delete_time block_id track_id start (end-start)

-- | Delete the time range for all tracks in the block.
delete_block_time :: State.M m => BlockId -> TrackTime -> TrackTime -> m ()
delete_block_time block_id start dur = do
    track_ids <- State.track_ids_of block_id
    forM_ track_ids $ \track_id ->
        delete_time block_id track_id start dur

delete_time :: State.M m => BlockId -> TrackId -> TrackTime -> TrackTime -> m ()
delete_time block_id track_id start dur = do
    when (dur < 0) $
        State.throw $ "delete_time: negative dur " <> pretty dur
    track <- State.get_track track_id
    case Events.split_at_before start (Track.track_events track) of
        (_, []) -> return ()
        (_, events@(event:_)) -> do
            remove_events_from track_id (min (Event.start event) start)
            State.insert_block_events block_id track_id
                (mapMaybe (delete_event_time start dur) events)

-- | Modify the event to delete time, shortening it the start time falls within
-- the event's duration, or removing it entirely if its 'Event.trigger' was
-- deleted.
delete_event_time :: TrackTime -> TrackTime -> Event.Event -> Maybe Event.Event
delete_event_time start shift event
    -- Why is this SO COMPLICATED?
    -- if overlaps trigger, then delete
    | Event.is_positive event && overlaps (Event.start event) = Nothing
    -- Negative events use an end range, except I don't want to delete
    -- a -0 dur when it touches the end, only when it passses it.
    | Event.is_negative event && Event.duration event > 0
        && overlaps_end (Event.end event) = Nothing
    | Event.is_negative event && Event.duration event == 0
        && overlaps (Event.end event) = Nothing
    -- if within, then subtract dur
    | Event.start event < start && end <= Event.end event =
        Just $ Event.modify_duration (subtract shift) event
    -- if overlaps end of positive, shorten to the end
    | Event.is_positive event && overlaps (Event.end event) =
        Just $ Event.modify_duration (subtract (Event.end event - start)) event
    -- if overlap start of negative, then place
    | Event.is_negative event && overlaps (Event.start event) =
        Just $ Event.place start
            (Event.duration event - (end - Event.start event)) event
    -- if before, then move.
    | end <= Event.start event = Just $ Event.move (subtract shift) event
    -- if after, then leave it alone
    | otherwise {- start >= Event.end event -} = Just event
    where
    end = start + shift
    overlaps p = start <= p && p < end
    overlaps_end p = start < p && p <= end

remove_events_from :: State.M m => TrackId -> TrackTime -> m ()
remove_events_from track_id start = do
    end <- State.track_event_end track_id
    -- +1 to get final event if it's 0 dur.  It seems gross, but it works and
    -- otherwise I'd need to add a function to Ui.State.
    State.remove_event_range track_id start (end + 1)

-- | If the range is a point, then expand it to one timestep.
point_to_timestep :: Cmd.M m => [TrackNum] -> TrackTime -> TrackTime
    -> m (TrackTime, TrackTime)
point_to_timestep (tracknum:_) start end
    | start == end = do
        block_id <- Cmd.get_focused_block
        step <- Cmd.get_current_step
        pos <- TimeStep.advance step block_id tracknum end
        return (start, fromMaybe end pos)
    | otherwise = return (start, end)
point_to_timestep [] start end = return (start, end)

-- | If the insertion selection is a point, clear any event under it.  If it's
-- a range, clear all events within its half-open extent.
cmd_clear_selected :: Cmd.M m => m ()
cmd_clear_selected = do
    (_, _, track_ids, start, end) <- Selection.tracks
    clear_range track_ids start end

clear_range :: State.M m => [TrackId] -> TrackTime -> TrackTime -> m ()
clear_range track_ids start end =
    forM_ track_ids $ \track_id -> State.remove_event_range track_id start end

cmd_clear_and_advance :: Cmd.M m => m ()
cmd_clear_and_advance = do
    cmd_clear_selected
    (_, sel) <- Selection.get
    when (Sel.is_point sel && Sel.start_track sel == Sel.cur_track sel)
        Selection.advance

-- | Toggle the note duration between the end of the block, and the current
-- time step.
toggle_note_duration :: Cmd.M m => m ()
toggle_note_duration = do
    dur <- Cmd.gets $ Cmd.state_note_duration . Cmd.state_edit
    step <- Cmd.gets $ Cmd.state_time_step . Cmd.state_edit
    Cmd.modify_edit_state $ \st ->
        st { Cmd.state_note_duration = if dur == to_end then step else to_end }
    where to_end = TimeStep.time_step TimeStep.BlockEdge

-- ** fancier start\/duration edits

cmd_set_call_duration :: Cmd.M m => m ()
cmd_set_call_duration = ModifyEvents.selection_expanded $
    \block_id track_id events ->
        Just <$> mapM (set_call_duration block_id track_id) events

-- | Set the event duration to the CallDuration of its call.  For block calls,
-- this is the natural block duration.
set_call_duration :: Cmd.M m => BlockId -> TrackId -> Event.Event
    -> m Event.Event
set_call_duration block_id track_id event =
    maybe event (flip Event.set_duration event) <$>
        lookup_call_duration block_id track_id event

-- | Evaluate the given event to find its 'Derive.get_call_duration'.
lookup_call_duration :: Cmd.M m => BlockId -> TrackId -> Event.Event
    -> m (Maybe TrackTime)
lookup_call_duration block_id track_id event =
    Perf.lookup_note_deriver block_id track_id event >>= \case
        Nothing -> return Nothing
        Just deriver -> do
            dur <- Perf.get_derive_at block_id track_id $
                Derive.get_score_duration deriver
            return $ Just $ case dur of
                Derive.Unknown -> Event.duration event
                Derive.CallDuration dur -> dur

cmd_invert_orientation :: Cmd.M m => m ()
cmd_invert_orientation = ModifyNotes.selection $ ModifyNotes.note invert
    where
    invert note = note
        { ModifyNotes.note_orientation =
            Event.invert (ModifyNotes.note_orientation note)
        , ModifyNotes.note_controls =
            invert_control note <$> ModifyNotes.note_controls note
        }
    invert_control note events = case Events.ascending events of
        [event]
            | zero_dur event && positive
                    && Event.start event == ModifyNotes.note_start note ->
                Events.singleton $
                    Event.move_to (ModifyNotes.note_end note) event
            | zero_dur event && Event.end event == ModifyNotes.note_end note ->
                Events.singleton $
                    Event.move_to (ModifyNotes.note_start note) event
        _ -> events
        where
        positive = ModifyNotes.note_orientation note == Event.Positive
        zero_dur = (==0) . Event.duration

-- * modify text

-- | Strip off the first transformer, and then the generator.
strip_call :: Cmd.M m => m ()
strip_call = do
    ModifyEvents.selected_track $
        ModifyEvents.text $ ModifyEvents.pipeline $ drop 1
    ModifyEvents.advance_if_point

-- ** record action

-- | If you create a new event, and there is explicit duration, then use it.
make_action :: Maybe Text -> Text -> Maybe TrackTime -> Cmd.Action
make_action maybe_old new dur = case maybe_old of
    Nothing -> Cmd.InsertEvent dur new
    Just old
        -- This is a way to record a ReplaceText for an existing event.
        | old == new || old == "" -> Cmd.ReplaceText new
        | old `Text.isPrefixOf` new ->
            Cmd.AppendText $ Text.drop (Text.length old) new
        | old `Text.isSuffixOf` new ->
            Cmd.PrependText $ Text.take (Text.length new - Text.length old) new
        | otherwise -> Cmd.ReplaceText new

insert_recorded_action :: Cmd.M m => Char -> Cmd.Action -> m ()
insert_recorded_action key action = Cmd.modify_edit_state $ \st -> st
    { Cmd.state_recorded_actions =
        Map.insert key action (Cmd.state_recorded_actions st)
    }

save_last_action_to :: Cmd.M m => Char -> m ()
save_last_action_to key = insert_recorded_action key =<< get_action '.'

run_action_at :: Cmd.M m => Char -> m ()
run_action_at key = run_action =<< get_action key

get_action :: Cmd.M m => Char -> m Cmd.Action
get_action key = Cmd.abort_unless
    =<< Cmd.gets (Map.lookup key . Cmd.state_recorded_actions . Cmd.state_edit)

run_action :: Cmd.M m => Cmd.Action -> m ()
run_action action = do
    pos <- EditUtil.get_pos
    let modify = EditUtil.modify_event_at pos False False
    case action of
        Cmd.InsertEvent maybe_dur text -> do
            let new_pos = case pos of
                    EditUtil.Pos block_id tracknum start _dur ->
                        EditUtil.Pos block_id tracknum start
                            (fromMaybe 0 maybe_dur)
            EditUtil.modify_event_at new_pos (maybe_dur == Just 0) True $
                const (Just text, True)
        Cmd.ReplaceText text -> modify $ const (Just text, True)
        Cmd.PrependText text -> modify $ \old -> case old of
            Nothing -> (Nothing, False)
            Just old -> (Just $ text <> old, True)
        Cmd.AppendText text -> modify $ \old -> case old of
            Nothing -> (Nothing, False)
            Just old -> (Just $ old <> text, True)

-- ** floating text input

append_text :: Cmd.M m => m Cmd.Status
append_text = open_floating (const Nothing)

prepend_text :: Cmd.M m => m Cmd.Status
prepend_text = open_floating (const $ Just (0, 0))

-- | This will be fooled by a @|@ inside a string, but I'll fix that if it's
-- ever actually a problem.
replace_first_call :: Cmd.M m => m Cmd.Status
replace_first_call = open_floating $ \text -> case Text.breakOn "|" text of
    (pre, _) ->
        let space = " " `Text.isSuffixOf` pre
        in Just (0, Text.length pre - (if space then 1 else 0))

replace_last_call :: Cmd.M m => m Cmd.Status
replace_last_call = open_floating $ \text -> case Text.breakOnEnd "|" text of
    (pre, post) ->
        let space = " " `Text.isPrefixOf` post
        in Just (Text.length pre + (if space then 1 else 0), Text.length text)

-- | Open a floating text entry with a selection set.
open_floating :: Cmd.M m => (Text -> Maybe (Int, Int)) -> m Cmd.Status
open_floating selection = do
    (view_id, sel) <- Selection.get
    (_, tracknum, track_id, _) <- Selection.get_insert
    let pos = Selection.point sel
    text <- fromMaybe "" <$> event_text_at track_id pos
    return $ Cmd.FloatingInput $ Cmd.FloatingOpen view_id tracknum pos text
        (selection text)

event_text_at :: State.M m => TrackId -> ScoreTime -> m (Maybe Text)
event_text_at track_id = fmap (fmap Event.text) . event_at track_id

event_at :: State.M m => TrackId -> ScoreTime -> m (Maybe Event.Event)
event_at track_id pos =
    Events.at pos . Track.track_events <$> State.get_track track_id

-- ** handle floating input msg

-- | Handle UpdateInput that comes back from the floating input.
--
-- A leading space will create a zero duration event.
handle_floating_input :: Cmd.M m =>
    Bool -- ^ True to always create a zero duration event.
    -> Msg.Msg -> m Cmd.Status
handle_floating_input always_zero_dur msg = do
    text <- Cmd.abort_unless $ floating_input_msg msg
    EditUtil.Pos block_id tracknum start dur <- EditUtil.get_pos
    track_id <- Cmd.require "handle_floating_input on non-event track"
        =<< State.event_track_at block_id tracknum
    old_text <- event_text_at track_id start
    let zero_dur = always_zero_dur || " " `Text.isPrefixOf` text
    EditUtil.modify_event_at (EditUtil.Pos block_id tracknum start dur)
        zero_dur False (const (Just (Text.strip text), False))
    -- 0 dur means a point selection, which means to use the time step.
    insert_recorded_action '.' $ make_action old_text (Text.strip text)
        (if zero_dur then Just 0 else if dur == 0 then Nothing else Just dur)
    when (old_text == Nothing) $
        try_set_call_duration block_id track_id start
    return Cmd.Done

-- | Set the event's duration to its CallDuration, if it has one.
--
-- Calling this from 'handle_floating_input' unconditionally means that every
-- edit to an event will potentially resize it.  Perhaps I'll want to scale it
-- back so that only adding a new event has this behaviour.
try_set_call_duration :: Cmd.M m => BlockId -> TrackId -> TrackTime -> m ()
try_set_call_duration block_id track_id pos =
    whenJustM (event_at track_id pos) $ \event ->
        whenJustM (lookup_call_duration block_id track_id event) $ \dur ->
            when (dur /= Event.duration event) $
                State.insert_event track_id (Event.set_duration dur event)

floating_input_msg :: Msg.Msg -> Maybe Text
floating_input_msg (Msg.Ui (UiMsg.UiMsg ctx
        (UiMsg.UiUpdate _ (UiMsg.UpdateInput (Just text)))))
    | UiMsg.ctx_floating_input ctx = Just text
floating_input_msg _ = Nothing
