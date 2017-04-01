-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase, MultiWayIf #-}
{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and control or note track
    commands, go in their own modules.
-}
module Cmd.Edit where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Zoom as Zoom

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Derive as Derive
import qualified Derive.ParseTitle as ParseTitle
import qualified Perform.Pitch as Pitch
import qualified App.Config as Config
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

cmd_toggle_val_edit_kbd_entry :: Cmd.M m => m ()
cmd_toggle_val_edit_kbd_entry = Cmd.modify_edit_state $ \st ->
    if Cmd.state_edit_mode st == Cmd.ValEdit
        then st
            { Cmd.state_edit_mode = Cmd.NoEdit
            , Cmd.state_kbd_entry = False
            }
        else st
            { Cmd.state_edit_mode = Cmd.ValEdit
            , Cmd.state_kbd_entry = True
            }

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
    Ui.insert_block_events block_id track_id [Event.event pos dur text]

-- | Different from 'cmd_insert_time' and 'cmd_delete_time' since it only
-- modifies one event.  Move back the next event, or move forward the previous
-- event.  If the selection is non-zero, the event's duration will be modified
-- to the selection.
cmd_move_event_forward :: Cmd.M m => m ()
cmd_move_event_forward = move_event $ \pos events ->
    case Events.split_lists pos events of
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

move_event :: Cmd.M m => (ScoreTime -> Events.Events -> Maybe Event.Event)
    -> m ()
move_event modify = do
    (block_id, _, track_ids, _) <- Selection.tracks
    pos <- Selection.point
    forM_ track_ids $ \track_id -> do
        events <- Ui.get_events track_id
        whenJust (modify pos events) $ \event -> do
            Ui.remove_event track_id event
            Ui.insert_block_events block_id track_id
                [Event.start_ #= pos $ event]

-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note, whichever is shorter.
--
-- If the selection is on an event, the previous or next one (depending on
-- 'Event.Orientation') is extended instead.  This is more useful than reducing
-- the event to 0, which has its own cmd anyway.  If the selection is between
-- a positive and negative event, the one corresponding to 'Sel.orientation' is
-- selected.
cmd_set_duration :: Cmd.M m => m ()
cmd_set_duration = modify_event_near_point modify
    where
    modify (start, end) event
        | Event.is_negative event = set_dur (start - Event.start event) event
        | otherwise = set_dur (end - Event.start event) event

-- | Similar to 'ModifyEvents.event', but if the selection is a point, modify
-- the previous or next event, depending on if it's positive or negative.
modify_event_near_point :: Cmd.M m =>
    ((ScoreTime, ScoreTime) -> Event.Event -> Event.Event) -> m ()
modify_event_near_point modify = do
    sel <- Selection.get
    if Sel.is_point sel
        then prev_or_next (Selection.sel_point sel) =<< Cmd.get_focused_block
        else selection (Sel.range sel)
    where
    -- TODO Should I integrate this into ModifyEvents?
    prev_or_next pos block_id = do
        tracks <- avoid_exact_match
        forM_ tracks $ \(track_id, event) -> do
            tracknum <- Ui.get_tracknum_of block_id track_id
            -- It's confusing to modify collapsed tracks because you don't
            -- see the change.  TODO this is recreating the stuff in
            -- ModifyEvents.
            unlessM (Ui.track_collapsed block_id tracknum) $ do
                Ui.remove_event track_id event
                Ui.insert_event track_id $ modify (pos, pos) event

    selection range = ModifyEvents.selection_visible $ ModifyEvents.events $
        return . map (modify range)

-- | Like 'Selection.events_around', but if a point selection is on an event
-- start, find a neighbor instead of matching that event.
avoid_exact_match :: Cmd.M m => m [(TrackId, Event.Event)]
avoid_exact_match = do
    pos <- Selection.sel_point <$> Selection.get
    Seq.map_maybe_snd (select pos) <$> Selection.events_around
    where
    select pos triple = case triple of
        (_, [within], _) | Event.start within /= pos -> Just within
        (prev:_, [within], _) | positive within && positive prev -> Just prev
        (_, [within], next:_) | negative within && negative next -> Just next
        _ -> Nothing
    positive = Event.is_positive
    negative = Event.is_negative

{- | Toggle duration between zero and non-zero.

    If the event is non-zero, then make it zero.  Otherwise, set its end to the
    cursor.  Unless the cursor is on the event start, and then extend it by
    a timestep.

    Also I previously used the same event selection strategy as
    'cmd_set_duration', which avoided the awkward point selection on zero-dur
    event case, but it turned out to be unintuitive to use in practice, because
    to toggle an event I'd have to put the selection on the next event.
-}
cmd_toggle_zero_timestep :: Cmd.M m => m ()
cmd_toggle_zero_timestep = do
    sel <- Selection.get
    ModifyEvents.selection_visible $ \block_id track_id events -> do
        tracknum <- Ui.get_tracknum_of block_id track_id
        let (start, end) = Sel.range sel
        Just <$> mapM (toggle_zero_timestep block_id tracknum start end) events

toggle_zero_timestep :: Cmd.M m => BlockId -> TrackNum
    -> TrackTime -> TrackTime -> Event.Event -> m Event.Event
toggle_zero_timestep block_id tracknum start end event
    | Event.duration event /= 0 = return $
        Event.duration_ #= (if Event.is_negative event then -0 else 0) $ event
    | start == end && start == Event.start event = do
        step <- Cmd.get_current_step
        maybe_pos <- TimeStep.step_from
            (if Event.is_negative event then -1 else 1) step block_id tracknum
            start
        case maybe_pos of
            Nothing -> return event
            Just pos -> return $ Event.set_end pos event
    | Event.is_negative event =
        return $ Event.duration_ #= (start - Event.start event) $ event
    | otherwise = return $ Event.duration_ #= (end - Event.start event) $ event

-- | Move only the beginning of an event.  As is usual for zero duration
-- events, their duration will not be changed so this is equivalent to a move.
--
-- Other cmds are biased towards the previous event, i.e. they affect the
-- previous event if nothing is selected.  This cmd is a little different:
-- if it overlaps with an event, it will affect that one.  Otherwise, it
-- affects the next positive event or the previous negative event.  The idea
-- is that it's not very useful to clip an event to 0 by moving it past its
-- end, so let's not do that.  So it's like set_duration backwards: at does
-- nothing, otherwise get next positive or prev negative.
--
-- Unlike 'cmd_set_duration', I can't think of a way for this to make sense
-- with a non-point selection, so it uses the point position.
--
-- TODO for zero duration events, this is equivalent to
-- 'cmd_move_event_backward'.  I'm not totally happy about the overlap, is
-- there a more orthogonal organization?
cmd_set_start :: Cmd.M m => m ()
cmd_set_start = do
    pos <- Selection.point
    mapM_ (process pos) =<< Selection.opposite_neighbor
    where
    process pos (track_id, event) = do
        Ui.remove_event track_id event
        Ui.insert_event track_id $ set_start pos event
    set_start p event
        | Event.duration event == 0 = Event.start_ #= p $ event
        | otherwise = Event.set_start p event

-- | Modify event durations by applying a function to them.  0 durations
-- are passed through, so you can't accidentally give control events duration.
modify_dur :: Cmd.M m => (ScoreTime -> ScoreTime) -> m ()
modify_dur f = ModifyEvents.selection_visible $ ModifyEvents.event $
    Event.duration_ %= apply
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
    process (track_id, selected) = case selected of
        (_, [cur], next:_) | Event.is_positive cur -> join track_id cur next
        (prev:_, [cur], _) | Event.is_negative cur -> join track_id prev cur
        (_, events@(_:_:_), _) -> join track_id (head events) (last events)
        _ -> return ()
    join track_id evt1 evt2 =
        case (Event.is_positive evt1, Event.is_positive evt2) of
            (True, True) -> do
                mapM_ (Ui.remove_event track_id) [evt1, evt2]
                Ui.insert_event track_id $
                    set_dur (Event.end evt2 - Event.start evt1) evt1
            (False, False) -> do
                mapM_ (Ui.remove_event track_id) [evt1, evt2]
                Ui.insert_event track_id $ if Event.duration evt2 == 0
                    then evt2
                    else Event.set_end (Event.end evt1) evt2
            _ -> return () -- no sensible way to join these


-- | Split the events under the cursor.
cmd_split_events :: Cmd.M m => m ()
cmd_split_events = do
    p <- Selection.point
    ModifyEvents.overlapping $ ModifyEvents.events $
        return . concatMap (split p)
    where
    split p event
        | not (Event.overlaps p event) || p == Event.start event = [event]
        | otherwise =
            [ Event.duration_ #= p - Event.start event $  event
            , Event.place p (Event.end event - p) event
            ]

-- | The same as 'Event.set_duration' except don't modify a zero dur event.
set_dur :: TrackTime -> Event.Event -> Event.Event
set_dur dur evt
    | Event.duration evt == 0 = evt
    | otherwise = Event.duration_ #= dur $ evt

-- | Insert empty space at the beginning of the selection for the length of
-- the selection, pushing subsequent events forwards.  If the selection is
-- a point, insert one timestep.
cmd_insert_time :: Cmd.M m => m ()
cmd_insert_time = do
    (block_id, tracknums, track_ids, range) <- Selection.tracks
    let (start, end) = Events.range_times range
    (start, end) <- point_to_timestep tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        events <- Ui.get_events track_id
        case Events.split_at_before start events of
            (_, []) -> return ()
            (_, events@(event:_)) -> do
                Ui.remove_from track_id (min (Event.start event) start)
                -- The above won't get a negative event at start.
                Ui.remove_event track_id event
                Ui.insert_block_events block_id track_id $
                    map (insert_event_time start (end-start)) events

-- | Modify the event to insert time, lengthening it if the start time falls
-- within the event's duration.
insert_event_time :: TrackTime -> TrackTime -> Event.Event -> Event.Event
insert_event_time start shift event
    | Event.is_positive event = if
        | start <= Event.start event -> Event.start_ %= (+shift) $ event
        | start < Event.end event -> Event.duration_ %= (+shift) $ event
        | otherwise -> event
    | otherwise = if
        | start <= Event.end event -> Event.start_ %= (+shift) $ event
        | start < Event.start event ->
            Event.set_start (Event.start event + shift) event
        | otherwise -> event

-- | Remove the notes under the selection, and move everything else back.  If
-- the selection is a point, delete one timestep.
cmd_delete_time :: Cmd.M m => m ()
cmd_delete_time = do
    (block_id, tracknums, track_ids, range) <- Selection.tracks
    let (start, end) = Events.range_times range
    (start, end) <- point_to_timestep tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id ->
        delete_time block_id track_id start (end-start)

-- | Delete the time range for all tracks in the block.
delete_block_time :: Ui.M m => BlockId -> TrackTime -> TrackTime -> m ()
delete_block_time block_id start dur = do
    track_ids <- Ui.track_ids_of block_id
    forM_ track_ids $ \track_id ->
        delete_time block_id track_id start dur

delete_time :: Ui.M m => BlockId -> TrackId -> TrackTime -> TrackTime -> m ()
delete_time block_id track_id start dur = do
    when (dur < 0) $
        Ui.throw $ "delete_time: negative dur " <> pretty dur
    events <- Ui.get_events track_id
    case Events.split_at_before start events of
        (_, []) -> return ()
        (_, events@(event:_)) -> do
            Ui.remove_from track_id (min (Event.start event) start)
            -- The above won't get a negative event at start.
            Ui.remove_event track_id event
            Ui.insert_block_events block_id track_id
                (mapMaybe (delete_event_time start dur) events)

-- | Modify the event to delete time, shortening it the start time falls within
-- the event's duration, or removing it entirely if its 'Event.start' was
-- deleted.
--
-- This is more complicated than 'insert_event_time' because it can delete
-- events, and because the event may only be partially shortened if the range
-- overlaps its end.
delete_event_time :: TrackTime -> TrackTime -> Event.Event -> Maybe Event.Event
delete_event_time start shift event
    | Event.is_positive event = if
        | end <= Event.start event ->
            Just $ Event.start_ %= subtract shift $ event
        | start < Event.start event -> Nothing
        | start < Event.end event -> Just $ Event.duration_ #=
            (max (Event.duration event - shift) (start - Event.start event)) $
            event
        | otherwise -> Just event
    | otherwise = if
        | end <= Event.end event ->
            Just $ Event.start_ %= subtract shift $ event
        | end < Event.start event ->
            Just $ Event.set_end start $ Event.start_ %= subtract shift $ event
        | start < Event.start event -> Nothing
        | otherwise -> Just event
    where end = start + shift

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
    (_, _, track_ids, range) <- Selection.tracks
    clear_range track_ids range

clear_range :: Ui.M m => [TrackId] -> Events.Range -> m ()
clear_range track_ids range =
    forM_ track_ids $ \track_id -> Ui.remove_event_range track_id range

cmd_clear_and_advance :: Cmd.M m => m ()
cmd_clear_and_advance = do
    cmd_clear_selected
    sel <- Selection.get
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
cmd_set_call_duration = ModifyEvents.selection_visible $
    \block_id track_id events ->
        Just <$> mapM (set_call_duration block_id track_id) events

-- | Set the event duration to the CallDuration of its call.  For block calls,
-- this is the natural block duration.
set_call_duration :: Cmd.M m => BlockId -> TrackId -> Event.Event
    -> m Event.Event
set_call_duration block_id track_id event =
    maybe event (\d -> Event.duration_ #= d $ event) <$>
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
cmd_invert_orientation = do
    track_ids <- Selection.track_ids
    let is_control = fmap ParseTitle.is_control_track . Ui.get_track_title
    ifM (allM is_control track_ids) invert_events invert_notes

invert_events :: Cmd.M m => m ()
invert_events =
    ModifyEvents.modify_selected False modify =<< Selection.events_at_point
    where
    modify = ModifyEvents.event $ \e ->
        Event.start_ %= (+ Event.duration e) $ Event.duration_ %= negate $ e

invert_notes :: Cmd.M m => m ()
invert_notes = ModifyNotes.selection $ ModifyNotes.note invert
    where
    invert note = invert_note $
        ModifyNotes.controls %= fmap (invert_control note) $ note
    invert_note note = ModifyNotes.start %= (+ ModifyNotes.note_duration note) $
        ModifyNotes.duration %= negate $ note
    -- If there's exactly one event at the start time, I can flip it with the
    -- note.
    invert_control note events = case Events.ascending events of
        [event] | Event.duration event == 0 && Event.start event == start ->
            Events.singleton $ Event.duration_ #= dur $
                Event.start_ #= ModifyNotes.note_end note $ event
        _ -> events
        where
        dur = case ModifyNotes.note_orientation note of
            Event.Positive -> -0
            Event.Negative -> 0
        start = ModifyNotes.note_start note

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
append_text = open_floating (\text -> (Text.length text, Text.length text))

prepend_text :: Cmd.M m => m Cmd.Status
prepend_text = open_floating (const (0, 0))

-- | This will be fooled by a @|@ inside a string, but I'll fix that if it's
-- ever actually a problem.
replace_first_call :: Cmd.M m => m Cmd.Status
replace_first_call = open_floating $ \text -> case Text.breakOn "|" text of
    (pre, _) ->
        let space = " " `Text.isSuffixOf` pre
        in (0, Text.length pre - (if space then 1 else 0))

replace_last_call :: Cmd.M m => m Cmd.Status
replace_last_call = open_floating $ \text -> case Text.breakOnEnd "|" text of
    (pre, post) ->
        let space = " " `Text.isPrefixOf` post
        in (Text.length pre + (if space then 1 else 0), Text.length text)

-- | If a selection is used to create an event, this is where the event's start
-- is.
edit_point :: Sel.Selection -> TrackTime
edit_point sel = case Sel.orientation sel of
    Sel.Negative -> Sel.max sel
    _ -> Sel.min sel

-- | Open a floating text entry with a selection set.
open_floating :: Cmd.M m => (Text -> (Int, Int)) -> m Cmd.Status
open_floating selection = do
    (view_id, sel) <- Selection.get_view
    (_, tracknum, track_id, _) <- Selection.get_insert
    let pos = edit_point sel
    maybe_event <- event_at track_id pos (Sel.event_orientation sel)
    let text = maybe "" Event.text maybe_event
        orient = maybe (Sel.event_orientation sel) Event.orientation maybe_event
    zoom <- Block.view_zoom <$> Ui.get_view view_id
    -- If I'm editing or opening a negative event, move the input up since
    -- the text will also be above the trigger.
    let open_pos = pos - case orient of
            Event.Negative -> Zoom.to_time zoom Config.track_title_height
            Event.Positive -> 0
    return $ Cmd.FloatingInput $ Cmd.FloatingOpen view_id tracknum open_pos text
        (selection text)

event_text_at :: Ui.M m => TrackId -> TrackTime -> Event.Orientation
    -> m (Maybe Text)
event_text_at track_id pos = fmap (fmap Event.text) . event_at track_id pos

event_at :: Ui.M m => TrackId -> TrackTime -> Event.Orientation
    -> m (Maybe Event.Event)
event_at track_id pos orient = Events.at pos orient <$> Ui.get_events track_id

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
        =<< Ui.event_track_at block_id tracknum
    old_text <- event_text_at track_id start (Event.orientation_of dur)
    let zero_dur = always_zero_dur || " " `Text.isPrefixOf` text
    EditUtil.modify_event_at (EditUtil.Pos block_id tracknum start dur)
        zero_dur False (const (Just (Text.strip text), False))
    -- 0 dur means a point selection, which means to use the time step.
    insert_recorded_action '.' $ make_action old_text (Text.strip text) $ if
        | zero_dur -> Just (if ScoreTime.is_negative dur then -0 else 0)
        | dur == 0 -> Nothing
        | otherwise -> Just dur
    when (old_text == Nothing) $
        try_set_call_duration block_id track_id start (Event.orientation_of dur)
    return Cmd.Done

-- | Set the event's duration to its CallDuration, if it has one.
try_set_call_duration :: Cmd.M m => BlockId -> TrackId -> TrackTime
    -> Event.Orientation -> m ()
try_set_call_duration block_id track_id pos orient =
    whenJustM (event_at track_id pos orient) $ \event ->
        whenJustM (lookup_call_duration block_id track_id event) $ \dur ->
            when (dur /= Event.duration event) $
                Ui.insert_event track_id (Event.duration_ #= dur $ event)

floating_input_msg :: Msg.Msg -> Maybe Text
floating_input_msg (Msg.Ui (UiMsg.UiMsg ctx
        (UiMsg.UiUpdate _ (UiMsg.UpdateInput (Just text)))))
    | UiMsg.ctx_floating_input ctx = Just text
floating_input_msg _ = Nothing
