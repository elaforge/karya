-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and control or note track
    commands, go in their own modules.
-}
module Cmd.Edit where
import qualified Data.List as List
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Pitch as Pitch
import Types


cmd_toggle_raw_edit :: (Cmd.M m) => m ()
cmd_toggle_raw_edit = do
    whenM ((== Cmd.RawEdit) <$> get_mode) record_recent_note
    modify_edit_mode $ \m -> case m of
        Cmd.RawEdit -> Cmd.NoEdit
        _ -> Cmd.RawEdit

-- | Unlike the other toggle commands, val edit, being the \"default\" toggle,
-- always turns other modes off.  So you can't switch directly from some other
-- kind of edit to val edit.
cmd_toggle_val_edit :: (Cmd.M m) => m ()
cmd_toggle_val_edit = do
    whenM ((== Cmd.RawEdit) <$> get_mode) record_recent_note
    modify_edit_mode $ \m -> case m of
        Cmd.NoEdit -> Cmd.ValEdit
        _ -> Cmd.NoEdit

cmd_toggle_method_edit :: (Cmd.M m) => m ()
cmd_toggle_method_edit = modify_edit_mode $ \m -> case m of
    Cmd.MethodEdit -> Cmd.ValEdit
    Cmd.ValEdit -> Cmd.MethodEdit
    _ -> m

get_mode :: (Cmd.M m) => m Cmd.EditMode
get_mode = Cmd.gets (Cmd.state_edit_mode . Cmd.state_edit)

-- | Turn on kbd entry mode, putting a K in the edit box as a reminder.  This
-- is orthogonal to the previous edit modes.
cmd_toggle_kbd_entry :: (Cmd.M m) => m ()
cmd_toggle_kbd_entry = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_kbd_entry = not (Cmd.state_kbd_entry st) }

-- ** util

modify_edit_mode :: (Cmd.M m) => (Cmd.EditMode -> Cmd.EditMode) -> m ()
modify_edit_mode f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_edit_mode = f (Cmd.state_edit_mode st) }

toggle_advance :: (Cmd.M m) => m ()
toggle_advance = modify_advance not

modify_advance :: (Cmd.M m) => (Bool -> Bool) -> m ()
modify_advance f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_advance = f (Cmd.state_advance st) }

toggle_chord :: (Cmd.M m) => m ()
toggle_chord = modify_chord not

modify_chord :: (Cmd.M m) => (Bool -> Bool) -> m ()
modify_chord f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_chord = f (Cmd.state_chord st) }

toggle_record_velocity :: Cmd.CmdL ()
toggle_record_velocity = modify_record_velocity not

modify_record_velocity :: (Bool -> Bool) -> Cmd.CmdL ()
modify_record_velocity f = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_record_velocity = f (Cmd.state_record_velocity st) }

-- * universal event cmds

-- | Insert an event at the current insert pos.
insert_event :: (Cmd.M m) => String -> ScoreTime -> m ()
insert_event text dur = do
    (_, _, track_id, pos) <- Selection.get_insert
    State.insert_event track_id $ Event.event pos dur text

-- | Different from insert/delete time since it only modifies one event.
-- Move back the next event, or move down the previous event.  If the
-- selection is non-zero, the event's duration will be modified to the
-- selection.
cmd_move_event_forward :: (Cmd.M m) => m ()
cmd_move_event_forward = move_event $ \pos events ->
    case Events.split pos events of
        (_, next : _) | Event.start next == pos -> Nothing
        (prev : _, _) -> Just prev
        _ -> Nothing

cmd_move_event_back :: (Cmd.M m) => m ()
cmd_move_event_back = move_event $ \pos events ->
    case Events.at_after pos events of
        next : _
            | Event.start next == pos -> Nothing
            | otherwise -> Just next
        [] -> Nothing

move_event :: (Cmd.M m) =>
    (ScoreTime -> Events.Events -> Maybe Event.Event) -> m ()
move_event get_event = do
    (_, _, track_ids, pos, _) <- Selection.tracks
    forM_ track_ids $ \track_id -> do
        events <- Track.track_events <$> State.get_track track_id
        whenJust (get_event pos events) $ \event -> do
            State.remove_event track_id (Event.start event)
            State.insert_event track_id $ Event.move (const pos) event


-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note, whichever is shorter.
--
-- If the selection is on an event, the previous one is extended instead.
-- This is more useful than reducing the event to 0, which has its own cmd
-- anyway.
cmd_set_duration :: (Cmd.M m) => m ()
cmd_set_duration = modify_prev $ \sel_pos event ->
    set_dur (sel_pos - Event.start event) event

-- | Similar to 'ModifyEvents.event', but if the selection is a point, modify
-- the previous event.  Also, pass the end of the selection.
modify_prev :: (Cmd.M m) => (ScoreTime -> ModifyEvents.Event) -> m ()
modify_prev modify = do
    (_, sel) <- Selection.get
    (if Types.sel_is_point sel then modify_prev else modify_events)
        (snd (Types.sel_range sel))
    where
    modify_events = ModifyEvents.selection . ModifyEvents.event . modify
    modify_prev sel_pos = do
        -- Wow it's a lot of work as soon as it's not the standard selection.
        (_, _, track_ids, _, _) <- Selection.tracks
        forM_ track_ids $ \track_id -> do
            prev <- Seq.head . fst . Events.split sel_pos . Track.track_events
                <$> State.get_track track_id
            whenJust prev $ State.insert_event track_id . modify sel_pos

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
    ModifyEvents.selection $ ModifyEvents.event (toggle end point)
    where
    toggle end point event
        | Event.duration event /= 0 = Event.set_duration 0 event
        | pos == point = Event.set_duration (end - pos) event
        | otherwise = Event.set_duration (point - pos) event
        where pos = Event.start event

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
    (_, _, track_ids, _, _) <- Selection.tracks
    let pos = Selection.point sel
    forM_ track_ids $ \track_id -> do
        (pre, post) <- Events.split pos . Track.track_events <$>
            State.get_track track_id
        let set = set_beginning track_id pos
        case (pre, post) of
            (prev:_, _) | Event.overlaps pos prev -> set prev
            (_, next:_) | Event.overlaps pos next -> set next
            (_, next:_) | Event.positive next -> set next
            (prev:_, _) | Event.negative prev -> set prev
            _ -> return ()
    where
    set_beginning track_id start event = do
        let end = (if Event.positive event then max else min)
                (Event.end event) start
            dur = if Event.duration event == 0 then 0 else end - start
        State.remove_event track_id (Event.start event)
        State.insert_event track_id $ place start dur event

-- | Modify event durations by applying a function to them.  0 durations
-- are passed through, so you can't accidentally give control events duration.
modify_dur :: (Cmd.M m) => (ScoreTime -> ScoreTime) -> m ()
modify_dur f = ModifyEvents.selection $ ModifyEvents.event $ \evt ->
    Event.set_duration (apply (Event.duration evt)) evt
    where apply dur = if dur == 0 then dur else f dur

-- | If there is a following event, delete it and extend this one to its end.
--
-- Since 0 dur events are neven lengthened, joining control events simply
-- deletes the later ones.
cmd_join_events :: (Cmd.M m) => m ()
cmd_join_events = mapM_ process =<< Selection.events_around
    where
    -- If I only selected one, join with the next.  Otherwise, join selected
    -- events.
    process (track_id, _, (_, [evt1], evt2:_)) = join track_id evt1 evt2
    process (track_id, _, (_, events@(_ : _ : _), _)) =
        join track_id (head events) (last events)
    process _ = return ()
    join track_id evt1 evt2 =
        -- Yes, this deletes any "backwards" events in the middle, but that
        -- should be ok.
        case (Event.negative evt1, Event.negative evt2) of
            (False, False) -> do
                let end = Event.end evt2
                State.remove_events track_id (Event.start evt1) end
                -- If evt2 is zero dur, the above half-open range won't get it.
                State.remove_event track_id (Event.start evt2)
                State.insert_event track_id $
                    set_dur (end - Event.start evt1) evt1
            (True, True) -> do
                let start = Event.end evt1
                State.remove_events track_id start (Event.start evt2)
                State.remove_event track_id (Event.start evt2)
                State.insert_event track_id $
                    set_dur (start - Event.start evt2) evt2
            _ -> return () -- no sensible way to join these

-- | Split the events under the cursor.
cmd_split_events :: (Cmd.M m) => m ()
cmd_split_events = do
    (_, _, _, p) <- Selection.get_insert
    ModifyEvents.overlapping $ ModifyEvents.events $
        return . concatMap (split p)
    where
    split p event
        | not (Event.overlaps p event) || p == Event.start event = [event]
        | Event.positive event =
            [ Event.set_duration (p - Event.start event) event
            , Event.place p (Event.end event - p) event
            ]
        | otherwise =
            [ Event.place p (Event.end event - p) event
            , Event.set_duration (- (Event.start event - p)) event
            ]

-- | Zero dur events are never lengthened.
set_dur :: ScoreTime -> Event.Event -> Event.Event
set_dur dur evt
    | Event.duration evt == 0 = evt
    | otherwise = Event.set_duration dur evt

place :: ScoreTime -> ScoreTime -> Event.Event -> Event.Event
place start dur = Event.move (const start) . set_dur dur

-- | Insert empty space at the beginning of the selection for the length of
-- the selection, pushing subsequent events forwards.  If the selection is
-- a point, insert one timestep.
cmd_insert_time :: (Cmd.M m) => m ()
cmd_insert_time = do
    (block_id, tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- expand_range tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        track <- State.get_track track_id
        case Events.split_at_before start (Track.track_events track) of
            (_, []) -> return ()
            (_, events@(event:_)) -> do
                track_end <- State.track_event_end track_id
                -- +1 to get final event if it's 0 dur, see move_events
                State.remove_events track_id (min (Event.start event) start)
                    (track_end + 1)
                State.insert_block_events block_id track_id
                    (map (insert_time start end) events)

-- | Modify the event to insert time from @start@ to @end@, lengthening
-- it if @start@ falls within the event's duration.
--
-- TODO both insert_time and delete_time could be faster by just mapping the
-- shift once the overlapping events are done, but it's probably not worth it.
insert_time :: ScoreTime -> ScoreTime -> Event.Event -> Event.Event
insert_time start end event
    | Event.positive event = insertp event
    | otherwise = insertn event
    where
    shift = end - start
    pos = Event.start event
    insertp
        | pos < start && Event.end event <= start = id
        | pos < start = Event.modify_duration (+shift)
        | otherwise = Event.move (+shift)
    insertn
        | pos <= start = id
        | Event.end event < start =
            Event.move (+shift) . Event.modify_duration (subtract shift)
        | otherwise = Event.move (+shift)

-- | Remove the notes under the selection, and move everything else back.  If
-- the selection is a point, delete one timestep.
cmd_delete_time :: (Cmd.M m) => m ()
cmd_delete_time = do
    (block_id, tracknums, track_ids, start, end) <- Selection.tracks
    (start, end) <- expand_range tracknums start end
    when (end > start) $ forM_ track_ids $ \track_id -> do
        track <- State.get_track track_id
        case Events.split_at_before start (Track.track_events track) of
            (_, []) -> return ()
            (_, events@(event:_)) -> do
                track_end <- State.track_event_end track_id
                -- +1 to get final event if it's 0 dur, see move_events
                State.remove_events track_id (min (Event.start event) start)
                    (track_end + 1)
                State.insert_block_events block_id track_id
                    (mapMaybe (delete_time start end) events)

-- | Modify the event to delete the time from @start@ to @end@, shortening it
-- if @start@ falls within the event's duration.
delete_time :: ScoreTime -> ScoreTime -> Event.Event -> Maybe Event.Event
delete_time start end event
    | Event.positive event = deletep
    | otherwise = deleten
    where
    shift = end - start
    pos = Event.start event
    deletep
        | pos < start && Event.end event <= start = Just event
        | pos < start =
            Just $ Event.modify_duration
                (subtract (min (Event.end event - start) shift)) event
        | pos < end = Nothing
        | otherwise = Just $ Event.move (subtract shift) event
    deleten
        | pos <= start = Just event
        | pos <= end = Nothing
        | Event.end event < end = Just $
            Event.move (subtract shift) $ Event.modify_duration
                (+ min shift (end - Event.end event)) event
        | otherwise = Just $ Event.move (subtract shift) event

-- | If the range is a point, then expand it to one timestep.
expand_range :: (Cmd.M m) => [TrackNum] -> ScoreTime -> ScoreTime
    -> m (ScoreTime, ScoreTime)
expand_range (tracknum:_) start end
    | start == end = do
        block_id <- Cmd.get_focused_block
        step <- Cmd.get_current_step
        pos <- TimeStep.advance step block_id tracknum end
        return (start, fromMaybe end pos)
    | otherwise = return (start, end)
expand_range [] start end = return (start, end)

-- | If the insertion selection is a point, clear any event under it.  If it's
-- a range, clear all events within its half-open extent.
cmd_clear_selected :: (Cmd.M m) => m ()
cmd_clear_selected = do
    (_, _, track_ids, start, end) <- Selection.tracks
    clear_range track_ids start end

clear_range :: (State.M m) => [TrackId] -> TrackTime -> TrackTime -> m ()
clear_range track_ids start end =
    forM_ track_ids $ \track_id -> if start == end
        then State.remove_event track_id start
        else State.remove_events track_id start end

cmd_clear_and_advance :: (Cmd.M m) => m ()
cmd_clear_and_advance = do
    cmd_clear_selected
    (_, sel) <- Selection.get
    when (Types.sel_is_point sel
            && Types.sel_start_track sel == Types.sel_cur_track sel)
        Selection.advance

-- | If the TimeStep is AbsoluteMark or RelativeMark, set its rank.  Otherwise,
-- set it to the deflt.  This means the marklist names are sticky, so if you
-- set it manually the default bindings won't mess it up.
set_step_rank :: (Cmd.M m) => TimeStep.TimeStep -> Ruler.Rank -> m ()
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
toggle_absolute_relative_step :: (Cmd.M m) => m ()
toggle_absolute_relative_step = Cmd.modify_edit_state $ \st ->
        st { Cmd.state_time_step = toggle (Cmd.state_time_step st) }
    where
    toggle step = case TimeStep.to_list step of
        [TimeStep.AbsoluteMark names rank] ->
            TimeStep.time_step (TimeStep.RelativeMark names rank)
        [TimeStep.RelativeMark names rank] ->
            TimeStep.time_step (TimeStep.AbsoluteMark names rank)
        _ -> step

set_step :: (Cmd.M m) => TimeStep.TimeStep -> m ()
set_step step = Cmd.modify_edit_state $ \st -> st { Cmd.state_time_step = step }

cmd_invert_step_direction :: (Cmd.M m) => m ()
cmd_invert_step_direction = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_note_direction = invert (Cmd.state_note_direction st) }
    where
    invert TimeStep.Advance = TimeStep.Rewind
    invert TimeStep.Rewind = TimeStep.Advance

cmd_modify_octave :: (Cmd.M m) => (Pitch.Octave -> Pitch.Octave) -> m ()
cmd_modify_octave f = Cmd.modify_edit_state $ \st -> st
    { Cmd.state_kbd_entry_octave = f (Cmd.state_kbd_entry_octave st) }

-- | Toggle the note duration between the end of the block, and the current
-- time step.
toggle_note_duration :: (Cmd.M m) => m ()
toggle_note_duration = do
    dur <- Cmd.gets $ Cmd.state_note_duration . Cmd.state_edit
    step <- Cmd.gets $ Cmd.state_time_step . Cmd.state_edit
    Cmd.modify_edit_state $ \st ->
        st { Cmd.state_note_duration = if dur == to_end then step else to_end }
    where to_end = TimeStep.time_step TimeStep.BlockEdge

-- * modify text

strip_transformer :: (Cmd.M m) => m ()
strip_transformer = ModifyEvents.selection_advance $
    ModifyEvents.text $ ModifyEvents.pipeline $ drop 1

-- ** recent note

cmd_insert_recent :: (Cmd.M m) => Int -> m ()
cmd_insert_recent num = do
    recent <- Cmd.gets (Cmd.state_recent_notes . Cmd.state_edit)
    insert_recent =<< Cmd.require (lookup num recent)

insert_recent :: (Cmd.M m) => Cmd.RecentNote -> m ()
insert_recent (Cmd.RecentNote recent zero_dur) =
    EditUtil.modify_event zero_dur True (const (Just recent, True))
insert_recent (Cmd.RecentTransform recent zero_dur) = do
    pos <- EditUtil.get_pos
    EditUtil.modify_event_at pos zero_dur False $
        (\s -> (Just (replace s), True)) . fromMaybe ""
    where
    -- "a |" -> "x |"
    -- "a" -> "x | a"
    replace text
        | "|" `Text.isInfixOf` text =
            recent <> " " <> Text.dropWhile (/='|') text
        | otherwise =
            recent <> " |" <> (if Text.null text then "" else " " <> text)

-- | Try to record the current event in the LIFO recent note queue, as
-- documented in 'Cmd.state_recent_notes'.
--
-- 'Cmd.RecentNote's are matched only on their first word, so if you change
-- the args or zero_dur then an existing one will be replaced.  If you want
-- an arg to vary a lot it should probably be extracted into a signal anyway.
record_recent_note :: (Cmd.M m) => m ()
record_recent_note = do
    (_, _, track_id, pos) <- Selection.get_insert
    maybe_event <- State.get_event track_id pos
    whenJust (recent_note =<< maybe_event) $ \note ->
        Cmd.modify_edit_state $ \st -> st { Cmd.state_recent_notes =
            record_recent note (Cmd.state_recent_notes st) }

recent_note :: Event.Event -> Maybe Cmd.RecentNote
recent_note event
    | Text.null post =
        if Text.null pre then Nothing else Just $ Cmd.RecentNote pre zero_dur
    | otherwise = if Text.null pre then Nothing
        else Just $ Cmd.RecentTransform pre zero_dur
    where
    (pre, post) = (Text.strip *** Text.strip) $ Text.break (=='|')
        (Event.event_text event)
    zero_dur = Event.duration event == 0

record_recent :: Cmd.RecentNote -> [(Int, Cmd.RecentNote)]
    -> [(Int, Cmd.RecentNote)]
record_recent note recent0 = (key, note) : recent
    where
    recent = take (max_recent - 1) (filter (not . match note . snd) recent0)
    key = fromMaybe (length recent) $
        (fst <$> List.find ((==note) . snd) recent0)
        `mplus`
        Seq.head (filter (`notElem` map fst recent) [1..max_recent])
    max_recent = 4
    match (Cmd.RecentNote n1 _) (Cmd.RecentNote n2 _) =
        Seq.head (Text.words n1) == Seq.head (Text.words n2)
    match n1 n2 = n1 == n2


-- ** edit input

append_text :: (Cmd.M m) => m Cmd.Status
append_text = edit_open (const Nothing)

prepend_text :: (Cmd.M m) => m Cmd.Status
prepend_text = edit_open (const $ Just (0, 0))

-- | This will be fooled by a @|@ inside a string, but I'll fix that if it's
-- ever actually a problem.
replace_first_call :: (Cmd.M m) => m Cmd.Status
replace_first_call = edit_open $ \text -> case Text.breakOn "|" text of
    (pre, _) ->
        let space = " " `Text.isSuffixOf` pre
        in Just (0, Text.length pre - (if space then 1 else 0))

replace_last_call :: (Cmd.M m) => m Cmd.Status
replace_last_call = edit_open $ \text -> case Text.breakOnEnd "|" text of
    (pre, post) ->
        let space = " " `Text.isPrefixOf` post
        in Just (Text.length pre + (if space then 1 else 0), Text.length text)

edit_open :: (Cmd.M m) => (Text -> Maybe (Int, Int)) -> m Cmd.Status
edit_open selection = do
    view_id <- Cmd.get_focused_view
    (_, tracknum, track_id, pos) <- Selection.get_insert
    text <- event_text_at track_id pos
    return $ Cmd.EditInput $ Cmd.EditOpen view_id tracknum pos text
        (selection text)

event_text_at :: (State.M m) => TrackId -> ScoreTime -> m Text
event_text_at track_id pos = do
    events <- Track.track_events <$> State.get_track track_id
    return $ maybe "" Event.event_text $ Events.at pos events

-- ** handle edit input msg

-- | Handle UpdateInput that comes back from the floating edit input.
--
-- A leading space will create a zero duration event.
edit_input :: Bool -> Cmd.Cmd
edit_input zero_dur msg = do
    text <- Cmd.require $ edit_input_msg msg
    pos <- EditUtil.get_pos
    let space = " " `Text.isPrefixOf` text
    EditUtil.modify_event_at pos (zero_dur || space) False $
        const (Just (Text.strip text), False)
    record_recent_note
    return Cmd.Done

edit_input_msg :: Msg.Msg -> Maybe Text
edit_input_msg (Msg.Ui (UiMsg.UiMsg ctx
        (UiMsg.UiUpdate _ (UiMsg.UpdateInput text))))
    | UiMsg.ctx_edit_input ctx = Just text
edit_input_msg _ = Nothing
