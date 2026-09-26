-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Event editing commands.  This is where generic event editing commands go.
    More specialized ones, like copy and paste and control or note track
    commands, go in their own modules.
-}
module Cmd.Edit (
    -- * global editing state
    cmdToggleValEdit
    , cmdToggleMethodEdit
    , cmdToggleKbdEntry
    , cmdToggleValEditKbdEntry
    , setStepRank
    , setStep
    , toggleAbsoluteRelativeStep
    , cmdToggleNoteOrientation
    , cmdModifyOctave
    , toggleAdvance
    , toggleChord

    -- * event start and duration
    , cmdMoveEventForward, cmdMoveEventBackward
    , cmdSetDuration
    , cmdToggleZeroTimestep
    , cmdSetStart
    , modifyDur
    , cmdJoinEvents
    , cmdSplitEvents
    , cmdInsertTime
    , cmdDeleteTime
    , deleteBlockTime
    , cmdClearSelected
    , clearRange
    , cmdClearAndAdvance
    , toggleNoteDuration
    , cmdSetCallDuration
    , cmdInvertOrientation

    -- * modify text
    , cmdToggleCommented
    , stripTransformer

    -- * record action
    , saveLastActionTo
    , runActionAt

    -- * floating text input
    , handleFloatingInput
    , appendText
    , prependText
    , replaceLastCall
    , replaceFirstCall
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified App.Config as Config
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
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Meter.Meter as Meter
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Zoom as Zoom

import           Global
import           Types


-- * global editing state

-- | Unlike the other toggle commands, val edit, being the \"default\" toggle,
-- always turns other modes off.  So you can't switch directly from some other
-- kind of edit to val edit.
cmdToggleValEdit :: Cmd.M m => m ()
cmdToggleValEdit = modifyEditMode $ \m -> case m of
    Cmd.NoEdit -> Cmd.ValEdit
    _ -> Cmd.NoEdit

cmdToggleMethodEdit :: Cmd.M m => m ()
cmdToggleMethodEdit = modifyEditMode $ \m -> case m of
    Cmd.MethodEdit -> Cmd.ValEdit
    Cmd.ValEdit -> Cmd.MethodEdit
    _ -> m

-- | Toggle kbd entry mode, putting a K in the edit box as a reminder.  This is
-- orthogonal to the previous edit modes.
cmdToggleKbdEntry :: Cmd.M m => m ()
cmdToggleKbdEntry = Cmd.modifyEditState $ \st ->
    st { Cmd.stateKbdEntry = not (Cmd.stateKbdEntry st) }

cmdToggleValEditKbdEntry :: Cmd.M m => m ()
cmdToggleValEditKbdEntry = Cmd.modifyEditState $ \st ->
    if Cmd.stateEditMode st == Cmd.ValEdit
        then st
            { Cmd.stateEditMode = Cmd.NoEdit
            , Cmd.stateKbdEntry = False
            }
        else st
            { Cmd.stateEditMode = Cmd.ValEdit
            , Cmd.stateKbdEntry = True
            }

-- | If the TimeStep is AbsoluteMark or RelativeMark, set its rank.  Otherwise,
-- set it to the deflt.  This means the marklist names are sticky, so if you
-- set it manually the default bindings won't mess it up.
setStepRank :: Cmd.M m => TimeStep.TimeStep -> Meter.Rank -> m ()
setStepRank deflt rank = Cmd.modifyEditState $ \st ->
    st { Cmd.stateTimeStep =
        set (TimeStep.toList (Cmd.stateTimeStep st)) }
    where
    set [TimeStep.AbsoluteMark names _] =
        TimeStep.timeStep (TimeStep.AbsoluteMark names rank)
    set [TimeStep.RelativeMark names _] =
        TimeStep.timeStep (TimeStep.RelativeMark names rank)
    set _ = deflt

setStep :: Cmd.M m => TimeStep.TimeStep -> m ()
setStep step = Cmd.modifyEditState $ \st -> st { Cmd.stateTimeStep = step }

-- | Toggle between absolute and relative mark step.
toggleAbsoluteRelativeStep :: Cmd.M m => m ()
toggleAbsoluteRelativeStep = Cmd.modifyEditState $ \st ->
        st { Cmd.stateTimeStep = toggle (Cmd.stateTimeStep st) }
    where
    toggle step = case TimeStep.toList step of
        [TimeStep.AbsoluteMark names rank] ->
            TimeStep.timeStep (TimeStep.RelativeMark names rank)
        [TimeStep.RelativeMark names rank] ->
            TimeStep.timeStep (TimeStep.AbsoluteMark names rank)
        _ -> step

cmdToggleNoteOrientation :: Cmd.M m => m ()
cmdToggleNoteOrientation = Cmd.modifyEditState $ \st -> st
    { Cmd.stateNoteOrientation = Types.invert (Cmd.stateNoteOrientation st)
    }

cmdModifyOctave :: Cmd.M m => (Pitch.Octave -> Pitch.Octave) -> m ()
cmdModifyOctave f = Cmd.modifyEditState $ \st -> st
    { Cmd.stateKbdEntryOctave = f (Cmd.stateKbdEntryOctave st) }

-- ** util

modifyEditMode :: Cmd.M m => (Cmd.EditMode -> Cmd.EditMode) -> m ()
modifyEditMode f = Cmd.modifyEditState $ \st ->
    st { Cmd.stateEditMode = f (Cmd.stateEditMode st) }

toggleAdvance :: Cmd.M m => m ()
toggleAdvance = modifyAdvance not

modifyAdvance :: Cmd.M m => (Bool -> Bool) -> m ()
modifyAdvance f = Cmd.modifyEditState $ \st ->
    st { Cmd.stateAdvance = f (Cmd.stateAdvance st) }

toggleChord :: Cmd.M m => m ()
toggleChord = modifyChord not

modifyChord :: Cmd.M m => (Bool -> Bool) -> m ()
modifyChord f = Cmd.modifyEditState $ \st ->
    st { Cmd.stateChord = f (Cmd.stateChord st) }

toggleRecordVelocity :: Cmd.CmdL ()
toggleRecordVelocity = modifyRecordVelocity not

modifyRecordVelocity :: (Bool -> Bool) -> Cmd.CmdL ()
modifyRecordVelocity f = Cmd.modifyEditState $ \st ->
    st { Cmd.stateRecordVelocity = f (Cmd.stateRecordVelocity st) }

-- * event start and duration

-- | Insert an event at the current insert pos.
insertEvent :: Cmd.M m => Text -> ScoreTime -> m ()
insertEvent text dur = do
    (blockId, _, trackId, pos) <- Selection.getInsert
    Ui.insertBlockEvents blockId trackId [Event.event pos dur text]

-- | Different from 'cmd_insert_time' and 'cmd_delete_time' since it only
-- moves some events.  Move back the next event, or move forward the previous
-- event.  Move the next or previous event for a point selection, or the
-- selected events.
cmdMoveEventForward :: Cmd.M m => m ()
cmdMoveEventForward = moveEvents TimeStep.Advance

cmdMoveEventBackward :: Cmd.M m => m ()
cmdMoveEventBackward = moveEvents TimeStep.Rewind

{- |
    . If selection is a point, expand it to the next or prev event.
    . Delete events from pos to end of selection.
    . Select all events in selection, move by pos - Event.start affected.
-}
moveEvents :: Cmd.M m => TimeStep.Direction -> m ()
moveEvents dir = do
    (blockId, _, trackIds, range) <- Selection.tracks
    tracks <- zip trackIds <$> mapM Ui.getEvents trackIds
    -- Use the first selected track as the reference for how much to move.
    -- This is predictable so let's try this for now.
    eventRange <- expand range . snd <$> Cmd.abortUnless (Lists.head tracks)
    affected <- Cmd.abortUnless $
        NonEmpty.nonEmpty . Events.ascending . Events.inRange eventRange . snd
            =<< Lists.head tracks
    let pos = case dir of
            TimeStep.Rewind -> Events.rangeStart range
            TimeStep.Advance -> Events.rangeEnd range
    let delta = (pos -) $ Event.start $ case dir of
            TimeStep.Rewind -> NonEmpty.head affected
            TimeStep.Advance -> NonEmpty.last affected
    when (delta /= 0) $ forM_ tracks $ \(tid, events) -> do
        Ui.removeEventsRange tid eventRange
        Ui.insertBlockEvents blockId tid $
            map (Event.start_ %= (+delta)) $ Events.ascending $
            Events.inRange eventRange events
    where
    expand range events = case range of
        Events.Point pos _orient ->
            maybe range Events.eventRange $ Lists.head $ case dir of
                TimeStep.Rewind -> Events.atAfter pos events
                TimeStep.Advance -> Events.atBefore pos events
        Events.Range {} -> range

-- | Extend the events in the selection to either the end of the selection or
-- the beginning of the next note, whichever is shorter.
--
-- If the selection is on an event, the previous or next one (depending on
-- 'Types.Orientation') is extended instead.  This is more useful than reducing
-- the event to 0, which has its own cmd anyway.  If the selection is between
-- a positive and negative event, the one corresponding to 'Sel.orientation' is
-- selected.
cmdSetDuration :: Cmd.M m => m ()
cmdSetDuration = modifyNeighbor modify =<< Selection.context
    where
    modify (start, end) event
        | Event.isNegative event = setDur (start - Event.start event) event
        | otherwise = setDur (end - Event.start event) event

-- | Similar to 'ModifyEvents.event', but if the selection is a point, modify
-- the previous or next event, depending on if it's positive or negative.
modifyNeighbor :: Ui.M m
    => ((ScoreTime, ScoreTime) -> Event.Event -> Event.Event)
    -> Selection.Context -> m ()
modifyNeighbor modify ctx
    | Sel.isPoint sel = prevOrNext (Selection.selPoint sel)
    | otherwise = selection (Sel.range sel)
    where
    sel = Selection.ctxSelection ctx
    -- TODO Should I integrate this into ModifyEvents?
    prevOrNext pos = do
        let blockId = Selection.ctxBlockId ctx
        tracks <- avoidExactMatch ctx
        forM_ tracks $ \(trackId, event) -> do
            tracknum <- Ui.getTracknumOf blockId trackId
            -- It's confusing to modify collapsed tracks because you don't
            -- see the change.  TODO this is recreating the stuff in
            -- ModifyEvents.
            unlessM (Ui.trackCollapsed blockId tracknum) $ do
                Ui.removeEvent trackId event
                Ui.insertEvent trackId $ modify (pos, pos) event
    selection range = ModifyEvents.ctxSelectionVisible
        (ModifyEvents.events $ pure . map (modify range)) ctx

-- | Like 'Selection.events_around', but if a point selection is on an event
-- start, find a neighbor instead of matching that event.
avoidExactMatch :: Ui.M m => Selection.Context -> m [(TrackId, Event.Event)]
avoidExactMatch ctx =
    Lists.mapMaybeSnd select <$> Selection.ctxEventsAround ctx
    where
    point = Selection.selPoint (Selection.ctxSelection ctx)
    select triple = case triple of
        (_, [within], _) | Event.start within /= point -> Just within
        (prev:_, [within], _) | positive within && positive prev -> Just prev
        (_, [within], next:_) | negative within && negative next -> Just next
        _ -> Nothing
    positive = Event.isPositive
    negative = Event.isNegative

{- | Toggle duration between zero and non-zero.

    If the event is non-zero, then make it zero.  Otherwise, set its end to the
    cursor.  Unless the cursor is on the event start, and then extend it by
    a timestep.

    Also I previously used the same event selection strategy as
    'cmd_set_duration', which avoided the awkward point selection on zero-dur
    event case, but it turned out to be unintuitive to use in practice, because
    to toggle an event I'd have to put the selection on the next event.
-}
cmdToggleZeroTimestep :: Cmd.M m => m ()
cmdToggleZeroTimestep = do
    sel <- Selection.get
    ModifyEvents.selectionVisible $ \blockId trackId events -> do
        tracknum <- Ui.getTracknumOf blockId trackId
        let (start, end) = Sel.range sel
        Just <$> mapM (toggleZeroTimestep blockId tracknum start end) events

toggleZeroTimestep :: Cmd.M m => BlockId -> TrackNum
    -> TrackTime -> TrackTime -> Event.Event -> m Event.Event
toggleZeroTimestep blockId tracknum start end event
    | Event.duration event /= 0 = pure $
        Event.duration_ #= (if Event.isNegative event then -0 else 0) $ event
    | start == end && start == Event.start event = do
        step <- Cmd.getCurrentStep
        maybePos <- TimeStep.stepFrom
            (if Event.isNegative event then -1 else 1) step blockId tracknum
            start
        case maybePos of
            Nothing -> pure event
            Just pos -> pure $ Event.setEnd pos event
    | Event.isNegative event =
        pure $ Event.duration_ #= (start - Event.start event) $ event
    | otherwise = pure $ Event.duration_ #= (end - Event.start event) $ event

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
cmdSetStart :: Cmd.M m => m ()
cmdSetStart = do
    pos <- Selection.point
    mapM_ (process pos) =<< Selection.oppositeNeighbor
    where
    process pos (trackId, event) = do
        Ui.removeEvent trackId event
        Ui.insertEvent trackId $ setStart pos event
    setStart p event
        | Event.duration event == 0 = Event.start_ #= p $ event
        | otherwise = Event.setStart p event

-- | Modify event durations by applying a function to them.  0 durations
-- are passed through, so you can't accidentally give control events duration.
modifyDur :: Cmd.M m => (ScoreTime -> ScoreTime) -> m ()
modifyDur f = ModifyEvents.selectionVisible $ ModifyEvents.event $
    Event.duration_ %= apply
    where apply dur = if dur == 0 then dur else f dur

-- | If there is a following event, delete it and extend this one to its end.
--
-- Since 0 dur events are never lengthened, joining control events simply
-- deletes the later ones.
--
-- If it's a point, only join up to (and including) the nearest event start.
-- Otherwise, if there's a collapsed pitch track and the next event doesn't
-- have a pitch event, it's easy to wind up deleting a distance pitch and not
-- noticing.
cmdJoinEvents :: Cmd.M m => m ()
cmdJoinEvents = joinSelected =<< Selection.eventsAround
    where
    joinSelected tracks = mapM_ (joinTrack nearest) tracks
        where nearest = Lists.minimumOn abs $ mapMaybe nearestOf tracks
    nearestOf (_, (prevs, [cur], nexts))
        | Event.isPositive cur =
            subtract (Event.start cur) . Event.start <$> Lists.head nexts
        | otherwise =
            subtract (Event.start cur) . Event.start <$> Lists.head prevs
    nearestOf _ = Nothing

    joinTrack distance (trackId, selected) = case selected of
        (_, [cur], next:_) | Event.isPositive cur
                && maybe True (Event.start next <=) nearest ->
            join trackId cur next
            where nearest = (Event.start cur +) <$> distance
        (prev:_, [cur], _) | Event.isNegative cur
                && maybe True (Event.start prev >=) nearest ->
            join trackId prev cur
            where nearest = (Event.start cur +) <$> distance
        (_, events@(_:_:_), _) -> join trackId (head events) (last events)
        _ -> pure ()
    join trackId evt1 evt2 =
        case (Event.isPositive evt1, Event.isPositive evt2) of
            (True, True) -> do
                mapM_ (Ui.removeEvent trackId) [evt1, evt2]
                Ui.insertEvent trackId $
                    setDur (Event.end evt2 - Event.start evt1) evt1
            (False, False) -> do
                mapM_ (Ui.removeEvent trackId) [evt1, evt2]
                Ui.insertEvent trackId $ if Event.duration evt2 == 0
                    then evt2
                    else Event.setEnd (Event.end evt1) evt2
            _ -> pure () -- no sensible way to join these


-- | Split the events under the cursor.
cmdSplitEvents :: Cmd.M m => m ()
cmdSplitEvents = do
    p <- Selection.point
    ModifyEvents.overlapping $ ModifyEvents.events $
        pure . concatMap (split p)
    where
    split p event
        | not (Event.overlaps p event) || p == Event.start event = [event]
        | otherwise =
            [ Event.duration_ #= p - Event.start event $  event
            , Event.place p (Event.end event - p) event
            ]

-- | The same as 'Event.set_duration' except don't modify a zero dur event.
setDur :: TrackTime -> Event.Event -> Event.Event
setDur dur evt
    | Event.duration evt == 0 = evt
    | otherwise = Event.duration_ #= dur $ evt

-- | Insert empty space at the beginning of the selection for the length of
-- the selection, pushing subsequent events forwards.  If the selection is
-- a point, insert one timestep.
cmdInsertTime :: Cmd.M m => m ()
cmdInsertTime = do
    (blockId, tracknums, trackIds, range) <- Selection.tracks
    let (start, end) = Events.rangeTimes range
    (start, end) <- pointToTimestep tracknums start end
    when (end > start) $ forM_ trackIds $ \trackId -> do
        events <- Ui.getEvents trackId
        case Events.splitAtBefore start events of
            (_, []) -> pure ()
            (_, events@(event:_)) -> do
                Ui.modifyEventsFrom trackId (min (Event.start event) start)
                    (const mempty)
                -- The above won't get a negative event at start.
                Ui.removeEvent trackId event
                Ui.insertBlockEvents blockId trackId $
                    map (insertEventTime start (end-start)) events

-- | Modify the event to insert time, lengthening it if the start time falls
-- within the event's duration.
insertEventTime :: TrackTime -> TrackTime -> Event.Event -> Event.Event
insertEventTime start shift event
    | Event.isPositive event = if
        | start <= Event.start event -> Event.start_ %= (+shift) $ event
        | start < Event.end event -> Event.duration_ %= (+shift) $ event
        | otherwise -> event
    | otherwise = if
        | start <= Event.end event -> Event.start_ %= (+shift) $ event
        | start < Event.start event ->
            Event.setStart (Event.start event + shift) event
        | otherwise -> event

-- | Remove the notes under the selection, and move everything else back.  If
-- the selection is a point, delete one timestep.
cmdDeleteTime :: Cmd.M m => m ()
cmdDeleteTime = do
    (blockId, tracknums, trackIds, range) <- Selection.tracks
    let (start, end) = Events.rangeTimes range
    (start, end) <- pointToTimestep tracknums start end
    when (end > start) $ forM_ trackIds $ \trackId ->
        deleteTime blockId trackId start (end-start)

-- | Delete the time range for all tracks in the block.
deleteBlockTime :: Ui.M m => BlockId -> TrackTime -> TrackTime -> m ()
deleteBlockTime blockId start dur = do
    trackIds <- Ui.trackIdsOf blockId
    forM_ trackIds $ \trackId ->
        deleteTime blockId trackId start dur

deleteTime :: Ui.M m => BlockId -> TrackId -> TrackTime -> TrackTime -> m ()
deleteTime blockId trackId start dur = do
    when (dur < 0) $
        Ui.throw $ "delete_time: negative dur " <> pretty dur
    events <- Ui.getEvents trackId
    case Events.splitAtBefore start events of
        (_, []) -> pure ()
        (_, events@(event:_)) -> do
            Ui.modifyEventsFrom trackId (min (Event.start event) start)
                (const mempty)
            -- The above won't get a negative event at start.
            Ui.removeEvent trackId event
            Ui.insertBlockEvents blockId trackId $
                mapMaybe (deleteEventTime start dur) events

-- | Modify the event to delete time, shortening it the start time falls within
-- the event's duration, or removing it entirely if its 'Event.start' was
-- deleted.
--
-- This is more complicated than 'insert_event_time' because it can delete
-- events, and because the event may only be partially shortened if the range
-- overlaps its end.
deleteEventTime :: TrackTime -> TrackTime -> Event.Event -> Maybe Event.Event
deleteEventTime start shift event
    | Event.isPositive event = if
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
            Just $ Event.start_ %= subtract shift $
                Event.duration_ %= (+ (min (end - Event.end event) shift)) $
                event
        | start < Event.start event -> Nothing
        | otherwise -> Just event
    where end = start + shift

-- | If the range is a point, then expand it to one timestep.
pointToTimestep :: Cmd.M m => [TrackNum] -> TrackTime -> TrackTime
    -> m (TrackTime, TrackTime)
pointToTimestep (tracknum:_) start end
    | start == end = do
        blockId <- Cmd.getFocusedBlock
        step <- Cmd.getCurrentStep
        pos <- TimeStep.advance step blockId tracknum end
        pure (start, fromMaybe end pos)
    | otherwise = pure (start, end)
pointToTimestep [] start end = pure (start, end)

-- | If the insertion selection is a point, clear any event under it.  If it's
-- a range, clear all events within its half-open extent.
cmdClearSelected :: Cmd.M m => m ()
cmdClearSelected = do
    (_, _, trackIds, range) <- Selection.tracks
    clearRange trackIds range

clearRange :: Ui.M m => [TrackId] -> Events.Range -> m ()
clearRange trackIds range =
    forM_ trackIds $ \trackId -> Ui.removeEventsRange trackId range

cmdClearAndAdvance :: Cmd.M m => m ()
cmdClearAndAdvance = do
    ctx <- Selection.context
    (_, _, trackIds, range) <- Selection.ctxTracks ctx
    let sel = Selection.ctxSelection ctx
    let isPoint = Sel.isPoint sel && Sel.startTrack sel == Sel.curTrack sel
    removed <- if isPoint
        then not . Events.null . Events.inRange range <$>
            Ui.getEvents (head trackIds)
        else pure False
    clearRange trackIds range
    when isPoint $ do
        -- Originally Cmd.NoteTrack deleted the pitch track too, but it seems
        -- more consistent to use the collapsed track if you want that.
        -- -- Clear out the pitch track too.
        -- maybe_pitch <- Info.pitch_of_note block_id sel_tracknum
        -- whenJust maybe_pitch $ \pitch -> EditUtil.remove_event_at
        --     (EditUtil.Pos block_id (Ui.track_tracknum pitch) pos 0)
        durStep <- Cmd.gets $ Cmd.stateNoteDuration . Cmd.stateEdit
        when (removed && durStep == TimeStep.eventEdge) $
            extendPrevious ctx
        Selection.advance

-- | Get previous event, if positive and its end >= pos, then set its end to
-- the next event or end of the block.  Or the reverse, if negative.
extendPrevious :: Ui.M m => Selection.Context -> m ()
extendPrevious ctx = do
    let blockId = Selection.ctxBlockId ctx
    prev <- TimeStep.rewind TimeStep.eventEdge blockId tracknum pos
    next <- TimeStep.advance TimeStep.eventEdge blockId tracknum pos
    modifyNeighbor (const (extend prev next)) ctx
    where
    extend prev next event
        | not $ eventTouches (Selection.selPoint sel) event = event
        | Event.isPositive event, Just next <- next =
            Event.end_ #= next $ event
        | Event.isNegative event, Just prev <- prev =
            Event.end_ #= prev $ event
        | otherwise = event
    sel = Selection.ctxSelection ctx
    pos = Selection.selPoint sel
    tracknum = Selection.selPointTrack sel

eventTouches :: TrackTime -> Event.Event -> Bool
eventTouches pos event =
    Event.max event ScoreTime.== pos || Event.min event ScoreTime.== pos

-- | Toggle the note duration between the end of the block, and the current
-- time step.
toggleNoteDuration :: Cmd.M m => m ()
toggleNoteDuration = do
    dur <- Cmd.gets $ Cmd.stateNoteDuration . Cmd.stateEdit
    step <- Cmd.gets $ Cmd.stateTimeStep . Cmd.stateEdit
    Cmd.modifyEditState $ \st ->
        st { Cmd.stateNoteDuration = if dur == toEnd then step else toEnd }
    where toEnd = TimeStep.eventEdge

-- ** fancier start\/duration edits

cmdSetCallDuration :: Cmd.M m => m ()
cmdSetCallDuration = ModifyEvents.selectionVisible $
    \blockId trackId events ->
        Just <$> mapM (setCallDuration blockId trackId) events

-- | Set the event duration to the CallDuration of its call.  For block calls,
-- this is the natural block duration.
setCallDuration :: Cmd.M m => BlockId -> TrackId -> Event.Event
    -> m Event.Event
setCallDuration blockId trackId event =
    maybe event (\d -> Event.duration_ #= d $ event) <$>
        lookupCallDuration blockId trackId event

-- | Evaluate the given event to find its 'Derive.get_call_duration'.
lookupCallDuration :: Cmd.M m => BlockId -> TrackId -> Event.Event
    -> m (Maybe TrackTime)
lookupCallDuration blockId trackId event =
    Perf.lookupNoteDeriver blockId trackId event >>= \case
        Nothing -> pure Nothing
        Just deriver -> do
            result <- Perf.deriveAtThrow blockId trackId $
                Derive.getScoreDuration deriver
            pure $ case result of
                Left _ -> Nothing
                Right Derive.Unknown -> Just $ Event.duration event
                Right (Derive.CallDuration dur) -> Just dur

cmdInvertOrientation :: Cmd.M m => m ()
cmdInvertOrientation = do
    trackIds <- Selection.trackIds
    let isControl = fmap ParseTitle.isControlTrack . Ui.getTrackTitle
    ifM (allM isControl trackIds) invertEvents invertNotes

invertEvents :: Cmd.M m => m ()
invertEvents =
    ModifyEvents.modifySelected ModifyEvents.ExcludeCollapsed modify
        =<< Selection.eventsAtPoint
    where
    modify = ModifyEvents.track $ ModifyEvents.event $ \e ->
        Event.start_ %= (+ Event.duration e) $ Event.duration_ %= negate $ e

invertNotes :: Cmd.M m => m ()
invertNotes = ModifyNotes.selection $ ModifyNotes.note invert
    where
    invert note = invertNote $
        ModifyNotes.controls %= fmap (invertControl note) $ note
    invertNote note = ModifyNotes.start %= (+ ModifyNotes.noteDuration note) $
        ModifyNotes.duration %= negate $ note
    -- If there's exactly one event at the start time, I can flip it with the
    -- note.
    invertControl note events = case Events.ascending events of
        [event] | Event.duration event == 0 && Event.start event == start ->
            Events.singleton $ Event.duration_ #= dur $
                Event.start_ #= ModifyNotes.noteEnd note $ event
        _ -> events
        where
        dur = case ModifyNotes.noteOrientation note of
            Types.Positive -> -0
            Types.Negative -> 0
        start = ModifyNotes.noteStart note

-- * modify text

cmdToggleCommented :: Cmd.M m => m ()
cmdToggleCommented = ModifyEvents.selectionTracks toggle
    where
    toggle blockId trackEvents
        | all (all (isCommented) . snd) trackEvents =
            modify $ Text.drop (Text.length cmt)
        | otherwise = modify $ \t ->
            if cmt `Text.isPrefixOf` t then t else "--|" <> t
        where
        modify f = ModifyEvents.track (ModifyEvents.text f)
            blockId trackEvents
    isCommented = (cmt `Text.isPrefixOf`) . Event.text
    cmt = "--|"

-- | Strip off the first transformer, and then the generator.
stripTransformer :: Cmd.M m => m ()
stripTransformer = do
    ModifyEvents.selectedTrack $
        ModifyEvents.text $ ModifyEvents.pipeline $ drop 1
    ModifyEvents.advanceIfPoint

-- * record action

-- | If you create a new event, and there is explicit duration, then use it.
makeAction :: Maybe Text -> Text -> Maybe TrackTime -> Cmd.Action
makeAction maybeOld new dur = case maybeOld of
    Nothing -> Cmd.InsertEvent dur new
    Just old
        -- This is a way to record a ReplaceText for an existing event.
        | old == new || old == "" -> Cmd.ReplaceText new
        | old `Text.isPrefixOf` new ->
            Cmd.AppendText $ Text.drop (Text.length old) new
        | old `Text.isSuffixOf` new ->
            Cmd.PrependText $ Text.take (Text.length new - Text.length old) new
        | otherwise -> Cmd.ReplaceText new

insertRecordedAction :: Cmd.M m => Char -> Cmd.Action -> m ()
insertRecordedAction key action = Cmd.modifyEditState $ \st -> st
    { Cmd.stateRecordedActions =
        Map.insert key action (Cmd.stateRecordedActions st)
    }

saveLastActionTo :: Cmd.M m => Char -> m ()
saveLastActionTo key = insertRecordedAction key =<< getAction '.'

runActionAt :: Cmd.M m => Char -> m ()
runActionAt key = runAction =<< getAction key

getAction :: Cmd.M m => Char -> m Cmd.Action
getAction key = Cmd.abortUnless
    =<< Cmd.gets (Map.lookup key . Cmd.stateRecordedActions . Cmd.stateEdit)

runAction :: Cmd.M m => Cmd.Action -> m ()
runAction action = do
    pos <- EditUtil.getPos
    let modify = EditUtil.modifyEventAt pos False False
    case action of
        Cmd.InsertEvent maybeDur text -> do
            let newPos = case pos of
                    EditUtil.Pos blockId tracknum start _dur ->
                        EditUtil.Pos blockId tracknum start
                            (fromMaybe 0 maybeDur)
            EditUtil.modifyEventAt newPos (maybeDur == Just 0) True $
                const (Just text, True)
        Cmd.ReplaceText text -> modify $ const (Just text, True)
        Cmd.PrependText text -> modify $ \old -> case old of
            Nothing -> (Nothing, False)
            Just old -> (Just $ text <> old, True)
        Cmd.AppendText text -> modify $ \old -> case old of
            Nothing -> (Nothing, False)
            Just old -> (Just $ old <> text, True)

-- * floating text input

appendText :: Cmd.M m => m Cmd.Status
appendText = openFloating (\text -> (Text.length text, Text.length text))

prependText :: Cmd.M m => m Cmd.Status
prependText = openFloating (const (0, 0))

-- | This will be fooled by a @|@ inside a string, but I'll fix that if it's
-- ever actually a problem.
replaceFirstCall :: Cmd.M m => m Cmd.Status
replaceFirstCall = openFloating $ \text -> case Text.breakOn "|" text of
    (pre, _) ->
        let space = " " `Text.isSuffixOf` pre
        in (0, Text.length pre - (if space then 1 else 0))

replaceLastCall :: Cmd.M m => m Cmd.Status
replaceLastCall = openFloating $ \text -> case Text.breakOnEnd "|" text of
    (pre, post) ->
        let space = " " `Text.isPrefixOf` post
        in (Text.length pre + (if space then 1 else 0), Text.length text)

-- | If a selection is used to create an event, this is where the event's start
-- is.
editPoint :: Sel.Selection -> TrackTime
editPoint sel = case Sel.orientation sel of
    Sel.Negative -> Sel.max sel
    _ -> Sel.min sel

-- | Open a floating text entry with a selection set.
openFloating :: Cmd.M m => (Text -> (Int, Int)) -> m Cmd.Status
openFloating selection = do
    (viewId, sel) <- Selection.getViewSel
    (_, tracknum, trackId, _) <- Selection.getInsert
    let pos = editPoint sel
    maybeEvent <- eventAt trackId pos (Sel.eventOrientation sel)
    let text = maybe "" Event.text maybeEvent
        orient = maybe (Sel.eventOrientation sel) Event.orientation maybeEvent
    zoom <- Block.viewZoom <$> Ui.getView viewId
    -- If I'm editing or opening a negative event, move the input up since
    -- the text will also be above the trigger.
    let openPos = pos - case orient of
            Types.Negative -> Zoom.toTime zoom Config.trackTitleHeight
            Types.Positive -> 0
    pure $ Cmd.FloatingInput $ Cmd.FloatingOpen viewId tracknum openPos text
        (selection text)

eventTextAt :: Ui.M m => TrackId -> TrackTime -> Types.Orientation
    -> m (Maybe Text)
eventTextAt trackId pos = fmap (fmap Event.text) . eventAt trackId pos

eventAt :: Ui.M m => TrackId -> TrackTime -> Types.Orientation
    -> m (Maybe Event.Event)
eventAt trackId pos orient = Events.at pos orient <$> Ui.getEvents trackId

-- ** handle floating input msg

-- | Handle UpdateInput that comes back from the floating input.
--
-- A leading space will create a zero duration event.
handleFloatingInput :: Cmd.M m =>
    Bool -- ^ True to always create a zero duration event.
    -> Msg.Msg -> m Cmd.Status
handleFloatingInput alwaysZeroDur msg = do
    text <- Cmd.abortUnless $ floatingInputMsg msg
    EditUtil.Pos blockId tracknum start dur <- EditUtil.getPos
    trackId <- Cmd.require "handle_floating_input on non-event track"
        =<< Ui.eventTrackAt blockId tracknum
    oldText <- eventTextAt trackId start (Event.orientationOf dur)
    let zeroDur = alwaysZeroDur || " " `Text.isPrefixOf` text
    EditUtil.modifyEventAt (EditUtil.Pos blockId tracknum start dur)
        zeroDur False (const (Just (Text.strip text), False))
    -- 0 dur means a point selection, which means to use the time step.
    insertRecordedAction '.' $ makeAction oldText (Text.strip text) $ if
        | zeroDur -> Just (if ScoreTime.isNegative dur then -0 else 0)
        | dur == 0 -> Nothing
        | otherwise -> Just dur
    when (oldText == Nothing) $
        trySetCallDuration blockId trackId start (Event.orientationOf dur)
    pure Cmd.Done

-- | Set the event's duration to its CallDuration, if it has one.
trySetCallDuration :: Cmd.M m => BlockId -> TrackId -> TrackTime
    -> Types.Orientation -> m ()
trySetCallDuration blockId trackId pos orient =
    whenJustM (eventAt trackId pos orient) $ \event ->
        whenJustM (lookupCallDuration blockId trackId event) $ \dur ->
            when (dur /= Event.duration event) $
                Ui.insertEvent trackId (Event.duration_ #= dur $ event)

floatingInputMsg :: Msg.Msg -> Maybe Text
floatingInputMsg (Msg.Ui (UiMsg.UiMsg ctx
        (UiMsg.UiUpdate _ (UiMsg.UpdateInput (Just text)))))
    | UiMsg.ctxFloatingInput ctx = Just text
floatingInputMsg _ = Nothing
