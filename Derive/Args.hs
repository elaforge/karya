-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances #-}
-- | Extract things from the PassedArgs data structure.
module Derive.Args where
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track

import qualified Derive.Derive as Derive
import Derive.Derive (PassedArgs, Context)
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


context :: PassedArgs a -> Context a
context = Derive.passed_ctx

event :: PassedArgs a -> Event.Event
event = Derive.ctx_event . context

orientation :: PassedArgs a -> Event.Orientation
orientation = Event.orientation . event

negative :: PassedArgs a -> Bool
negative = (==Event.Negative) . orientation

-- * prev and next

{- NOTE [previous-pitch]
    There are many ways to get the previous pitch:

    'prev_pitch' - The simplest, it uses 'Derive.ctx_prev_val', so it only
    works when you are in a pitch track, and only gets the immediately previous
    pitch.  A special hack should make it work even if the pitch track is
    inverted and the previous pitch is in a different slice.

    'prev_event_pitch' - This is like 'prev_pitch', except that it works at
    the note track level.  It gets the entire previous event and picks out the
    last pitch it played.

    'lookup_prev_pitch' - This uses 'Derive.state_note_track', which means that
    it should work even in an inversion under a note track.

    'lookup_prev_logical_pitch' - This is actually an entirely different
    mechanism than the others.  Documented in 'Derive.NotePitchQuery'.

    Clearly this is too many.  Probably I can come up with a way to combine the
    first three.
-}

-- ** from 'Derive.ctx_prev_val'

prev_control :: Derive.ControlArgs -> Maybe (RealTime, Signal.Y)
prev_control = Signal.last <=< prev_val

prev_pitch :: Derive.PitchArgs -> Maybe (RealTime, PSignal.Pitch)
prev_pitch = PSignal.last <=< prev_val

prev_event :: Derive.NoteArgs -> Maybe Score.Event
prev_event = prev_val

prev_event_pitch :: PassedArgs Score.Event -> Maybe PSignal.Pitch
prev_event_pitch args =
    fmap snd . PSignal.last . Score.event_untransformed_pitch =<< prev_val args

-- | Polymorphic version of 'prev_control' or 'prev_pitch'.
prev_val_end :: Derive.Taggable a => PassedArgs a -> Maybe RealTime
prev_val_end = extract <=< prev_val
    where
    extract val = case Derive.to_tagged val of
        Derive.TagEvent event -> Just $ Score.event_end event
        Derive.TagControl sig -> fst <$> Signal.last sig
        Derive.TagPitch sig -> fst <$> PSignal.last sig

-- | Get the previous val.  See NOTE [prev-val].
prev_val :: PassedArgs a -> Maybe a
prev_val = Derive.ctx_prev_val . context

-- ** from 'Derive.state_prev_val'

-- TODO do I really need so many ways to get the previous pitch?
prev_note_pitch :: Derive.Deriver (Maybe (RealTime, PSignal.Pitch))
prev_note_pitch = do
    prev <- prev_note
    return $ PSignal.last . Score.event_untransformed_pitch =<< prev

-- | Get the previous note.  Unlike 'prev_val', this always gets the previous
-- Score.Event, even if you're evaluating a control track under the note track.
--
-- TODO it doesn't really belong here since it doesn't use PassedArgs, but
-- this is where I would look for a function for the previous value.
prev_note :: Derive.Deriver (Maybe Score.Event)
prev_note = do
    -- TODO this happens if you're not inverting, but that should be ok, right?
    addr <- Derive.require "lookup_prev_note: no state_note_track"
        =<< Derive.gets (Derive.state_note_track . Derive.state_dynamic)
    Derive.gets $ Derive.from_tagged <=< Map.lookup addr . Derive.state_prev_val
        . Derive.state_threaded

-- ** 'Derive.state_pitch_map'

-- | Pitch at the time of the next event in this track.
lookup_next_pitch :: PassedArgs a -> Derive.Deriver (Maybe PSignal.Pitch)
lookup_next_pitch =
    maybe (return Nothing) (lookup_pitch_at <=< Derive.real) . next_start

lookup_prev_pitch :: PassedArgs a -> Derive.Deriver (Maybe PSignal.Pitch)
lookup_prev_pitch =
    maybe (return Nothing) (lookup_pitch_at <=< Derive.real) . prev_start

-- | Pitch at the time of the next note event of this track's
-- 'Derive.state_note_track'.
lookup_next_note_pitch :: PassedArgs a -> Derive.Deriver (Maybe PSignal.Pitch)
lookup_next_note_pitch args =
    justm (note_after (start args) <$> get_note_events) $
    lookup_pitch_at <=< Derive.real . Event.start

lookup_prev_note_pitch :: PassedArgs a -> Derive.Deriver (Maybe PSignal.Pitch)
lookup_prev_note_pitch args =
    justm (note_before (start args) <$> get_note_events) $
    lookup_pitch_at <=< Derive.real . Event.start

note_before :: TrackTime -> Events.Events -> Maybe Event.Event
note_before p = Events.last . fst . Events.split_exclude p

note_after :: TrackTime -> Events.Events -> Maybe Event.Event
note_after p = Events.head . snd . Events.split_exclude p

get_note_events :: Derive.Deriver Events.Events
get_note_events = do
    -- TODO this happens if you're not inverting, is that ok?
    (_, track_id) <- Derive.require "no state_note_track"
        =<< Derive.gets (Derive.state_note_track . Derive.state_dynamic)
    Track.track_events <$> Derive.get_track track_id

-- | Get a logical pitch at the given time, via 'Derive.state_pitch_map'.
-- As documented by 'Derive.state_pitch_map', the pitch map is unset while
-- evaluating the pitch map, so pitch calls that use this shouldn't be
-- surprised if it's Nothing.
lookup_pitch_at :: RealTime -> Derive.Deriver (Maybe PSignal.Pitch)
lookup_pitch_at pos = justm (Internal.get_dynamic Derive.state_pitch_map) $
    \(maybe_sig, logs) -> do
        mapM_ Log.write $ Log.add_prefix ("lookup_pitch_at " <> pretty pos) $
            filter ((>=Log.Warn) . Log.msg_priority) logs
        return $ PSignal.at pos =<< maybe_sig

-- | Like 'lookup_pitch_at', except for parsed pitches.  Normally you'd pass
-- 'Derive.Call.get_pitch_functions' to make a 'Pitch.Pitch'.
lookup_parsed_pitch_at :: (Pitch.Note -> Maybe a) -> RealTime
    -> Derive.Deriver (Maybe a)
lookup_parsed_pitch_at parse_pitch pos =
    justm (lookup_pitch_at pos) $ \pitch -> do
        note <- Pitches.pitch_note =<< Derive.resolve_pitch pos pitch
        Just <$> Derive.require "unparseable pitch" (parse_pitch note)

-- ** eval

-- | Unused, but might be used again if I need to evaluate the next event.
eval :: Derive.Callable d => Context x -> Event.Event -> [Event.Event]
    -> Derive.Deriver (Stream.Stream d)
eval ctx event prev = case Parse.parse_expr (Event.text event) of
    Left err -> Derive.throw $ "parse error: " <> err
    Right expr -> Eval.eval_expr False prev_ctx expr
        where
        prev_ctx = ctx
            { Derive.ctx_prev_val = Nothing
            , Derive.ctx_event = event
            , Derive.ctx_prev_events = prev
            , Derive.ctx_next_events =
                Derive.ctx_event ctx : Derive.ctx_next_events ctx
            , Derive.ctx_event_end = Event.start $ Derive.ctx_event ctx
            }

-- | Get the pitch at the time of the next event.  Since the pitch hasn't
-- been evaluated yet, it has to be evaluated here.  So if it depends on the
-- previous pitch, you won't get a pitch back.
--
-- Actually, the pitch likely *has* been evaluated, I just can't get at it
-- here.  If it's uninverted then I have the whole pitch track, and if it's
-- inverted then the event at or after the end of the event will be included.
-- But 'Derive.Control.trim_signal' will clip that sample off to avoid
-- a spurious pitch change at the end of the note.
eval_next_pitch :: Derive.PitchArgs -> Derive.Deriver (Maybe PSignal.Pitch)
eval_next_pitch = maybe (return Nothing) eval_pitch . Seq.head . next_events

eval_pitch :: Event.Event -> Derive.Deriver (Maybe PSignal.Pitch)
eval_pitch event = justm (to_maybe <$> Eval.eval_event event) $ \stream -> do
    start <- Derive.real (Event.start event)
    return $ PSignal.at start $ mconcat $ Stream.events_of stream
    where to_maybe = either (const Nothing) Just

-- * event timing

start :: PassedArgs a -> TrackTime
start = Event.start . event

duration :: PassedArgs a -> TrackTime
duration = Event.duration . event

real_duration :: PassedArgs a -> Derive.Deriver RealTime
real_duration args = (-) <$> real_end args <*> real_start args

real_start :: PassedArgs a -> Derive.Deriver RealTime
real_start = Derive.real . start

end :: PassedArgs a -> TrackTime
end = Event.end . event

real_end :: PassedArgs a -> Derive.Deriver RealTime
real_end = Derive.real . end

-- | The start of the next event, or the end of the block if there is no next
-- event.
--
-- Used by calls to determine their extent, especially control calls, which
-- have no explicit duration.
next :: PassedArgs a -> TrackTime
next = Derive.ctx_event_end . context

-- | End of the next event, or the end of the block if there is no next event.
next_end :: PassedArgs a -> TrackTime
next_end args = maybe (next args) Event.end (Seq.head (next_events args))

-- | Get the start of the next event, if there is one.
--
-- This is similar to 'next', except that it will be Nothing at the end of
-- the block.
next_start :: PassedArgs a -> Maybe TrackTime
next_start = fmap Event.start . Seq.head . next_events

-- | Start time of the previous event.
prev_start :: PassedArgs a -> Maybe TrackTime
prev_start = fmap Event.start . Seq.head . prev_events

-- | End time of the previous event.
prev_end :: PassedArgs a -> Maybe TrackTime
prev_end = fmap Event.end . Seq.head . prev_events

prev_events, next_events :: PassedArgs a -> [Event.Event]
next_events = Derive.ctx_next_events . context
prev_events = Derive.ctx_prev_events . context

-- ** modify

-- | Modify the duration of the ctx_event.  This is a hack, because calls run
-- in TrackTime, instead of using Derive.place.
set_duration :: TrackTime -> PassedArgs a -> PassedArgs a
set_duration dur args = args
    { Derive.passed_ctx = ctx
        { Derive.ctx_event = Event.duration_ #= dur $ Derive.ctx_event ctx }
    }
    where ctx = Derive.passed_ctx args

-- ** range

-- | Range of the called event, i.e. (min, max).  Note that range is the
-- minimum to maximum, which is not the same as the start and end if the event
-- has negative duration.
range :: PassedArgs a -> (TrackTime, TrackTime)
range = Event.range . event

real_range :: PassedArgs a -> Derive.Deriver (RealTime, RealTime)
real_range args = (,) <$> Derive.real start <*> Derive.real end
    where (start, end) = range args

-- | Like 'range', but if the duration is 0, then the end is 'next' event.
range_or_next :: PassedArgs a -> (TrackTime, TrackTime)
range_or_next args
    | start == end = (start, next args)
    | otherwise = (start, end)
    where (start, end) = range args

real_range_or_next :: PassedArgs a -> Derive.Deriver (RealTime, RealTime)
real_range_or_next args = (,) <$> Derive.real start <*> Derive.real end
    where (start, end) = range_or_next args

-- | The current event's start and end times.
--
-- If the event has a duration, then use that.  Otherwise, if there is
-- a 'EnvKey.note_end', return that.  Otherwise, return 'next'.  However, if
-- there's an 'EnvKey.note_start' and it's past the event start, then this
-- event isn't contained within the range of its parent note, which means that
-- its expected note end has also passed.  In that case, it returns
-- (note_start, note_start), which should cause the call to just emit its final
-- sample at the note-start, which will both get the correct value for the
-- event and not destroy the earlier track signal fragment.
range_or_note_end :: PassedArgs a -> Derive.Deriver (TrackTime, TrackTime)
range_or_note_end args
    | start == end = do
        note_start <- Derive.lookup_val EnvKey.note_start
        note_end <- Derive.lookup_val EnvKey.note_end
        case (note_start, note_end) of
            (Just note_start, Just note_end)
                | start < note_start -> return (note_start, note_start)
                | otherwise ->
                    return (start, min (next args) (max end note_end))
            _ -> return (start, next args)
    | otherwise = return (start, end)
    where (start, end) = range args

-- | Start and duration of the event.  This is probably the right thing for
-- calls that generate a note since it will give a negative duration when
-- appropriate.
extent :: PassedArgs a -> (TrackTime, TrackTime)
extent = (\e -> (Event.start e, Event.duration e)) . event

real_extent :: PassedArgs a -> Derive.Deriver (RealTime, RealTime)
real_extent args = do
    let e = event args
    start <- Derive.real (Event.start e)
    end <- Derive.real (Event.end e)
    return (start, end - start)

-- | Event range as it appears on the track, regardless of slicing.
range_on_track :: PassedArgs a -> (TrackTime, TrackTime)
range_on_track args = (shifted + start, shifted + end)
    where
    (start, end) = range args
    shifted = Derive.ctx_track_shifted (context args)

-- | This normalizes a deriver to start at 0 and have a duration of 1, provided
-- that the deriver is placed at the start and dur of the given args.  This is
-- the case if the deriver is a transformer arg, so this is useful for
-- a transformer to manipulate its argument.
normalized :: PassedArgs a -> Derive.Deriver b -> Derive.Deriver b
normalized args = Derive.place (- (start / dur)) (1 / dur)
    where
    (start, dur_) = extent args
    dur = if dur_ == 0 then 1 else dur_

{- NOTE [prev-val]

    Many control calls rely on the last value emitted by the previous call.
    I can't think of a way around that, because it's really fundamental to how
    the notation works, and it would be a real pain (and redundant) to have to
    write where interpolation comes from all the time.

    So conceptually each call takes the last val of the previous one as an
    argument.  This is problematic because it means you never know how far back
    in the track a given call's dependence extends.  Since track slicing
    divides control tracks into lots of little chunks it's not so simple to
    get the previous value.

    Initially I relied entirely on 'Derive.ctx_prev_val' and a hack where
    certain calls were marked as requiring the previous value, which 'slice'
    would then use.  The problem with that is that slice is working purely
    syntactically, and it doesn't know what's really in scope, nor does it
    understand val calls.  This is #2 below.

    After that, I tried #3, but ran into trouble wanting to get the previous
    Score.Event.  Actually, I could have supported Score.Event with the
    evaluating technique, but I forgot that I had already done all these work
    before, implemented most of #2 before stumbling on #1 again, when its
    earlier problems seemed less severe than before.

    So the current solution is #1.

    1. Extend the 'Derive.ctx_prev_val' mechanism to work even across sliced
    tracks.  Since they are no longer evaluated in sequence, I have to save
    them in a `Map (BlockId, TrackId) (RealTime, Either Signal.Y
    PSignal.Pitch))`.  However, this is problematic in its own way because
    it's actually threaded state, which is new.  This isn't actually so bad,
    because I would add it in a new Threaded state, and it's only making
    explicit the already threaded nature of track derivation, due to prev_val.
    An additional problem is that, once again due to slicing, control events
    are evaluated twice, which means that the second evaluation gets the first
    evaluation's value as it's \"previous\" value.  An extra hack in
    "Derive.EvalTrack" avoids recording a previous value when the event is past
    the end of a slice.

    2. Make 'slice' figure out which calls will need the previous val.  This is
    like the old syntactic mechanism only more robust.  Calls already include
    a `prev` tag that indicates they rely on the previous value.  This is
    complicated because what is in scope can change dynamically, so the slicing
    has to be done under the track's transform at least.  That means slicing
    is split into two halves, where the first part just marks slice boundaries,
    and the actual slice is done in the track deriver.

    3. If a call doesn't have a prev val already, it can go evaluate the prev
    event itself, which must be able to continue recursively until there really
    isn't a prev event.  This can do more work, but is appealing because it
    removes the serialization requirement of 'Derive.ctx_prev_val'.
        - This means if multiple calls want the prev val, it'll be evaluated
        multiple times, unless I cache it somehow.
        - I should clear out the next events so it doesn't get in a loop if it
        wants the next event.  Actually it's fine if it wants to look at it, it
        just can't want to evaluate it.
-}
