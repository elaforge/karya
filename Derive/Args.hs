-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances #-}
-- | Extract things from the PassedArgs data structure.
module Derive.Args where
import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Derive.Derive as Derive
import Derive.Derive (PassedArgs, CallInfo)
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PitchSignal as PitchSignal

import qualified Perform.Signal as Signal
import Types


info :: PassedArgs a -> CallInfo a
info = Derive.passed_info

event :: PassedArgs a -> Event.Event
event = Derive.info_event . info

require_prev_val :: EvalPrev a => PassedArgs a -> Derive.Deriver (RealTime, a)
require_prev_val = Derive.require "previous value" <=< prev_val

class Derive.ToTagged d => EvalPrev d where
    -- | Get the previous derived val.  This is used by control derivers so
    -- they can interpolate from the previous sample.
    prev_val :: PassedArgs d -> Derive.Deriver (Maybe (RealTime, d))

instance EvalPrev Signal.Y where prev_val = prev_control
instance EvalPrev PitchSignal.Pitch where prev_val = prev_pitch
instance EvalPrev Derive.Tagged where
    prev_val args = case Derive.info_track_type (info args) of
        Just ParseTitle.ControlTrack ->
            fmap (second Derive.TagControl) <$> prev_control coerced
        Just ParseTitle.PitchTrack ->
            fmap (second Derive.TagPitch) <$> prev_pitch coerced
        typ -> Derive.throw $ "no previous value for track type " ++ show typ
        where
        coerced = args
            { Derive.passed_info = Derive.coerce_call_info (info args) }

prev_control :: Derive.ControlArgs
    -> Derive.Deriver (Maybe (RealTime, Signal.Y))
prev_control args = case Derive.info_prev_val $ info args of
    Just (x, y) -> return $ Just (x, y)
    Nothing -> case prev_events args of
        [] -> return Nothing
        e : es -> last_control <$> eval (info args) e es
    where
    last_control :: [LEvent.LEvent Signal.Control] -> Maybe (RealTime, Signal.Y)
    last_control = msum . map Signal.last . reverse . LEvent.events_of

prev_pitch :: Derive.PitchArgs
    -> Derive.Deriver (Maybe (RealTime, PitchSignal.Pitch))
prev_pitch args = case Derive.info_prev_val $ info args of
    Just (x, y) -> return $ Just (x, y)
    Nothing -> case prev_events args of
        [] -> return Nothing
        e : es -> last_pitch <$> eval (info args) e es
    where
    last_pitch :: [LEvent.LEvent PitchSignal.Signal]
        -> Maybe (RealTime, PitchSignal.Pitch)
    last_pitch = msum . map PitchSignal.last . reverse . LEvent.events_of

eval :: Derive.Callable d => CallInfo x -> Event.Event
    -> [Event.Event] -> Derive.LogsDeriver d
eval cinfo event prev = case Parse.parse_expr text of
    Left err -> Derive.throw $ "parse error: " ++ err
    Right expr ->
        let prev_cinfo = cinfo
                { Derive.info_expr = text
                , Derive.info_prev_val = Nothing
                , Derive.info_event = event
                , Derive.info_prev_events = prev
                , Derive.info_next_events =
                    Derive.info_event cinfo : Derive.info_next_events cinfo
                , Derive.info_event_end = Event.start $ Derive.info_event cinfo
                }
        in Eval.eval_expr False prev_cinfo expr
    where text = Event.event_text event

-- * event timing

start :: PassedArgs a -> ScoreTime
start = Event.start . event

duration :: PassedArgs a -> ScoreTime
duration = Event.duration . event

real_start :: PassedArgs a -> Derive.Deriver RealTime
real_start = Derive.real . start

end :: PassedArgs a -> ScoreTime
end = Event.end . event

real_end :: PassedArgs a -> Derive.Deriver RealTime
real_end = Derive.real . end

-- | The start of the next event, or the end of the block if there is no next
-- event.
--
-- Used by calls to determine their extent, especially control calls, which
-- have no explicit duration.
next :: PassedArgs a -> ScoreTime
next = Derive.info_event_end . info

-- | End of the next event, or the end of the block if there is no next event.
next_end :: PassedArgs a -> ScoreTime
next_end args = maybe (next args) Event.end (Seq.head (next_events args))

-- | Get the start of the next event, if there is one.
--
-- This is similar to 'next', except that it will be Nothing at the end of
-- the block.
next_start :: PassedArgs a -> Maybe ScoreTime
next_start = fmap Event.start . Seq.head . next_events

prev_start :: PassedArgs a -> Maybe ScoreTime
prev_start = fmap Event.start . Seq.head . prev_events

prev_end :: PassedArgs a -> Maybe ScoreTime
prev_end = fmap Event.end . Seq.head . prev_events

prev_events, next_events :: PassedArgs a -> [Event.Event]
next_events = Derive.info_next_events . info
prev_events = Derive.info_prev_events . info

-- ** range

-- | Range of the called event, i.e. (start, end).  Note that range is the
-- minimum to maximum, which is not the same as the start and end if the event
-- has negative duration.
range :: PassedArgs a -> (ScoreTime, ScoreTime)
range = Event.range . event

real_range :: PassedArgs a -> Derive.Deriver (RealTime, RealTime)
real_range args = (,) <$> Derive.real start <*> Derive.real end
    where (start, end) = range args

-- | Like 'range', but if the duration is 0, then the end is 'next' event.
range_or_next :: PassedArgs a -> (ScoreTime, ScoreTime)
range_or_next args
    | start == end = (start, next args)
    | otherwise = (start, end)
    where (start, end) = range args

real_range_or_next :: PassedArgs a -> Derive.Deriver (RealTime, RealTime)
real_range_or_next args = (,) <$> Derive.real start <*> Derive.real end
    where (start, end) = range_or_next args

-- | Start and duration of the event.  This is probably the right thing for
-- calls that generate a note since it will give a negative duration when
-- appropriate.
extent :: PassedArgs a -> (ScoreTime, ScoreTime)
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
    shifted = Derive.info_track_shifted (info args)

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
    divides control tracks into lots of little chunks, it has to provide some
    overlap, but how much?

    Initially I relied entirely on 'Derive.info_prev_val' and a hack where
    certain calls were marked as requiring the previous value, which 'slice'
    would then use.  The problem with that is that slice is working purely
    syntactically, and it doesn't know what's really in scope, nor does it
    understand val calls.

    I think there are three choices:

    1. Extend the 'Derive.info_prev_val' mechanism to work even across sliced
    tracks.  Since they are no longer evaluated in sequence, I have to save
    them in a `Map (BlockId, TrackId) (RealTime, Either Signal.Y
    PitchSignal.Pitch))`.  However, this is problematic in its own way since if
    I put it in Collect, it destroys Collects Writer-nature.  Worse, as I found
    out only after I'd implemented it, it's just plain wrong, because slicing
    overlaps on the following event, since some calls emit signal before.  So
    the last sample of the previous slice is not necessarily the last sample of
    the call before the current slice.  To get that right I'd need even more
    crazy hackery.

    2. Make 'slice' figure out which calls will need the previous val.  This is
    like the old syntactic mechanism only more robust.  Calls already include
    a `prev` tag that indicates they rely on the previous value.  But the
    process seems laborious:
        - Before slicing, get the scopes of each track below.
        - Make a predicate that looks up a symbol, and says if it has a 'prev'
        tag.
        - Pass a `Map TrackId (CallId -> Bool)` to slice.
        - Now slice has to parse every call, extract all the CallIds, and see
        if any have a 'prev' tag.

    3. If a call doesn't have a prev val already, it can go evaluate the prev
    event itself, which must be able to continue recursively until there really
    isn't a prev event.  This can do more work, but is appealing because it
    removes the serialization requirement of 'Derive.info_prev_val'.
        - This means if multiple calls want the prev val, it'll be evaluated
        multiple times, unless I cache it somehow.
        - I should clear out the next events so it doesn't get in a loop if it
        wants the next event.  Actually it's fine if it wants to look at it, it
        just can't want to evaluate it.

    I eventually wound up with 3, but keeping the original scheme around,
    because while it's not 100% reliable, it does work most of the time and
    should be more efficient.
-}
