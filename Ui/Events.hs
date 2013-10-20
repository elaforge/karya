-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | The 'Events' type contains the events of a track.

    This is the largest part of the score and also the part most often
    modified, so there is a plethora of access functions.
-}
module Ui.Events (
    -- * events
    Events
    , empty, null, length, time_begin, time_end

    -- ** list conversion
    , singleton, from_list
    , ascending, descending

    -- ** transformation
    , map_events, clip

    -- ** insert / remove
    , insert, remove, remove_event
    , merge

    -- ** lookup
    , at, overlapping, first, last

    -- ** split
    -- *** events
    , split_range, split_at, split_at_exclude
    , in_range, in_range_point
    , around
    -- *** List [Event]
    , split
    , at_after, after, before
    , split_at_before

#ifdef TESTING
    , clip_events
#endif
) where
import Prelude hiding (last, length, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import Util.Control hiding (first)
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Ui.Event as Event
import Types


-- * events

empty :: Events
empty = Events Map.empty

null :: Events -> Bool
null (Events m) = Map.null m

length :: Events -> Int
length = Map.size . get

time_begin :: Events -> ScoreTime
time_begin = maybe 0 Event.min . first

time_end :: Events -> ScoreTime
time_end = maybe 0 Event.max . last

-- ** list conversion

singleton :: Event.Event -> Events
singleton event = Events $ Map.singleton (Event.start event) event

from_list :: [Event.Event] -> Events
from_list evts = insert evts empty

-- | Get all events in ascending order.  Like @snd . split (ScoreTime 0)@.
ascending :: Events -> [Event.Event]
ascending = to_asc_list . get

descending :: Events -> [Event.Event]
descending = to_desc_list . get

-- ** transformation

-- | Map a function across the events in Events.
map_events :: (Event.Event -> Event.Event) -> Events -> Events
map_events f = from_list . map f . ascending

-- | Clip off the events after the given end time.  Also shorten the last
-- event so it doesn't cross the end, if necessary.
clip :: ScoreTime -> [Event.Event] -> [Event.Event]
clip _ [] = []
clip end (event : events)
    | Event.start event >= end = []
    | Event.end event > end =
        [Event.modify_duration (\d -> min d (end - Event.start event)) event]
    | otherwise = event : clip end events
    -- TODO negative durations

-- ** insert / remove

-- | Merge events into the given Events.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
--
-- This should be the the only way to create a 'Events', short of
-- debugging, since it enforces important invariants.
insert :: [Event.Event] -> Events -> Events
insert [] events = events
insert new_events events = merge clipped events
    where
    clipped = Events $ Map.fromAscList $ Seq.key_on Event.start $
        clip_events (Seq.sort_on Event.start new_events)

-- | Remove events in the half-open range.  Since the range is half-open, if
-- start==end this will never remove any events.  Use 'remove_event' for that.
remove :: ScoreTime -> ScoreTime -> Events -> Events
remove start end events = emap (`Map.difference` deletes) events
    where (_, deletes, _) = _split_range start end (get events)

-- | Remove an event if it occurs exactly at the given pos.
remove_event :: ScoreTime -> Events -> Events
remove_event pos = emap (Map.delete pos)

-- ** lookup

-- | An event exactly at the given pos, or Nothing.
at :: ScoreTime -> Events -> Maybe Event.Event
at pos = Map.lookup pos . get

-- | Like 'at', but return an event that overlaps the given pos.
overlapping :: ScoreTime -> Events -> Maybe Event.Event
overlapping pos events
    | (next:_) <- post, Event.start next == pos || Event.end next < pos =
        Just next
    | (prev:_) <- pre, Event.end prev > pos = Just prev
    | otherwise = Nothing
    where (pre, post) = split pos events

first :: Events -> Maybe Event.Event
first (Events events) = snd <$> Map.min events

-- | Final event, if there is one.
last :: Events -> Maybe Event.Event
last (Events events) = snd <$> Map.max events

-- ** split

-- *** events

-- | Split into tracks before, within, and after the half-open range.
--
-- This is complicated due to negative events.  The idea is that when positive
-- events are present, the range is half-open where the end is excluded, as is
-- normal.  But whn negative events are present, it's the other way around, the
-- start of the range is excluded and the end is excluded.
--
-- Since this is a half-open range, if start==end then within will always be
-- empty.
split_range :: ScoreTime -> ScoreTime -> Events -> (Events, Events, Events)
split_range start end events = (Events pre, Events within, Events post)
    where (pre, within, post) = _split_range start end (get events)

-- | Split at the given time.  An event that starts at the give time will
-- appear in the above events.
split_at :: ScoreTime -> Events -> (Events, Events)
split_at pos (Events events) = (Events pre, Events post)
    where (pre, post) = Map.split2 pos events

-- | Liek 'split_at', but an event that matches exactly is excluded from the
-- result.
split_at_exclude :: ScoreTime -> Events -> (Events, Events)
split_at_exclude pos (Events events) = (Events pre, Events post)
    where (pre, post) = Map.split pos events

-- | Like 'split_range', but only return the middle part.
in_range :: ScoreTime -> ScoreTime -> Events -> Events
in_range start end events = within
    where (_, within, _) = split_range start end events

-- | Like 'in_range', but if start==end then get an event that matches exactly.
in_range_point :: ScoreTime -> ScoreTime -> Events -> Events
in_range_point start end events
    | start == end = maybe mempty singleton $ at start events
    | otherwise = in_range start end events

-- | Get events in the given range, plus surrounding.  If there is no event at
-- 'start', the previous event will be included.  The event after 'end' is
-- always included.
around :: ScoreTime -> ScoreTime -> Events -> Events
around start end = emap (split_around start end)
    where
    split_around start end events = above (below within)
        where
        (pre, within, post) = Map.split3 start end events
        below m
            | Just (lowest, _) <- Map.min within, lowest == start = m
            | otherwise = maybe m (\(pos, evt) -> Map.insert pos evt m)
                (Map.max pre)
        above m = maybe m (\(pos, evt) -> Map.insert pos evt m)
            (Map.min post)

-- *** List [Event]

-- | Return the events before the given @pos@, and the events at and after it.
split :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split pos (Events events) = (to_desc_list pre, to_asc_list post)
    where (pre, post) = Map.split2 pos events

-- | Events at or after @pos@.
at_after :: ScoreTime -> Events -> [Event.Event]
at_after pos = snd . split pos

-- | Events strictly after @pos@.
after :: ScoreTime -> Events -> [Event.Event]
after pos events = case at_after pos events of
    next : rest | Event.start next == pos -> rest
    events -> events

-- | Events before @pos@.
before :: ScoreTime -> Events -> [Event.Event]
before pos = fst . split pos

-- | This is like 'split', but if there isn't an event exactly at the pos and
-- the previous event is positive (i.e. has a chance of overlapping), include
-- that in the after event.
split_at_before :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split_at_before pos events
    | next : _ <- post, Event.start next == pos = (pre, post)
    | before : prepre <- pre, Event.positive before = (prepre, before:post)
    | otherwise = (pre, post)
    where (pre, post) = split pos events

-- * implementation

-- | This is the underlying storage for a sequence of events.  The invariant
-- is that events start + duration don't overlap.
--
-- This type should remain abstract, and you should manipulate events using
-- functions in this model.
newtype Events = Events EventMap
    deriving (DeepSeq.NFData, Eq, Show, Read)

-- | The ScoreTime is redundant since it's also stored in the Event itself.
-- I used to have them separate, but then I had to pass (ScoreTime, Event)
-- pairs around everywhere.
type EventMap = Map.Map ScoreTime Event.Event

to_asc_list :: EventMap -> [Event.Event]
to_asc_list = map snd . Map.toAscList

to_desc_list :: EventMap -> [Event.Event]
to_desc_list = map snd . Map.toDescList

instance Pretty.Pretty Events where
    format = Pretty.format . map event . ascending
        where
        event event = (Event.start event, Event.duration event,
            Event.event_string event)

instance Monoid.Monoid Events where
    mempty = empty
    mappend = merge

get :: Events -> EventMap
get (Events evts) = evts

emap :: (EventMap -> EventMap) -> Events -> Events
emap f (Events evts) = Events (f evts)

-- | The hairiness here is documented in 'split_range'.
_split_range :: ScoreTime -> ScoreTime -> EventMap
    -> (EventMap, EventMap, EventMap)
_split_range start end events
    -- A point selection always divides events into pre and post.
    | start == end =
        let (pre, post) = Map.split2 start events
        in (pre, mempty, post)
    | otherwise = (pre2, within3, post2)
    where
    (pre, within, post) = Map.split3 start end events
    (within2, post2) = case Map.min post of
        Just (pos, evt) | pos == end && Event.negative evt ->
            (Map.insert pos evt within, Map.delete pos post)
        _ -> (within, post)
    (pre2, within3) = case Map.min within2 of
        Just (pos, evt) | pos == start && not (Event.positive evt) ->
            (Map.insert pos evt pre, Map.delete pos within2)
        _ -> (pre, within2)

-- -- | Like 'in_range', except shorten the last event if it goes past the
-- -- end.
-- clip_to_range :: ScoreTime -> ScoreTime -> Events -> [Event]
-- clip_to_range start end events = to_asc_list clipped
--     where
--     (_, within, _) = split_range start end (get events)
--     clipped = case last (Events within) of
--         Nothing -> within
--         Just (pos, evt) -> Map.insert pos (clip_event (end-pos) evt) within
--     clip_event max_dur evt =
--         evt { Event.event_duration = min max_dur (Event.event_duration evt) }


-- | Merge @evts2@ into @evts1@.  Events that overlap other events will be
-- clipped so they don't overlap.  If events occur simultaneously, the
-- event from @evts1@ wins.
--
-- The strategy is to extract the overlapping section and clip only that,
-- then merge it back into the input maps, before merging them.  So in the
-- common cases of non-overlapping maps or a small narrow map inserted into
-- a large wide one this should traverse only a small portion of the large
-- one, and it should do so in one pass.  However, if the small map straddles
-- the large one, it will force an unnecessary traversal of the large one.  In
-- that case, I'd be better off merging each event individually.
merge :: Events -> Events -> Events
merge (Events evts1) (Events evts2)
    | Map.null evts1 = Events evts2
    | Map.null evts2 = Events evts1
    | otherwise = Events $ overlapping `Map.union` evts1 `Map.union` evts2
    where
    -- minimal overlapping range
    start = max (find_min evts1) (find_min evts2)
    end = min (find_max evts2) (find_max evts2)
    find_min = Event.min . snd . Map.findMin
    find_max = Event.max . snd . Map.findMax
    overlapping = Map.fromAscList $ Seq.key_on Event.start $ clip_events $
        Seq.merge_on Event.start
            (ascending (around start end (Events evts2)))
            (ascending (around start end (Events evts1)))

-- | Clip overlapping event durations.  If two event positions coincide, the
-- last prevails.  An event with duration overlapping another event will be
-- clipped.  If a positive duration event is followed by a negative duration
-- event, the duration of the positive one will clip the negative one.
--
-- The precondition is that the input events are sorted, the postcondition is
-- that no [pos .. pos+dur) ranges will overlap.
clip_events :: [Event.Event] -> [Event.Event]
clip_events =
    map clip_duration . Seq.zip_neighbors . Seq.drop_initial_dups Event.start
    where
    clip_duration (maybe_prev, cur, maybe_next)
        | Event.positive cur = maybe cur clip_from_next maybe_next
        | otherwise = maybe cur clip_from_prev maybe_prev
        where
        clip_from_next next
                -- If the following event is negative it will clip, but don't
                -- pass its pos.  That will leave a 0 dur event, but will
                -- prevent overlapping.
            | Event.end cur > Event.start next =
                set_dur (Event.start next - Event.start cur)
            | otherwise = cur
        clip_from_prev prev
            | Event.positive prev = if Event.end prev > Event.end cur
                then set_dur
                    (min (-0) (Event.end prev - Event.start cur))
                else cur
            | otherwise = if Event.end cur < Event.start prev
                then set_dur (Event.start prev - Event.start cur)
                else cur
        set_dur dur = Event.set_duration dur cur

-- * serialize

instance Serialize.Serialize Events where
    put (Events a) = Serialize.put_version 3 >> Serialize.put a
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                events :: Map.Map ScoreTime Event.Event <- Serialize.get
                return $ Events events
            _ -> Serialize.bad_version "Events" v
