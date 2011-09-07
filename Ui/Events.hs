{-# LANGUAGE PatternGuards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{- | The 'Events' type contains the events of a track.

    This is the largest part of the score and also the part most often
    modified, so there is a plethora of access functions.

    It's implemented as a plain "Data.Map", but no one outside this module
    should have direct contact with that.
-}
module Ui.Events (
    -- * PosEvent
    PosEvent, start, end, min, max, range
    , positive, negative, overlaps
    , sort

    -- * events
    , Events(..)
    , empty, length, time_begin, time_end

    -- ** list conversion
    , singleton
    , make, make_sorted
    , ascending, descending

    -- ** transformation
    , map_events, map_sorted

    -- ** insert / remove
    , insert_events, insert_sorted_events
    , remove_events, remove_event
    , merge

    -- ** lookup
    , at, overlapping, first, last

    -- ** split
    , split_range
    , split
    , at_after, after
    , split_at_before
    , in_range
    , around

#ifdef TESTING
    , clip_events
#endif
) where
import qualified Prelude
import Prelude hiding (last, length, min, max)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map

import qualified Util.Map as Map
import qualified Util.Seq as Seq
import Ui
import qualified Ui.Event as Event


-- * PosEvent

type PosEvent = (ScoreTime, Event.Event)

start :: PosEvent -> ScoreTime
start = fst

-- | Return the position at the end of the event.  Could be before @pos@ if
-- the event has a negative duration.
end :: PosEvent -> ScoreTime
end (pos, evt) = pos + Event.event_duration evt

min, max :: PosEvent -> ScoreTime
min e@(pos, _) = Prelude.min pos (end e)
max e@(pos, _) = Prelude.max pos (end e)

range :: PosEvent -> (ScoreTime, ScoreTime)
range e = (min e, max e)

positive, negative :: PosEvent -> Bool
positive = Event.is_positive . snd
negative = Event.is_negative . snd

overlaps :: ScoreTime -> PosEvent -> Bool
overlaps p e@(pos, evt)
    | Event.is_positive evt = p == pos || p >= pos && p < end e
    | otherwise = p == pos || p <= pos && p > end e

sort :: [PosEvent] -> [PosEvent]
sort = Seq.sort_on start


-- * events

empty :: Events
empty = Events Map.empty

length :: Events -> Int
length = Map.size . get

time_begin :: Events -> ScoreTime
time_begin = maybe 0 min . first

time_end :: Events -> ScoreTime
time_end = maybe 0 max . last

-- ** list conversion

singleton :: ScoreTime -> Event.Event -> Events
singleton pos event = Events (Map.singleton pos event)

make :: [PosEvent] -> Events
make evts = insert_events evts empty

-- | Like 'make', but more efficient and the input must be sorted.
make_sorted :: [PosEvent] -> Events
make_sorted evts = insert_sorted_events evts empty

-- | Get all events in ascending order.  Like @snd . split (ScoreTime 0)@.
ascending :: Events -> [PosEvent]
ascending = Map.toAscList . get

descending :: Events -> [PosEvent]
descending = Map.toDescList . get

-- ** transformation

-- | Map a function across the events in Events.
map_events :: (PosEvent -> PosEvent) -> Events -> Events
map_events f = emap (Map.fromList . map f . Map.toList)

map_sorted :: (PosEvent -> PosEvent) -> Events -> Events
map_sorted f = emap (Map.fromAscList . map f . Map.toAscList)

-- ** insert / remove

-- | Merge events into the given Events.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
--
-- This should be the the only way to create a 'Events', short of
-- debugging, since it enforces important invariants.
insert_events :: [PosEvent] -> Events -> Events
insert_events pos_events = insert_sorted_events (sort pos_events)

-- | Like 'insert_events', but more efficient and the input must be sorted.
insert_sorted_events :: [PosEvent] -> Events -> Events
insert_sorted_events [] events = events
insert_sorted_events pos_events events =
    merge (Events (Map.fromAscList clipped)) events
    where clipped = clip_events pos_events

-- | Remove events in range.
remove_events :: ScoreTime -> ScoreTime -> Events -> Events
remove_events start end events =
    emap (`Map.difference` deletes) events
    where (_, deletes, _) = _split_range start end (get events)

-- | Remove an event if it occurs exactly at the given pos.
remove_event :: ScoreTime -> Events -> Events
remove_event pos = emap (Map.delete pos)

-- ** lookup

-- | An event exactly at the given pos, or Nothing.
at :: ScoreTime -> Events -> Maybe Event.Event
at pos = Map.lookup pos . get

-- | Like 'at', but return an event that overlaps the given pos.
overlapping :: ScoreTime -> Events -> Maybe PosEvent
overlapping pos events
    | (next:_) <- post, fst next == pos || end next < pos = Just next
    | (prev:_) <- pre, end prev > pos = Just prev
    | otherwise = Nothing
    where (pre, post) = split pos events

first :: Events -> Maybe PosEvent
first (Events events) = Map.min events

-- | Final event, if there is one.
last :: Events -> Maybe PosEvent
last (Events events) = Map.max events

-- ** split

-- | Split into tracks before, within, and after the half-open range.
-- @before@ events are descending, the rest are ascending.
split_range :: ScoreTime -> ScoreTime -> Events -> (Events, Events, Events)
split_range start end events = (Events pre, Events within, Events post)
    where (pre, within, post) = _split_range start end (get events)

-- | Return the events before the given @pos@, and the events at and after it.
split :: ScoreTime -> Events -> ([PosEvent], [PosEvent])
split pos (Events events) = (Map.toDescList pre, Map.toAscList post)
    where (pre, post) = Map.split2 pos events

-- | Events at or after @pos@.
at_after :: ScoreTime -> Events -> [PosEvent]
at_after pos events = snd (split pos events)

-- | Events after @pos@.
after :: ScoreTime -> Events -> [PosEvent]
after pos events = case at_after pos events of
    (p, _) : rest | p == pos -> rest
    events -> events

-- | This is like 'split', but if there isn't an event exactly at the pos and
-- the previous event is positive (i.e. has a chance of overlapping), include
-- that in the after event.
split_at_before :: ScoreTime -> Events -> ([PosEvent], [PosEvent])
split_at_before pos events
    | (epos, _) : _ <- post, epos == pos = (pre, post)
    | before : prepre <- pre, positive before = (prepre, before:post)
    | otherwise = (pre, post)
    where (pre, post) = split pos events

in_range :: ScoreTime -> ScoreTime -> Events -> Events
in_range start end events = within
    where (_, within, _) = split_range start end events

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

-- * implementation

{- TODO I probably need a more efficient data structure here.  Requirements:
    1 Sparse mapping from ScoreTime -> Event.
    1 Non-destructive updates share memory related to size of change, not size
    of map.
    1 Repeatedly inserting ascending elements is efficient.
    1 Getting ascending and descending lists from a given ScoreTime is
    efficient.

    2 Space efficient, contiguous storage.  If I am storing Doubles, they
    should be unboxed.

    2 Diff one map with another takes time relative to size of difference, not
    size of maps.  This is also important for storing snapshots, since I'd like
    to store a diff in the common case.

    IntMap claims to be much faster than Map, but uses Ints.  It looks like
    I can change the types to Word64 to store ScoreTime's.  Then maybe I can
    implement toDescList with foldl?
-}
newtype Events = Events EventMap
    deriving (DeepSeq.NFData, Eq, Show, Read)

type EventMap = Map.Map ScoreTime Event.Event

get :: Events -> EventMap
get (Events evts) = evts

-- | Events is not in Functor because this should be private.
emap :: (EventMap -> EventMap) -> Events -> Events
emap f (Events evts) = Events (f evts)

_split_range :: ScoreTime -> ScoreTime -> EventMap
    -> (EventMap, EventMap, EventMap)
_split_range start end events = (pre2, within3, post2)
    where
    (pre, within, post) = Map.split3 start end events
    (within2, post2) = case Map.min post of
        Just (pos, evt) | pos == end && Event.is_negative evt ->
            (Map.insert pos evt within, Map.delete pos post)
        _ -> (within, post)
    (pre2, within3) = case Map.min within2 of
        Just (pos, evt) | pos == start && not (Event.is_positive evt) ->
            (Map.insert pos evt pre, Map.delete pos within2)
        _ -> (pre, within2)

-- -- | Like 'in_range', except shorten the last event if it goes past the
-- -- end.
-- clip_to_range :: ScoreTime -> ScoreTime -> Events -> [PosEvent]
-- clip_to_range start end events = Map.toAscList clipped
--     where
--     (_, within, _) = split_range start end (get events)
--     clipped = case last (Events within) of
--         Nothing -> within
--         Just (pos, evt) -> Map.insert pos (clip_event (end-pos) evt) within
--     clip_event max_dur evt =
--         evt { Event.event_duration = Prelude.min max_dur (Event.event_duration evt) }


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
    | otherwise = Events $
        overlapping `Map.union2` evts1 `Map.union2` evts2
    where
    -- minimal overlapping range
    start = Prelude.max (min (Map.findMin evts1)) (min (Map.findMin evts2))
    end = Prelude.min (max (Map.findMax evts2)) (max (Map.findMax evts2))
    overlapping = Map.fromAscList $ clip_events $ Seq.merge_on fst
        (ascending (around start end (Events evts2)))
        (ascending (around start end (Events evts1)))

-- | Clip overlapping event durations.  If two event positions coincide, the
-- last prevails.  An event with duration overlapping another event will be
-- clipped.  If a positive duration event is followed by a negative duration
-- event, the duration of the positive one will clip the negative one.
--
-- The postcondition is that no [pos .. pos+dur) ranges will overlap.
clip_events :: [PosEvent] -> [PosEvent]
clip_events = map clip_duration . Seq.zip_neighbors . Seq.drop_initial_dups fst
    where
    clip_duration (maybe_prev, cur@(cur_pos, cur_evt), maybe_next)
        | positive cur = maybe cur clip_from_next maybe_next
        | otherwise = maybe cur clip_from_prev maybe_prev
        where
        clip_from_next (next_pos, _)
                -- If the following event is negative it will clip, but don't
                -- pass its pos.  That will leave a 0 dur event, but will
                -- prevent overlapping.
            | end cur > next_pos = set_dur (next_pos - cur_pos)
            | otherwise = cur
        clip_from_prev prev@(prev_pos, _)
            | positive prev = if end prev > end cur
                then set_dur (Prelude.min (-0) (end prev - cur_pos))
                else cur
            | otherwise = if end cur < prev_pos
                then set_dur (prev_pos - cur_pos)
                else cur
        set_dur dur = (cur_pos, cur_evt { Event.event_duration = dur })
