-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
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
    , at, overlapping, head, last

    -- ** split
    -- *** events
    , split_range, split_range_or_point, split_at, split_at_exclude
    , in_range, in_range_point
    , around
    -- *** List [Event]
    , split
    , at_after, after, before
    , split_at_before
    , find_overlaps

#ifdef TESTING
    , module Ui.Events
#endif
) where
import qualified Prelude
import Prelude hiding (head, last, length, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map

import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime
import Global
import Types


-- * events

empty :: Events
empty = Events Map.empty

null :: Events -> Bool
null (Events m) = Map.null m

length :: Events -> Int
length = Map.size . get

time_begin :: Events -> ScoreTime
time_begin = maybe 0 Event.start . head

time_end :: Events -> ScoreTime
time_end = maybe 0 Event.end . last

-- ** list conversion

singleton :: Event.Event -> Events
singleton event = Events $ Map.singleton (Event.start event) event

from_list :: [Event.Event] -> Events
from_list evts = insert evts empty

-- | Get all events in ascending order.
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
clip :: Bool -> ScoreTime -> [Event.Event] -> [Event.Event]
clip _ _ [] = []
clip allow_zero at (event : events)
    | allow_zero && Event.start event > at = []
    | not allow_zero && Event.start event >= at = []
    | Event.end event > at =
        [Event.modify_duration (\d -> min d (at - Event.start event)) event]
    | otherwise = event : clip allow_zero at events

-- ** insert / remove

-- | Merge events into the given Events.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
--
-- This should be the the only way to create a 'Events', short of
-- debugging, since it enforces important invariants.
insert :: [Event.Event] -> Events -> Events
insert [] events = events
insert new_events_ (Events events) =
    Events $ Map.unions [pre, overlapping, post]
    where
    new_events = map round_event (Seq.sort_on Event.start new_events_)
    start = Event.start (Prelude.head new_events)
    end = Event.end (Prelude.last new_events)
    (pre, within, post) = _split_overlapping start end events
    overlapping = make $ clip_events $
        Seq.merge_on Event.start (Map.elems within) new_events

-- | Round event times as described in 'ScoreTime.round'.
round_event :: Event.Event -> Event.Event
round_event event =
    Event.place (ScoreTime.round (Event.start event))
        (ScoreTime.round (Event.duration event)) event

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

head :: Events -> Maybe Event.Event
head (Events events) = snd <$> Map.min events

-- | Final event, if there is one.
last :: Events -> Maybe Event.Event
last (Events events) = snd <$> Map.max events

-- ** split

-- *** events

{- | Split into tracks before, within, and after the half-open range.

    This is complicated due to negative events.  The idea is that for
    a positive event, the range is half-open where the end is excluded, as is
    normal.  But for a negative event, it's the other way around, the start of
    the range is excluded and the end is excluded.

    Since this is a half-open range, if start==end then within will always be
    empty.
-}
split_range :: ScoreTime -> ScoreTime -> Events -> (Events, Events, Events)
split_range start end events = (Events pre, Events within, Events post)
    where (pre, within, post) = _split_range start end (get events)

-- | Like 'split_range', but if start==end, an event whose trigger exactly
-- matches will be in the within value.
split_range_or_point :: ScoreTime -> ScoreTime -> Events
    -> (Events, Events, Events)
split_range_or_point start end events
    | start == end = case at of
        Just e -> (Events pre, singleton e, Events post)
        Nothing -> case Map.max pre of
            Just (p, e) | Event.trigger e == start ->
                (Events (Map.delete p pre), singleton e, Events post)
            _ -> (Events pre, mempty, Events post)
    | otherwise = split_range start end events
    where (pre, at, post) = Map.splitLookup start (get events)

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

-- | Adjust a (pre, within, post) triple based on the orientation of the
-- first and last events within.
--
-- The idea is that when positive events are present, the range is half-open
-- where the end is excluded, as is normal.  But when negative events are
-- present, it's the other way around, the start of the range is excluded and
-- the end is excluded.
--
-- TODO I didn't wind up using this, but it still seems like a nice way to make
-- split functions aware of orientation.
adjust_for_orientation :: ScoreTime -> ScoreTime
    -> (EventMap, EventMap, EventMap) -> (EventMap, EventMap, EventMap)
adjust_for_orientation start end (pre, within, post) = (pre2, within3, post2)
    where
    -- Include at end.
    (within2, post2) = case Map.min post of
        Just (pos, evt) | pos == end && Event.is_negative evt ->
            (Map.insert pos evt within, Map.delete pos post)
        _ -> (within, post)
    -- Omit at beginning.
    (pre2, within3) = case Map.min within2 of
        Just (pos, evt) | pos == start && Event.is_negative evt ->
            (Map.insert pos evt pre, Map.delete pos within2)
        _ -> (pre, within2)

-- *** List [Event]

-- | Return the events before the given @pos@, and the events at and after it.
-- No special treatment for negative events.
split :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split pos (Events events) = (to_desc_list pre, to_asc_list post)
    where (pre, post) = Map.split2 pos events

-- | Events whose start is at or after @pos@.
at_after :: ScoreTime -> Events -> [Event.Event]
at_after pos = snd . split pos

-- | Events whose start is strictly after @pos@.
after :: ScoreTime -> Events -> [Event.Event]
after pos events = case at_after pos events of
    next : rest | Event.start next == pos -> rest
    events -> events

-- | Events whose start before @pos@.
before :: ScoreTime -> Events -> [Event.Event]
before pos = fst . split pos

-- | This is like 'split', but if there isn't an event exactly at the pos then
-- put the previous one in the post list.
split_at_before :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split_at_before pos events
    | next : _ <- post, Event.start next == pos = (pre, post)
    | before : prepre <- pre = (prepre, before:post)
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

-- | This assumes the input is already sorted!
make :: [Event.Event] -> EventMap
make = Map.fromAscList . Seq.key_on Event.start

to_asc_list :: EventMap -> [Event.Event]
to_asc_list = map snd . Map.toAscList

to_desc_list :: EventMap -> [Event.Event]
to_desc_list = map snd . Map.toDescList

instance Pretty.Pretty Events where
    format = Pretty.format . map event . ascending
        where
        event e = Pretty.text $
            o <> pretty (Event.start e, Event.duration e, Event.text e)
            where o = if Event.orientation e == Event.Positive then "" else "-"

instance Monoid Events where
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
        Just (pos, evt) | pos == end && Event.is_negative evt ->
            (Map.insert pos evt within, Map.delete pos post)
        _ -> (within, post)
    (pre2, within3) = case Map.min within2 of
        Just (pos, evt) | pos == start && Event.is_negative evt ->
            (Map.insert pos evt pre, Map.delete pos within2)
        _ -> (pre, within2)

_split_overlapping :: ScoreTime -> ScoreTime -> EventMap
    -> (EventMap, EventMap, EventMap)
_split_overlapping start end events = (pre2, within3, post2)
    where
    (pre, within, post) = Map.split3 start end events
    (within2, post2) = case Map.min post of
        Just (pos, evt) | Event.overlaps end evt ->
            (Map.insert pos evt within, Map.delete pos post)
        _ -> (within, post)
    (pre2, within3) = case Map.max pre of
        Just (pos, evt) | Event.overlaps start evt ->
            (Map.delete pos pre, Map.insert pos evt within2)
        _ -> (pre, within2)

{- | Merge @evts2@ into @evts1@.  Events that overlap other events will be
    clipped so they don't overlap.  If events occur simultaneously, the event
    from @evts1@ wins.

    The strategy is to extract the overlapping section and clip only that, then
    merge it back into the input maps, before merging them.  So in the common
    cases of non-overlapping maps or a small narrow map inserted into a large
    wide one this should traverse only a small portion of the large one, and it
    should do so in one pass.  However, if the small map straddles the large
    one, it will force an unnecessary traversal of the large one.  In that
    case, I'd be better off merging each event individually.
-}
merge :: Events -> Events -> Events
merge (Events evts1) (Events evts2)
    | Map.null evts1 = Events evts2
    | Map.null evts2 = Events evts1
    | otherwise = Events $ make $ clip_events $
        Seq.merge_on Event.start (Map.elems evts1) (Map.elems evts2)
    -- Previously I would extract the overlapping sections and clip only those,
    -- but I moved that to 'insert'.  Perhaps it's a bit more elegant here, but
    -- I think I'm never really merging large Events, just inserting small
    -- lists into a large EventMap.  And in any case, EventMaps never get very
    -- big.  Also, putting it in 'insert' avoids having to clip_events an extra
    -- time to create the new Events.

{- | Clip overlapping event durations.  If two event positions coincide, the
    last prevails.  An event with duration overlapping another event will be
    clipped.  If a positive duration event is followed by a negative duration
    event, the duration of the positive one will clip the negative one.

    The idea is that the 'Event.trigger' can't be moved, but the other end
    corresponds to the duration, so it can be shortened by an overlapping
    event.  However, it's assymetrical because a positive duration will win
    over a negative one (so the end of the previous event is always unmoveable
    to the following negative event looking back).  Also, a positive event can
    be deleted if the next event's trigger point is <= its start.  Given the
    precondition, this only happens if the next event is positive and its start
    is equal.

    The precondition is that the input events are sorted, the postcondition is
    that no [pos .. pos+dur) ranges will overlap.  The output events will also
    be sorted, though their 'Event.start's may have moved.

    Though tracks should never have events starting <0, this can still happen
    when Events are constructed by track slicing.
-}
clip_events :: [Event.Event] -> [Event.Event]
clip_events =
    Seq.sort_on Event.start . mapMaybe clip_duration . Seq.zip_neighbors
    where
    -- Why is this so complicated and irregular?
    clip_duration (maybe_prev, cur, maybe_next)
        | Event.is_positive cur = case positive_clip cur maybe_next of
            Nothing -> Just cur
            Just p
                -- If two events have the same start, the second one wins.
                | p <= Event.start cur -> Nothing
                | otherwise ->
                    Just $ Event.set_duration (p - Event.start cur) cur
        | otherwise = case negative_clip maybe_prev cur maybe_next of
            Nothing -> Just cur
            Just p
                | p > Event.end cur -> Nothing
                | otherwise -> Just $ Event.set_start p cur
    -- Get the clip point for a positive event.
    positive_clip cur maybe_next = maybe_next >>= \next ->
        if Event.trigger next < Event.end cur
            then Just (Event.trigger next) else Nothing
    negative_clip maybe_prev cur maybe_next = case (maybe_prev, maybe_next) of
        (_, Just next)
            | Event.is_positive next && Event.start next < Event.end cur ->
                Just (Event.end next)
        (Just prev, _)
            | Event.end prev > Event.start cur -> Just (Event.end prev)
        _ -> Nothing

find_overlaps :: Events -> [(Event.Event, Event.Event)]
find_overlaps = mapMaybe check . Seq.zip_next . ascending
    where
    check (cur, Just next)
        | Event.end cur > Event.start next = Just (cur, next)
        | otherwise = Nothing
    check _ = Nothing

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
