-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{- | The 'Events' type contains the events of a track.

    This is the largest part of the score and also the part most often
    modified, so there is a plethora of access functions.
-}
module Ui.Events (
    -- * range
    Range(..), selection_range
    , range_times, range_start, range_end, range_duration

    -- * events
    , Events
    , empty, null, length, time_begin, time_end

    -- ** list conversion
    , singleton, from_list
    , ascending, descending

    -- ** transformation
    , map_events, move, clip

    -- ** insert / remove
    , insert, remove
    , merge

    -- ** lookup
    , at, overlapping, head, last

    -- ** split
    -- *** events
    , split_range, split, split_exclude
    , in_range
    , around
    -- *** List [Event]
    , split_lists
    , at_after, after, before
    , split_at_before

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
import qualified Ui.Sel as Sel

import Global
import Types


-- * Range

data Range =
    -- | A range between the given points.  It will select a positive event at
    -- the start time, or a negative one at the end time.  Effectively it's
    -- half-open from the start for Positive events, and half-open from the end
    -- for Negative ones.
    Range !TrackTime !TrackTime
    -- | Select an event at exactly the given time and orientation.
    | Point !TrackTime !Event.Orientation
    deriving (Eq, Show)

instance Pretty Range where
    pretty r = case r of
        Range s e -> pretty s <> "--" <> pretty e
        Point p orient ->
            "@" <> pretty p <> (if orient == Event.Positive then "+" else "-")

selection_range :: Sel.Selection -> Range
selection_range sel
    | start == end = Point start $ case Sel.orientation sel of
        Sel.Positive -> Event.Positive
        Sel.Negative -> Event.Negative
        -- The event selection shouldn't be None so this shouldn't happen.
        Sel.None -> Event.Positive
    | otherwise = Range start end
    where (start, end) = Sel.range sel

range_times :: Range -> (TrackTime, TrackTime)
range_times (Range s e) = (s, e)
range_times (Point s _) = (s, s)

range_start :: Range -> TrackTime
range_start = fst . range_times

range_end :: Range -> TrackTime
range_end = snd . range_times

range_duration :: Range -> TrackTime
range_duration range = end - start
    where (start, end) = range_times range

-- * events

-- TODO Probably the ScoreTimes in here should be TrackTime.  This module dates
-- from before TrackTime existed.

empty :: Events
empty = Events Map.empty

null :: Events -> Bool
null (Events m) = Map.null m

length :: Events -> Int
length = Map.size . get

time_begin :: Events -> ScoreTime
time_begin = maybe 0 Event.min . head

time_end :: Events -> ScoreTime
time_end = maybe 0 Event.max . last

-- ** list conversion

singleton :: Event.Event -> Events
singleton event = Events $ Map.singleton (event_key event) event

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

-- | Move events by a constant amount.  It's more efficient than 'map_events'
-- because it doesn't have to sort and clip the events.
move :: ScoreTime -> Events -> Events
move delta (Events events) =
    Events . Map.fromAscList . map m . Map.toAscList $ events
    where
    m (Key t orient, event) =
        (Key (t+delta) orient, Event.start_ %= (+delta) $ event)

-- | Clip off the events after the given end time.  Also shorten the last
-- event so it doesn't cross the end, if necessary.
clip :: Bool -> ScoreTime -> [Event.Event] -> [Event.Event]
clip _ _ [] = []
clip allow_zero end (event : events)
    | allow_zero && Event.start event > end = []
    | Event.is_negative event && Event.start event > end = []
    | Event.is_positive event && Event.start event >= end && not allow_zero = []
    | Event.end event > end = [Event.set_end end event]
    | otherwise = event : clip allow_zero end events

-- ** insert / remove

-- | Merge events into the given Events.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
--
-- This should be the the only way to create a 'Events', short of
-- debugging, since it enforces that events don't overlap.
insert :: [Event.Event] -> Events -> Events
insert [] events = events
insert unsorted_events (Events events) =
    Events $ Map.unions [pre, overlapping, post]
    where
    new_events = Seq.sort_on fst $ Seq.key_on event_key $
        map round_event unsorted_events
    start = Event.min $ snd $ Prelude.head new_events
    end = Event.max $ snd $ Prelude.last new_events
    (pre, within, post) = _split_overlapping start end events
    overlapping = merge_and_clip (Map.toAscList within) new_events

-- | Round event times as described in 'ScoreTime.round'.
round_event :: Event.Event -> Event.Event
round_event event =
    Event.place (ScoreTime.round (Event.start event))
        (ScoreTime.round (Event.duration event)) event

-- | Remove events in the range.
remove :: Range -> Events -> Events
remove (Point t orient) events = emap (Map.delete (Key t orient)) events
remove range events = emap (`Map.difference` within) events
    where (_, Events within, _) = split_range range events

-- ** lookup

-- | An event exactly at the given pos, or Nothing.
-- TODO this is just in_range (Point ...), merge them?
at :: ScoreTime -> Event.Orientation -> Events -> Maybe Event.Event
at pos orient = Map.lookup (Key pos orient) . get

-- | Like 'at', but return an event that overlaps the given pos.
overlapping :: ScoreTime -> Events -> Maybe Event.Event
overlapping pos events
    | next : _ <- post, Event.start next == pos || Event.end next < pos =
        Just next
    | prev : _ <- pre, Event.end prev > pos = Just prev
    | otherwise = Nothing
    where (pre, post) = (descending *** ascending) $ split pos events

head :: Events -> Maybe Event.Event
head (Events events) = snd <$> Map.min events

-- | Final event, if there is one.
last :: Events -> Maybe Event.Event
last (Events events) = snd <$> Map.max events

-- ** split

-- *** events

split_range :: Range -> Events -> (Events, Events, Events)
split_range (Point t orient) (Events events) =
    (Events pre, maybe mempty singleton at, Events post)
    where (pre, at, post) = Map.splitLookup (Key t orient) events
split_range (Range start end) (Events events) =
    (Events pre, Events within, Events post)
    where
    (pre, within, post) =
        Map.split3 (Key start Event.Positive) (Key end Event.Positive) events

-- | Split at the given time.  A positive event that starts at the given time
-- will appear in the after events, a negative event in the previous events.
split :: ScoreTime -> Events -> (Events, Events)
split pos (Events events) = (Events pre, Events post)
    where (pre, post) = Map.split2 (Key pos Event.Positive) events

-- | Like 'split', but a positive event that matches exactly is excluded from
-- the result.
split_exclude :: ScoreTime -> Events -> (Events, Events)
split_exclude pos (Events events) =
    (Events (Map.delete (Key pos Event.Negative) pre), Events post)
    where (pre, post) = Map.split (Key pos Event.Positive) events

-- | Like 'split_range', but only return the middle part.
in_range :: Range -> Events -> Events
in_range range events = within
    where (_, within, _) = split_range range events

-- | Get events in the given range, plus surrounding.  If there is no event at
-- 'start', the previous event will be included.  The event after 'end' is
-- always included.
around :: ScoreTime -> ScoreTime -> Events -> Events
around start end events = Events $ above $ below within
    where
    (Events pre, Events within, Events post) =
        split_range (Range start end) events
    below m
        | Just (Key lowest _, _) <- Map.min within, lowest == start = m
        | otherwise = maybe m (\(k, e) -> Map.insert k e m) (Map.max pre)
    above m = maybe m (\(k, e) -> Map.insert k e m) (Map.min post)

-- *** List [Event]

split_lists :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split_lists pos = (descending *** ascending) . split pos

-- | Events whose start is at or after @pos@.
at_after :: ScoreTime -> Events -> [Event.Event]
at_after pos = snd . split_lists pos

-- | Events whose start is strictly after @pos@.
after :: ScoreTime -> Events -> [Event.Event]
after pos events = case at_after pos events of
    next : rest | Event.start next == pos -> rest
    events -> events

-- | Events whose start before @pos@.
before :: ScoreTime -> Events -> [Event.Event]
before pos = fst . split_lists pos

-- | This is like 'split', but if there isn't an event exactly at the pos then
-- put the previous one in the post list.
split_at_before :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split_at_before pos events
    | next : _ <- post, Event.start next == pos = (pre, post)
    | before : prepre <- pre = (prepre, before : post)
    | otherwise = (pre, post)
    where (pre, post) = split_lists pos events

-- * implementation

-- | This is the underlying storage for a sequence of events.  The invariant
-- is that events start + duration don't overlap.
--
-- This type should remain abstract, and you should manipulate events using
-- functions in this module.
newtype Events = Events EventMap
    deriving (DeepSeq.NFData, Eq, Show, Read)

type EventMap = Map Key Event.Event

-- | This determines event order, and which events can coexist.
--
-- Technically, since 'Event.start' is in here, it doesn't have to be in
-- 'Event.Event'.  I used to have them separate, but it was a pain to pass
-- (ScoreTime, Event) pairs around everywhere.
data Key = Key !TrackTime !Event.Orientation
    deriving (Eq, Ord, Show, Read)

instance DeepSeq.NFData Key where rnf _ = ()

instance Pretty Key where
    pretty (Key t o) = pretty t <> case o of
        Event.Negative -> "-"
        Event.Positive -> "+"

event_key :: Event.Event -> Key
event_key event = Key (Event.start event) (Event.orientation event)

-- | This assumes the input is already sorted!
from_ascending :: [Event.Event] -> EventMap
from_ascending = Map.fromAscList . Seq.key_on event_key

to_asc_list :: EventMap -> [Event.Event]
to_asc_list = map snd . Map.toAscList

to_desc_list :: EventMap -> [Event.Event]
to_desc_list = map snd . Map.toDescList

instance Pretty Events where
    format = Pretty.format . map event . ascending
        where
        event e = Pretty.text $
            pretty (Event.start e, Event.duration e, Event.text e)

instance Monoid Events where
    mempty = empty
    mappend = merge

get :: Events -> EventMap
get (Events evts) = evts

emap :: (EventMap -> EventMap) -> Events -> Events
emap f (Events evts) = Events (f evts)

-- | Put events that overlap the range into within.
_split_overlapping :: ScoreTime -> ScoreTime -> EventMap
    -> (EventMap, EventMap, EventMap)
_split_overlapping start end events = (pre2, within3, post2)
    where
    (Events pre, Events within, Events post) =
        split_range (Range start end) (Events events)
    (pre2, within2) = case Map.max pre of
        Just (k, e) | Event.overlaps start e ->
            (Map.delete k pre, Map.insert k e within)
        _ -> (pre, within)
    (post2, within3) = case Map.min post of
        Just (k, e) | Event.overlaps end e ->
            (Map.delete k post, Map.insert k e within2)
        _ -> (post, within2)

{- | Merge @evts2@ into @evts1@.  Events that overlap other events will be
    clipped so they don't overlap.  If events occur simultaneously, the event
    from @evts1@ wins.
-}
merge :: Events -> Events -> Events
merge (Events evts1) (Events evts2)
    | Map.null evts1 = Events evts2
    | Map.null evts2 = Events evts1
    | otherwise = Events $
        merge_and_clip (Map.toAscList evts2) (Map.toAscList evts1)
    -- Previously I would extract the overlapping sections and clip only those,
    -- but I moved that to 'insert'.  Perhaps it's a bit more elegant here, but
    -- I think I'm never really merging large Events, just inserting small
    -- lists into a large EventMap.  And in any case, EventMaps never get very
    -- big.  Also, putting it in 'insert' avoids having to clip_events an extra
    -- time to create the new Events.

merge_and_clip :: [(Key, Event.Event)] -> [(Key, Event.Event)] -> EventMap
merge_and_clip old new = from_ascending $ clip_events $ map snd $
    Seq.merge_on fst (map (first (,False)) old) (map (first (,True)) new)
    -- Seq.merge_on should put elements from the first argument first, but
    -- it doesn't guarantee it, so let's be explicit.

{- | Clip overlapping event durations.  An event with duration overlapping
    another event will be clipped.  Positive events are clipped by following
    events, and negative ones are clipped by previous ones.  In the event of
    a conflict between positive and negative, they are both clipped to the
    middle of the overlap.  This may seem a bit weird, but it has the nice
    properties that I never clip an event to 0, and never have to drop an
    event due to clipping.  From an implementation point of view, it lets me
    write a single-pass algorithm.

    If there are multiple events with the same start and orientation, the last
    one wins.

    The precondition is that the input events are sorted by 'event_key', the
    postcondition is that they are still sorted and no [pos .. pos+dur) ranges
    will overlap.
-}
clip_events :: [Event.Event] -> [Event.Event]
clip_events =
    map clip . Seq.zip_neighbors
        . Seq.drop_initial_dups (\e -> (Event.start e, Event.orientation e))
    where
    clip (maybe_prev, cur, maybe_next)
        | Event.is_negative cur = case maybe_prev of
            Nothing -> cur
            Just prev
                -- |--->    prev
                --    <---| cur
                | Event.is_positive prev -> if Event.end cur < Event.end prev
                    then Event.end_ #= midpoint prev cur $ cur
                    else cur
                -- <---|
                --    <---|
                | Event.start prev > Event.end cur ->
                    Event.end_ #= Event.start prev $ cur
                | otherwise -> cur
        | otherwise = case maybe_next of
            Nothing -> cur
            Just next
                -- |--->    cur
                --    <---| next
                | Event.is_negative next -> if Event.end next < Event.end cur
                    then Event.end_ #= midpoint cur next $ cur
                    else cur
                -- |--->
                --    |--->
                | Event.start next < Event.end cur ->
                    Event.end_ #= Event.start next $ cur
                | otherwise -> cur
    midpoint pos neg =
        (max (Event.start pos) (Event.end neg)
            + min (Event.end pos) (Event.start neg))
        / 2

-- * serialize

instance Serialize.Serialize Events where
    put (Events a) = Serialize.put_version 4 >> Serialize.put a
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                events :: Map ScoreTime Event.Event <- Serialize.get
                return $ Events $ from_ascending $ Map.elems events
            4 -> do
                events :: Map Key Event.Event <- Serialize.get
                return $ Events events
            _ -> Serialize.bad_version "Events" v

-- Key has no version because there are a lot of them and they're all the same
-- and Events has a version.
instance Serialize.Serialize Key where
    put (Key a b) = Serialize.put a >> Serialize.put b
    get = Key <$> Serialize.get <*> Serialize.get
