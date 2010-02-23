{-# LANGUAGE PatternGuards, TupleSections #-}
module Ui.Track where
import qualified Data.Array.IArray as IArray
import qualified Data.Map as Map

import qualified Util.Map as Map
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Color as Color
import qualified Ui.Event as Event


type PosEvent = (ScoreTime, Event.Event)

-- | Constructor for tests, analogous to 'Event.event'.
event :: ScoreTime -> ScoreTime -> String -> PosEvent
event start dur txt = (start, Event.event txt dur)

data Track = Track {
    track_title :: String
    , track_events :: TrackEvents
    , track_bg :: Color
    , track_render :: RenderConfig
    } deriving (Eq, Show, Read)

-- | Construct an empty Track.
track :: String -> [PosEvent] -> Color -> RenderConfig -> Track
track title events bg render =
    Track title (insert_events events empty_events) bg render

-- * samples

data RenderConfig = RenderConfig {
    render_style :: RenderStyle
    , render_color :: Color.Color
    } deriving (Eq, Show, Read)

data RenderStyle = NoRender | Line | Filled
    deriving (Eq, Show, Read)

type TrackSamples = [(TrackId, Samples)]
newtype Samples = Samples (IArray.Array Int (ScoreTime, Double))
    deriving (Show)
samples :: [(ScoreTime, Double)] -> Samples
samples smps = Samples $ IArray.listArray (0, length smps - 1) smps

no_samples = samples []

set_render_style :: RenderStyle -> Track -> Track
set_render_style style track =
    track { track_render = (track_render track) { render_style = style } }

-- * track events

make_track_events :: [PosEvent] -> TrackEvents
make_track_events = TrackEvents . Map.fromList

set_events :: TrackEvents -> Track -> Track
set_events events track = modify_events (const events) track

modify_events :: (TrackEvents -> TrackEvents) -> Track -> Track
modify_events f track@(Track { track_events = events }) =
    track { track_events = f events }

track_time_end :: Track -> ScoreTime
track_time_end track = time_end (track_events track)

time_end :: TrackEvents -> ScoreTime
time_end events = maybe 0 event_max (last_event events)


-- * TrackEvents implementation

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
newtype TrackEvents =
    TrackEvents (Map.Map ScoreTime Event.Event)
    deriving (Eq, Show, Read)
    -- alternate efficient version for control tracks?
    -- ControlTrack (Array (ScoreTime, Double))

-- | Create a TrackEvents.  The input must be in ascending order!
event_map_asc :: [PosEvent] -> TrackEvents
event_map_asc pos_events = TrackEvents (Map.fromAscList pos_events)

te_map (TrackEvents evts) = evts
-- Not in Functor because this should be private.
emap f (TrackEvents evts) = TrackEvents (f evts)
empty_events = TrackEvents Map.empty

events_length :: TrackEvents -> Int
events_length = Map.size . te_map

-- | Map a function across the events in TrackEvents.
map_events :: (PosEvent -> PosEvent) -> TrackEvents -> TrackEvents
map_events f events = emap (Map.fromList . map f . Map.toList) events

sort_events :: [PosEvent] -> [PosEvent]
sort_events = Seq.sort_on event_start

-- | Merge events into the given TrackEvents.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
--
-- This should be the the only way to create a 'TrackEvents', short of
-- debugging, since it enforces import invariants.  However, the inserted
-- are assumed to be sorted, for efficiency's sake.
insert_sorted_events :: [PosEvent] -> TrackEvents -> TrackEvents
insert_sorted_events [] events = events
insert_sorted_events pos_events events =
    merge (TrackEvents (Map.fromAscList clipped)) events
    where clipped = clip_events pos_events

-- | Like 'insert_sorted_events' but safer and less efficient.
insert_events :: [PosEvent] -> TrackEvents -> TrackEvents
insert_events pos_events events =
    insert_sorted_events (sort_events pos_events) events

from_events, from_sorted_events :: [PosEvent] -> TrackEvents
from_events evts = insert_events evts empty_events
from_sorted_events evts = insert_sorted_events evts empty_events

-- | Remove events in range.
remove_events :: ScoreTime -> ScoreTime -> TrackEvents -> TrackEvents
remove_events start end track_events =
    emap (`Map.difference` deletes) track_events
    where (_, deletes, _) = _split_range start end (te_map track_events)

-- | Remove an event if it occurs exactly at the given pos.
remove_event :: ScoreTime -> TrackEvents -> TrackEvents
remove_event pos track_events = emap (Map.delete pos) track_events

-- | Return the events before the given @pos@, and the events at and after it.
split :: ScoreTime -> TrackEvents -> ([PosEvent], [PosEvent])
split pos (TrackEvents events) = (Map.toDescList pre, Map.toAscList post)
    where (pre, post) = Map.split2 pos events

-- | Events at or after @pos@.
events_after :: ScoreTime -> TrackEvents -> [PosEvent]
events_after pos track_events = snd (split pos track_events)

-- | This is like 'split', but if there isn't an event exactly at the pos and
-- the previous event is positive (i.e. has a chance of overlapping), include
-- that in the after event.
split_at_before :: ScoreTime -> TrackEvents -> ([PosEvent], [PosEvent])
split_at_before pos events
    | (epos, _) : _ <- post, epos == pos = (pre, post)
    | before : prepre <- pre, event_positive before = (prepre, before:post)
    | otherwise = (pre, post)
    where (pre, post) = split pos events

-- | An event exactly at the given pos, or Nothing.
event_at :: ScoreTime -> TrackEvents -> Maybe Event.Event
event_at pos track_events = case events_after pos track_events of
    ((epos, event):_) | epos == pos -> Just event
    _ -> Nothing

-- | Like 'event_at', but return an event that overlaps the given pos.
event_overlapping :: ScoreTime -> TrackEvents -> Maybe PosEvent
event_overlapping pos track_events
    | (next:_) <- post, fst next == pos || event_end next < pos = Just next
    | (prev:_) <- pre, event_end prev > pos = Just prev
    | otherwise = Nothing
    where (pre, post) = split pos track_events

-- | Get all events in ascending order.  Like @snd . split (ScoreTime 0)@.
event_list :: TrackEvents -> [PosEvent]
event_list (TrackEvents events) = Map.toAscList events

-- | Final event, if there is one.
last_event :: TrackEvents -> Maybe PosEvent
last_event (TrackEvents events) = Map.find_max events

event_start :: PosEvent -> ScoreTime
event_start = fst

-- | Return the position at the end of the event.  Could be before @pos@ if
-- the event has a negative duration.
event_end :: PosEvent -> ScoreTime
event_end (pos, evt) = pos + Event.event_duration evt

event_min, event_max :: PosEvent -> ScoreTime
event_min e@(pos, _) = min pos (event_end e)
event_max e@(pos, _) = max pos (event_end e)

event_positive, event_negative :: PosEvent -> Bool
event_positive = Event.is_positive . snd
event_negative = Event.is_negative . snd

event_overlaps :: ScoreTime -> PosEvent -> Bool
event_overlaps p e@(pos, evt)
    | Event.is_positive evt = p == pos || p >= pos && p < event_end e
    | otherwise = p == pos || p <= pos && p > event_end e

-- | Everything from @start@ to @end@ exclusive, plus one before @start@ and
-- one after @end@.
in_range_around :: ScoreTime -> ScoreTime -> TrackEvents -> [PosEvent]
in_range_around start end =
    Seq.take1 ((<end) . fst) .  snd . split_at_before start

events_in_range :: ScoreTime -> ScoreTime -> TrackEvents -> [PosEvent]
events_in_range start end events = within
    where (_, within, _) = split_range start end events

-- | Split into tracks before, within, and after the half-open range.
-- @before@ events are descending, the rest are ascending.
split_range :: ScoreTime -> ScoreTime -> TrackEvents
    -> ([PosEvent], [PosEvent], [PosEvent])
split_range start end events =
    (Map.toDescList pre, Map.toAscList within, Map.toAscList post)
    where (pre, within, post) = _split_range start end (te_map events)

split_lookup :: ScoreTime -> TrackEvents
    -> ([PosEvent], Maybe PosEvent, [PosEvent])
split_lookup pos events =
    (Map.toDescList pre, fmap (pos,) at, Map.toAscList post)
    where (pre, at, post) = Map.splitLookup pos (te_map events)

_split_range :: ScoreTime -> ScoreTime -> Map.Map ScoreTime Event.Event
    -> (Map.Map ScoreTime Event.Event, Map.Map ScoreTime Event.Event,
        Map.Map ScoreTime Event.Event)
_split_range start end emap = (pre2, within3, post2)
    where
    (pre, within, post) = Map.split3 start end emap
    (within2, post2) = case Map.find_min post of
        Just (pos, evt) | pos == end && Event.is_negative evt ->
            (Map.insert pos evt within, Map.delete pos post)
        _ -> (within, post)
    (pre2, within3) = case Map.find_min within2 of
        Just (pos, evt) | pos == start && not (Event.is_positive evt) ->
            (Map.insert pos evt pre, Map.delete pos within2)
        _ -> (pre, within2)

-- -- | Like 'events_in_range', except shorten the last event if it goes past the
-- -- end.
-- clip_to_range :: ScoreTime -> ScoreTime -> TrackEvents -> [PosEvent]
-- clip_to_range start end events = Map.toAscList clipped
--     where
--     (_, within, _) = _split_range start end (te_map events)
--     clipped = case last_event (TrackEvents within) of
--         Nothing -> within
--         Just (pos, evt) -> Map.insert pos (clip_event (end-pos) evt) within
--     clip_event max_dur evt =
--         evt { Event.event_duration = min max_dur (Event.event_duration evt) }


-- * private implementation


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
merge :: TrackEvents -> TrackEvents -> TrackEvents
merge (TrackEvents evts1) (TrackEvents evts2)
    | Map.null evts1 = TrackEvents evts2
    | Map.null evts2 = TrackEvents evts1
    | otherwise = TrackEvents $
        overlapping `Map.union2` evts1 `Map.union2` evts2
    where
    -- minimal overlapping range
    start = max (event_min (Map.findMin evts1)) (event_min (Map.findMin evts2))
    end = min (event_max (Map.findMax evts2)) (event_max (Map.findMax evts2))
    overlapping = Map.fromAscList $ clip_events $ Seq.merge_on fst
        (in_range_around start end (TrackEvents evts2))
        (in_range_around start end (TrackEvents evts1))

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
        | event_positive cur = maybe cur clip_from_next maybe_next
        | otherwise = maybe cur clip_from_prev maybe_prev
        where
        clip_from_next (next_pos, _)
                -- If the following event is negative it will clip, but don't
                -- pass its pos.  That will leave a 0 dur event, but will
                -- prevent overlapping.
            | event_end cur > next_pos = set_dur (next_pos - cur_pos)
            | otherwise = cur
        clip_from_prev prev@(prev_pos, _)
            | event_positive prev = if event_end prev > event_end cur
                then set_dur (min (-0) (event_end prev - cur_pos))
                else cur
            | otherwise = if event_end cur < prev_pos
                then set_dur (prev_pos - cur_pos)
                else cur
        set_dur dur = (cur_pos, cur_evt { Event.event_duration = dur })
