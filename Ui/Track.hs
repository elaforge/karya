{-# LANGUAGE PatternGuards #-}
module Ui.Track where
import qualified Data.Array.IArray as IArray
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.Read as Read

import qualified Util.Map as Map
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Color as Color
import qualified Ui.Event as Event


type PosEvent = (TrackPos, Event.Event)

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
newtype Samples = Samples (IArray.Array Int (TrackPos, Double))
    deriving (Show)
samples :: [(TrackPos, Double)] -> Samples
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

track_time_end :: Track -> TrackPos
track_time_end track = time_end (track_events track)

time_end events = maybe (TrackPos 0) event_end (last_event events)

-- * TrackEvents implementation

{- TODO I probably need a more efficient data structure here.  Requirements:
    1 Sparse mapping from TrackPos -> Event.
    1 Non-destructive updates share memory related to size of change, not size
    of map.
    1 Repeatedly inserting ascending elements is efficient.
    1 Getting ascending and descending lists from a given TrackPos is
    efficient.

    2 Space efficient, contiguous storage.  If I am storing Doubles, they
    should be unboxed.

    2 Diff one map with another takes time relative to size of difference, not
    size of maps.  This is also important for storing snapshots, since I'd like
    to store a diff in the common case.

    IntMap claims to be much faster than Map, but uses Ints.  It looks like
    I can change the types to Word64 to store TrackPos's.  Then maybe I can
    implement toDescList with foldl?
-}
newtype TrackEvents =
    TrackEvents (Map.Map TrackPos Event.Event)
    deriving (Eq, Show, Read)
    -- alternate efficient version for control tracks?
    -- ControlTrack (Array (TrackPos, Double))

-- | Create a TrackEvents.  The input must be in ascending order!
event_map_asc :: [PosEvent] -> TrackEvents
event_map_asc pos_events = TrackEvents (Map.fromAscList pos_events)

un_event_map (TrackEvents evts) = evts
-- Not in Functor because this should be private.
emap f (TrackEvents evts) = TrackEvents (f evts)
empty_events = TrackEvents Map.empty

events_length :: TrackEvents -> Int
events_length = Map.size . un_event_map

-- | Map a function across the events in TrackEvents.
map_events :: (PosEvent -> PosEvent) -> TrackEvents -> TrackEvents
map_events f events = emap (Map.fromList . map f . Map.toList) events

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
    insert_sorted_events (Seq.sort_on fst pos_events) events

-- | Remove events between @start@ and @end@, not including @end@.
remove_events :: TrackPos -> TrackPos -> TrackEvents -> TrackEvents
remove_events start end track_events =
    emap (`Map.difference` deletes) track_events
    where
    (_, deletes, _) = Map.split3 start end (un_event_map track_events)

-- | Remove an event if it occurs exactly at the given pos.
remove_event :: TrackPos -> TrackEvents -> TrackEvents
remove_event pos track_events = emap (Map.delete pos) track_events

-- | Return the events before the given @pos@, and the events at and after it.
events_at :: TrackPos -> TrackEvents -> ([PosEvent], [PosEvent])
events_at pos (TrackEvents events) = (Map.toDescList pre, Map.toAscList post)
    where (pre, post) = Map.split2 pos events

-- | All events at or after @pos@.  Implement as snd of events_at.
events_after :: TrackPos -> TrackEvents -> [PosEvent]
events_after pos track_events = snd (events_at pos track_events)

-- | This is like 'events_at', but if there isn't an event exactly at the pos,
-- start at the event right before it.
events_at_before :: TrackPos -> TrackEvents -> ([PosEvent], [PosEvent])
events_at_before pos events
    | (epos, _) : _ <- post, epos == pos = (pre, post)
    | before : prepre <- pre = (prepre, before:post)
    | otherwise = (pre, post)
    where (pre, post) = events_at pos events

-- | The event on or before the pos.
event_before :: TrackPos -> TrackEvents -> Maybe PosEvent
event_before pos events = case snd (events_at_before pos events) of
    before : _ -> Just before
    [] -> Nothing


-- | The event on or after the pos.
event_after :: TrackPos -> TrackEvents -> Maybe PosEvent
event_after pos events = case snd (events_at pos events) of
    after : _ -> Just after
    [] -> Nothing

-- | An event exactly at the given pos, or Nothing.
event_at :: TrackPos -> TrackEvents -> Maybe Event.Event
event_at pos track_events = case events_after pos track_events of
    ((epos, event):_) | epos == pos -> Just event
    _ -> Nothing

event_strictly_after :: TrackPos -> TrackEvents -> Maybe PosEvent
event_strictly_after pos events = case evts of
        after : _ -> Just after
        [] -> Nothing
    where evts = dropWhile ((<=pos) . fst) (snd (events_at pos events))

-- | Like 'event_at', but return an event that overlaps the given pos.
event_overlapping :: TrackPos -> TrackEvents -> Maybe PosEvent
event_overlapping pos track_events
    | (next:_) <- post, fst next == pos = Just next
    | (prev:_) <- pre, event_end prev > pos = Just prev
    | otherwise = Nothing
    where
    (pre, post) = events_at pos track_events

-- | Get all events in ascending order.  Like @snd . events_at (TrackPos 0)@.
event_list :: TrackEvents -> [PosEvent]
event_list (TrackEvents events) = Map.toAscList events

-- | Final event, if there is one.
last_event :: TrackEvents -> Maybe PosEvent
last_event (TrackEvents events) = Map.find_max events

-- | Return the position at the end of the event.
event_end :: PosEvent -> TrackPos
event_end (pos, evt) = pos + Event.event_duration evt

events_in_range :: TrackPos -> TrackPos -> TrackEvents -> [PosEvent]
events_in_range start end events = within
    where (_, within, _) = split_range start end events

-- | Split into tracks before, within, and after the half-open range.  @before@
-- events are descending, the rest are ascending.  Unlike most half-open
-- ranges, @start==end@ will include an event at @start@.
split_range :: TrackPos -> TrackPos -> TrackEvents ->
    ([PosEvent], [PosEvent], [PosEvent])
split_range start end events = (pre, within2, post2)
    where
    (pre_m, within_m, post_m) = Map.split3 start end (un_event_map events)
    (pre, within, post) = (Map.toDescList pre_m, Map.toAscList within_m,
        Map.toAscList post_m)
    (within2, post2) = if start == end then splitAt 1 post else (within, post)

-- | Like 'events_in_range', except shorten the last event if it goes past the
-- end.
clip_to_range :: TrackPos -> TrackPos -> TrackEvents -> [PosEvent]
clip_to_range start end events = Map.toAscList clipped
    where
    (_, within, _) = Map.split3 start end (un_event_map events)
    clipped = case last_event (TrackEvents within) of
        Nothing -> within
        Just (pos, evt) -> Map.insert pos (clip_event (end-pos) evt) within
    clip_event max_dur evt =
        evt { Event.event_duration = min max_dur (Event.event_duration evt) }


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
    | otherwise = TrackEvents$ overlapping `Map.union2` evts1 `Map.union2` evts2
    where
    -- minimal overlapping range
    start = max (fst (Map.findMin evts1)) (fst (Map.findMin evts2))
    end = min (event_end (Map.findMax evts2)) (event_end (Map.findMax evts2))
    overlapping = Map.fromAscList $ clip_events $ Seq.merge_by cmp
        (in_range_before start end evts2) (in_range_before start end evts1)
    cmp a b = compare (fst a) (fst b)

-- | Clip overlapping event durations.  If two event beginnings coincide, the
-- last prevails.
clip_events :: [PosEvent] -> [PosEvent]
clip_events (pos_evt@(pos, evt) : rest@((next_pos, _) : _))
    | pos == next_pos = clip_events rest
    | event_end pos_evt > next_pos =
        (pos, evt { Event.event_duration = (next_pos-pos) }) : clipped
    | otherwise = pos_evt : clipped
    where clipped = clip_events rest
clip_events pos_events = pos_events

-- | Everything from @start@ to @end@, plus one before @start@.
in_range_before :: TrackPos -> TrackPos -> Map.Map TrackPos Event.Event
    -> [PosEvent]
in_range_before start end events = takeWhile ((<end) . fst) pos_events
    where pos_events = snd $ events_at_before start (TrackEvents events)
