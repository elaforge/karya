module Ui.Track where
import qualified Data.List as List
import qualified Data.Map as Map

import Ui.Types
import qualified Ui.Event as Event


newtype TrackId = TrackId String deriving (Eq, Ord, Show, Read)

data Track = Track {
    track_title :: String
    , track_events :: TrackEvents
    , track_bg :: Color
    -- track attrs
    } deriving (Show, Read)

-- | Construct an empty Track.
track title events bg = Track title (insert_events events empty_events) bg

modify_events :: Track -> (TrackEvents -> TrackEvents) -> Track
modify_events track@(Track { track_events = events }) f =
    track { track_events = f events }

{- TODO I probably need a more efficient data structure here.  Requirements:
1 Sparse mapping from TrackPos -> Event.
1 Non-destructive updates share memory related to size of change, not size of
map.
1 Repeatedly inserting ascending elements is efficient.
1 Getting ascending and descending lists from a given TrackPos is efficient.

2 Space efficient, contiguous storage.  If I am storing Doubles, they should
be unboxed.

2 Diff one map with another takes time relative to size of difference, not
size of maps.  This is also important for storing snapshots, since I'd like to
store a diff in the common case.

IntMap claims to be much faster than Map, but uses Ints.  It looks like I can
change the types to Word64 to store TrackPos's.  Then maybe I can implement
toDescList with foldl?
-}
-- IntMap is more efficient than Map, but only takes Int keys...
newtype TrackEvents = TrackEvents (Map.Map TrackPos Event.Event)
    deriving (Show, Read)
    -- alternate efficient version for controller tracks?
    -- ControllerTrack (Array (TrackPos, Double))
-- This should be opaque, with a few operations to query and modify it,
-- so I keep freedom to change the implemenctation.  Also needs to expose
-- the queries to c++.

empty_events = TrackEvents Map.empty


-- | Right-biased union.  Keys from the right map win.
union_right :: Ord k => Map.Map k a -> Map.Map k a -> Map.Map k a
union_right = Map.unionWith (\_ a -> a)

-- | Merge events into the given TrackEvents.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
insert_events :: [(TrackPos, Event.Event)] -> TrackEvents -> TrackEvents
insert_events pos_events events =
    merge events (TrackEvents (Map.fromAscList pos_events))

-- | Remove events between @start@ and @end@, not including @end@.
remove_events :: TrackPos -> TrackPos -> TrackEvents -> TrackEvents
remove_events start end track_events@(TrackEvents events) = TrackEvents $
    List.foldl' (\events pos -> Map.delete pos events) events del_pos
    where
    -- TODO Map.difference would be more efficient than deletes
    del_pos = takeWhile (<end) $ map fst $ snd (events_at start track_events)

-- | Return the events before the given @pos@, and the events at and after it.
events_at :: TrackPos -> TrackEvents
    -> ([(TrackPos, Event.Event)], [(TrackPos, Event.Event)])
events_at pos (TrackEvents events) = (toDescList pre, Map.toAscList post)
    where (pre, post) = split pos events

-- | Get all events in ascending order.  Like @snd . events_at (TrackPos 0)@.
event_list :: TrackEvents -> [(TrackPos, Event.Event)]
event_list (TrackEvents events) = Map.toAscList events

last_event :: TrackEvents -> Maybe (TrackPos, Event.Event)
last_event (TrackEvents events)
    | Map.null events = Nothing
    | otherwise = Just (Map.findMax events)

-- utilities

forward :: TrackEvents -> TrackPos -> [(TrackPos, Event.Event)]
forward track_events pos = snd (events_at pos track_events)

event_at :: TrackEvents -> TrackPos -> Maybe Event.Event
event_at track_events pos = case forward track_events pos of
    ((epos, event):_) | epos == pos -> Just event
    _ -> Nothing

-- | Return the position at the end of the event.
event_end :: (TrackPos, Event.Event) -> TrackPos
event_end (pos, evt) = pos + dur evt

-- implementation

-- this is implemented in Map but not exported for some reason
toDescList map = reverse (Map.toAscList map)


-- | Merge @evts2@ into @evts1@.  Events that overlap other events will be
-- shortened until they don't overlap.  If events occur simultaneously, the
-- event from @evts2@ wins.  This is more efficient if @evts2@ is small.
merge :: TrackEvents -> TrackEvents -> TrackEvents
merge (TrackEvents evts1) (TrackEvents evts2)
    | Map.size evts1 < Map.size evts2 = TrackEvents (_merge evts2 evts1)
    | otherwise = TrackEvents (_merge evts1 evts2)

_merge evts1 evts2
    | Map.null evts1 = evts2
    | Map.null evts2 = evts1
    | otherwise = union_right evts1 (Map.fromAscList clipped)
    where
    -- The strategy is to merge the evts from evts1 and evts2, and then clip
    -- the durations of the merged sequence so they don't overlap.  To improve
    -- efficiency when evts1 is large and evts2 is small, only the
    -- possibly overlapping subsets of evts1 and evts2 are merged and clipped.
    -- So if the two maps don't overlap, very little work should be done.
    -- However, since there is only one overlapping range, this will be
    -- inefficient if evts2 straddles evts1.
    -- TODO: is this overkill for the common case of merging in one event?
    -- do benchmarks and maybe insert a special case for when evts2 is 1 or 2

    first_pos = max (fst (Map.findMin evts1)) (fst (Map.findMin evts2))
    last_pos = min
        (event_end (Map.findMax evts2)) (event_end (Map.findMax evts2))
    relevant = in_range first_pos last_pos
    merged = union_right (relevant evts1) (relevant evts2)
    clipped = Map.foldWithKey fold_clip [] merged

fold_clip pos evt [] = [(pos, evt)]
fold_clip pos evt rest@((next_pos, _next_evt) : _) =
    (pos, set_dur clipped_dur evt) : rest
    where clipped_dur = min (dur evt) (next_pos - pos)

-- dur (text, dur) = dur
-- set_dur dur (text, _) = (text, dur)
dur = Event.event_duration
set_dur dur evt = evt { Event.event_duration = dur }

-- | Extract a submap whose keys are from one below @low@ to @high@.
in_range :: (Ord k) => k -> k -> Map.Map k a -> Map.Map k a
in_range low high fm = one_below
    where
    (below, above) = split low fm
    (within, _way_above) = split high above
    one_below = if Map.null below then within
        else let (k, v) = Map.findMax below in Map.insert k v within

-- Like Map.split, except include a matched key in the above map.
split :: (Ord k) => k -> Map.Map k a -> (Map.Map k a, Map.Map k a)
split k fm = (pre, post')
    where
    (pre, at, post) = Map.splitLookup k fm
    post' = maybe post (\v -> Map.insert k v post) at
