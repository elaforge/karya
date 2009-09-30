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
    -- alternate efficient version for controller tracks?
    -- ControllerTrack (Array (TrackPos, Double))

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
insert_events :: [PosEvent] -> TrackEvents -> TrackEvents
insert_events pos_events events =
    merge events (TrackEvents (Map.fromAscList clipped))
    where
    clipped = map clip (Seq.zip_next pos_events)
    clip ((pos, event), Nothing) = (pos,
        event { Event.event_duration = max 0 (Event.event_duration event) })
    clip ((pos, event), Just (next_pos, _)) =
        (pos, event { Event.event_duration = dur })
        where dur = max 0 $ min (Event.event_duration event) (next_pos - pos)

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
events_at pos (TrackEvents events) = (toDescList pre, Map.toAscList post)
    where (pre, post) = Map.split2 pos events

-- | This is like 'events_at', but if there isn't an event exactly at the pos,
-- start at the event right before it.
events_at_before :: TrackPos -> TrackEvents -> ([PosEvent], [PosEvent])
events_at_before pos events
    | (epos, _) : _ <- post, epos == pos = (pre, post)
    | before : prepre <- pre = (prepre, before:post)
    | otherwise = (pre, post)
    where (pre, post) = events_at pos events

event_at :: TrackEvents -> TrackPos -> Maybe Event.Event
event_at track_events pos = case forward pos track_events of
    ((epos, event):_) | epos == pos -> Just event
    _ -> Nothing

event_before pos events = let es = snd (events_at_before pos events)
    in if null es then Nothing else Just (head es)

-- | Like 'event_at', but return an event that overlaps the given pos.
event_overlapping :: TrackPos -> TrackEvents -> Maybe PosEvent
event_overlapping pos track_events
    | (next:_) <- post, fst next == pos = Just next
    | (prev:_) <- pre, event_end prev > pos = Just prev
    | otherwise = Nothing
    where
    (pre, post) = events_at pos track_events

-- | All events at or after @pos@.  Implement as snd of events_at.
forward :: TrackPos -> TrackEvents -> [PosEvent]
forward pos track_events = snd (events_at pos track_events)

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
events_in_range start end events = Map.toAscList within
    where
    (_, within, _) = Map.split3 start end (un_event_map events)

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

-- this is implemented in Map but not exported for some reason
-- toDescList map = reverse (Map.toAscList map)
toDescList = Map.toDescList


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
    relevant = merge_range first_pos last_pos
    merged = union_right (relevant evts1) (relevant evts2)
    clipped = Map.foldWithKey fold_clip [] merged

fold_clip pos evt [] = [(pos, evt)]
fold_clip pos evt rest@((next_pos, _next_evt) : _) =
    (pos, evt { Event.event_duration = clipped_dur }) : rest
    where clipped_dur = min (Event.event_duration evt) (next_pos - pos)

-- | Extract a submap whose keys are from one below @low@ to @high@.
merge_range :: (Ord k) => k -> k -> Map.Map k a -> Map.Map k a
merge_range low high fm = expanded
    where
    (below, within, above) = Map.split3 low high fm
    expanded = maybe_add (Map.find_min above)
        (maybe_add (Map.find_max below) within)

maybe_add Nothing fm = fm
maybe_add (Just (k, v)) fm = Map.insert k v fm

-- | Right-biased union.  Keys from the right map win.
union_right :: Ord k => Map.Map k a -> Map.Map k a -> Map.Map k a
union_right = Map.unionWith (\_ a -> a)
