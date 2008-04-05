module Ui.Track where
import qualified Data.List as List
import qualified Data.Map as Map

import Ui.Types
import qualified Ui.Event as Event


newtype TrackId = TrackId String deriving (Eq, Ord, Show)

data Track = Track {
    track_title :: String
    , track_events :: TrackEvents
    , track_bg_color :: Color
    -- track attrs
    } deriving (Show)

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
    deriving (Show)
    -- alternate efficient version for controller tracks?
    -- | ControllerTrack (Array (TrackPos, Double))
-- This should be opaque, with a few operations to query and modify it,
-- so I keep freedom to change the implemenctation.  Also needs to expose
-- the queries to c++.

empty_events = TrackEvents Map.empty

-- TODO use Map.union / difference

insert_events :: [(TrackPos, Event.Event)] -> TrackEvents -> TrackEvents
insert_events pos_events (TrackEvents events) = TrackEvents $
    List.foldl' (\events (pos, event) -> Map.insert pos event events)
        events pos_events
-- TODO: when does union become more efficient than a series of inserts?
-- TODO: flag to overwrite / clip existing events?

-- | Remove events between @start@ and @end@, not including @end@.
remove_events :: TrackPos -> TrackPos -> TrackEvents -> TrackEvents
remove_events start end track_events@(TrackEvents events) = TrackEvents $
    List.foldl' (\events pos -> Map.delete pos events) events del_pos
    where
    del_pos = takeWhile (<end) $ map fst $ snd (events_at start track_events)

-- | Return the events before the given @pos@, and the events at and after it.
events_at :: TrackPos -> TrackEvents
    -> ([(TrackPos, Event.Event)], [(TrackPos, Event.Event)])
events_at pos (TrackEvents events) = (toDescList pre, post')
    where
    (pre, at, post) = Map.splitLookup pos events
    post' = maybe [] (\event -> [(pos, event)]) at ++ Map.toAscList post

last_event :: TrackEvents -> Maybe (TrackPos, Event.Event)
last_event (TrackEvents events)
    | Map.null events = Nothing
    | otherwise = Just (Map.findMax events)

-- this is implemented in Map but not exported for some reason
toDescList map = reverse (Map.toAscList map)
