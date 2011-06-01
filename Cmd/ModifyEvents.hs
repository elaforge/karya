-- | Utilities to modify events in tracks.
module Cmd.ModifyEvents where
import Control.Monad
import Util.Control
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection


-- * main modification

-- | Map a function over the selected events.  Returning Nothing will remove
-- the event.
events :: (Cmd.M m) => (Track.PosEvent -> Maybe Track.PosEvent) -> m ()
events f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        State.insert_events track_id (Seq.map_maybe f events)

-- | This is like 'events'.  It's more efficient but the modify function must
-- promise to return events in sorted order.
events_sorted :: (Cmd.M m) => (Track.PosEvent -> Maybe Track.PosEvent) -> m ()
events_sorted f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        State.insert_sorted_events track_id (Seq.map_maybe f events)

-- | Map a function over the selected events, passing the track id.  Unlike
-- 'events', returning Nothing will leave the track unchanged.
tracks :: (Cmd.M m) =>
    (TrackId -> [Track.PosEvent] -> m (Maybe [Track.PosEvent])) -> m ()
tracks f = tracks_sorted $ \track_id events ->
    fmap Track.sort_events <$> f track_id events

-- | As with 'events_sorted', this is a more efficient version for sorted
-- events.
tracks_sorted :: (Cmd.M m) =>
    (TrackId -> [Track.PosEvent] -> m (Maybe [Track.PosEvent])) -> m ()
tracks_sorted f = do
    track_events <- Selection.events
    forM_ track_events $ \(track_id, (start, end), events) -> do
        maybe_new_events <- f track_id events
        case maybe_new_events of
            Just new_events -> do
                State.remove_events track_id start end
                State.insert_sorted_events track_id new_events
            Nothing -> return ()

-- * convenience

-- | Modify events in the selection.  For efficiency, this can't move the
-- events.
modify_pos_events :: (Cmd.M m) => (ScoreTime -> Event.Event -> Event.Event)
    -> m ()
modify_pos_events f = do
    track_events <- Selection.events
    forM_ track_events $ \(track_id, _, events) -> do
        let insert = [(pos, f pos evt) | (pos, evt) <- events]
        State.insert_sorted_events track_id insert

-- | A version of 'modify_events_pos' that doesn't pass the pos.
modify_events :: (Cmd.M m) => (Event.Event -> Event.Event) -> m ()
modify_events f = modify_pos_events (\_ evt -> f evt)

-- | Modify the start time and duration of the selected events.
pos_dur :: (Cmd.M m) => (ScoreTime -> ScoreTime) -> m ()
pos_dur f = events $ \(pos, event) ->
    Just $ (f pos, Event.modify_duration f event)

pos_dur_sorted :: (Cmd.M m) => (ScoreTime -> ScoreTime) -> m ()
pos_dur_sorted f = events_sorted $ \(pos, event) ->
    Just $ (f pos, Event.modify_duration f event)

-- | Move everything at or after @start@ by @shift@.
move_track_events :: (State.M m) => ScoreTime -> ScoreTime -> TrackId -> m ()
move_track_events start shift track_id = State.modify_track_events track_id $
    \events -> move_events start shift events

-- | All events starting at and after a point to the end are shifted by the
-- given amount.
move_events :: ScoreTime -> ScoreTime -> Track.TrackEvents -> Track.TrackEvents
move_events point shift events = merged
    where
    -- If the last event has 0 duration, the selection will not include it.
    -- Ick.  Maybe I need a less error-prone way to say "select until the end
    -- of the track"?
    end = Track.time_end events + 1
    shifted = map (\(pos, evt) -> (pos+shift, evt))
        (Track.events_at_after point events)
    merged = Track.insert_sorted_events shifted
        (Track.remove_events point end events)

-- * util

map_track_sorted :: (Cmd.M m) => (Track.PosEvent -> Maybe Track.PosEvent)
    -> TrackId -> m ()
map_track_sorted f track_id = State.modify_track_events track_id $
    Track.from_sorted_events . Seq.map_maybe f . Track.event_list

map_track :: (Cmd.M m) => (Track.PosEvent -> Maybe Track.PosEvent)
    -> TrackId -> m ()
map_track f track_id = State.modify_track_events track_id $
    Track.from_events . Seq.map_maybe f . Track.event_list

-- | Mostly convenient for REPL use.
for_track :: (Cmd.M m) => TrackId -> (Track.PosEvent -> Maybe Track.PosEvent)
    -> m ()
for_track = flip map_track
