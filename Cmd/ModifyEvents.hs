-- | Utilities to modify events in tracks.
module Cmd.ModifyEvents where
import Control.Monad
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection


-- * main modification

-- | Map a function over the selected events.
events :: (Monad m) => (Track.PosEvent -> Maybe Track.PosEvent) -> Cmd.CmdT m ()
events f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        State.insert_events track_id (Seq.map_maybe f events)

-- | This is like 'events'.  It's more efficient but the modify function must
-- promise to return events in sorted order.
events_sorted :: (Monad m) => (Track.PosEvent -> Maybe Track.PosEvent)
    -> Cmd.CmdT m ()
events_sorted f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        State.insert_sorted_events track_id (Seq.map_maybe f events)

-- | Map a function over the selected events, passing the track id.
tracks :: (Monad m) =>
    (TrackId -> [Track.PosEvent] -> Cmd.CmdT m [Track.PosEvent])
    -> Cmd.CmdT m ()
tracks f = tracks_sorted $ \track_id events ->
    fmap Track.sort_events (f track_id events)

tracks_sorted :: (Monad m) =>
    (TrackId -> [Track.PosEvent] -> Cmd.CmdT m [Track.PosEvent])
    -> Cmd.CmdT m ()
tracks_sorted f = do
    track_events <- Selection.events
    forM_ track_events $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        new_events <- f track_id events
        State.insert_sorted_events track_id new_events

-- * convenience

-- | Modify events in the selection.  For efficiency, this can't move the
-- events.
modify_pos_events :: (Monad m) => (ScoreTime -> Event.Event -> Event.Event)
    -> Cmd.CmdT m ()
modify_pos_events f = do
    track_events <- Selection.events
    forM_ track_events $ \(track_id, _, events) -> do
        let insert = [(pos, f pos evt) | (pos, evt) <- events]
        State.insert_sorted_events track_id insert

-- | A version of 'modify_events_pos' that doesn't pass the pos.
modify_events :: (Monad m) => (Event.Event -> Event.Event) -> Cmd.CmdT m ()
modify_events f = modify_pos_events (\_ evt -> f evt)

-- | Modify the start time and duration of the selected events.
pos_dur :: (Monad m) => (ScoreTime -> ScoreTime) -> Cmd.CmdT m ()
pos_dur f = events $ \(pos, event) ->
    Just $ (f pos, Event.modify_duration f event)

pos_dur_sorted :: (Monad m) => (ScoreTime -> ScoreTime) -> Cmd.CmdT m ()
pos_dur_sorted f = events_sorted $ \(pos, event) ->
    Just $ (f pos, Event.modify_duration f event)

-- | Move everything at or after @start@ by @shift@.
move_track_events :: (State.UiStateMonad m) =>
    ScoreTime -> ScoreTime -> TrackId -> m ()
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
        (Track.events_after point events)
    merged = Track.insert_sorted_events shifted
        (Track.remove_events point end events)

-- * util

map_track_sorted :: (Monad m) => (Track.PosEvent -> Maybe Track.PosEvent)
    -> TrackId -> Cmd.CmdT m ()
map_track_sorted f track_id = State.modify_track_events track_id $
    Track.from_sorted_events . Seq.map_maybe f . Track.event_list

map_track :: (Monad m) => (Track.PosEvent -> Maybe Track.PosEvent)
    -> TrackId -> Cmd.CmdT m ()
map_track f track_id = State.modify_track_events track_id $
    Track.from_events . Seq.map_maybe f . Track.event_list

-- | Mostly convenient for REPL use.
for_track :: (Monad m) => TrackId -> (Track.PosEvent -> Maybe Track.PosEvent)
    -> Cmd.CmdT m ()
for_track = flip map_track
