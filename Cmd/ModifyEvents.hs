-- | Utilities to modify events in tracks.
module Cmd.ModifyEvents where
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.TrackInfo as TrackInfo
import Types


-- * main modification

type Event = Events.PosEvent -> Maybe Events.PosEvent

-- | Map a function over the selected events.  Returning Nothing will remove
-- the event.
events :: (Cmd.M m) => Event -> m ()
events f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        State.insert_events track_id (Maybe.mapMaybe f events)

-- | This is like 'events'.  It's more efficient but the modify function must
-- promise to return events in sorted order.
events_sorted :: (Cmd.M m) => Event -> m ()
events_sorted f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        State.insert_sorted_events track_id (Maybe.mapMaybe f events)

-- | Map a function over the selected events, passing the track id.  Unlike
-- 'events', returning Nothing will leave the track unchanged.
type Track m = TrackId -> [Events.PosEvent] -> m (Maybe [Events.PosEvent])

tracks :: (Cmd.M m) => Track m -> m ()
tracks f = tracks_sorted $ \track_id events ->
    fmap Events.sort <$> f track_id events

-- | As with 'events_sorted', this is a more efficient version for sorted
-- events.
tracks_sorted :: (Cmd.M m) => Track m -> m ()
tracks_sorted f = do
    track_events <- Selection.events
    forM_ track_events $ \(track_id, (start, end), events) -> do
        maybe_new_events <- f track_id events
        case maybe_new_events of
            Just new_events -> do
                State.remove_events track_id start end
                State.insert_sorted_events track_id new_events
            Nothing -> return ()

-- ** block tracks

-- | Like 'tracks', but maps over an entire block.
block_tracks :: (Cmd.M m) => BlockId -> Track m -> m ()
block_tracks block_id f = do
    track_ids <- Block.block_track_ids <$> State.get_block block_id
    forM_ track_ids $ \track_id -> do
        events <- State.get_all_events track_id
        maybe (return ())
                (State.modify_events track_id . const . Events.from_list)
            =<< f track_id events

-- | Map over the events in note tracks.
map_note_tracks :: (Cmd.M m) => BlockId
    -> (Events.PosEvent -> Maybe Events.PosEvent) -> m ()
map_note_tracks block_id f = block_tracks block_id $ \track_id events ->
    ifM (TrackInfo.is_note_track <$> State.get_track_title track_id)
        (return $ Just $ Maybe.mapMaybe f events)
        (return Nothing)


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
    Just (f pos, Event.modify_duration f event)

pos_dur_sorted :: (Cmd.M m) => (ScoreTime -> ScoreTime) -> m ()
pos_dur_sorted f = events_sorted $ \(pos, event) ->
    Just (f pos, Event.modify_duration f event)

-- | Move everything at or after @start@ by @shift@.
move_track_events :: (State.M m) => ScoreTime -> ScoreTime -> TrackId -> m ()
move_track_events start shift track_id = State.modify_events track_id $
    \events -> move_events start shift events

-- | All events starting at and after a point to the end are shifted by the
-- given amount.
move_events :: ScoreTime -> ScoreTime -> Events.Events -> Events.Events
move_events point shift events = merged
    where
    -- If the last event has 0 duration, the selection will not include it.
    -- Ick.  Maybe I need a less error-prone way to say "select until the end
    -- of the track"?
    end = Events.time_end events + 1
    shifted = map (\(pos, evt) -> (pos+shift, evt))
        (Events.at_after point events)
    merged = Events.insert_sorted_events shifted
        (Events.remove_events point end events)

-- * util

map_track_sorted :: (Cmd.M m) => (Events.PosEvent -> Maybe Events.PosEvent)
    -> TrackId -> m ()
map_track_sorted f track_id = State.modify_events track_id $
    Events.from_asc_list . Maybe.mapMaybe f . Events.ascending

map_track :: (Cmd.M m) => (Events.PosEvent -> Maybe Events.PosEvent)
    -> TrackId -> m ()
map_track f track_id = State.modify_events track_id $
    Events.from_list . Maybe.mapMaybe f . Events.ascending

-- | Mostly convenient for REPL use.
for_track :: (Cmd.M m) => TrackId -> (Events.PosEvent -> Maybe Events.PosEvent)
    -> m ()
for_track = flip map_track
