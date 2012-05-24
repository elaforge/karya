-- | Utilities to modify events in tracks.
module Cmd.ModifyEvents where
import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.TrackInfo as TrackInfo
import Types


-- | Transform or delete an event.
type TrackPosEvent m = TrackId -> PosEvent m
type PosEvent m = Events.PosEvent -> m [Events.PosEvent]
type Event m = ScoreTime -> Event.Event -> m [Event.Event]

-- | Map a function over the selected events, passing the track id.  Returning
-- Nothing will leave the track unchanged.
type Track m = BlockId -> TrackId -> [Events.PosEvent]
    -> m (Maybe [Events.PosEvent])

-- | The transformation functions take the most general 'PosEvent', but most
-- uses won't need that generality, so these functions convert to more specific
-- usage.
event :: (Monad m) => (ScoreTime -> Event.Event -> m [Event.Event])
    -> PosEvent m
event f = \(pos, event) -> map ((,) pos) `liftM` f pos event

event1 :: (Monad m) => (ScoreTime -> Event.Event -> Event.Event) -> PosEvent m
event1 f = \(pos, event) -> return [(pos, f pos event)]

text :: (Monad m) => (String -> String) -> PosEvent m
text f = \(pos, event) -> return [(pos, Event.modify_string f event)]

-- | Take a text transformation that can fail to a Track transformation that
-- transforms all the events and throws if any of the text transformations
-- failed.
failable_texts :: (Cmd.M m) => (String -> Maybe String) -> Track m
failable_texts f block_id track_id events = do
    let (failed, ok) = Seq.partition_either $ map (failing_text f) events
    unless (null failed) $
        Cmd.throw $ "transformation failed on events at: "
            ++ Seq.join ", " (map (Cmd.log_event block_id track_id) failed)
    return $ Just ok
    where
    failing_text f = \(pos, event) -> case f (Event.event_string event) of
        Nothing -> Left (pos, event)
        Just text -> Right (pos, Event.set_string text event)

-- | Convert a PosEvent function to work on a Track.
track_events :: (Monad m) => PosEvent m -> Track m
track_events f = \_bid _tid events -> (liftM (Just . concat) (mapM f events))

track_text :: (Monad m) => (String -> String) -> Track m
track_text = track_events . text


-- * modify selections

-- | Map a function over the selected events.
events :: (Cmd.M m) => PosEvent m -> m ()
events f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        events <- concat <$> mapM f events
        State.insert_events track_id events

-- | This is like 'events'.  It's more efficient but the modify function must
-- promise to return events in sorted order.
events_sorted :: (Cmd.M m) => PosEvent m -> m ()
events_sorted f = do
    selected <- Selection.events
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        events <- concat <$> mapM f events
        State.insert_sorted_events track_id events

tracks :: (Cmd.M m) => Track m -> m ()
tracks f = tracks_sorted $ \block_id track_id events ->
    fmap Events.sort <$> f block_id track_id events

-- | As with 'events_sorted', this is a more efficient version for sorted
-- events.
tracks_sorted :: (Cmd.M m) => Track m -> m ()
tracks_sorted f = do
    block_id <- Cmd.get_focused_block
    track_events <- Selection.events
    forM_ track_events $ \(track_id, (start, end), events) -> do
        maybe_new_events <- f block_id track_id events
        case maybe_new_events of
            Just new_events -> do
                State.remove_events track_id start end
                State.insert_sorted_events track_id new_events
            Nothing -> return ()

-- | Make a 'PosEvent' into a 'Track' that only maps over certain named tracks.
tracks_named :: (Cmd.M m) => (String -> Bool) -> Track m -> Track m
tracks_named wanted f = \block_id track_id events ->
    ifM (not . wanted <$> State.get_track_title track_id)
        (return Nothing) (f block_id track_id events)

-- | Like 'tracks' but only for note tracks.
note_tracks :: (Cmd.M m) => Track m -> m ()
note_tracks = tracks . tracks_named TrackInfo.is_note_track

control_tracks :: (Cmd.M m) => Track m -> m ()
control_tracks = tracks . tracks_named TrackInfo.is_signal_track

pitch_tracks :: (Cmd.M m) => Track m -> m ()
pitch_tracks = tracks . tracks_named TrackInfo.is_pitch_track


-- * block tracks

-- | Like 'tracks', but maps over an entire block.
block_tracks :: (Cmd.M m) => BlockId -> Track m -> m ()
block_tracks block_id f = do
    track_ids <- Block.block_track_ids <$> State.get_block block_id
    forM_ track_ids $ \track_id -> do
        events <- State.get_all_events track_id
        maybe (return ())
                (State.modify_events track_id . const . Events.from_list)
            =<< f block_id track_id events

all_blocks :: (Cmd.M m) => Track m -> m ()
all_blocks f = mapM_ (flip block_tracks f) =<< State.get_all_block_ids


-- * misc

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
