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


-- | Replace an event with zero or more events.
type PosEvent m = Event.Event -> m [Event.Event]

-- | Map a function over the selected events, passing the track id.  Returning
-- Nothing will leave the track unchanged.
type Track m = BlockId -> TrackId -> [Event.Event] -> m (Maybe [Event.Event])

-- | The transformation functions take the most general 'PosEvent', but most
-- uses won't need that generality, so these functions convert to more specific
-- usage.
event :: (Monad m) => (Event.Event -> Event.Event) -> PosEvent m
event f = \event -> return [f event]

text :: (Monad m) => (String -> String) -> PosEvent m
text f = \event -> return [Event.modify_string f event]

-- | Take a text transformation that can fail to a Track transformation that
-- transforms all the events and throws if any of the text transformations
-- failed.
failable_texts :: (Cmd.M m) => (String -> Either String String) -> Track m
failable_texts f block_id track_id events = do
    let (failed, ok) = Seq.partition_either $ map (failing_text f) events
        errs = [err ++ ": " ++ Cmd.log_event block_id track_id evt
            | (err, evt) <- failed]
    unless (null errs) $ Cmd.throw $
        "transformation failed: " ++ Seq.join ", " errs
    return $ Just ok
    where
    failing_text f event = case f (Event.event_string event) of
        Left err -> Left (err, event)
        Right text -> Right $ Event.set_string text event

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
    block_id <- Cmd.get_focused_block
    forM_ selected $ \(track_id, (start, end), events) -> do
        State.remove_events track_id start end
        events <- concatMapM f events
        State.insert_block_events block_id track_id events

-- | Map a function over the selected tracks.
tracks :: (Cmd.M m) => Track m -> m ()
tracks f = do
    selected <- Selection.events
    block_id <- Cmd.get_focused_block
    forM_ selected $ \(track_id, (start, end), events) -> do
        maybe_new_events <- f block_id track_id events
        case maybe_new_events of
            Just new_events -> do
                State.remove_events track_id start end
                State.insert_block_events block_id track_id new_events
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
all_blocks f = mapM_ (flip block_tracks f) =<< State.all_block_ids


-- * misc

-- | Move everything at or after @start@ by @shift@.
move_track_events :: (State.M m) => ScoreTime -> ScoreTime -> ScoreTime
    -> TrackId -> m ()
move_track_events block_end start shift track_id =
    State.modify_events track_id $ \events ->
        move_events block_end start shift events

-- | All events starting at and after a point to the end are shifted by the
-- given amount.
move_events :: ScoreTime -- ^ events past the block end are shortened or removed
    -> ScoreTime -> ScoreTime -> Events.Events -> Events.Events
move_events block_end point shift events = merged
    where
    -- If the last event has 0 duration, the selection will not include it.
    -- Ick.  Maybe I need a less error-prone way to say "select until the end
    -- of the track"?
    end = Events.time_end events + 1
    shifted = Events.clip block_end $
        map (Event.move (+shift)) (Events.at_after point events)
    merged = Events.insert shifted (Events.remove point end events)
