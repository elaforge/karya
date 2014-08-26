-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to modify events in tracks.
module Cmd.ModifyEvents where
import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import Types


-- | Map a function over events on a certain track.  Returning Nothing will
-- leave the track unchanged.
type Track m = BlockId -> TrackId -> [Event.Event] -> m (Maybe [Event.Event])

-- | Map a function over a set of events.
events :: Monad m => ([Event.Event] -> m [Event.Event]) -> Track m
events f _ _ = liftM Just . f

-- | Map a function over a single event.
event :: Monad m => (Event.Event -> Event.Event) -> Track m
event f = events (return . map f)

text :: Monad m => (Text -> Text) -> Track m
text = event . (\f event -> Event.set_text (f (Event.event_text event)) event)

-- | Split up a pipeline and lex the calls.
pipeline :: ([[Text]] -> [[Text]]) -> Text -> Text
pipeline modify = Parse.join_pipeline . modify . Parse.split_pipeline

-- | Take a text transformation that can fail to a Track transformation that
-- transforms all the events and throws if any of the text transformations
-- failed.
failable_texts :: Cmd.M m => (Text -> Either String Text) -> Track m
failable_texts f block_id track_id events = do
    let (failed, ok) = Seq.partition_either $ map (failing_text f) events
        errs = [err ++ ": " ++ Cmd.log_event block_id track_id evt
            | (err, evt) <- failed]
    unless (null errs) $ Cmd.throw $
        "transformation failed: " ++ Seq.join ", " errs
    return $ Just ok
    where
    failing_text f event = case f (Event.event_text event) of
        Left err -> Left (err, event)
        Right text -> Right $ Event.set_text text event


-- * modify selections

-- | Map a function over the selected events, as per 'Selection.events'.
selection :: Cmd.M m => Track m -> m ()
selection f = do
    selected <- Selection.events
    block_id <- Cmd.get_focused_block
    forM_ selected $ \(track_id, (start, end), events) -> do
        maybe_new_events <- f block_id track_id events
        whenJust maybe_new_events $ \new_events -> do
            State.remove_events track_id start end
            State.insert_block_events block_id track_id new_events

-- | Like 'selection', but advance it afterwards if it was a point selection.
-- This is convenient for applying a transformation repeatedly.
selection_advance :: Cmd.M m => Track m -> m ()
selection_advance f = do
    selection f
    whenM (Types.sel_is_point . snd <$> Selection.get)
        Selection.advance

-- | Map a function over the events that overlap the selection point.
overlapping :: Cmd.M m => Track m -> m ()
overlapping f = do
    (block_id, _, track_ids, start, end) <- Selection.tracks
    let pos = Selection.point_pos start end
    forM_ track_ids $ \track_id -> do
        maybe_event <- Events.overlapping pos . Track.track_events <$>
            State.get_track track_id
        whenJust maybe_event $ \event -> do
            maybe_new_events <- f block_id track_id [event]
            whenJust maybe_new_events $ \new_events -> do
                State.remove_event track_id pos
                State.insert_block_events block_id track_id new_events

-- | Map over tracks whose name matches the predicate.
tracks_named :: Cmd.M m => (Text -> Bool) -> Track m -> Track m
tracks_named wanted f = \block_id track_id events ->
    ifM (wanted <$> State.get_track_title track_id)
        (f block_id track_id events) (return Nothing)

-- | Like 'tracks' but only for note tracks.
note_tracks :: Cmd.M m => Track m -> m ()
note_tracks = selection . tracks_named ParseTitle.is_note_track

control_tracks :: Cmd.M m => Track m -> m ()
control_tracks = selection . tracks_named ParseTitle.is_signal_track

pitch_tracks :: Cmd.M m => Track m -> m ()
pitch_tracks = selection . tracks_named ParseTitle.is_pitch_track


-- * block tracks

-- | Like 'selection', but maps over an entire block.
block :: Cmd.M m => BlockId -> Track m -> m ()
block block_id f = do
    track_ids <- Block.block_track_ids <$> State.get_block block_id
    forM_ track_ids $ \track_id -> do
        events <- State.get_all_events track_id
        maybe (return ())
                (State.modify_events track_id . const . Events.from_list)
            =<< f block_id track_id events

all_blocks :: Cmd.M m => Track m -> m ()
all_blocks f = mapM_ (flip block f) =<< State.all_block_ids

all_tracks_named :: Cmd.M m => (Text -> Bool) -> Track m -> m ()
all_tracks_named wanted = all_blocks . tracks_named wanted

all_note_tracks :: Cmd.M m => Track m -> m ()
all_note_tracks = all_tracks_named ParseTitle.is_note_track

all_control_tracks :: Cmd.M m => Track m -> m ()
all_control_tracks = all_tracks_named ParseTitle.is_control_track

all_pitch_tracks :: Cmd.M m => Track m -> m ()
all_pitch_tracks = all_tracks_named ParseTitle.is_pitch_track

-- * misc

-- | Move everything at or after @start@ by @shift@.
move_track_events :: State.M m => ScoreTime -> ScoreTime -> ScoreTime
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
