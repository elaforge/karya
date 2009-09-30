{- | Simple Events are supposed to be easy to read, and easy to serialize to
    text and load back again.  Functions here convert them to and from text
    form, stashing converted simple blocks in the clipboard.
-}
module Cmd.Simple where
import Control.Monad
import qualified Control.Monad.Trans as Trans

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection

import qualified App.Config as Config


-- | TODO should it have a ruler?  otherwise they come in without a ruler... but
-- copy and paste can't copy and paste the ruler...
--
-- (id_name, title, tracks)
type Block = (String, String, [Track])

-- | (id_name, title, events)
type Track = (String, String, [Event])

-- | (start, duration, text)
type Event = (Double, Double, String)

event :: Track.PosEvent -> Event
event (start, event) = (realToFrac start,
    realToFrac (Event.event_duration event), Event.event_text event)

dump_block :: (State.UiStateMonad m) => BlockId -> m Block
dump_block block_id = do
    block <- State.get_block block_id
    let track_ids = Block.block_track_ids block
    tracks <- mapM dump_track track_ids
    return (show block_id, Block.block_title block, tracks)

dump_track :: (State.UiStateMonad m) => TrackId -> m Track
dump_track track_id = do
    track <- State.get_track track_id
    return (simplify_track track_id track)

simplify_track :: TrackId -> Track.Track -> Track
simplify_track track_id track =
    (show track_id, Track.track_title track, map event events)
    where events = Track.event_list (Track.track_events track)

dump_selection :: Cmd.CmdL [(TrackId, [Event])]
dump_selection = do
    (_, _, track_events) <- Selection.selected_events True Config.insert_selnum
    return [(track_id, map event events)
        | (track_id, events) <- track_events]

-- * load

load_block :: FilePath -> Cmd.CmdL ()
load_block fn = read_block fn >>= Clip.state_to_clip

read_block :: FilePath -> Cmd.CmdL State.State
read_block fn = do
    simple_block <- Trans.liftIO $ (readIO =<< readFile fn :: IO Block)
    convert_block simple_block

convert_block :: (State.UiStateMonad m) => Block -> m State.State
convert_block (id_name, title, tracks) =
    State.exec_rethrow "convert block" State.empty $ do
        tracks <- mapM convert_track tracks
        State.create_block (Id.read_id id_name) $
            Block.block title Config.block_config
                (State.no_ruler_track:tracks) Config.schema

convert_track :: (State.UiStateMonad m) =>
    Track -> m Block.BlockTrack
convert_track (id_name, title, events) = do
    let pos_events = map convert_event events
    track_id <- State.create_track (Id.read_id id_name) $
        Track.track title pos_events Config.track_bg Config.render_config
    return $ Block.block_track
        (Block.TId track_id State.no_ruler) Config.track_width

convert_event :: Event -> Track.PosEvent
convert_event (start, dur, text) =
    (realToFrac start, Event.event text (realToFrac dur))
