{- | Simple Events are supposed to be easy to read, and easy to serialize to
text and load back again.  Functions here convert them to and from text form,
stashing converted simple blocks in the clipboard.
-}
module Cmd.Simple where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.Event as Event
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State

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

dump_block :: Block.BlockId -> Cmd.CmdL Block
dump_block block_id = do
    block <- State.get_block block_id
    let track_ids = Maybe.catMaybes $
            map Block.track_id_of (map fst (Block.block_tracks block))
    tracks <- mapM dump_track track_ids
    return (Id.show_id (Block.un_block_id block_id), Block.block_title block,
        tracks)

dump_track :: Track.TrackId -> Cmd.CmdL Track
dump_track track_id = do
    track <- State.get_track track_id
    let events = Track.event_list (Track.track_events track)
    return (Id.show_id (Track.un_track_id track_id), Track.track_title track,
        map event events)

dump_selection :: Cmd.CmdL [(Track.TrackId, [Event])]
dump_selection = do
    track_events <- Selection.selected_events Config.insert_selnum
    return [(track_id, map event events)
        | (track_id, events) <- track_events]

-- | Replace the clipboard with the given state.
clip_state :: (Monad m) => State.State -> Cmd.CmdT m ()
clip_state state = do
    clip <- fmap Cmd.state_clipboard_namespace Cmd.get_state
    destroy_namespace clip
    state' <- State.throw_either (set_namespace clip state)
    global_st <- State.get
    merged <- State.throw_either (State.merge_states global_st state')
    State.put merged

-- | Put all the the Ids in the state into a new namespace.
-- Collisions will throw.
set_namespace :: Id.Namespace -> State.State
    -> Either State.StateError State.State
set_namespace ns = State.map_state_ids $ \ident ->
    let (_, name) = Id.un_id ident in Id.id ns name

-- | Destroy all views, blocks, tracks, and rulers with the given namespace.
destroy_namespace :: (State.UiStateMonad m) => Id.Namespace -> m ()
destroy_namespace ns = do
    block_ids <- fmap (filter ((==ns) . Id.id_namespace . Block.un_block_id))
        State.get_all_block_ids
    blocks <- mapM State.get_block block_ids
    let tracks = concatMap Block.block_tracks_xx blocks
        track_ids = Seq.unique (Maybe.catMaybes (map Block.track_id_of tracks))
        ruler_ids = Seq.unique (Maybe.catMaybes (map Block.ruler_id_of tracks))
    mapM_ State.destroy_block block_ids -- will destroy any views too
    mapM_ State.destroy_track track_ids
    mapM_ State.destroy_ruler ruler_ids

-- * load

load_block :: FilePath -> Cmd.CmdL ()
load_block fn = read_block fn >>= clip_state

read_block :: FilePath -> Cmd.CmdL State.State
read_block fn = do
    simple_block <- Trans.liftIO $ (readIO =<< readFile fn :: IO Block)
    convert_block simple_block

convert_block :: (State.UiStateMonad m) => Block -> m State.State
convert_block (id_name, title, tracks) = State.exec_rethrow State.empty $ do
    tracks <- mapM convert_track tracks
    State.create_block (Id.read_id id_name) $
        Block.block title Config.block_config
            (State.no_ruler_track:tracks) Config.schema

convert_track :: (State.UiStateMonad m) =>
    Track -> m (Block.TracklikeId, Block.Width)
convert_track (id_name, title, events) = do
    let pos_events = map convert_event events
    track_id <- State.create_track (Id.read_id id_name) $
        Track.track title pos_events Config.track_bg Config.render_config
    return (Block.TId track_id State.no_ruler, Config.track_width)

convert_event :: Event -> Track.PosEvent
convert_event (start, dur, text) =
    (realToFrac start, Config.event text (realToFrac dur))
