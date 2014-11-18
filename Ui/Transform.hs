-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for larger scale transformations on a State.
module Ui.Transform where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Map as Map
import qualified Util.Memory as Memory
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track

import Global
import Types


-- | Transform TracklikeIds.
tracks :: State.M m => BlockId
    -> (Block.TracklikeId -> Block.TracklikeId) -> m ()
tracks block_id f = do
    block <- modify <$> State.get_block block_id
    State.modify $ \st -> st { State.state_blocks =
        Map.insert block_id block (State.state_blocks st) }
    where
    modify block = block
        { Block.block_tracks = map modify_track (Block.block_tracks block) }
    modify_track t = t { Block.tracklike_id = f (Block.tracklike_id t) }

-- * map IDs

-- | Map a function across the IDs in the given state.  Any collisions are
-- thrown in Left.
map_state_ids :: (Id.Id -> Id.Id) -> State.State
    -> Either State.Error State.State
map_state_ids f state = State.exec state (map_ids f)

-- | Transform IDs, but don't update view_id pointer map.  So only use this
-- when you are sure there are no visible views (\"invisible\" views occur
-- after they are created but before the sync).  This should probably only be
-- used by 'map_state_ids'.
map_ids :: State.M m => (Id.Id -> Id.Id) -> m ()
map_ids f = do
    map_view_ids f
    map_block_ids f
    map_track_ids f
    map_ruler_ids f

map_namespace :: State.M m => (Id.Namespace -> Id.Namespace) -> m ()
map_namespace modify = map_ids set
    where set ident = Id.set_namespace (modify (Id.id_namespace ident)) ident

map_view_ids :: State.M m => (Id.Id -> Id.Id) -> m ()
map_view_ids f = do
    views <- State.gets State.state_views
    let view_f = Id.ViewId . f . Id.unpack_id
    new_views <- safe_map_keys "state_views" view_f views
    State.modify $ \st -> st { State.state_views = new_views }

-- | Rename a BlockId.  Views are updated to point to the new block.
map_block_ids :: State.M m => (Id.Id -> Id.Id) -> m ()
map_block_ids f = do
    maybe_root <- State.lookup_root_id
    let new_root = fmap (Id.BlockId . f . Id.unpack_id) maybe_root

    blocks <- State.gets State.state_blocks
    let block_f = Id.BlockId . f . Id.unpack_id
    new_blocks <- safe_map_keys "state_blocks" block_f blocks

    views <- State.gets State.state_views
    let new_views = Map.map
            (\v -> v { Block.view_block = block_f (Block.view_block v) })
            views
    State.modify $ \st -> st
        { State.state_blocks = new_blocks, State.state_views = new_views }
    State.modify_config $ \config -> config { State.config_root = new_root }

map_track_ids :: State.M m => (Id.Id -> Id.Id) -> m ()
map_track_ids f = do
    tracks <- State.gets State.state_tracks
    let track_f = Id.TrackId . f . Id.unpack_id
    new_tracks <- safe_map_keys "state_tracks" track_f tracks

    blocks <- State.gets State.state_blocks
    let new_blocks = Map.map
            (\b -> b { Block.block_tracks =
                map (map_track track_f) (Block.block_tracks b) })
            blocks
    State.modify $ \st -> st { State.state_tracks = new_tracks,
        State.state_blocks = new_blocks }
    where
    map_track f = map_merged f . map_track_id f
    map_track_id f track = case Block.tracklike_id track of
        Block.TId tid rid ->
            Block.modify_id (const (Block.TId (f tid) rid)) track
        _ -> track
    map_merged f track = track
        { Block.track_merged = Set.map f (Block.track_merged track) }

map_ruler_ids :: State.M m => (Id.Id -> Id.Id) -> m ()
map_ruler_ids f = do
    rulers <- State.gets State.state_rulers
    let ruler_f = Id.RulerId . f . Id.unpack_id
    new_rulers <- safe_map_keys "state_rulers" ruler_f rulers

    blocks <- State.gets State.state_blocks
    let new_blocks = Map.map
            (\b -> b { Block.block_tracks =
                map (map_track ruler_f) (Block.block_tracks b) })
            blocks
    State.modify $ \st ->
        st { State.state_rulers = new_rulers, State.state_blocks = new_blocks }
    where
    map_track f track = case Block.tracklike_id track of
        Block.TId tid rid ->
            Block.modify_id (const (Block.TId tid (f rid))) track
        Block.RId rid -> Block.modify_id (const (Block.RId (f rid))) track
        _ -> track

safe_map_keys :: (State.M m, Ord k, Show k) =>
    String -> (k -> k) -> Map.Map k v -> m (Map.Map k v)
safe_map_keys name f fm0
    | Map.size fm1 == Map.size fm0 = return fm1
    | otherwise = State.throw $ "keys collided in " ++ show name ++ ": "
        ++ show (Map.keys (Map.difference fm0 fm1))
    where fm1 = Map.mapKeys f fm0

-- * namespace

-- | Destroy all views, blocks, tracks, and rulers with the given namespace.
destroy_namespace :: State.M m => Id.Namespace -> m ()
destroy_namespace ns = do
    -- Will destroy any views too.
    mapM_ State.destroy_block
        =<< State.gets (in_ns . Map.keys . State.state_blocks)
    mapM_ State.destroy_track
        =<< State.gets (in_ns . Map.keys . State.state_tracks)
    mapM_ State.destroy_ruler
        =<< State.gets (in_ns . Map.keys . State.state_rulers)
    where
    in_ns :: (Id.Ident a) => [a] -> [a]
    in_ns = filter $ (==ns) . Id.ident_namespace


-- * merge

-- | Merge ID maps from the states together.  Collisions will throw.
-- The 'State.Config' comes from the first state.
merge_states :: State.State -> State.State -> Either State.Error State.State
merge_states st0 st1 = State.exec st0 $ do
    views <- safe_union "views"
        (State.state_views st0) (State.state_views st1)
    blocks <- safe_union "blocks"
        (State.state_blocks st0) (State.state_blocks st1)
    tracks <- safe_union "tracks"
        (State.state_tracks st0) (State.state_tracks st1)
    rulers <- safe_union "rulers"
        (State.state_rulers st0) (State.state_rulers st1)
    State.modify $ \st -> st
        { State.state_views = views, State.state_blocks = blocks
        , State.state_tracks = tracks, State.state_rulers = rulers
        }

safe_union :: (State.M m, Ord k, Show k) => String
    -> Map.Map k a -> Map.Map k a -> m (Map.Map k a)
safe_union name fm0 fm1
    | Map.null overlapping = return fm
    | otherwise = State.throw $
        "keys collided in " ++ show name ++ ": " ++ show (Map.keys overlapping)
    where (fm, overlapping) = Map.unique_union fm0 fm1


-- * intern

-- | Increase sharing in event text with an intern table.
intern_text :: State.State -> (State.State, Map.Map Text Int)
intern_text state =
    (state { State.state_tracks = Map.fromAscList tracks }, Map.map snd table)
    where
    (table, tracks) = List.mapAccumL intern_track Map.empty
        (Map.toAscList (State.state_tracks state))
    intern_track state (track_id, track) =
        (state2, (track_id, track
            { Track.track_events = Events.from_list events }))
        where
        (state2, events) = List.mapAccumL Event.intern_event state
            (Events.ascending (Track.track_events track))

intern_stats :: Map.Map Text Int -> (Memory.Size, Int)
intern_stats table =
    (Memory.from_bytes $ sum (map stats (Map.toList table)), total_hits)
    where
    total_hits = sum (Map.elems table) - Map.size table
    stats (text, hits) = size * (hits - 1)
        where size = Text.length text * 2 + 3 * 4 -- pointer + length + start
