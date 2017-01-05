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
import qualified Ui.Ui as Ui
import qualified Ui.Track as Track

import Global
import Types


-- | Transform TracklikeIds.
tracks :: Ui.M m => BlockId -> (Block.TracklikeId -> Block.TracklikeId) -> m ()
tracks block_id f = do
    block <- modify <$> Ui.get_block block_id
    Ui.modify $ \st -> st
        { Ui.state_blocks = Map.insert block_id block (Ui.state_blocks st) }
    where
    modify block = block
        { Block.block_tracks = map modify_track (Block.block_tracks block) }
    modify_track t = t { Block.tracklike_id = f (Block.tracklike_id t) }

-- * map IDs

-- | Map a function across the IDs in the given state.  Any collisions are
-- thrown in Left.
map_state_ids :: (Id.Id -> Id.Id) -> Ui.State -> Either Ui.Error Ui.State
map_state_ids f state = Ui.exec state (map_ids f)

-- | Transform IDs, but don't update view_id pointer map.  So only use this
-- when you are sure there are no visible views (\"invisible\" views occur
-- after they are created but before the sync).  This should probably only be
-- used by 'map_state_ids'.
map_ids :: Ui.M m => (Id.Id -> Id.Id) -> m ()
map_ids f = do
    map_view_ids f
    map_block_ids f
    map_track_ids f
    map_ruler_ids f

map_namespace :: Ui.M m => (Id.Namespace -> Id.Namespace) -> m ()
map_namespace modify = map_ids set
    where set ident = Id.set_namespace (modify (Id.id_namespace ident)) ident

map_view_ids :: Ui.M m => (Id.Id -> Id.Id) -> m ()
map_view_ids f = do
    views <- Ui.gets Ui.state_views
    let view_f = Id.ViewId . f . Id.unpack_id
    new_views <- safe_map_keys "state_views" view_f views
    Ui.modify $ \st -> st { Ui.state_views = new_views }

-- | Rename a BlockId.  Views are updated to point to the new block.
map_block_ids :: Ui.M m => (Id.Id -> Id.Id) -> m ()
map_block_ids f = do
    maybe_root <- Ui.lookup_root_id
    let new_root = fmap (Id.BlockId . f . Id.unpack_id) maybe_root

    blocks <- Ui.gets Ui.state_blocks
    let block_f = Id.BlockId . f . Id.unpack_id
    new_blocks <- safe_map_keys "state_blocks" block_f blocks

    views <- Ui.gets Ui.state_views
    let new_views = Map.map
            (\v -> v { Block.view_block = block_f (Block.view_block v) })
            views
    Ui.modify $ \st -> st
        { Ui.state_blocks = new_blocks, Ui.state_views = new_views }
    Ui.modify_config $ \config -> config { Ui.config_root = new_root }

map_track_ids :: Ui.M m => (Id.Id -> Id.Id) -> m ()
map_track_ids f = do
    tracks <- Ui.gets Ui.state_tracks
    let track_f = Id.TrackId . f . Id.unpack_id
    new_tracks <- safe_map_keys "state_tracks" track_f tracks

    blocks <- Ui.gets Ui.state_blocks
    let new_blocks = Map.map
            (\b -> b { Block.block_tracks =
                map (map_track track_f) (Block.block_tracks b) })
            blocks
    Ui.modify $ \st -> st
        { Ui.state_tracks = new_tracks, Ui.state_blocks = new_blocks }
    where
    map_track f = map_merged f . map_track_id f
    map_track_id f track = case Block.tracklike_id track of
        Block.TId tid rid ->
            Block.modify_id (const (Block.TId (f tid) rid)) track
        _ -> track
    map_merged f track = track
        { Block.track_merged = Set.map f (Block.track_merged track) }

map_ruler_ids :: Ui.M m => (Id.Id -> Id.Id) -> m ()
map_ruler_ids f = do
    rulers <- Ui.gets Ui.state_rulers
    let ruler_f = Id.RulerId . f . Id.unpack_id
    new_rulers <- safe_map_keys "state_rulers" ruler_f rulers

    blocks <- Ui.gets Ui.state_blocks
    let new_blocks = Map.map
            (\b -> b { Block.block_tracks =
                map (map_track ruler_f) (Block.block_tracks b) })
            blocks
    Ui.modify $ \st ->
        st { Ui.state_rulers = new_rulers, Ui.state_blocks = new_blocks }
    where
    map_track f track = case Block.tracklike_id track of
        Block.TId tid rid ->
            Block.modify_id (const (Block.TId tid (f rid))) track
        Block.RId rid -> Block.modify_id (const (Block.RId (f rid))) track
        _ -> track

safe_map_keys :: (Ui.M m, Ord k, Show k) =>
    String -> (k -> k) -> Map.Map k v -> m (Map.Map k v)
safe_map_keys name f fm0
    | Map.size fm1 == Map.size fm0 = return fm1
    | otherwise = Ui.throw $ "keys collided in " <> showt name <> ": "
        <> showt (Map.keys (Map.difference fm0 fm1))
    where fm1 = Map.mapKeys f fm0

-- * namespace

-- | Destroy all views, blocks, tracks, and rulers with the given namespace.
destroy_namespace :: Ui.M m => Id.Namespace -> m ()
destroy_namespace ns = do
    -- Will destroy any views too.
    mapM_ Ui.destroy_block =<< Ui.gets (in_ns . Map.keys . Ui.state_blocks)
    mapM_ Ui.destroy_track =<< Ui.gets (in_ns . Map.keys . Ui.state_tracks)
    mapM_ Ui.destroy_ruler =<< Ui.gets (in_ns . Map.keys . Ui.state_rulers)
    where
    in_ns :: Id.Ident a => [a] -> [a]
    in_ns = filter $ (==ns) . Id.ident_namespace

-- | Replace the namespace of the second state with the one from the first
-- state.
replace_namespace :: Id.Namespace -> Ui.State -> Ui.State -> Ui.State
replace_namespace ns from to = to
    { Ui.state_views = merge Ui.state_views
    , Ui.state_blocks = merge Ui.state_blocks
    , Ui.state_tracks = merge Ui.state_tracks
    , Ui.state_rulers = merge Ui.state_rulers
    }
    where
    merge field =
        Map.union (Map.filterWithKey (\k _ -> wanted k) (field from)) (field to)
        where wanted = (==ns) . Id.ident_namespace


-- * merge

-- | Merge ID maps from the states together.  Collisions will throw.
-- The 'Ui.Config' comes from the first state.
merge_states :: Ui.State -> Ui.State -> Either Ui.Error Ui.State
merge_states st0 st1 = Ui.exec st0 $ do
    views <- safe_union "views" (Ui.state_views st0) (Ui.state_views st1)
    blocks <- safe_union "blocks" (Ui.state_blocks st0) (Ui.state_blocks st1)
    tracks <- safe_union "tracks" (Ui.state_tracks st0) (Ui.state_tracks st1)
    rulers <- safe_union "rulers" (Ui.state_rulers st0) (Ui.state_rulers st1)
    Ui.modify $ \st -> st
        { Ui.state_views = views, Ui.state_blocks = blocks
        , Ui.state_tracks = tracks, Ui.state_rulers = rulers
        }

safe_union :: (Ui.M m, Ord k, Show k) => String
    -> Map.Map k a -> Map.Map k a -> m (Map.Map k a)
safe_union name fm0 fm1
    | Map.null overlapping = return fm
    | otherwise = Ui.throw $ "keys collided in " <> showt name <> ": "
        <> showt (Map.keys overlapping)
    where (fm, overlapping) = Map.unique_union fm0 fm1


-- * intern

-- | Increase sharing in event text with an intern table.
intern_text :: Ui.State -> (Ui.State, Map.Map Text Int)
intern_text state =
    (state { Ui.state_tracks = Map.fromAscList tracks }, Map.map snd table)
    where
    (table, tracks) = List.mapAccumL intern_track Map.empty
        (Map.toAscList (Ui.state_tracks state))
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
