-- | Functions for larger scale transformations on a State.
module Ui.Transform where
import qualified Data.Map as Map

import qualified Util.Map as Map

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types


-- * map IDs

-- | Map a function across the IDs in the given state.  Any collisions are
-- thrown in Left.
map_state_ids :: (Id.Id -> Id.Id) -> State.State
    -> Either State.StateError State.State
map_state_ids f state = State.exec state (map_ids f)

-- | Transform IDs, but don't update view_id pointer map.  So only use this
-- when you are sure there are no visible views (\"invisible\" views occur
-- after they are created but before the sync).  This should probably only be
-- used by 'map_state_ids'.
--
-- SchemaIds are not mapped, because they point to a global resource.
map_ids :: (State.M m) => (Id.Id -> Id.Id) -> m ()
map_ids f = do
    map_view_ids f
    map_block_ids f
    map_track_ids f
    map_ruler_ids f

map_view_ids :: (State.M m) => (Id.Id -> Id.Id) -> m ()
map_view_ids f = do
    views <- State.gets State.state_views
    let view_f = Types.ViewId . f . Id.unpack_id
    new_views <- safe_map_keys "state_views" view_f views
    State.modify $ \st -> st { State.state_views = new_views }

map_block_ids :: (State.M m) => (Id.Id -> Id.Id) -> m ()
map_block_ids f = do
    maybe_root <- State.lookup_root_id
    let new_root = fmap (Types.BlockId . f . Id.unpack_id) maybe_root

    blocks <- State.gets State.state_blocks
    let block_f = Types.BlockId . f . Id.unpack_id
    new_blocks <- safe_map_keys "state_blocks" block_f blocks

    views <- State.gets State.state_views
    let new_views = Map.map
            (\v -> v { Block.view_block = block_f (Block.view_block v) })
            views
    State.modify $ \st -> st
        { State.state_blocks = new_blocks, State.state_views = new_views }
    State.modify_config $ \config -> config { State.config_root = new_root }

map_track_ids :: (State.M m) => (Id.Id -> Id.Id) -> m ()
map_track_ids f = do
    tracks <- State.gets State.state_tracks
    let track_f = Types.TrackId . f . Id.unpack_id
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
        Block.TId tid rid -> Block.modify_id track $
            const (Block.TId (f tid) rid)
        _ -> track
    map_merged f track = track
        { Block.track_merged = map f (Block.track_merged track) }

map_ruler_ids :: (State.M m) => (Id.Id -> Id.Id) -> m ()
map_ruler_ids f = do
    rulers <- State.gets State.state_rulers
    let ruler_f = Types.RulerId . trans . Id.unpack_id
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
        Block.TId tid rid -> Block.modify_id track $
            const (Block.TId tid (f rid))
        Block.RId rid -> Block.modify_id track $
            const (Block.RId (f rid))
        _ -> track
    trans ident
        | ident == Id.unpack_id State.no_ruler = ident
        | otherwise = f ident

safe_map_keys :: (State.M m, Ord k, Show k) =>
    String -> (k -> k) -> Map.Map k v -> m (Map.Map k v)
safe_map_keys name f fm0
    | Map.size fm1 == Map.size fm0 = return fm1
    | otherwise = State.throw $ "keys collided in " ++ show name ++ ": "
        ++ show (Map.keys (Map.difference fm0 fm1))
    where fm1 = Map.mapKeys f fm0


-- * merge

-- | Merge ID maps from the states together.  Collisions will throw.
merge_states :: State.State -> State.State
    -> Either State.StateError State.State
merge_states st0 st1 = State.exec st0 $ do
    views <- safe_union "views"
        (State.state_views st0) (State.state_views st1)
    blocks <- safe_union "blocks"
        (State.state_blocks st0) (State.state_blocks st1)
    tracks <- safe_union "tracks"
        (State.state_tracks st0) (State.state_tracks st1)
    -- Everyone has a no_ruler, so it shouldn't count as a collision.
    let rulers1 = Map.delete State.no_ruler (State.state_rulers st1)
    rulers <- safe_union "rulers" (State.state_rulers st0) rulers1
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

