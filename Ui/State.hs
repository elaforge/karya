{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | The overall UI state is described here.  This is an immutable data
    structure that contains all the tracks, rulers, note data, and so forth.
    It exports a StateT monad for modification and access.

    Since the same block may have \>=0 views, and a single track may appear in
    \>=0 blocks, these are stored as IDs rather than directly in their
    containers.  Using explicit references introduces all the usual problems
    with pointers like invalid references and unreferenced data.  The latter is
    actually a feature (e.g. having a block with no associated view is
    perfectly normal), but the former is a pain.  To ease the pain, IDs should
    only be created via the monadic create_* interface in this module, even
    though I'm forced to export their constructors to avoid circular imports.
    There may still be problems with IDs from one State being applied to
    a different State (likely an older and newer version of the same State),
    but I'll deal with that when I get there.

    A higher level interface (e.g. 'Cmd.Create') may ease this by automatically
    creating objects with automatically generated IDs.
-}
module Ui.State where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as State
import qualified Data.Generics as Generics
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Map as Map
import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Id as Id
import qualified Ui.Update as Update
import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument


data State = State {
    -- | The project name is used as the namespace of automatically created
    -- IDs, so each project can import other projects without clashes.  The
    -- save file is also derived from the project name.
    state_project :: Id.Namespace
    , state_project_dir :: String
    , state_views :: Map.Map Block.ViewId Block.View
    , state_blocks :: Map.Map Block.BlockId Block.Block
    -- Track data also gets a symbol table.  This is so that I can
    -- efficiently compare a track for identity, and also so I can
    -- change it here and all of its occurrances change.
    , state_tracks :: Map.Map Track.TrackId Track.Track
    , state_rulers :: Map.Map Ruler.RulerId Ruler.Ruler

    -- | This maps the midi instruments used in this State to their Addrs.
    , state_midi_config :: Instrument.Config
    } deriving (Read, Show, Generics.Typeable)

-- TODO "initial_state" would be more consistent
empty = State "untitled" "save" Map.empty Map.empty Map.empty ruler_map
    (Instrument.config [] Nothing)
    where ruler_map = Map.fromList [(no_ruler, Ruler.no_ruler)]

-- | Since all TracklikeIds must have a ruler, all States have a special empty
-- ruler that can be used in a \"no ruler\" situation.
--
-- To enforce its global nature, this should never be renamed or deleted, which
-- is enforced by 'map_ids' and 'destroy_ruler', but it's still possible.  So
-- don't do that.
no_ruler :: Ruler.RulerId
no_ruler = Ruler.RulerId (Id.global "_no_ruler_")

-- | A non-existent ruler, ready for inclusion into create_block's track list.
no_ruler_track :: Block.BlockTrack
no_ruler_track = Block.block_track (Block.RId no_ruler) 0

-- * StateT monadic access

-- | Run the given StateT with the given initial state, and return a new
-- state along with updates.  Normally updates are produced by 'Ui.Diff.diff',
-- but for efficiency updates to track data are accumulated when they are
-- actually made.  All the UI needs is a TrackPos range to redraw in, and
-- redrawing the whole track isn't that expensive.
--
-- See the StateStack comment for more.
run :: (Monad m) =>
   State -> StateT m a -> m (Either StateError (a, State, [Update.Update]))
run state m = do
    res <- (Error.runErrorT . Logger.run . flip State.runStateT state
        . run_state_t) m
    return $ case res of
        Left err -> Left err
        Right ((val, state), updates) -> Right (val, state, updates)

eval_rethrow :: (UiStateMonad m) => String -> State
    -> StateT Identity.Identity a -> m a
eval_rethrow msg state = throw_either msg . eval state

-- | A form of 'run' that returns only the val.
eval :: State -> StateT Identity.Identity a -> Either StateError a
eval state m = case result of
        Left err -> Left err
        Right (val, _, _) -> Right val
    where result = Identity.runIdentity (run state m)

exec :: State -> StateT Identity.Identity a -> Either StateError State
exec state m = case result of
        Left err -> Left err
        Right (_, state', _) -> Right state'
    where result = Identity.runIdentity (run state m)

exec_rethrow :: (UiStateMonad m) => String -> State
    -> StateT Identity.Identity a -> m State
exec_rethrow msg state = throw_either msg . exec state

throw_either :: (UiStateMonad m) => String -> Either StateError a -> m a
throw_either msg = either (throw . ((msg ++ ": ") ++) . show) return

-- | Like 'throw_either', but throw an IO exception.  Useful for tests.
error_either :: (Show a, Monad m) => String -> Either StateError a -> m a
error_either msg = either (error . ((msg ++ ": ") ++) . show) return

-- | TrackUpdates are stored directly instead of being calculated from the
-- state diff.
--
-- Is there any way they could get out of sync with the actual change?  I don't
-- see how, since the updates are stored by track_id, which should always be
-- associated with the same track, and an operation to move event positions
-- will simply generate another TrackUpdate over the whole track.  This does
-- mean TrackUpdates can overlap, so 'Ui.Sync.sync' should collapse them.
type StateStack m = State.StateT State
    (Logger.LoggerT Update.Update
        (Error.ErrorT StateError m))
newtype StateT m a = StateT (StateStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError StateError)
run_state_t (StateT x) = x

instance Trans.MonadTrans StateT where
    lift = StateT . lift . lift . lift

data StateError = StateError String deriving (Generics.Typeable)
instance Error.Error StateError where
    strMsg = StateError
instance Show StateError where
    show (StateError msg) = "StateError: " ++ msg

-- TODO remove modify and implement in terms of get and put?
-- TODO I also think I can remove throw since it's in Error
class (Monad m, Functor m) => UiStateMonad m where
    get :: m State
    put :: State -> m ()
    modify :: (State -> State) -> m ()
    update :: Update.Update -> m ()
    throw :: String -> m a

instance Monad m => UiStateMonad (StateT m) where
    get = StateT State.get
    put st = StateT (State.put st)
    modify f = StateT (State.modify f)
    update upd = (StateT . lift) (Logger.record upd)
    throw msg = (StateT . lift . lift) (Error.throwError (StateError msg))


-- * global changes

structure state = (views, blocks, tracks, rulers)
    where
    views = Map.keys (state_views state)
    blocks = [(block_id, Block.block_tracks block)
        | (block_id, block) <- Map.assocs (state_blocks state)]
    tracks = Map.keys (state_tracks state)
    rulers = Map.keys (state_rulers state)

-- | Map a function across the IDs in the given state.  Any collisions are
-- thrown in Left.
map_state_ids :: (Id.Id -> Id.Id) -> State -> Either StateError State
map_state_ids f state = exec state (pure_map_ids f)

-- | Transform IDs, but don't update view_id pointer map.  So only use this
-- when you are sure there are no visible views ("invisible" views occur after
-- they are created but before the sync).  This should probably only be used
-- by 'map_state_ids'.
--
-- SchemaIds are not mapped, because they point to a global resource.
pure_map_ids :: (UiStateMonad m) => (Id.Id -> Id.Id) -> m ()
pure_map_ids f = do
    map_view_ids f
    map_block_ids f
    map_track_ids f
    map_ruler_ids f

-- | Apply a transformation to all IDs.  Most likely used to rename a project,
-- see Cmd.Create.  Colliding IDs will throw.
--
-- This must be in IO because it modifies the global view_id pointer map.
map_ids :: (State.MonadIO m, UiStateMonad m) => (Id.Id -> Id.Id) -> m ()
map_ids f = do
    pure_map_ids f
    -- Do this last because it throws an IO exception on failure.
    let view_f = Block.ViewId . f . Id.unpack_id
    Trans.liftIO $ Block.map_ids view_f

map_view_ids :: (UiStateMonad m) => (Id.Id -> Id.Id) -> m ()
map_view_ids f = do
    views <- fmap state_views get
    let view_f = Block.ViewId . f . Id.unpack_id
    new_views <- safe_map_keys "state_views" view_f views
    modify $ \st -> st { state_views = new_views }

map_block_ids :: (UiStateMonad m) => (Id.Id -> Id.Id) -> m ()
map_block_ids f = do
    blocks <- fmap state_blocks get
    let block_f = Block.BlockId . f . Id.unpack_id
    new_blocks <- safe_map_keys "state_blocks" block_f blocks

    views <- fmap state_views get
    let new_views = Map.map
            (\v -> v { Block.view_block = block_f (Block.view_block v) })
            views
    modify $ \st -> st { state_blocks = new_blocks, state_views = new_views }

map_track_ids :: (UiStateMonad m) => (Id.Id -> Id.Id) -> m ()
map_track_ids f = do
    tracks <- fmap state_tracks get
    let track_f = Track.TrackId . f . Id.unpack_id
    new_tracks <- safe_map_keys "state_tracks" track_f tracks

    blocks <- fmap state_blocks get
    let new_blocks = Map.map
            (\b -> b { Block.block_tracks =
                map (map_track track_f) (Block.block_tracks b) })
            blocks
    modify $ \st -> st { state_tracks = new_tracks, state_blocks = new_blocks }
    where
    map_track f track = case Block.tracklike_id track of
        Block.TId tid rid -> Block.modify_id track $
            const (Block.TId (f tid) rid)
        _ -> track

map_ruler_ids :: (UiStateMonad m) => (Id.Id -> Id.Id) -> m ()
map_ruler_ids f = do
    rulers <- fmap state_rulers get
    let ruler_f = Ruler.RulerId . trans . Id.unpack_id
    new_rulers <- safe_map_keys "state_rulers" ruler_f rulers

    blocks <- fmap state_blocks get
    let new_blocks = Map.map
            (\b -> b { Block.block_tracks =
                map (map_track ruler_f) (Block.block_tracks b) })
            blocks
    modify $ \st -> st { state_rulers = new_rulers, state_blocks = new_blocks }
    where
    map_track f track = case Block.tracklike_id track of
        Block.TId tid rid -> Block.modify_id track $
            const (Block.TId tid (f rid))
        Block.RId rid -> Block.modify_id track $
            const (Block.RId (f rid))
        _ -> track
    trans ident
        | ident == Id.unpack_id no_ruler = ident
        | otherwise = f ident


-- | Merge ID maps from the states together.  Collisions will throw.
merge_states :: State -> State -> Either StateError State
merge_states st0 st1 = exec st0 $ do
    views <- safe_union "views" (state_views st0) (state_views st1)
    blocks <- safe_union "blocks" (state_blocks st0) (state_blocks st1)
    tracks <- safe_union "tracks" (state_tracks st0) (state_tracks st1)
    -- Everyone has a no_ruler, so it shouldn't count as a collision.
    let rulers1 = Map.delete no_ruler (state_rulers st1)
    rulers <- safe_union "rulers" (state_rulers st0) rulers1
    modify $ \st -> st
        { state_views = views, state_blocks = blocks
        , state_tracks = tracks, state_rulers = rulers
        }


-- ** util

safe_map_keys :: (UiStateMonad m, Ord k, Show k) =>
    String -> (k -> k) -> Map.Map k v -> m (Map.Map k v)
safe_map_keys name f fm0
    | Map.size fm1 == Map.size fm0 = return fm1
    | otherwise = throw $ "keys collided in " ++ show name ++ ": "
        ++ show (Map.keys (Map.difference fm0 fm1))
    where fm1 = Map.mapKeys f fm0

safe_union name fm0 fm1
    | Map.null overlapping = return fm
    | otherwise = throw $
        "keys collided in " ++ show name ++ ": " ++ show (Map.keys overlapping)
    where (fm, overlapping) = Map.unique_union fm0 fm1

-- * misc

-- | Unfortunately there are some invariants to protect within State.  This
-- will check the invariants, log warnings and fix them if possible (that's why
-- it returns another state), or throw an error if not.
--
-- The invariants should be protected by the modifiers in this module, but
-- this is just in case.
verify :: State -> (Either StateError State, [Log.Msg])
verify state = (fmap (\(_, s, _) -> s) result, error_log ++ logs)
    where
    (result, logs) = Identity.runIdentity (Log.run (run state do_verify))
    error_log = case result of
        Left err -> [Log.msg Log.Error $ "state error: " ++ show err]
        _ -> []

-- TODO
-- check that all views refer to valid blocks, and all TracklikeIds have
-- referents
-- anything else?
do_verify = do
    view_ids <- get_all_view_ids
    mapM_ verify_view view_ids

    block_ids <- get_all_block_ids
    blocks <- mapM get_block block_ids
    mapM_ verify_block blocks

verify_view :: Block.ViewId -> StateT (Log.LogT Identity.Identity) ()
verify_view view_id = do
    view <- get_view view_id
    block <- get_block (Block.view_block view)
    let btracks = length (Block.block_tracks block)
        vtracks = length (Block.view_tracks view)
    when (btracks /= vtracks) $
        Trans.lift $ Log.warn $ "block has " ++ show btracks
            ++ " tracks while view has " ++ show vtracks ++ ", fixing"
    -- Add track views for all the block tracks.
    forM_ [vtracks .. btracks-1] $ \tracknum ->
        modify_view view_id $ \v -> insert_into_view tracknum 20 v

verify_block block = do
    mapM_ get_track (Block.block_track_ids block)
    mapM_ get_ruler (Block.block_ruler_ids block)

get_project :: (UiStateMonad m) => m Id.Namespace
get_project = fmap state_project get

set_project :: (UiStateMonad m) => Id.Namespace -> m ()
set_project ns = modify $ \st -> st { state_project = ns }

get_midi_config :: (UiStateMonad m) => m Instrument.Config
get_midi_config = fmap state_midi_config get

set_midi_config :: (UiStateMonad m) => Instrument.Config -> m ()
set_midi_config config = modify $ \st -> st { state_midi_config = config}

-- * view

get_view :: (UiStateMonad m) => Block.ViewId -> m Block.View
get_view view_id = get >>= lookup_id view_id . state_views

lookup_view :: (UiStateMonad m) => Block.ViewId -> m (Maybe Block.View)
lookup_view view_id = get >>= return . Map.lookup view_id . state_views

get_all_view_ids :: (UiStateMonad m) => m [Block.ViewId]
get_all_view_ids = fmap (Map.keys . state_views) get

-- | Create a new view.  Block.view_tracks can be left empty, since it will
-- be replaced by views generated from the the block.  If the caller uses the
-- 'Block.view' constructor, it won't have to worry about this.
create_view :: (UiStateMonad m) => Id.Id -> Block.View -> m Block.ViewId
create_view id view = do
    block <- get_block (Block.view_block view)
    let view' = view { Block.view_tracks = initial_track_views block }
    get >>= insert (Block.ViewId id) view' state_views
        (\views st -> st { state_views = views })
initial_track_views block = map Block.TrackView widths
    where widths = map Block.track_width (Block.block_tracks block)

destroy_view :: (UiStateMonad m) => Block.ViewId -> m ()
destroy_view view_id = modify $ \st ->
    st { state_views = Map.delete view_id (state_views st) }

set_view_config :: (UiStateMonad m) => Block.ViewId -> Block.ViewConfig -> m ()
set_view_config view_id config =
    modify_view view_id (\view -> view { Block.view_config = config })

-- | Update @tracknum@ of @view_id@ to have width @width@.
set_track_width :: (UiStateMonad m) =>
    Block.ViewId -> Block.TrackNum -> Block.Width -> m ()
set_track_width view_id tracknum width = do
    view <- get_view view_id
    -- Functional update still sucks.  An imperative language would have:
    -- state.get_view(view_id).tracks[tracknum].width = width
    track_views <- modify_at "set_track_width"
        (Block.view_tracks view) tracknum $ \tview ->
            tview { Block.track_view_width = width }
    update_view view_id (view { Block.view_tracks = track_views })

-- ** zoom and track scroll

set_zoom :: (UiStateMonad m) => Block.ViewId -> Block.Zoom -> m ()
set_zoom view_id zoom =
    modify_view view_id (\view -> view { Block.view_zoom = clamped })
    where
    clamped = zoom
        { Block.zoom_offset = max (TrackPos 0) (Block.zoom_offset zoom) }

set_track_scroll :: (UiStateMonad m) => Block.ViewId -> Block.Width -> m ()
set_track_scroll view_id offset =
    modify_view view_id (\view -> view { Block.view_track_scroll = offset })

set_view_rect :: (UiStateMonad m) => Block.ViewId -> Block.Rect -> m ()
set_view_rect view_id rect =
    modify_view view_id (\view -> view { Block.view_rect = rect })

-- | Only 'Cmd.Cmd.ui_update' is supposed to call this, because track_size is
-- only set from the UI.
set_track_size :: (UiStateMonad m) => Block.ViewId -> (Int, Int) -> m ()
set_track_size view_id (visible_track, visible_time) =
    modify_view view_id $ \view -> view {
        Block.view_visible_track = visible_track
        , Block.view_visible_time = visible_time }

-- ** selections

-- | Get @view_id@'s selection at @selnum@, or Nothing if there is none.
get_selection :: (UiStateMonad m) => Block.ViewId -> Block.SelNum
    -> m (Maybe Block.Selection)
get_selection view_id selnum = do
    view <- get_view view_id
    return (Map.lookup selnum (Block.view_selections view))

-- | Replace any selection on @view_id@ at @selnum@ with @sel@.
set_selection :: (UiStateMonad m) => Block.ViewId -> Block.SelNum
    -> Maybe Block.Selection -> m ()
set_selection view_id selnum maybe_sel = do
    view <- get_view view_id
    let sels = case maybe_sel of
            Nothing -> Map.delete selnum (Block.view_selections view)
            Just sel -> Map.insert selnum sel (Block.view_selections view)
    update_view view_id (view { Block.view_selections = sels })

-- ** util

update_view view_id view = modify $ \st -> st
    { state_views = Map.adjust (const view) view_id (state_views st) }
modify_view view_id f = do
    view <- get_view view_id
    update_view view_id (f view)

-- * block

get_all_block_ids :: (UiStateMonad m) => m [Block.BlockId]
get_all_block_ids = fmap (Map.keys . state_blocks) get

get_block :: (UiStateMonad m) => Block.BlockId -> m Block.Block
get_block block_id = get >>= lookup_id block_id . state_blocks

lookup_block :: (UiStateMonad m) => Block.BlockId -> m (Maybe Block.Block)
lookup_block block_id = get >>= return . Map.lookup block_id . state_blocks

create_block :: (UiStateMonad m) => Id.Id -> Block.Block -> m Block.BlockId
create_block id block = get >>= insert (Block.BlockId id) block state_blocks
    (\blocks st -> st { state_blocks = blocks })

-- | Destroy the block and all the views that display it.
-- Leaves its tracks intact.
destroy_block :: (UiStateMonad m) => Block.BlockId -> m ()
destroy_block block_id = do
    views <- get_views_of block_id
    mapM_ destroy_view (Map.keys views)
    modify $ \st -> st { state_blocks = Map.delete block_id (state_blocks st) }

block_of_view :: (UiStateMonad m) => Block.ViewId -> m Block.Block
block_of_view view_id = get_block . Block.view_block =<< get_view view_id

set_block_config :: (UiStateMonad m) => Block.BlockId -> Block.Config -> m ()
set_block_config block_id config =
    modify_block block_id (\block -> block { Block.block_config = config })

set_edit_box :: (UiStateMonad m) => Block.BlockId -> Color -> Char -> m ()
set_edit_box block_id color char = do
    block <- get_block block_id
    set_block_config block_id $
        (Block.block_config block) { Block.config_track_box = (color, char) }

-- | The play box doesn't use a char, so I leave that out.
set_play_box :: (UiStateMonad m) => Block.BlockId -> Color -> m ()
set_play_box block_id color = do
    block <- get_block block_id
    set_block_config block_id $
        (Block.block_config block) { Block.config_sb_box = (color, ' ') }

-- | Get the end of the block according to the ruler.  This means that if the
-- block has no rulers (e.g. a clipboard block) then ruler_end will be 0.
ruler_end :: (UiStateMonad m) => Block.BlockId -> m TrackPos
ruler_end block_id = do
    block <- get_block block_id
    case Block.block_ruler_ids block of
        [] -> return $ TrackPos 0
        ruler_id : _ -> fmap Ruler.time_end (get_ruler ruler_id)

-- | Get the end of the block according to the last event of the block.
event_end :: (UiStateMonad m) => Block.BlockId -> m TrackPos
event_end block_id = do
    block <- get_block block_id
    track_ends <- mapM track_end (Block.block_track_ids block)
    return $ maximum (TrackPos 0 : track_ends)

-- ** skeleton

get_skeleton :: (UiStateMonad m) => Block.BlockId -> m Skeleton.Skeleton
get_skeleton block_id = fmap Block.block_skeleton (get_block block_id)

set_skeleton :: (UiStateMonad m) => Block.BlockId -> Skeleton.Skeleton
    -> m ()
set_skeleton block_id skel =
    modify_block block_id (\block -> block { Block.block_skeleton = skel })

-- | Toggle the given edge in the block's skeleton.  If a cycle would be
-- created, refuse to add the edge and return False.
toggle_skeleton_edge :: (UiStateMonad m) => Block.BlockId
    -> (Block.TrackNum, Block.TrackNum) -> m Bool
toggle_skeleton_edge block_id edge = do
    block <- get_block block_id
    let skel = Block.block_skeleton block
    case Skeleton.toggle_edge edge skel of
        Nothing -> return False
        Just new_skel -> do
            set_block block_id $ block { Block.block_skeleton = new_skel }
            return True

-- *** TrackTree

-- | A TrackTree is the Skeleton resolved to the tracks it references.
type TrackTree = Tree.Forest TrackInfo

-- | Summary information on a Track.
data TrackInfo = TrackInfo {
    track_title :: String
    , track_id :: Track.TrackId
    , track_tracknum :: Block.TrackNum
    } deriving (Show)

get_track_info :: (UiStateMonad m) => Block.BlockId -> m [TrackInfo]
get_track_info block_id = do
    block <- get_block block_id
    state <- get
    return [TrackInfo (Track.track_title track) tid i
        | (i, tid, track) <- _tracks_of block (state_tracks state)]

get_track_tree :: (UiStateMonad m) => Block.BlockId -> m TrackTree
get_track_tree block_id = do
    skel <- get_skeleton block_id
    tracks <- get_track_info block_id
    ntracks <- fmap (length . Block.block_tracklike_ids) (get_block block_id)
    let by_tracknum = Map.fromList $ zip (map track_tracknum tracks) tracks
    let (resolved, missing) = _resolve by_tracknum
            (Skeleton.to_forest ntracks skel)
    -- Rulers and dividers should show up as missing.  They're ok as long as
    -- they have no edges.
    let really_missing = filter (not . (Skeleton.lonely_vertex skel)) missing
    when (not (null really_missing)) $
        throw $ "skeleton of " ++ show block_id
            ++ " names missing tracknums: " ++ show really_missing
    return resolved

_tracks_of :: Block.Block -> Map.Map Track.TrackId Track.Track
    -> [(Block.TrackNum, Track.TrackId, Track.Track)]
_tracks_of block tracks = do
    (i, Block.TId tid _) <- Seq.enumerate (Block.block_tracklike_ids block)
    track <- maybe mzero (:[]) (Map.lookup tid tracks)
    return (i, tid, track)

_resolve :: Map.Map Block.TrackNum TrackInfo -> Tree.Forest Block.TrackNum
    -> (Tree.Forest TrackInfo, [Block.TrackNum])
_resolve tracknums trees = foldr cat_tree ([], []) $ map go trees
    where
    go (Tree.Node tracknum subs) = case Map.lookup tracknum tracknums of
        Nothing -> (Nothing, [tracknum])
        Just track_info ->
            let (subforest, missing) = _resolve tracknums subs
            in (Just (Tree.Node track_info subforest), missing)
    cat_tree (maybe_tree, missing) (forest, all_missing) = case maybe_tree of
        Nothing -> (forest, missing ++ all_missing)
        Just tree -> (tree : forest, missing ++ all_missing)

-- ** tracks

insert_track :: (UiStateMonad m) => Block.BlockId -> Block.TrackNum
    -> Block.BlockTrack -> m ()
insert_track block_id tracknum track = do
    block <- get_block block_id
    views <- get_views_of block_id
    let tracks = Block.block_tracks block
        tracks' = Seq.insert_at tracks tracknum track
        -- Make sure the views are up to date.
        views' = Map.map
            (insert_into_view tracknum (Block.track_width track)) views
    set_block block_id $ block
        { Block.block_tracks = tracks'
        , Block.block_skeleton =
            Skeleton.insert tracknum (Block.block_skeleton block)
        }
    modify $ \st -> st { state_views = Map.union views' (state_views st) }

remove_track :: (UiStateMonad m) => Block.BlockId -> Block.TrackNum -> m ()
remove_track block_id tracknum = do
    block <- get_block block_id
    views <- get_views_of block_id
    let tracks' = Seq.remove_at (Block.block_tracks block) tracknum
        views' = Map.map (remove_from_view tracknum) views
    set_block block_id $ block
        { Block.block_tracks = tracks'
        , Block.block_skeleton =
            Skeleton.remove tracknum (Block.block_skeleton block)
        }
    modify $ \st -> st { state_views = Map.union views' (state_views st) }

-- | Get the TracklikeId at @tracknum@, or Nothing if its out of range.
-- This is inconsistent with 'insert_track' and 'remove_track' which clip to
-- range, but is convenient in practice.
-- TODO why?
track_at :: (UiStateMonad m) => Block.BlockId -> Block.TrackNum
    -> m (Maybe Block.BlockTrack)
track_at block_id tracknum = do
    block <- get_block block_id
    return $ Seq.at (Block.block_tracks block) tracknum

-- | Convenience function like 'track_at', but get the track_id if it's an
-- event track.
event_track_at :: (UiStateMonad m) =>
    Block.BlockId -> Block.TrackNum -> m (Maybe Track.TrackId)
event_track_at block_id tracknum = do
    maybe_track <- track_at block_id tracknum
    return $ do
        track <- maybe_track
        Block.track_id_of (Block.tracklike_id track)

tracks :: (UiStateMonad m) => Block.BlockId -> m Block.TrackNum
tracks block_id = do
    block <- get_block block_id
    return $ length (Block.block_tracks block)

get_tracklike :: (UiStateMonad m) => Block.TracklikeId -> m Block.Tracklike
get_tracklike track = case track of
    Block.TId track_id ruler_id ->
        liftM2 Block.T (get_track track_id) (get_ruler ruler_id)
    Block.RId ruler_id ->
        liftM Block.R (get_ruler ruler_id)
    Block.DId divider -> return (Block.D divider)

-- *** track util

-- Insert a new track into Block.view_tracks, moving selections as
-- appropriate.  @tracknum@ is clipped to be in range.
insert_into_view tracknum width view = view
    { Block.view_tracks = Seq.insert_at (Block.view_tracks view) tracknum
        (Block.TrackView width)
    , Block.view_selections =
        Map.map (insert_into_selection tracknum) (Block.view_selections view)
    }

-- Remove @tracknum@ from Block.view_tracks, moving selections as
-- appropriate.  Ignored if @tracknum@ is out of range.
remove_from_view tracknum view = view
    { Block.view_tracks = Seq.remove_at (Block.view_tracks view) tracknum
    , Block.view_selections = Map.mapMaybe
        (remove_from_selection tracknum) (Block.view_selections view)
    }

-- If tracknum is before or at the selection, push it to the right.  If it's
-- inside, extend it.  If it's to the right, do nothing.
insert_into_selection tracknum sel
    | tracknum <= min track0 track1 = Block.sel_modify_tracks (+1) sel
    | tracknum <= max track0 track1 = Block.sel_expand_tracks 1 sel
    | otherwise = sel
    where (track0, track1) = Block.sel_track_range sel

remove_from_selection tracknum sel
    | tracknum <= min track0 track1  =
        Just $ Block.sel_modify_tracks (+(-1)) sel
    | tracknum == track0 && tracknum == track1 = Nothing
    | tracknum <= max track0 track1 = Just $ Block.sel_expand_tracks (-1) sel
    | otherwise = Just sel
    where (track0, track1) = Block.sel_track_range sel

-- ** other

set_block_title :: (UiStateMonad m) => Block.BlockId -> String -> m ()
set_block_title block_id title =
    modify_block block_id (\block -> block { Block.block_title = title })

-- | Set a status variable on a view.
set_view_status :: (UiStateMonad m) => Block.ViewId -> String -> Maybe String
    -> m ()
set_view_status view_id key val =
    modify_view view_id $ \view -> view { Block.view_status =
        Map.alter (const val) key (Block.view_status view) }

-- ** util

set_block block_id block = modify $ \st -> st
    { state_blocks = Map.adjust (const block) block_id (state_blocks st) }
modify_block block_id f = do
    block <- get_block block_id
    set_block block_id (f block)

-- * track

get_track :: (UiStateMonad m) => Track.TrackId -> m Track.Track
get_track track_id = get >>= lookup_id track_id . state_tracks

lookup_track :: (UiStateMonad m) => Track.TrackId -> m (Maybe Track.Track)
lookup_track track_id = get >>= return . Map.lookup track_id . state_tracks

create_track :: (UiStateMonad m) => Id.Id -> Track.Track -> m Track.TrackId
create_track id track = get >>= insert (Track.TrackId id) track state_tracks
    (\tracks st -> st { state_tracks = tracks })

-- | Destroy the track and remove it from all the blocks it's in.
destroy_track :: (UiStateMonad m) => Track.TrackId -> m ()
destroy_track track_id = do
    blocks <- blocks_with_track track_id
    forM_ blocks $ \(block_id, tracks) -> forM_ tracks $ \(tracknum, _) -> do
        remove_track block_id tracknum
    modify $ \st -> st { state_tracks = Map.delete track_id (state_tracks st) }

set_track_title :: (UiStateMonad m) => Track.TrackId -> String -> m ()
set_track_title track_id text = modify_track track_id $ \track ->
    track { Track.track_title = text }

set_track_bg :: (UiStateMonad m) => Track.TrackId -> Color -> m ()
set_track_bg track_id color = modify_track track_id $ \track ->
    track { Track.track_bg = color }

modify_track_render :: (UiStateMonad m) => Track.TrackId
    -> (Track.RenderConfig -> Track.RenderConfig) -> m ()
modify_track_render track_id f = modify_track track_id $ \track ->
    track { Track.track_render = f (Track.track_render track) }

-- | Insert events into track_id as per 'Track.insert_events'.
insert_events :: (UiStateMonad m) =>
    Track.TrackId -> [(TrackPos, Event.Event)] -> m ()
insert_events track_id pos_evts = do
    -- Stash a track update, see 'run' comment.
    modify_events track_id (Track.insert_events pos_evts)
    unless (null pos_evts) $
        update $ Update.TrackUpdate track_id $ Update.TrackEvents
            (fst (head pos_evts)) (Track.event_end (last pos_evts))

-- | Remove any events whose starting positions fall within the half-open
-- range given.
remove_events :: (UiStateMonad m) =>
    Track.TrackId -> TrackPos -> TrackPos -> m ()
remove_events track_id start end = do
    track <- get_track track_id
    let evts = takeWhile ((<end) . fst)
            (Track.forward start (Track.track_events track))
    modify_events track_id (Track.remove_events start end)
    unless (null evts) $
        update $ Update.TrackUpdate track_id
            (Update.TrackEvents start (Track.event_end (last evts)))

-- | Set the events of the track.
set_events :: (UiStateMonad m) => Track.TrackId -> Track.TrackEvents -> m ()
set_events track_id events = do
    modify_events track_id (const events)
    update $ Update.TrackUpdate track_id Update.TrackAllEvents

-- | Remove a single event at @pos@, if there is one.
remove_event :: (UiStateMonad m) => Track.TrackId -> TrackPos -> m ()
remove_event track_id pos = do
    track <- get_track track_id
    case Track.event_at (Track.track_events track) pos of
        Nothing -> return ()
        Just evt -> do
            modify_events track_id (Track.remove_event pos)
            let end = Track.event_end (pos, evt)
            update $ Update.TrackUpdate track_id (Update.TrackEvents pos end)

-- | Get the end of the last event of the block.
track_end :: (UiStateMonad m) => Track.TrackId -> m TrackPos
track_end track_id = fmap Track.track_time_end (get_track track_id)

-- | An EventTransformer applies a given transformation to the events in
-- a track.  TODO make sure this can also be used as a deriver, so I can
-- use the same functions in derivers as in edit commands.
--
-- This will be called from the event at or previous (if there is no event at)
-- to the selection.
--
-- Will this be efficient for a large number of events?  Track.merge_events
-- will have to take care of efficiently merging a large input.
type EventTransformer = [Track.PosEvent] -- previous events
    -> [Track.PosEvent] -- subsequent events
    -> Track.PosEvent -- event in question
    -> [Track.PosEvent] -- produces these events

{-
-- | Map a function across events in track_id from the range start to end.
-- If start doesn't fall on an event, this maps from the event /before/ the
-- start.
-- TODO this behaviour turns out to be handy in practice, but I'm not satisfied
-- with it in general.
modify_event_range :: (UiStateMonad m) => Track.TrackId
    -> EventTransformer -> TrackPos -> TrackPos -> m ()
modify_event_range track_id f start end = do
    modify_events track_id (map_events f start end)
    update $ Update.TrackUpdate track_id Update.TrackAllEvents

map_events :: EventTransformer -> TrackPos -> TrackPos -> Track.TrackEvents
    -> Track.TrackEvents
map_events f start end track_events = process track_events
    where
    (pre, post) = Track.events_at_before start track_events
    events = concat $ zipper_map f ((>=end) . fst) pre post
    process = Track.insert_events events . Track.remove_events start end

zipper_map _ _ _ [] = []
zipper_map f stop prev (val:next)
    | stop val = []
    | otherwise = f prev next val : zipper_map f stop (val:prev) next
-}

-- | Emit track updates for all tracks.  Use this when events have changed but
-- I don't know which ones, e.g. when loading a file or restoring a previous
-- state.
update_all_tracks :: (UiStateMonad m) => m ()
update_all_tracks = do
    st <- get
    let updates = map (flip Update.TrackUpdate Update.TrackAllEvents)
            (Map.keys (state_tracks st))
    mapM_ update updates

-- ** util

update_track track_id track = modify $ \st -> st
    { state_tracks = Map.adjust (const track) track_id (state_tracks st) }
modify_track track_id f = do
    track <- get_track track_id
    update_track track_id (f track)

-- This doesn't file TrackUpdates, so don't call this unless you do!
modify_events track_id f = modify_track track_id $ \track ->
    track { Track.track_events = f (Track.track_events track) }

-- * ruler

get_ruler :: (UiStateMonad m) => Ruler.RulerId -> m Ruler.Ruler
get_ruler ruler_id = get >>= lookup_id ruler_id . state_rulers

lookup_ruler :: (UiStateMonad m) => Ruler.RulerId -> m (Maybe Ruler.Ruler)
lookup_ruler ruler_id = get >>= return . Map.lookup ruler_id . state_rulers

create_ruler :: (UiStateMonad m) => Id.Id -> Ruler.Ruler -> m Ruler.RulerId
create_ruler id ruler
        -- no_ruler is global and assumed to always exist.
    | id == Id.unpack_id no_ruler = return no_ruler
    | otherwise = get >>= insert (Ruler.RulerId id) ruler state_rulers
        (\rulers st -> st { state_rulers = rulers })

-- | Destroy the ruler and remove it from all the blocks it's in.
destroy_ruler :: (UiStateMonad m) => Ruler.RulerId -> m ()
destroy_ruler ruler_id = when (ruler_id /= no_ruler) $ do
    blocks <- blocks_with_ruler ruler_id
    forM_ blocks $ \(block_id, tracks) -> do
        let tracknums = map fst tracks
            setr i = if i `elem` tracknums then Block.set_rid no_ruler else id
            deruler (i, track) = Block.modify_id track (setr i)
        modify_block block_id $ \block -> block { Block.block_tracks =
            map deruler (Seq.enumerate (Block.block_tracks block)) }
    modify $ \st -> st { state_rulers = Map.delete ruler_id (state_rulers st) }

insert_marklist :: (UiStateMonad m) =>
    Ruler.RulerId -> Int -> (Ruler.MarklistName, Ruler.Marklist) -> m ()
insert_marklist ruler_id i marklist = modify_ruler ruler_id $ \ruler ->
    ruler { Ruler.ruler_marklists =
        Seq.insert_at (Ruler.ruler_marklists ruler) i marklist }

remove_marklist :: (UiStateMonad m) => Ruler.RulerId -> Block.TrackNum -> m ()
remove_marklist ruler_id n = modify_ruler ruler_id $ \ruler -> ruler
    { Ruler.ruler_marklists = Seq.remove_at (Ruler.ruler_marklists ruler) n }

modify_ruler ruler_id f = do
    ruler <- get_ruler ruler_id
    modify $ \st ->
        st { state_rulers = Map.insert ruler_id (f ruler) (state_rulers st) }

-- * search

-- | Get all views of a given block.
get_views_of :: (UiStateMonad m) =>
    Block.BlockId -> m (Map.Map Block.ViewId Block.View)
get_views_of block_id = do
    views <- fmap state_views get
    return $ Map.filter ((==block_id) . Block.view_block) views

-- | Get all the tracks in a given block.
get_tracks_of :: (UiStateMonad m) =>
    Block.BlockId -> m (Map.Map Track.TrackId Track.Track)
get_tracks_of block_id = do
    block <- get_block block_id
    let track_ids = Block.block_track_ids block
    tracks <- mapM get_track track_ids
    return $ Map.fromList (zip track_ids tracks)

-- | Find @track_id@ in all the blocks it exists in, and return the track info
-- for each tracknum at which @track_id@ lives.
blocks_with_track :: (UiStateMonad m) =>
    Track.TrackId -> m [(Block.BlockId, [(Block.TrackNum, Block.TracklikeId)])]
blocks_with_track track_id =
    find_tracks ((== Just track_id) . Block.track_id_of)

-- | Just like 'blocks_with_track' except for ruler_id.
blocks_with_ruler :: (UiStateMonad m) =>
    Ruler.RulerId -> m [(Block.BlockId, [(Block.TrackNum, Block.TracklikeId)])]
blocks_with_ruler ruler_id =
    find_tracks ((== Just ruler_id) . Block.ruler_id_of)

find_tracks :: (UiStateMonad m) => (Block.TracklikeId -> Bool)
    -> m [(Block.BlockId, [(Block.TrackNum, Block.TracklikeId)])]
find_tracks f = do
    st <- get
    let all_tracks block = Seq.enumerate (Block.block_tracks block)
    let get_tracks block = [(tracknum, Block.tracklike_id track)
            | (tracknum, track) <- all_tracks block
            , f (Block.tracklike_id track)]
    return [(block_id, get_tracks block)
        | (block_id, block) <- Map.assocs (state_blocks st)]

-- * util

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k, UiStateMonad m) => k -> Map.Map k a -> m a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- | Insert @val@ at @key@ in @get_map state@, throwing if it already exists.
-- Put the map back into @state@ by applying @set_map new_map state@ to it.
insert :: (UiStateMonad m, Ord k, Show k) =>
    k -> a -> (t -> Map.Map k a) -> (Map.Map k a -> t -> State) -> t -> m k
insert key val get_map set_map state = do
    when (key `Map.member` get_map state) $
        throw $ show key ++ " already exists"
    put (set_map (Map.insert key val (get_map state)) state)
    return key

-- | Modify the @i@th element of @xs@ by applying @f@ to it.
modify_at :: (UiStateMonad m) => String -> [a] -> Int -> (a -> a) -> m [a]
modify_at msg xs i f = case post of
    [] -> throw $ msg ++ ": can't replace index " ++ show i
        ++ " of list with length " ++ show (length xs)
    (elt:rest) -> return (pre ++ f elt : rest)
    where (pre, post) = splitAt i xs
