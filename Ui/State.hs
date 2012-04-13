{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
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

    A higher level interface (e.g. "Cmd.Create") may ease this by automatically
    creating objects with automatically generated IDs.
-}
module Ui.State (module Ui.State, Config(..), Default(..)) where
import qualified Control.Applicative as Applicative
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)

import qualified Data.Generics as Generics
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Logger as Logger
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Util.Tree as Tree

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import Ui.StateConfig (Config(..), Default(..))
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified App.Config as Config
import Types


-- * types

data State = State {
    state_views :: Map.Map ViewId Block.View
    , state_blocks :: Map.Map BlockId Block.Block
    -- | Track data also gets a symbol table.  This is so that I can
    -- efficiently compare a track for identity, and also so I can
    -- change it here and all of its occurrances change.
    , state_tracks :: Map.Map TrackId Track.Track
    , state_rulers :: Map.Map RulerId Ruler.Ruler
    , state_config :: Config
    } deriving (Eq, Read, Show, Generics.Typeable)

-- TODO "initial_state" would be more consistent
empty :: State
empty = State {
    state_views = Map.empty
    , state_blocks = Map.empty
    , state_tracks = Map.empty
    , state_rulers = Map.empty
    , state_config = empty_config
    }

-- | Clear out data that shouldn't be saved.
clear :: State -> State
clear state = state { state_views = Map.map clear_view (state_views state) }
    where clear_view view = view { Block.view_status = mempty }

instance Pretty.Pretty State where
    format (State views blocks tracks rulers config) =
        Pretty.record (Pretty.text "State")
            [ ("views", Pretty.format views)
            , ("blocks", Pretty.format blocks)
            , ("tracks", Pretty.format tracks)
            , ("rulers", Pretty.format rulers)
            , ("config", Pretty.format config)
            ]

instance DeepSeq.NFData State where
    rnf (State views blocks tracks rulers config) =
        DeepSeq.rnf views `seq` DeepSeq.rnf blocks
        `seq` DeepSeq.rnf tracks `seq` DeepSeq.rnf rulers
        `seq` config `seq` ()

empty_config :: Config
empty_config = Config
    { config_namespace = Id.unsafe_namespace "untitled"
    , config_project_dir = "save"
    , config_root = Nothing
    , config_midi = Instrument.config []
    , config_default = initial_default
    }

initial_default :: Default
initial_default = Default {
    default_scale = Pitch.ScaleId Config.default_scale_id
    , default_key = Nothing
    , default_instrument = Nothing
    , default_tempo = 1
    }

-- | Address a position on a track.  Usually functions take the parameters
-- separately, but this is more convenient when a position is passed around.
data Pos = Pos !BlockId !TrackNum !ScoreTime
    deriving (Eq, Show)

instance Pretty.Pretty Pos where
    pretty (Pos block_id tracknum pos) = Pretty.pretty block_id ++ "/"
        ++ show tracknum ++ "/" ++ Pretty.pretty pos

-- | Address a track in a block.  This is similar to a TrackId, except it
-- doesn't guarantee that the track is an event track.
data Track = Track !BlockId !TrackNum
    deriving (Eq, Show)

instance Pretty.Pretty Track where
    pretty (Track block_id tracknum) =
        Pretty.pretty block_id ++ "/" ++ show tracknum

-- * constants

-- | Since all TracklikeIds must have a ruler, all States have a special empty
-- ruler that can be used in a \"no ruler\" situation.
--
-- This RulerId is implicitly present in every block.  It's not actually in
-- 'state_rulers' to avoid it getting renamed or deleted, but 'get_ruler' will
-- pretend it exists.  As long as everyone that cares about no_ruler (which is
-- only 'verify' and 'get_tracklike' for "Ui.Sync") uses 'get_ruler' then
-- they won't be confused by tracks that have no_ruler.
no_ruler :: RulerId
no_ruler = Types.RulerId (Id.global "-no-ruler-")

-- | A non-existent ruler, ready for inclusion into create_block's track list.
no_ruler_track :: Block.Track
no_ruler_track = Block.track (Block.RId no_ruler) 0

-- * StateT monad

-- | Run the given StateT with the given initial state, and return a new
-- state along with updates.  Normally updates are produced by 'Ui.Diff.diff',
-- but for efficiency updates to track data are accumulated when they are
-- actually made.  All the UI needs is a ScoreTime range to redraw in, and
-- redrawing the whole track isn't that expensive.
--
-- See the StateStack comment for more.
run :: (Monad m) =>
   State -> StateT m a -> m (Either StateError (a, State, [Update.CmdUpdate]))
run state m = do
    res <- (Error.runErrorT . Logger.run . flip State.runStateT state
        . run_state_t) m
    return $ case res of
        Left err -> Left err
        Right ((val, state), updates) -> Right (val, state, updates)

run_id :: State -> StateId a
    -> Either StateError (a, State, [Update.CmdUpdate])
run_id state m = Identity.runIdentity (run state m)

eval_rethrow :: (M m) => String -> State -> StateId a -> m a
eval_rethrow msg state = throw_either msg . eval state

-- | A form of 'run' that returns only the val and automatically runs in
-- Identity.
eval :: State -> StateId a -> Either StateError a
eval state m = case result of
        Left err -> Left err
        Right (val, _, _) -> Right val
    where result = Identity.runIdentity (run state m)

exec :: State -> StateId a -> Either StateError State
exec state m = case result of
        Left err -> Left err
        Right (_, state', _) -> Right state'
    where result = Identity.runIdentity (run state m)

exec_rethrow :: (M m) => String -> State -> StateId a -> m State
exec_rethrow msg state = throw_either msg . exec state

throw_either :: (M m) => String -> Either StateError a -> m a
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
    (Logger.LoggerT Update.CmdUpdate
        (Error.ErrorT StateError m))
newtype StateT m a = StateT (StateStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError StateError,
        Applicative.Applicative)
run_state_t (StateT x) = x

-- | Just a convenient abbreviation.
type StateId a = StateT Identity.Identity a

instance Trans.MonadTrans StateT where
    lift = StateT . lift . lift . lift

-- | Abort is used by Cmd, so don't throw it from here.  This isn't exactly
-- modular, but ErrorT can't be composed and extensible exceptions are too
-- much bother at the moment.
data StateError = StateError String | Abort deriving (Generics.Typeable, Show)
instance Error.Error StateError where
    strMsg = StateError

instance Pretty.Pretty StateError where
    pretty (StateError msg) = msg
    pretty Abort = "(abort)"

-- | Monads implementing this class can call the UI state functions directly.
class (Applicative.Applicative m, Monad m) => M m where
    -- Note that these aren't the MonadState get and put, and can't be, because
    -- when this monad is layered under another state monad (as it is with
    -- Cmd), MonadState couldn't tell which one you wanted.
    get :: m State
    put :: State -> m ()
    update :: Update.CmdUpdate -> m ()
    get_updates :: m [Update.CmdUpdate]
    throw :: String -> m a

instance (Applicative.Applicative m, Monad m) => M (StateT m) where
    get = StateT State.get
    put st = StateT (State.put st)
    update upd = (StateT . lift) (Logger.log upd)
    get_updates = (StateT . lift) Logger.peek
    throw msg = (StateT . lift . lift) (Error.throwError (StateError msg))

gets :: (M m) => (State -> a) -> m a
gets f = fmap f get

modify :: (M m) => (State -> State) -> m ()
modify f = do
    state <- get
    put $! f state

require :: (M m) => String -> Maybe a -> m a
require err = maybe (throw err) return

-- * global

all_block_ids :: (M m) => m [BlockId]
all_block_ids = Map.keys . state_blocks <$> get

all_track_ids :: (M m) => m [TrackId]
all_track_ids = Map.keys . state_tracks <$> get

-- * misc

-- | Unfortunately there are some invariants to protect within State.  This
-- will check the invariants and return an error if it's broken.
verify :: State -> Maybe StateError
verify state = either Just (const Nothing) (exec state do_verify)

-- TODO
-- check that all TracklikeIds have referents
-- anything else?
do_verify :: StateId ()
do_verify = do
    views <- gets (Map.elems . state_views)
    mapM_ (get_block . Block.view_block) views
    block_ids <- get_all_block_ids
    blocks <- mapM get_block block_ids
    mapM_ verify_block blocks

verify_block :: (M m) => Block.Block -> m ()
verify_block block = do
    mapM_ get_track (Block.block_track_ids block)
    mapM_ get_ruler (Block.block_ruler_ids block)

get_namespace :: (M m) => m Id.Namespace
get_namespace = get_config config_namespace

set_namespace :: (M m) => Id.Namespace -> m ()
set_namespace ns = modify_config $ \st -> st { config_namespace = ns }

get_midi_config :: (M m) => m Instrument.Config
get_midi_config = get_config config_midi

set_midi_config :: (M m) => Instrument.Config -> m ()
set_midi_config config = modify_config $ \st -> st { config_midi = config }

get_midi_alloc :: (M m) => m (Map.Map Score.Instrument [Instrument.Addr])
get_midi_alloc = Instrument.config_alloc <$> get_midi_config

get_default :: (M m) => (Default -> a) -> m a
get_default f = f <$> get_config config_default

modify_default :: (M m) => (Default -> Default) -> m ()
modify_default f = modify_config $ \st ->
    st { config_default = f (config_default st) }

modify_config :: (M m) => (Config -> Config) -> m ()
modify_config f = modify $ \st -> st { state_config = f (state_config st) }

get_config :: (M m) => (Config -> a) -> m a
get_config f = gets (f . state_config)

-- * root

get_root_id :: (M m) => m BlockId
get_root_id = require "root_id" =<< lookup_root_id

lookup_root_id :: (M m) => m (Maybe BlockId)
lookup_root_id = get_config config_root

set_root_id :: (M m) => BlockId -> m ()
set_root_id block_id =
    modify_config $ \st -> st { config_root = Just block_id }

-- * view

get_view :: (M m) => ViewId -> m Block.View
get_view view_id = get >>= lookup_id view_id . state_views

lookup_view :: (M m) => ViewId -> m (Maybe Block.View)
lookup_view view_id = get >>= return . Map.lookup view_id . state_views

get_all_view_ids :: (M m) => m [ViewId]
get_all_view_ids = gets (Map.keys . state_views)

-- | Create a new view.  Block.view_tracks can be left empty, since it will
-- be replaced by views generated from the the block.  If the caller uses the
-- 'Block.view' constructor, it won't have to worry about this.
--
-- Throw if the ViewId already exists.
create_view :: (M m) => Id.Id -> Block.View -> m ViewId
create_view id view = insert (Types.ViewId id) view state_views $ \views st ->
    st { state_views = views }

destroy_view :: (M m) => ViewId -> m ()
destroy_view view_id = modify $ \st ->
    st { state_views = Map.delete view_id (state_views st) }

-- ** zoom and track scroll

get_zoom :: (M m) => ViewId -> m Types.Zoom
get_zoom view_id = fmap Block.view_zoom (get_view view_id)

set_zoom :: (M m) => ViewId -> Types.Zoom -> m ()
set_zoom view_id zoom =
    modify_view view_id (\view -> view { Block.view_zoom = clamped })
    where clamped = zoom { Types.zoom_offset = max 0 (Types.zoom_offset zoom) }

set_track_scroll :: (M m) => ViewId -> Types.Width -> m ()
set_track_scroll view_id offset =
    modify_view view_id (\view -> view { Block.view_track_scroll = offset })

set_view_rect :: (M m) => ViewId -> Rect.Rect -> m ()
set_view_rect view_id rect =
    modify_view view_id (\view -> view { Block.view_rect = rect })

-- | Only 'Cmd.Cmd.ui_update' is supposed to call this, because track_size is
-- only set from the UI.
set_track_size :: (M m) => ViewId -> (Int, Int) -> m ()
set_track_size view_id (visible_track, visible_time) =
    modify_view view_id $ \view -> view {
        Block.view_visible_track = visible_track
        , Block.view_visible_time = visible_time }

-- ** selections

-- | Get @view_id@'s selection at @selnum@, or Nothing if there is none.
get_selection :: (M m) => ViewId -> Types.SelNum -> m (Maybe Types.Selection)
get_selection view_id selnum = do
    view <- get_view view_id
    return (Map.lookup selnum (Block.view_selections view))

-- | Replace any selection on @view_id@ at @selnum@ with @sel@.
set_selection :: (M m) => ViewId -> Types.SelNum
    -> Maybe Types.Selection -> m ()
set_selection view_id selnum maybe_sel = do
    view <- get_view view_id
    update_view view_id $ view { Block.view_selections =
        (maybe (Map.delete selnum) (Map.insert selnum) maybe_sel)
        (Block.view_selections view) }

-- ** util

update_view view_id view = modify $ \st -> st
    { state_views = Map.adjust (const view) view_id (state_views st) }
modify_view view_id f = do
    view <- get_view view_id
    update_view view_id (f view)

-- * block

get_all_block_ids :: (M m) => m [BlockId]
get_all_block_ids = gets (Map.keys . state_blocks)

get_block :: (M m) => BlockId -> m Block.Block
get_block block_id = get >>= lookup_id block_id . state_blocks

lookup_block :: (M m) => BlockId -> m (Maybe Block.Block)
lookup_block block_id = get >>= return . Map.lookup block_id . state_blocks

-- | Make a new block.  If it's the first one, it will be set as the root.
-- This is the low level version, you probably want to use 'create_block'.
--
-- Throw if the BlockId already exists.
create_config_block :: (M m) => Id.Id -> Block.Block -> m BlockId
create_config_block id block = insert (Types.BlockId id) block state_blocks $
    \blocks st -> st
        { state_blocks = blocks
        , state_config = let c = state_config st
            in c { config_root = if Map.size blocks == 1
                then Just (Types.BlockId id) else config_root c }
        }

-- | Make a new block with the default 'Block.Config'.
create_block :: (M m) => Id.Id -> String -> [Block.Track] -> m BlockId
create_block block_id title tracks =
    create_config_block block_id
        (Block.block Block.default_config title tracks)

-- | Destroy the block and all the views that display it.  If the block was
-- the root, it will be be unset.  The block's tracks are left intact.
destroy_block :: (M m) => BlockId -> m ()
destroy_block block_id = do
    views <- get_views_of block_id
    mapM_ destroy_view (Map.keys views)
    modify $ \st -> st
        { state_blocks = Map.delete block_id (state_blocks st)
        , state_config = let c = state_config st
            in c { config_root = if config_root c == Just block_id
                then Nothing else config_root c }
        }

block_of :: (M m) => ViewId -> m Block.Block
block_of view_id = get_block . Block.view_block =<< get_view view_id

block_id_of :: (M m) => ViewId -> m BlockId
block_id_of view_id = Block.view_block <$> get_view view_id

set_block_config :: (M m) => BlockId -> Block.Config -> m ()
set_block_config block_id config =
    modify_block block_id (\block -> block { Block.block_config = config })

set_edit_box :: (M m) => BlockId -> Block.Box -> Block.Box -> m ()
set_edit_box block_id skel track = do
    block <- get_block block_id
    set_block_config block_id $ (Block.block_config block)
        { Block.config_skel_box = skel
        , Block.config_track_box = track
        }

-- | The play box doesn't use a char, so I leave that out.
set_play_box :: (M m) => BlockId -> Color.Color -> m ()
set_play_box block_id color = do
    block <- get_block block_id
    set_block_config block_id $ (Block.block_config block)
        { Block.config_sb_box = Block.Box color ' ' }

-- | Get the end of the block according to the ruler.  This means that if the
-- block has no rulers (e.g. a clipboard block) then block_ruler_end will be 0.
block_ruler_end :: (M m) => BlockId -> m ScoreTime
block_ruler_end block_id = do
    block <- get_block block_id
    case Block.block_ruler_ids block of
        [] -> return 0
        ruler_id : _ -> Ruler.time_end <$> get_ruler ruler_id

-- | Get the end of the block according to the last event of the block.
block_event_end :: (M m) => BlockId -> m ScoreTime
block_event_end block_id = do
    block <- get_block block_id
    track_ends <- mapM track_end (Block.block_track_ids block)
    return $ maximum (0 : track_ends)

-- ** skeleton

get_skeleton :: (M m) => BlockId -> m Skeleton.Skeleton
get_skeleton block_id = Block.block_skeleton <$> get_block block_id

set_skeleton :: (M m) => BlockId -> Skeleton.Skeleton -> m ()
set_skeleton block_id skel = modify_skeleton block_id (const skel)

modify_skeleton :: (M m) => BlockId
    -> (Skeleton.Skeleton -> Skeleton.Skeleton) -> m ()
modify_skeleton block_id f = modify_block block_id $
    \block -> block { Block.block_skeleton = f (Block.block_skeleton block) }

-- | Toggle the given edge in the block's skeleton.  If a cycle would be
-- created, refuse to add the edge and return False.  The edge is in (parent,
-- child) order.
toggle_skeleton_edge :: (M m) => BlockId -> Skeleton.Edge -> m Bool
toggle_skeleton_edge block_id edge = do
    block <- get_block block_id
    when_just (verify_edge block edge) (throw . ("toggle: " ++))
    let skel = Block.block_skeleton block
    case Skeleton.toggle_edge edge skel of
        Nothing -> return False
        Just new_skel -> do
            set_block block_id $ block { Block.block_skeleton = new_skel }
            return True

-- | Add the edges to the skeleton.  Throw if they would produce a cycle.
add_edges :: (M m) => BlockId -> [Skeleton.Edge] -> m ()
add_edges block_id edges = do
    skel <- get_skeleton block_id
    block <- get_block block_id
    when_just (msum (map (verify_edge block) edges))
        (throw . ("add_adges: " ++))
    maybe (throw $ "add_edges " ++ show edges ++ " to " ++ show skel
            ++ " would have caused a cycle")
        (set_skeleton block_id) (Skeleton.add_edges edges skel)

remove_edges :: (M m) => BlockId -> [Skeleton.Edge] -> m ()
remove_edges block_id edges =
    modify_skeleton block_id (Skeleton.remove_edges edges)

-- | The first tracknum is spliced above the second.
splice_skeleton_above :: (M m) => BlockId -> TrackNum -> TrackNum -> m ()
splice_skeleton_above = _splice_skeleton True

-- | The first tracknum is spliced below the second.
splice_skeleton_below :: (M m) => BlockId -> TrackNum -> TrackNum -> m ()
splice_skeleton_below = _splice_skeleton False

-- | Splice the given tracknum into the skeleton, either above or below
-- the @to@ tracknum.  What this means exactly is documented in
-- 'Util.Graph.splice_above' and 'Util.Graph.slice_below'.
_splice_skeleton :: (M m) => Bool -> BlockId -> TrackNum -> TrackNum -> m ()
_splice_skeleton above block_id new to = do
    block <- get_block block_id
    when_just (msum (map (verify_track block) [new, to]))
        (throw . ("splice: " ++))
    let splice = if above then Skeleton.splice_above else Skeleton.splice_below
    maybe (throw $ "splice_skeleton: " ++ show (new, to)
            ++ " would have caused a cycle")
        (set_skeleton block_id) (splice new to (Block.block_skeleton block))

verify_track :: Block.Block -> TrackNum -> Maybe String
verify_track block tracknum =
    case Seq.at (Block.block_tracks block) tracknum of
        Nothing -> Just $ "tracknum out of range: " ++ show tracknum
        Just t -> case Block.tracklike_id t of
            Block.TId {} -> Nothing
            _ -> Just $ "edge points to non-event track: " ++ show t

verify_edge :: Block.Block -> Skeleton.Edge -> Maybe String
verify_edge block (from, to) =
    mplus (verify_track block from) (verify_track block to)

-- *** TrackTree

-- | A TrackTree is the Skeleton resolved to the tracks it references.
type TrackTree = [Tree.Tree TrackInfo]
-- | A TrackTree annotated with which tracks are muted.
type TrackTreeMutes = [Tree.Tree (TrackInfo, Bool)]

-- | Summary information on a Track.
data TrackInfo = TrackInfo {
    track_title :: String
    , track_id :: TrackId
    , track_tracknum :: TrackNum
    } deriving (Eq, Show)

get_track_tree :: (M m) => BlockId -> m TrackTree
get_track_tree block_id = do
    skel <- get_skeleton block_id
    tracks <- get_track_info block_id
    ntracks <- fmap (length . Block.block_tracklike_ids) (get_block block_id)
    let by_tracknum = Map.fromList $ zip (map track_tracknum tracks) tracks
    let (resolved, missing) = resolve_track_tree by_tracknum
            (Skeleton.to_forest ntracks skel)
    -- Rulers and dividers should show up as missing.  They're ok as long as
    -- they have no edges.
    let really_missing = filter (not . Skeleton.lonely_vertex skel) missing
    unless (null really_missing) $
        throw $ "skeleton of " ++ show block_id
            ++ " names missing tracknums: " ++ show really_missing
    return resolved

get_track_tree_mutes :: (M m) => BlockId -> m TrackTreeMutes
get_track_tree_mutes block_id = do
    tree <- get_track_tree block_id
    block <- get_block block_id
    return $ track_tree_mutes (muted_tracknums block tree) tree

muted_tracknums :: Block.Block -> TrackTree -> [TrackNum]
muted_tracknums block tree
    | null solo = mute
    | otherwise = map fst tracks List.\\ soloed
    where
    tracks =
        [(i, track) | (i, track) <- Seq.enumerate (Block.block_tracks block),
            is_track track]
    is_track track = case Block.tracklike_id track of
        Block.TId {} -> True
        _ -> False
    solo = [i | (i, t) <- tracks, Block.Solo `elem` Block.track_flags t]
    mute = [i | (i, t) <- tracks, Block.Mute `elem` Block.track_flags t]
    -- A soloed track will keep all its parents and children unmuted.
    soloed = List.nub $ concat
        [ track_tracknum t : map track_tracknum (ps ++ cs)
        | (t, ps, cs) <- Tree.flat_paths tree, track_tracknum t `elem` solo ]

track_tree_mutes :: [TrackNum] -> TrackTree -> TrackTreeMutes
track_tree_mutes muted forest = map f forest
    where
    f (Tree.Node info subs) = Tree.Node (add_mute info) (map f subs)
    add_mute info = (info, track_tracknum info `elem` muted)

-- | Resolve the TrackNum indices in a tree into whatever values as given by
-- a map.
resolve_track_tree :: Map.Map TrackNum a -> [Tree.Tree TrackNum]
    -> ([Tree.Tree a], [TrackNum]) -- ^ resolved tree, and missing TrackNums
resolve_track_tree tracknums = foldr cat_tree ([], []) . map go
    where
    go (Tree.Node tracknum subs) = case Map.lookup tracknum tracknums of
        Nothing -> (Nothing, [tracknum])
        Just track_info ->
            let (subforest, missing) = resolve_track_tree tracknums subs
            in (Just (Tree.Node track_info subforest), missing)
    cat_tree (maybe_tree, missing) (forest, all_missing) = case maybe_tree of
        Nothing -> (forest, missing ++ all_missing)
        Just tree -> (tree : forest, missing ++ all_missing)

type EventsTree = [EventsNode]
type EventsNode = Tree.Tree TrackEvents

data TrackEvents = TrackEvents {
    tevents_title :: !String
    , tevents_events :: !Events.Events
    -- | If this TrackEvents is from a real track, then its evaluation can
    -- generate a render signal as a side-effect.
    , tevents_track_id :: !(Maybe TrackId)

    -- | Tracks often extend beyond the end of the last event.  The derivers
    -- need to know the track end to get the controls of the last note, and
    -- for the block stretch hack.  Note that this is the end of the longest
    -- track of the block, so it's not the same as @snd . tevents_range@.
    , tevents_end :: !ScoreTime

    -- | Range of the track.  This may be past the end of the last event since
    -- it's the range of the block as a whole.
    --
    -- Used by "Derive.Cache": due to inverting calls, a control track may be
    -- sliced to a shorter range.  In that case, I shouldn't bother with
    -- damage outside of its range.
    --
    -- This is a (start, end) range, not (start, dur).
    --
    -- Tracks often extend beyond the end of the last event.  The derivers
    -- need to know the track end to get the controls of the last note, and
    -- for the block stretch hack.
    , tevents_range :: !(ScoreTime, ScoreTime)
    -- | True if this is a sliced track.  That means it's a fragment of
    -- a track and so certain track-level things, like recording a track
    -- signal, should be skipped.
    , tevents_sliced :: !Bool
    -- | These events are not evaluated, but go in the
    -- 'Derive.Derive.info_prev_events' and info_next_events.  This is so that
    -- sliced calls (such as inverting calls) can see previous and following
    -- events.
    , tevents_around :: !([Events.PosEvent], [Events.PosEvent])

    -- | If the events have been shifted from their original positions on the
    -- track, this can be added to them to put them back in track time.  This
    -- is for the stack, which should always be in track time.  It's probably
    -- the same as @fst . tevents_range@, but only applies if the events have
    -- been shifted, which you can't tell from just looking at
    -- @tevents_range@.
    , tevents_shifted :: !ScoreTime
    } deriving (Show)

track_events :: String -> Events.Events -> ScoreTime -> TrackEvents
track_events title events end = TrackEvents
    { tevents_title = title
    , tevents_events = events
    , tevents_track_id = Nothing
    , tevents_end = end
    , tevents_range = (0, end)
    , tevents_sliced = False
    , tevents_around = ([], [])
    , tevents_shifted = 0
    }

events_tree :: (M m) => ScoreTime -> TrackTree -> m EventsTree
events_tree events_end tree = mapM resolve tree
    where
    resolve (Tree.Node (TrackInfo title track_id _) subs) =
        Tree.Node <$> make title track_id <*> mapM resolve subs
    make title track_id = do
        track <- get_track track_id
        return $ (track_events title (Track.track_events track) events_end)
            { tevents_track_id = Just track_id }

-- ** tracks

insert_track :: (M m) => BlockId -> TrackNum -> Block.Track -> m ()
insert_track block_id tracknum track = do
    block <- get_block block_id
    views <- get_views_of block_id
    let tracks = Seq.insert_at tracknum track (Block.block_tracks block)
        -- Make sure the views are up to date.
        views' = Map.map (insert_into_view block tracknum) views
    set_block block_id $ block
        { Block.block_tracks = tracks
        , Block.block_skeleton =
            Skeleton.insert tracknum (Block.block_skeleton block)
        }
    modify $ \st -> st { state_views = Map.union views' (state_views st) }

remove_track :: (M m) => BlockId -> TrackNum -> m ()
remove_track block_id tracknum = do
    block <- get_block block_id
    let tracks = Seq.remove_at tracknum (Block.block_tracks block)
    views <- Map.map (remove_from_view block tracknum) <$>
        get_views_of block_id
    set_block block_id $ block
        { Block.block_tracks = tracks
        , Block.block_skeleton =
            Skeleton.remove tracknum (Block.block_skeleton block)
        }
    modify $ \st -> st { state_views = Map.union views (state_views st) }

-- | Get the Track at @tracknum@, or Nothing if its out of range.
-- This is inconsistent with 'insert_track' and 'remove_track' which clip to
-- range, but is convenient in practice.
block_track_at :: (M m) => BlockId -> TrackNum -> m (Maybe Block.Track)
block_track_at block_id tracknum
    | tracknum < 0 =
        throw $ "block_track_at: negative tracknum " ++ show tracknum
    | otherwise = do
        block <- get_block block_id
        return $ Seq.at (Block.block_tracks block) tracknum

track_at :: (M m) => BlockId -> TrackNum -> m (Maybe Block.TracklikeId)
track_at block_id tracknum = do
    maybe_track <- block_track_at block_id tracknum
    return $ fmap Block.tracklike_id maybe_track

-- | Like 'track_at', but only for event tracks.
event_track_at :: (M m) => BlockId -> TrackNum -> m (Maybe TrackId)
event_track_at block_id tracknum = do
    maybe_track <- track_at block_id tracknum
    return $ Block.track_id_of =<< maybe_track

-- | Like 'event_track_at' but throws if it's not there.
get_event_track_at :: (M m) => String -> BlockId -> TrackNum -> m TrackId
get_event_track_at caller block_id tracknum =
    require msg =<< event_track_at block_id tracknum
    where
    msg = caller ++ ": tracknum " ++ show tracknum ++ " not in "
        ++ show block_id

get_track_info :: (M m) => BlockId -> m [TrackInfo]
get_track_info block_id = do
    block <- get_block block_id
    state <- get
    return [TrackInfo (Track.track_title track) tid i
        | (i, tid, track) <- track_info block (state_tracks state)]
    where
    track_info block tracks = do
        (i, Block.TId tid _) <- Seq.enumerate (Block.block_tracklike_ids block)
        track <- maybe mzero (:[]) (Map.lookup tid tracks)
        return (i, tid, track)

-- | Like 'track_at', but only for event tracks.  It defaults to 'no_ruler'
-- if the tracknum is out of range or doesn't have a ruler.
ruler_track_at :: (M m) => BlockId -> TrackNum -> m RulerId
ruler_track_at block_id tracknum = do
    maybe_track <- track_at block_id tracknum
    return $ Maybe.fromMaybe no_ruler $ Block.ruler_id_of =<< maybe_track

tracks :: (M m) => BlockId -> m TrackNum
tracks block_id = do
    block <- get_block block_id
    return $ length (Block.block_tracks block)

get_tracklike :: (M m) => Block.TracklikeId -> m Block.Tracklike
get_tracklike track = case track of
    Block.TId track_id ruler_id ->
        Block.T <$> get_track track_id <*> get_ruler ruler_id
    Block.RId ruler_id ->
        Block.R <$> get_ruler ruler_id
    Block.DId divider -> return (Block.D divider)

-- *** block track

get_block_track :: (M m) => BlockId -> TrackNum -> m Block.Track
get_block_track block_id tracknum = do
    block <- get_block block_id
    let msg = "State.get_block_track: bad tracknum for " ++ show block_id
            ++ ": " ++ show tracknum
    require msg $ Seq.at (Block.block_tracks block) tracknum

set_track_width :: (M m) => BlockId -> TrackNum -> Types.Width -> m ()
set_track_width block_id tracknum width =
    modify_block_track block_id tracknum $ \btrack ->
        btrack { Block.track_width = width }

toggle_track_flag :: (M m) => BlockId -> TrackNum -> Block.TrackFlag -> m ()
toggle_track_flag block_id tracknum flag =
    modify_track_flags block_id tracknum toggle
    where
    toggle flags
        | flag `elem` flags = List.delete flag flags
        | otherwise = flag : flags

add_track_flag, remove_track_flag
    :: (M m) => BlockId -> TrackNum -> Block.TrackFlag -> m ()
add_track_flag block_id tracknum flag =
    modify_track_flags block_id tracknum (List.union [flag])
remove_track_flag block_id tracknum flag =
    modify_track_flags block_id tracknum (List.delete flag)

modify_track_flags :: (M m) => BlockId -> TrackNum
    -> ([Block.TrackFlag] -> [Block.TrackFlag]) -> m ()
modify_track_flags block_id tracknum f =
    modify_block_track block_id tracknum $ \btrack ->
        btrack { Block.track_flags = f (Block.track_flags btrack) }

-- | Merge the @from@ tracknum into the @to@ tracknum and collapse @from@.
merge_track :: (M m) => BlockId -> TrackNum -> TrackNum -> m ()
merge_track block_id to from = do
    from_id <- get_event_track_at "State.merge_track" block_id from
    modify_block_track block_id to $ \btrack ->
        btrack { Block.track_merged = from_id : Block.track_merged btrack }
    add_track_flag block_id from Block.Collapse

-- | Reverse 'merge_track': remove the merged tracks and expand their
-- occurrances in the given block.  \"Unmerge\" is not graceful, but at least
-- it's obviously the opposite of \"merge\".
unmerge_track :: (M m) => BlockId -> TrackNum -> m ()
unmerge_track block_id tracknum = do
    track_ids <- Block.track_merged <$> get_block_track block_id tracknum
    unmerged_tracknums <-
        concat <$> mapM (track_id_tracknums block_id) track_ids
    forM_ unmerged_tracknums $ \tracknum ->
        remove_track_flag block_id tracknum Block.Collapse
    set_merged_tracks block_id tracknum []

set_merged_tracks :: (M m) => BlockId -> TrackNum -> [TrackId] -> m ()
set_merged_tracks block_id tracknum merged =
    modify_block_track block_id tracknum $ \btrack ->
        btrack { Block.track_merged = merged }

track_id_tracknums :: (M m) => BlockId -> TrackId -> m [TrackNum]
track_id_tracknums block_id track_id = do
    block_tracks <- blocks_with_track track_id
    return [tracknum | (bid, tracks) <- block_tracks, bid == block_id,
        (tracknum, _) <- tracks]

modify_block_track :: (M m) => BlockId -> TrackNum
    -> (Block.Track -> Block.Track) -> m ()
modify_block_track block_id tracknum modify = do
    block <- get_block block_id
    btracks <- modify_at "modify_block_track"
        (Block.block_tracks block) tracknum modify
    modify_block block_id $ \b -> b { Block.block_tracks = btracks }

-- *** track util

-- | Insert a new track into Block.view_tracks, moving selections as
-- appropriate.  @tracknum@ is clipped to be in range.
insert_into_view :: Block.Block -> TrackNum -> Block.View -> Block.View
insert_into_view block tracknum view = view
    { Block.view_selections = Map.map (insert_into_selection block tracknum)
        (Block.view_selections view)
    }

-- | Remove @tracknum@ from Block.view_tracks, moving selections as
-- appropriate.  Ignored if @tracknum@ is out of range.
remove_from_view :: Block.Block -> TrackNum -> Block.View -> Block.View
remove_from_view block tracknum view = view
    { Block.view_selections =
        Map.mapMaybe (remove_from_selection block tracknum)
            (Block.view_selections view)
    }

-- | If tracknum is before or at the selection, push it to the right.  If it's
-- inside, extend it.  If it's to the right, do nothing.
insert_into_selection :: Block.Block -> TrackNum -> Types.Selection
    -> Types.Selection
insert_into_selection block tracknum sel
    | tracknum <= low = shift_selection block 1 sel
    | tracknum <= high = Types.sel_expand_tracks 1 sel
    | otherwise = sel
    where (low, high) = Types.sel_track_range sel

-- | Remove the given track from the selection.  The selection will be moved
-- or shrunk as per 'insert_into_selection'.
remove_from_selection :: Block.Block -> TrackNum -> Types.Selection
    -> Maybe Types.Selection
remove_from_selection block tracknum sel
    | tracknum <= low = Just $ shift_selection block (-1) sel
        -- Just $ Types.sel_modify_tracks (subtract 1) sel
    | tracknum <= high = Just $ Types.sel_expand_tracks (-1) sel
    | otherwise = Just sel
    where (low, high) = Types.sel_track_range sel

-- | Shift the selection along selectable tracks, clipping if it's out of
-- range.  While the sel_cur_track won't be on a non-selectable track after
-- this, the selection may still include one.
shift_selection :: Block.Block -> TrackNum -> Types.Selection
    -> Types.Selection
shift_selection block shift sel =
    Types.sel_modify_tracks (Num.clamp 0 max_track . (+shift2)) sel
    where
    new_tracknum = shift_tracknum block (Types.sel_cur_track sel) shift
    shift2 = new_tracknum - Types.sel_cur_track sel
    max_track = length (Block.block_tracks block)

-- | Shift a tracknum to another track, skipping unselectable tracks.
shift_tracknum :: Block.Block -> TrackNum -> Int -> TrackNum
shift_tracknum block tracknum shift
    | shift == 0 = tracknum
    | shift > 0 = find_track (dropWhile (<tracknum) selectable)
    | otherwise = find_track (dropWhile (>tracknum) (List.reverse selectable))
    where
    selectable = selectable_tracks block
    find_track [] = tracknum
    find_track tracks@(first:_) =
        Maybe.fromMaybe tracknum $ Seq.head $ drop abs_shift tracks
        where
        abs_shift = if tracknum /= first then abs shift - 1 else abs shift

-- | Get the tracknums from a block that should be selectable.
selectable_tracks :: Block.Block -> [TrackNum]
selectable_tracks block = do
    (i, track@(Block.Track { Block.tracklike_id = Block.TId _ _}))
        <- zip [0..] (Block.block_tracks block)
    guard (not (Block.track_collapsed track))
    return i

-- ** other

set_block_title :: (M m) => BlockId -> String -> m ()
set_block_title block_id title =
    modify_block block_id (\block -> block { Block.block_title = title })

-- | Set a status variable on a view.
set_view_status :: (M m) => ViewId -> String -> Maybe String -> m ()
set_view_status view_id key val =
    modify_view view_id $ \view -> view { Block.view_status =
        Map.alter (const val) key (Block.view_status view) }

-- ** util

set_block :: (M m) => BlockId -> Block.Block -> m ()
set_block block_id block = modify $ \st -> st
    { state_blocks = Map.adjust (const block) block_id (state_blocks st) }

modify_block :: (M m) => BlockId -> (Block.Block -> Block.Block) -> m ()
modify_block block_id f = do
    block <- get_block block_id
    set_block block_id (f block)

-- * track

get_track :: (M m) => TrackId -> m Track.Track
get_track track_id = get >>= lookup_id track_id . state_tracks

lookup_track :: (M m) => TrackId -> m (Maybe Track.Track)
lookup_track track_id = get >>= return . Map.lookup track_id . state_tracks

-- | Insert the given track with the given ID.
--
-- Throw if the TrackId already exists.
create_track :: (M m) => Id.Id -> Track.Track -> m TrackId
create_track id track = do
    track_id <- insert (Types.TrackId id) track state_tracks $
        \tracks st -> st { state_tracks = tracks }
    -- Since I don't diff events but rely on changes being recorded here,
    -- I have to mark this track as having new events.  Otherwise, if the same
    -- TrackId is destroyed and recreated then diff won't notice the changed
    -- events.
    update $ Update.TrackUpdate track_id
        (Update.TrackAllEvents (Track.track_events track))
    return track_id

-- | Destroy the track and remove it from all the blocks it's in.  No-op if
-- the TrackId doesn't exist.
destroy_track :: (M m) => TrackId -> m ()
destroy_track track_id = do
    blocks <- blocks_with_track track_id
    forM_ blocks $ \(block_id, tracks) -> forM_ tracks $ \(tracknum, _) ->
        remove_track block_id tracknum
    modify $ \st -> st { state_tracks = Map.delete track_id (state_tracks st) }

get_track_title :: (M m) => TrackId -> m String
get_track_title = (Track.track_title <$>) . get_track

set_track_title :: (M m) => TrackId -> String -> m ()
set_track_title track_id text = modify_track_title track_id (const text)

modify_track_title :: (M m) => TrackId -> (String -> String) -> m ()
modify_track_title track_id f = modify_track track_id $ \track ->
    track { Track.track_title = f (Track.track_title track) }

set_track_bg :: (M m) => TrackId -> Color.Color -> m ()
set_track_bg track_id color = modify_track track_id $ \track ->
    track { Track.track_bg = color }

modify_track_render :: (M m) => TrackId
    -> (Track.RenderConfig -> Track.RenderConfig) -> m ()
modify_track_render track_id f = modify_track track_id $ \track ->
    track { Track.track_render = f (Track.track_render track) }

set_render_style :: (M m) => Track.RenderStyle -> TrackId -> m ()
set_render_style style track_id = modify_track_render track_id $
    \render -> render { Track.render_style = style }

-- ** events

-- There are two interpretations of a range: the strict one is that when
-- start==end nothing can be selected.  A more relaxed one is that start==end
-- will still select an event at start.  The relaxed one is often convenient
-- for commands, so there are typically three variants of each ranged command:
-- select events in the strict half-open range (functions end with _range),
-- select an event at a certain point (functions use the singular), and select
-- events in the relaxed half-open range (functions use the plural).

-- | Insert events into track_id as per 'Events.insert_events'.
insert_events :: (M m) => TrackId -> [Events.PosEvent] -> m ()
insert_events track_id pos_evts =
    -- Calculating updates is easiest if it's sorted, and insert likes sorted
    -- anyway.
    insert_sorted_events track_id (Seq.sort_on fst pos_evts)

-- | Like 'insert_events', but more efficient and dangerous.
insert_sorted_events :: (M m) => TrackId -> [(ScoreTime, Event.Event)] -> m ()
insert_sorted_events track_id pos_evts = _modify_events track_id $ \events ->
    (Events.insert_sorted_events pos_evts events, _events_updates pos_evts)

insert_event :: (M m) => TrackId -> ScoreTime -> Event.Event -> m ()
insert_event track_id pos evt = insert_sorted_events track_id [(pos, evt)]

get_events :: (M m) => TrackId -> ScoreTime -> ScoreTime -> m [Events.PosEvent]
get_events track_id start end = do
    events <- Track.track_events <$> get_track track_id
    return (_events_in_range start end events)

get_event :: (M m) => TrackId -> ScoreTime -> m (Maybe Events.PosEvent)
get_event track_id pos = Seq.head <$> get_events track_id pos pos

get_all_events :: (M m) => TrackId -> m [Events.PosEvent]
get_all_events = (Events.ascending . Track.track_events <$>) . get_track

modify_events :: (M m) => TrackId -> (Events.Events -> Events.Events) -> m ()
modify_events track_id f = do
    track1 <- get_track track_id
    let track2 = track1 { Track.track_events = f (Track.track_events track1) }
    _set_track track_id track2
    update $ Update.TrackUpdate track_id
        (Update.TrackAllEvents (Track.track_events track2))

-- | Remove any events whose starting positions fall within the half-open
-- range given, or under the point if the selection is a point.
remove_events :: (M m) => TrackId -> ScoreTime -> ScoreTime -> m ()
remove_events track_id start end
    | start == end = remove_event track_id start
    | otherwise = remove_event_range track_id start end

-- | Remove a single event at @pos@, if there is one.
remove_event :: (M m) => TrackId -> ScoreTime -> m ()
remove_event track_id pos = _modify_events track_id $ \events ->
    case Events.at pos events of
        Nothing -> (events, [])
        Just evt ->
            (Events.remove_event pos events, _events_updates [(pos, evt)])

-- | Remove any events whose starting positions strictly fall within the
-- half-open range given.
remove_event_range :: (M m) => TrackId -> ScoreTime -> ScoreTime -> m ()
remove_event_range track_id start end =
    _modify_events track_id $ \events ->
        let evts = Events.ascending (Events.in_range start end events)
        in (Events.remove_events start end events, _events_updates evts)

map_events_sorted :: (M m) => TrackId -> ScoreTime -> ScoreTime
    -> ([Events.PosEvent] -> [Events.PosEvent]) -> m ()
map_events_sorted track_id start end f = _modify_events track_id $ \events ->
    let old = _events_in_range start end events
        new = f old
        deleted = if start == end
            then Events.remove_event start events
            else Events.remove_events start end events
        starts = map Events.min $ Maybe.mapMaybe Seq.head [old, new]
        ends = map Events.max $ Maybe.mapMaybe Seq.last [old, new]
        updates = if null starts || null ends then []
            else [(minimum starts, maximum ends)]
    in (Events.insert_sorted_events new deleted, updates)

_events_in_range :: ScoreTime -> ScoreTime -> Events.Events
    -> [Events.PosEvent]
_events_in_range start end events
    | start == end = maybe [] ((:[]) . (,) start)
        (Events.at start events)
    | otherwise = Events.ascending (Events.in_range start end events)

-- | Get the end of the last event of the block.
track_end :: (M m) => TrackId -> m ScoreTime
track_end track_id =
    Events.time_end . Track.track_events <$> get_track track_id

-- | Emit track updates for all tracks.  Use this when events have changed but
-- I don't know which ones, e.g. when loading a file or restoring a previous
-- state.
update_all_tracks :: (M m) => m ()
update_all_tracks = do
    st <- get
    let mk (tid, track) = Update.TrackUpdate tid $
            Update.TrackAllEvents (Track.track_events track)
    mapM_ update $ map mk (Map.toList (state_tracks st))

-- ** util

modify_track :: (M m) => TrackId -> (Track.Track -> Track.Track) -> m ()
modify_track track_id f = do
    track <- get_track track_id
    _set_track track_id (f track)

_modify_events :: (M m) => TrackId
    -> (Events.Events -> (Events.Events, [(ScoreTime, ScoreTime)]))
    -> m ()
_modify_events track_id f = do
    track <- get_track track_id
    let (new_events, updates) = f (Track.track_events track)
    _set_track track_id (track { Track.track_events = new_events })
    let mk (s, e) = Update.TrackUpdate track_id $ Update.TrackEvents s e $
            Events.in_range s e new_events
    mapM_ (update . mk) updates

_events_updates :: [Events.PosEvent] -> [(ScoreTime, ScoreTime)]
_events_updates [] = []
_events_updates evts = [(Events.min (head evts), Events.max (last evts))]

_set_track track_id track = modify $ \st -> st
    { state_tracks = Map.adjust (const track) track_id (state_tracks st) }

-- * ruler

ruler_of :: (M m) => BlockId -> m RulerId
ruler_of block_id = require ("no ruler in " ++ show block_id)
    =<< Seq.head <$> Block.block_ruler_ids <$> get_block block_id

rulers_of :: (M m) => BlockId -> m [RulerId]
rulers_of block_id = Seq.unique . Block.block_ruler_ids <$> get_block block_id

get_ruler :: (M m) => RulerId -> m Ruler.Ruler
get_ruler ruler_id
    | ruler_id == no_ruler = return Ruler.no_ruler
    | otherwise = get >>= lookup_id ruler_id . state_rulers

lookup_ruler :: (M m) => RulerId -> m (Maybe Ruler.Ruler)
lookup_ruler ruler_id = get >>= return . Map.lookup ruler_id . state_rulers

-- | Insert the given ruler with the given ID.
--
-- Throw if the RulerId already exists.
create_ruler :: (M m) => Id.Id -> Ruler.Ruler -> m RulerId
create_ruler id ruler
        -- no_ruler is global and assumed to always exist.
    | id == Id.unpack_id no_ruler =
        throw $ "can't insert no-ruler: " ++ Pretty.pretty no_ruler
    | otherwise = insert (Types.RulerId id) ruler state_rulers $ \rulers st ->
        st { state_rulers = rulers }

-- | Destroy the ruler and remove it from all the blocks it's in.
destroy_ruler :: (M m) => RulerId -> m ()
destroy_ruler ruler_id = do
    blocks <- blocks_with_ruler ruler_id
    forM_ blocks $ \(block_id, tracks) -> do
        let tracknums = map fst tracks
            setr i = if i `elem` tracknums then Block.set_rid no_ruler else id
            deruler (i, track) = Block.modify_id track (setr i)
        modify_block block_id $ \block -> block { Block.block_tracks =
            map deruler (Seq.enumerate (Block.block_tracks block)) }
    modify $ \st -> st { state_rulers = Map.delete ruler_id (state_rulers st) }

modify_ruler :: (M m) => RulerId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
modify_ruler ruler_id f = do
    ruler <- get_ruler ruler_id
    modify $ \st ->
        st { state_rulers = Map.insert ruler_id (f ruler) (state_rulers st) }

-- * search

-- | Get all views of a given block.
get_views_of :: (M m) => BlockId -> m (Map.Map ViewId Block.View)
get_views_of block_id = do
    views <- gets state_views
    return $ Map.filter ((==block_id) . Block.view_block) views

-- | Get all TrackIds of the given block.
track_ids_of :: (M m) => BlockId -> m [TrackId]
track_ids_of block_id = Block.block_track_ids <$> get_block block_id

-- | Find @track_id@ in all the blocks it exists in, and return the track info
-- for each tracknum at which @track_id@ lives.  Blocks with no matching tracks
-- won't be returned, so the return track lists will always be non-null.
blocks_with_track :: (M m) =>
    TrackId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
blocks_with_track track_id =
    find_tracks_m ((== Just track_id) . Block.track_id_of)

-- | Just like 'blocks_with_track' except for ruler_id.
blocks_with_ruler :: (M m) =>
    RulerId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
blocks_with_ruler ruler_id =
    find_tracks_m ((== Just ruler_id) . Block.ruler_id_of)

find_tracks_m :: (M m) => (Block.TracklikeId -> Bool)
    -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
find_tracks_m f = gets (find_tracks f . state_blocks)

find_tracks :: (Block.TracklikeId -> Bool) -> Map.Map BlockId Block.Block
    -> [(BlockId, [(TrackNum, Block.TracklikeId)])]
find_tracks f blocks = do
    (bid, b) <- Map.assocs blocks
    let tracks = get_tracks b
    guard (not (null tracks))
    return (bid, tracks)
    where
    all_tracks block = Seq.enumerate (Block.block_tracks block)
    get_tracks block =
        [ (tracknum, Block.tracklike_id track)
        | (tracknum, track) <- all_tracks block, f (Block.tracklike_id track)]

-- * util

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k, M m) => k -> Map.Map k a -> m a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "State.lookup: unknown " ++ show key
    Just val -> return val

-- | Insert @val@ at @key@ in @get_map state@, throwing if it already exists.
-- Put the map back into @state@ by applying @set_map new_map state@ to it.
insert :: (M m, Ord k, Show k) => k -> a -> (State -> Map.Map k a)
    -> (Map.Map k a -> State -> State) -> m k
insert key val get_map set_map = do
    state <- get
    when (key `Map.member` get_map state) $
        throw $ show key ++ " already exists"
    put (set_map (Map.insert key val (get_map state)) state)
    return key

-- | Modify the @i@th element of @xs@ by applying @f@ to it.
modify_at :: (M m) => String -> [a] -> Int -> (a -> a) -> m [a]
modify_at msg xs i f = case post of
    [] -> throw $ msg ++ ": can't replace index " ++ show i
        ++ " of list with length " ++ show (length xs)
    (elt:rest) -> return (pre ++ f elt : rest)
    where (pre, post) = splitAt i xs
