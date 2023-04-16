-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
module Ui.Ui (
    State(..), views, blocks, tracks, rulers, config
    , empty, create, clear
    -- * address types
    , Track(..), Range(..), TrackInfo(..)
    -- * StateT monad
    , M, StateT, StateId, get, unsafe_put, damage, get_damage
    , throw_error, throw
    , run, run_id, eval, eval_rethrow, exec, exec_rethrow
    , gets, unsafe_modify, put, modify
    , update_all
    -- ** errors
    , Error(..)
    , require, require_right

    -- * config
    , get_namespace, set_namespace
    , get_default, modify_default, get_root_id, lookup_root_id, set_root_id
    , modify_config, get_config, with_config
    , modify_meta
    , modify_allocation, allocation

    -- * view
    , get_view, lookup_view, all_view_ids
    , create_view, destroy_view, put_views
    , set_view_status
    -- ** zoom and track scroll
    , get_zoom, modify_zoom, set_track_scroll, set_view_rect
    , set_view_padding
    -- ** selections
    , get_selection, set_selection
    , shift_selection, skip_unselectable_tracks
    , selectable_tracks

    -- * block
    , get_block, lookup_block, all_block_ids, all_block_track_ids
    , create_config_block, create_block, destroy_block
    , block_of, block_id_of, views_of
    , get_block_title, set_block_title
    , modify_block_meta
    , set_integrated_block, modify_integrated_tracks, set_integrated_manual
    , set_edit_box, set_play_box
    , block_ruler_end, block_event_end, block_end, block_logical_range
    -- ** skeleton
    , set_skeleton_config
    , has_explicit_skeleton
    , get_skeleton, set_skeleton, modify_skeleton
    , toggle_skeleton_edge, add_edges, remove_edges
    , splice_skeleton_above, splice_skeleton_below
    -- ** tracks
    , insert_track, remove_track, move_track
    -- *** tracks by tracknum
    , track_count
    , block_track_at, get_block_track_at, track_at
    , event_track_at, get_event_track_at
    , ruler_track_at, block_ruler
    -- *** tracks by TrackId
    , track_ids_of, tracknums_of, block_tracknums
    , tracknum_of, get_tracknum_of
    -- *** block track
    , set_track_width, set_track_suggested_width
    , track_flags, track_collapsed
    , toggle_track_flag, add_track_flag, remove_track_flag
    , modify_track_flags
    , set_track_ruler
    , merge_track, unmerge_track, set_merged_tracks
    , track_merged
    , set_ruler_ids, replace_ruler_id, set_ruler_id
    , get_tracklike

    -- * track
    , get_track, lookup_track, all_track_ids
    , create_track, destroy_track
    , get_track_title, set_track_title, modify_track_title
    , set_track_bg
    , modify_track_render, set_render_style, modify_waveform
    , blocks_with_track_id
    -- ** events
    , insert_events, insert_block_events, insert_event
    , get_events, modify_events, modify_events_range, modify_events_from
    , modify_some_events, calculate_damage
    , remove_event, remove_events, remove_events_range
    , track_event_end, range_from

    -- * ruler
    , get_ruler, lookup_ruler, all_ruler_ids
    , create_ruler, destroy_ruler, modify_ruler
    , ruler_of, rulers_of
    , blocks_with_ruler_id
    , no_ruler

    -- * util
    , find_tracks

    -- * verify
    , quick_verify, verify -- TODO should be done automatically by put
    , fix_state

    -- * ID
    , read_id, namespace
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified GHC.Stack

import qualified Util.CallStack as CallStack
import qualified Util.Lens as Lens
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect

import qualified App.Config as Config
import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Update as Update
import qualified Ui.Zoom as Zoom

import           Global
import           Types


-- * types

-- | Score state.  When you save a score, this is what is saved to disk.
data State = State {
    state_views :: Map ViewId Block.View
    , state_blocks :: Map BlockId Block.Block
    , state_tracks :: Map TrackId Track.Track
    , state_rulers :: Map RulerId Ruler.Ruler
    , state_config :: UiConfig.Config
    } deriving (Eq, Show)

views :: Lens.Lens State (Map ViewId Block.View)
views = Lens.lens state_views
    (\f r -> r { state_views = f (state_views r) })

blocks :: Lens.Lens State (Map BlockId Block.Block)
blocks = Lens.lens state_blocks
    (\f r -> r { state_blocks = f (state_blocks r) })

tracks :: Lens.Lens State (Map TrackId Track.Track)
tracks = Lens.lens state_tracks
    (\f r -> r { state_tracks = f (state_tracks r) })

rulers :: Lens.Lens State (Map RulerId Ruler.Ruler)
rulers = Lens.lens state_rulers
    (\f r -> r { state_rulers = f (state_rulers r) })

config :: Lens.Lens State UiConfig.Config
config = Lens.lens state_config
    (\f r -> r { state_config = f (state_config r) })

empty :: State
empty = State
    { state_views = Map.empty
    , state_blocks = Map.empty
    , state_tracks = Map.empty
    , state_rulers = Map.empty
    , state_config = UiConfig.empty_config
    }

-- | Like 'empty', but the state is initialized with the current creation time.
create :: IO State
create = do
    now <- Time.getCurrentTime
    return $ (config#UiConfig.meta#UiConfig.creation #= now) empty

-- | Clear out data that shouldn't be saved.
clear :: State -> State
clear state = state { state_views = Map.map clear_view (state_views state) }
    where
    clear_view view = view
        { Block.view_status = mempty
        , Block.view_selections =
            -- Non-insert selections indicate ephemeral state.
            maybe mempty (Map.singleton Config.insert_selnum) $
                Map.lookup Config.insert_selnum (Block.view_selections view)
        }

instance Pretty State where
    format (State views blocks tracks rulers config) = Pretty.record "State"
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

-- * address types

-- | Address a track in a block.  This is similar to a TrackId, except it
-- doesn't guarantee that the track is an event track.
data Track = Track !BlockId !TrackNum
    deriving (Eq, Show)

instance Pretty Track where
    pretty (Track block_id tracknum) =
        pretty block_id <> "/" <> showt tracknum

-- | A position on a track that can be indicated on the UI.  Its Pretty
-- instance emits a string, which if logged or copy-pasted into the REPL, will
-- cause that section of score to be highlighted.
data Range = Range !(Maybe BlockId) !TrackId !TrackTime !TrackTime
    deriving (Eq, Show)

instance Pretty Range where
    pretty (Range maybe_block_id track_id start end) =
        Stack.log_ui_frame (maybe_block_id, Just track_id, Just (start, end))

-- * other types

-- | Summary information on a Track.
data TrackInfo = TrackInfo {
    track_title :: !Text
    , track_id :: !TrackId
    , track_tracknum :: !TrackNum
    , track_block :: !Block.Track
    } deriving (Eq, Show)

instance Pretty TrackInfo where
    pretty (TrackInfo title track_id tracknum _) =
        "(" <> Text.unwords
            ["TrackInfo", showt title, showt track_id, showt tracknum]
        <> ")"

-- * StateT monad

-- | 'Update.UiDamage' is recorded directly instead of being calculated from
-- the state diff.
--
-- Is there any way it could get out of sync with the actual change?  I don't
-- see how, since the updates are stored by track_id, which should always be
-- associated with the same track, and an operation to move event positions
-- will simply generate another TrackUpdate over the whole track.
type StateStack m = State.StateT State
    (State.StateT Update.UiDamage
        (Except.ExceptT Error m))
newtype StateT m a = StateT (StateStack m a)
    deriving (Functor, Monad, MonadIO, Except.MonadError Error, Applicative)

-- | Just a convenient abbreviation.
type StateId a = StateT Identity.Identity a

instance Trans.MonadTrans StateT where
    lift = StateT . lift . lift . lift

-- | Monads implementing this class can call the UI state functions directly.
class (Applicative m, Monad m) => M m where
    -- Note that these aren't the MonadState get and put, and can't be, because
    -- when this monad is layered under another state monad (as it is with
    -- Cmd), MonadState couldn't tell which one you wanted.
    get :: m State
    -- | This directly modifies the state, and can break internal invariants.
    -- 'put' is slower but safer since it checks those invariants.
    unsafe_put :: State -> m ()
    damage :: Update.UiDamage -> m ()
    get_damage :: m Update.UiDamage
    throw_error :: Error -> m a

instance Monad m => M (StateT m) where
    get = StateT State.get
    unsafe_put st = StateT (State.put st)
    damage upd = (StateT . lift) (State.modify' (upd<>))
    get_damage = (StateT . lift) State.get
    throw_error = StateT . lift . lift . Except.throwError

-- Basic level membership in the MTL club.
instance M m => M (State.StateT state m) where
    get = lift get
    unsafe_put = lift . unsafe_put
    damage = lift . damage
    get_damage = lift get_damage
    throw_error = lift . throw_error

instance M m => M (Except.ExceptT exc m) where
    get = lift get
    unsafe_put = lift . unsafe_put
    damage = lift . damage
    get_damage = lift get_damage
    throw_error = lift . throw_error

throw :: (CallStack.Stack, M m) => Text -> m a
throw msg = throw_error $ Error GHC.Stack.callStack msg

gets :: M m => (State -> a) -> m a
gets f = fmap f get

-- | As with 'unsafe_put', this directly modifies the state.  'modify' is
-- the safe version.
unsafe_modify :: M m => (State -> State) -> m ()
unsafe_modify f = do
    state <- get
    unsafe_put $! f state

-- | TODO verify
--
-- This updates all tracks because I don't know what you modified in there.
put :: M m => State -> m ()
put state = unsafe_put state >> update_all

-- | An arbitrary modify.  It's unsafe because it doesn't check internal
-- invariants, and inefficient because it damages all tracks.  Use more
-- specific modify_* functions, if possible.
modify :: M m => (State -> State) -> m ()
modify f = do
    state <- get
    put $! f state

-- | Emit track updates for all tracks.  Use this when events have changed but
-- I don't know which ones, e.g. when loading a file or restoring a previous
-- state.
update_all :: M m => m ()
update_all = do
    st <- get
    damage $ mempty { Update._blocks = Map.keysSet (state_blocks st) }

-- | Run the given StateT with the given initial state, and return a new
-- state along with updates.  Normally updates are produced by 'Ui.Diff.diff',
-- but for efficiency updates to track data are accumulated when they are
-- actually made.  All the UI needs is a TrackTime range to redraw in, and
-- redrawing the whole track isn't that expensive.
--
-- See the StateStack comment for more.
run :: Monad m => State -> StateT m a
    -> m (Either Error (a, State, Update.UiDamage))
run state action = do
    res <- (Except.runExceptT . flip State.runStateT mempty
        . flip State.runStateT state . (\(StateT x) -> x)) action
    return $ case res of
        Left err -> Left err
        Right ((val, state), damage) ->
            Right (val, state, block_to_view_damage (state_views state) damage)

run_id :: State -> StateId a -> Either Error (a, State, Update.UiDamage)
run_id state m = Identity.runIdentity (run state m)

-- | A form of 'run' that returns only the val and automatically runs in
-- Identity.
eval :: State -> StateId a -> Either Error a
eval state m = case result of
        Left err -> Left err
        Right (val, _, _) -> Right val
    where result = Identity.runIdentity (run state m)

eval_rethrow :: M m => Text -> State -> StateId a -> m a
eval_rethrow msg state =
    require_right (((msg <> ": ") <>) . pretty) . eval state

exec :: State -> StateId a -> Either Error State
exec state m = case result of
        Left err -> Left err
        Right (_, state', _) -> Right state'
    where result = Identity.runIdentity (run state m)

exec_rethrow :: M m => Text -> State -> StateId a -> m State
exec_rethrow msg state =
    require_right (((msg <> ": ") <>) . pretty) . exec state

-- | Promote block damage to damage on all of that block's views.  This is run
-- before returning the UiDamage out of the Ui monad.  Otherwise, Diff isn't
-- smart enough to update views when the underlying blocks change.
block_to_view_damage :: Map ViewId Block.View -> Update.UiDamage
    -> Update.UiDamage
block_to_view_damage views damage
    | null view_ids = damage
    | otherwise = damage
        { Update._views = Update._views damage <> Set.fromList view_ids }
    where
    -- This is O(blocks*views) because I have no index BlockId -> [ViewId].
    -- But damaged blocks and views should both be small.
    view_ids = mconcatMap views_of (Set.toList (Update._blocks damage))
    views_of block_id = map fst $
        filter ((==block_id) . Block.view_block . snd) $ Map.toList views


-- ** error

-- | Abort is used by Cmd, so don't throw it from here.  This isn't exactly
-- modular, but ErrorT can't be composed and extensible exceptions are too
-- much bother at the moment.
data Error = Error !GHC.Stack.CallStack !Text | Abort deriving (Show)

instance Pretty Error where
    pretty (Error stack msg) =
        CallStack.showCaller (CallStack.caller stack) <> " " <> msg
    pretty Abort = "(abort)"

require :: (CallStack.Stack, M m) => Text -> Maybe a -> m a
require err = maybe (throw err) return

require_right :: (CallStack.Stack, M m) => (err -> Text) -> Either err a -> m a
require_right fmt_err = either (throw . fmt_err) return

-- * config

get_namespace :: M m => m Id.Namespace
get_namespace = get_config UiConfig.config_namespace

set_namespace :: M m => Id.Namespace -> m ()
set_namespace ns = modify_config $ \st -> st { UiConfig.config_namespace = ns }

get_default :: M m => (UiConfig.Default -> a) -> m a
get_default f = f <$> get_config UiConfig.config_default

modify_default :: M m => (UiConfig.Default -> UiConfig.Default) -> m ()
modify_default f = modify_config $ \st ->
    st { UiConfig.config_default = f (UiConfig.config_default st) }

get_root_id :: M m => m BlockId
get_root_id = require "no root root_id" =<< lookup_root_id

lookup_root_id :: M m => m (Maybe BlockId)
lookup_root_id = get_config UiConfig.config_root

set_root_id :: M m => BlockId -> m ()
set_root_id block_id =
    modify_config $ \st -> st { UiConfig.config_root = Just block_id }

-- | Unlike other State fields, you can modify Config freely without worrying
-- about breaking invariants.  TODO except allocations have invariants.
modify_config :: M m => (UiConfig.Config -> UiConfig.Config) -> m ()
modify_config f = unsafe_modify $ \st ->
    st { state_config = f (state_config st) }

modify_meta :: M m => (UiConfig.Meta -> UiConfig.Meta) -> m ()
modify_meta f = modify_config $ UiConfig.meta %= f

modify_allocation :: M m => ScoreT.Instrument
    -> (UiConfig.Allocation -> UiConfig.Allocation) -> m ()
modify_allocation inst modify = do
    allocs <- config#UiConfig.allocations <#> get
    allocs <- require_right (("modify " <> pretty inst <> ": ")<>) $
        UiConfig.modify_allocation inst (Right . modify) allocs
    modify_config $ UiConfig.allocations #= allocs

get_config :: M m => (UiConfig.Config -> a) -> m a
get_config f = gets (f . state_config)

-- | Run the action with a modified state, and restore it.
with_config :: M m => (UiConfig.Config -> UiConfig.Config) -> m a -> m a
with_config f action = do
    old <- get_config id
    modify_config f
    -- I think this is exception safe because the state is reverted after an
    -- exception, and there's no way to catch an exception.
    result <- action
    modify_config $ const old
    return result

-- | TODO use this for read only.  If used for write it bypasses
-- 'UiConfig.allocate'.
allocation :: ScoreT.Instrument -> Lens State (Maybe UiConfig.Allocation)
allocation inst = config # UiConfig.allocations_map # Lens.map inst

-- * view

get_view :: M m => ViewId -> m Block.View
get_view view_id = lookup_id view_id . state_views =<< get

lookup_view :: M m => ViewId -> m (Maybe Block.View)
lookup_view view_id = gets (Map.lookup view_id . state_views)

-- | All ViewIds, in sorted order.
all_view_ids :: M m => m [ViewId]
all_view_ids = gets (Map.keys . state_views)

-- | Create a new view.  Block.view_tracks can be left empty, since it will
-- be replaced by views generated from the the block.  If the caller uses the
-- 'Block.view' constructor, it won't have to worry about this.
--
-- Throw if the ViewId already exists.
create_view :: M m => Id.Id -> Block.View -> m ViewId
create_view id view = do
    view <- _update_view_status view
    insert (Id.ViewId id) view damage_view state_views $ \views st ->
        st { state_views = views }

destroy_view :: M m => ViewId -> m ()
destroy_view view_id = do
    unsafe_modify $ \st ->
        st { state_views = Map.delete view_id (state_views st) }
    damage_view view_id

put_views :: M m => Map ViewId Block.View -> m ()
put_views view_map = do
    let (view_ids, views) = unzip (Map.toList view_map)
    views <- mapM _update_view_status views
    unsafe_modify $ \st -> st
        { state_views = Map.fromList (zip view_ids views) }
    mapM_ damage_view view_ids

-- | Set a status variable on a view.
set_view_status :: M m => ViewId -> (Int, Text) -> Maybe Text -> m ()
set_view_status view_id key val = modify_view view_id $ \view -> view
    { Block.view_status = Map.alter (const val) key (Block.view_status view) }

_update_view_status :: M m => Block.View -> m Block.View
_update_view_status view = do
    block <- get_block (Block.view_block view)
    return $ case Block.block_integrated block of
        Just (source_block, _) -> view
            { Block.view_status = Map.insert Config.status_integrate_source
                (Id.ident_text source_block) (Block.view_status view)
            }
        Nothing -> view

-- ** zoom and track scroll

get_zoom :: M m => ViewId -> m Zoom.Zoom
get_zoom = fmap Block.view_zoom . get_view

modify_zoom :: M m => ViewId -> (Zoom.Zoom -> Zoom.Zoom) -> m ()
modify_zoom view_id modify = modify_view view_id $ \view ->
    view { Block.view_zoom = clamp $ modify $ Block.view_zoom view }
    where clamp zoom = zoom { Zoom.offset = max 0 (Zoom.offset zoom) }

set_track_scroll :: M m => ViewId -> Types.Width -> m ()
set_track_scroll view_id offset =
    modify_view view_id (\view -> view { Block.view_track_scroll = offset })

set_view_rect :: M m => ViewId -> Rect.Rect -> m ()
set_view_rect view_id rect =
    modify_view view_id (\view -> view { Block.view_rect = rect })

-- | Only 'Cmd.Cmd.ui_update' is supposed to call this, because the UI is
-- responsible for the padding.
set_view_padding :: M m => ViewId -> Block.Padding -> m ()
set_view_padding view_id padding = modify_view view_id $ \view ->
    view { Block.view_padding = padding }

-- ** selections

-- | Get @view_id@'s selection at @selnum@, or Nothing if there is none.
get_selection :: M m => ViewId -> Sel.Num -> m (Maybe Sel.Selection)
get_selection view_id selnum = do
    view <- get_view view_id
    return (Map.lookup selnum (Block.view_selections view))

-- | Replace any selection on @view_id@ at @selnum@ with @sel@.
set_selection :: M m => ViewId -> Sel.Num -> Maybe Sel.Selection -> m ()
set_selection view_id selnum maybe_sel = do
    view <- get_view view_id
    update_view view_id $ view
        { Block.view_selections =
            maybe (Map.delete selnum) (Map.insert selnum) maybe_sel
                (Block.view_selections view)
        }

-- ** util

modify_view :: M m => ViewId -> (Block.View -> Block.View) -> m ()
modify_view view_id f = do
    view <- get_view view_id
    update_view view_id (f view)

update_view :: M m => ViewId -> Block.View -> m ()
update_view view_id view = do
    unsafe_modify $ \st -> st
        { state_views = Map.adjust (const view) view_id (state_views st) }
    damage_view view_id

-- * block

get_block :: M m => BlockId -> m Block.Block
get_block block_id = get >>= lookup_id block_id . state_blocks

lookup_block :: M m => BlockId -> m (Maybe Block.Block)
lookup_block block_id = get >>= return . Map.lookup block_id . state_blocks

all_block_ids :: M m => m [BlockId]
all_block_ids = gets (Map.keys . state_blocks)

-- | Get all blocks along with their tracks.
all_block_track_ids :: M m => m [(BlockId, [TrackId])]
all_block_track_ids =
    map (second Block.block_track_ids) <$> gets (Map.toList . state_blocks)

-- | Make a new block.  If it's the first one, it will be set as the root.
-- This is the low level version, you probably want to use 'create_block'.
--
-- Throw if the BlockId already exists.
create_config_block :: M m => Id.Id -> Block.Block -> m BlockId
create_config_block id block = do
    bid <- insert (Id.BlockId id) block damage_block state_blocks $
        \blocks st -> st
            { state_blocks = blocks
            , state_config = let c = state_config st
                in c { UiConfig.config_root = if Map.size blocks == 1
                    then Just (Id.BlockId id) else UiConfig.config_root c }
            }
    update_skeleton bid
    return bid

-- | Make a new block with the default 'Block.Config'.
create_block :: M m => Id.Id -> Text -> [Block.Track] -> m BlockId
create_block block_id title tracks =
    create_config_block block_id (Block.block Block.default_config title tracks)

-- | Destroy the block and all the views that display it.  If the block was
-- the root, it will be be unset.  The block's tracks are left intact.
destroy_block :: M m => BlockId -> m ()
destroy_block block_id = do
    views <- views_of block_id
    mapM_ destroy_view (Map.keys views)
    unsafe_modify $ \st -> st
        { state_blocks = Map.delete block_id (state_blocks st)
        , state_config = let c = state_config st in c
            { UiConfig.config_root = if UiConfig.config_root c == Just block_id
                then Nothing else UiConfig.config_root c
            }
        }
    damage_block block_id
    blocks <- gets (Map.toList . state_blocks)
    -- Remove integration destinations of any blocks that were generated from
    -- this one.
    mapM_ (uncurry fix_integrated_block) blocks

block_of :: M m => ViewId -> m Block.Block
block_of view_id = get_block . Block.view_block =<< get_view view_id

block_id_of :: M m => ViewId -> m BlockId
block_id_of view_id = Block.view_block <$> get_view view_id

-- | Get all views of a given block.
views_of :: M m => BlockId -> m (Map ViewId Block.View)
views_of block_id = do
    views <- gets state_views
    return $ Map.filter ((==block_id) . Block.view_block) views

get_block_title :: M m => BlockId -> m Text
get_block_title = fmap Block.block_title . get_block

set_block_title :: M m => BlockId -> Text -> m ()
set_block_title block_id title =
    modify_block block_id (\block -> block { Block.block_title = title })

modify_block_meta :: M m => BlockId -> (Block.Meta -> Block.Meta) -> m ()
modify_block_meta block_id f = modify_block block_id $ \block ->
    block { Block.block_meta = f (Block.block_meta block) }

-- | Set or clear this block as an integrate destination.  The automatic
-- integration system will update it from the given source block.
set_integrated_block :: M m => BlockId
    -> Maybe (BlockId, Block.TrackDestinations) -> m ()
set_integrated_block block_id integrated = do
    modify_block block_id $ \block ->
        block { Block.block_integrated = integrated }
    block <- get_block block_id
    require_valid ("set_integrated_block " <> pretty block_id)
        (fix_integrated_block block_id block)

modify_integrated_tracks :: M m => BlockId
    -> ([(TrackId, Block.TrackDestinations)]
        -> [(TrackId, Block.TrackDestinations)])
    -> m ()
modify_integrated_tracks block_id modify = do
    pre <- dest_track_ids <$> get_block block_id
    modify_block block_id $ \block -> block
        { Block.block_integrated_tracks =
            modify (Block.block_integrated_tracks block)
        }
    block <- get_block block_id
    require_valid ("modify_integrated_tracks " <> pretty block_id) $
        fix_integrated_tracks block_id block
    -- fix_integrated_tracks will detect integrates where the track is gone.
    -- If the track remains but a previously existing integration was deleted,
    -- clear out the corresponding Event.stacks.
    let post = dest_track_ids block
    let gone = filter (`notElem` post) pre
    mapM_ clear_integration gone
    where
    -- TODO This also gets derive integration, is that correct?
    dest_track_ids = concatMap (Block.dest_track_ids . snd)
        . Block.block_integrated_tracks

-- | Set or clear the block's manual integration 'Block.NoteDestination's.
-- This just attaches (or removes) the integrate information to the block so
-- a future integration can use it to merge, and then call this function again.
set_integrated_manual :: M m => BlockId -> Block.SourceKey
    -> Maybe [Block.NoteDestination] -> m ()
set_integrated_manual block_id key dests =
    modify_block block_id $ \block -> block
        { Block.block_integrated_manual =
            maybe (Map.delete key) (Map.insert key) dests
                (Block.block_integrated_manual block)
        }
    -- TODO require_valid?

modify_block_config :: M m => BlockId -> (Block.Config -> Block.Config) -> m ()
modify_block_config block_id modify = modify_block block_id $
    \block -> block { Block.block_config = modify (Block.block_config block) }

set_edit_box :: M m => BlockId -> Block.Box -> Block.Box -> m ()
set_edit_box block_id skel track = modify_block_config block_id $
    \config -> config
        { Block.config_skel_box = skel
        , Block.config_track_box = track
        }

-- | The play box doesn't use a char, so I leave that out.
set_play_box :: M m => BlockId -> Color.Color -> m ()
set_play_box block_id color = modify_block_config block_id $ \config -> config
    { Block.config_sb_box = Block.Box color ' ' }

-- | Get the end of the block according to the ruler.  This means that if the
-- block has no rulers (e.g. a clipboard block) then block_ruler_end will be 0.
block_ruler_end :: M m => BlockId -> m TrackTime
block_ruler_end block_id = do
    block <- get_block block_id
    case Block.block_ruler_ids block of
        [] -> return 0
        ruler_id : _ -> Ruler.time_end <$> get_ruler ruler_id

-- | Get the end of the block according to the last event of the block.
block_event_end :: M m => BlockId -> m TrackTime
block_event_end block_id = do
    block <- get_block block_id
    track_ends <- mapM track_event_end (Block.block_track_ids block)
    return $ maximum (0 : track_ends)

-- | Get the maximum of ruler end and event end.  The end may still be 0 if the
-- block is totally empty.
block_end :: M m => BlockId -> m TrackTime
block_end block_id =
    max <$> block_ruler_end block_id <*> block_event_end block_id

-- | The logical range is defined by 'Ruler.bounds_of' and is intended to
-- correspond to the \"note\" that this block defines.
block_logical_range :: M m => BlockId -> m (TrackTime, TrackTime)
block_logical_range block_id = do
    block <- get_block block_id
    case Block.block_ruler_ids block of
        [] -> (,) 0 <$> block_event_end block_id
        ruler_id : _ -> do
            (start, end) <- Ruler.bounds_of <$> get_ruler ruler_id
            end <- maybe (block_event_end block_id) return end
            return (start, end)

-- ** skeleton

set_skeleton_config :: M m => BlockId -> Block.Skeleton -> m ()
set_skeleton_config block_id skel = do
    modify_block_config block_id $ \config -> config
        { Block.config_skeleton = skel }
    update_skeleton block_id
    damage_score

has_explicit_skeleton :: M m => BlockId -> m Bool
has_explicit_skeleton =
    fmap ((==Block.Explicit) . Block.config_skeleton . Block.block_config)
        . get_block

get_skeleton :: M m => BlockId -> m Skeleton.Skeleton
get_skeleton block_id = Block.block_skeleton <$> get_block block_id

set_skeleton :: M m => BlockId -> Skeleton.Skeleton -> m ()
set_skeleton block_id skel = modify_skeleton block_id (const skel)

modify_skeleton :: M m => BlockId
    -> (Skeleton.Skeleton -> Skeleton.Skeleton) -> m ()
modify_skeleton block_id modify = do
    block <- get_block block_id
    let config = Block.config_skeleton (Block.block_config block)
    unless (config == Block.Explicit) $
        throw $ "can't modify skeleton of " <> pretty block_id
            <> ", it's not explicit: " <> showt config
    _set_skeleton block_id block $ modify (Block.block_skeleton block)

_set_skeleton :: M m => BlockId -> Block.Block -> Skeleton.Skeleton -> m ()
_set_skeleton block_id block skel = do
    forM_ (Skeleton.flatten skel) $ \(parent, child) ->
        unless (Num.inRange 1 tracks parent && Num.inRange 1 tracks child) $
            throw $ "modify_skeleton: edge " <> showt (parent, child)
                <> " out of range for " <> showt block_id
    modify_block block_id $ \block -> block { Block.block_skeleton = skel }
    where tracks = length $ Block.block_tracks block

-- | Toggle the given edge in the block's skeleton.  If a cycle would be
-- created, refuse to add the edge and return False.  The edge is in (parent,
-- child) order.
toggle_skeleton_edge :: M m => Bool
    -- ^ If not true, the child's existing parents will be unlinked.
    -- While a track with multiple parents is possible, and is a way to
    -- express the same score derived under different conditions, in practice
    -- I never do that.
    -> BlockId -> Skeleton.Edge -> m Bool
toggle_skeleton_edge allow_multiple_parents block_id edge@(_, child) = do
    block <- get_block block_id
    whenJust (edges_in_range block edge) (throw . ("toggle: "<>))
    let skel = drop_parents (Block.block_skeleton block)
    case Skeleton.toggle_edge edge skel of
        Nothing -> return False
        Just new_skel -> do
            set_skeleton block_id new_skel
            return True
    where
    drop_parents skel
        | allow_multiple_parents || Skeleton.has_edge skel edge = skel
        | otherwise = Skeleton.remove_edges parents skel
        where parents = map (, child) (Skeleton.parents skel child)

-- | Add the edges to the skeleton.  Throw if they would produce a cycle.
add_edges :: M m => BlockId -> [Skeleton.Edge] -> m ()
add_edges block_id edges = do
    skel <- get_skeleton block_id
    block <- get_block block_id
    whenJust (msum (map (edges_in_range block) edges))
        (throw . ("add_edges: " <>))
    maybe (throw $ "add_edges " <> showt edges <> " to " <> showt skel
            <> " would have caused a cycle")
        (set_skeleton block_id) (Skeleton.add_edges edges skel)

remove_edges :: M m => BlockId -> [Skeleton.Edge] -> m ()
remove_edges block_id edges =
    modify_skeleton block_id (Skeleton.remove_edges edges)

-- | The first tracknum is spliced above the second.
splice_skeleton_above :: M m => BlockId -> TrackNum -> TrackNum -> m ()
splice_skeleton_above = _splice_skeleton True

-- | The first tracknum is spliced below the second.
splice_skeleton_below :: M m => BlockId -> TrackNum -> TrackNum -> m ()
splice_skeleton_below = _splice_skeleton False

-- | Splice the given tracknum into the skeleton, either above or below
-- the @to@ tracknum.  What this means exactly is documented in
-- 'Graphs.splice_above' and 'Graphs.slice_below'.
_splice_skeleton :: M m => Bool -> BlockId -> TrackNum -> TrackNum -> m ()
_splice_skeleton above block_id new to = do
    block <- get_block block_id
    whenJust (msum (map (edge_in_range block) [new, to]))
        (throw . ("splice: " <>))
    let splice = if above then Skeleton.splice_above else Skeleton.splice_below
    maybe (throw $ "splice_skeleton: " <> showt (new, to)
            <> " would have caused a cycle")
        (set_skeleton block_id) (splice new to (Block.block_skeleton block))

edge_in_range :: Block.Block -> TrackNum -> Maybe Text
edge_in_range block tracknum =
    case Lists.at (Block.block_tracks block) tracknum of
        Nothing -> Just $ "tracknum out of range: " <> showt tracknum
        Just t -> case Block.tracklike_id t of
            Block.TId {} -> Nothing
            _ -> Just $ "edge points to non-event track: " <> showt t

edges_in_range :: Block.Block -> Skeleton.Edge -> Maybe Text
edges_in_range block (from, to) =
    edge_in_range block from <|> edge_in_range block to

-- ** tracks

-- | Insert a track at the given TrackNum.  The TrackNum can be out of range to
-- insert a track at the beginning or append it to the end.
--
-- This will throw if it's an event track and the block already contains that
-- TrackId.  This invariant ensures that a (BlockId, TrackNum) is
-- interchangeable with a TrackId.
insert_track :: M m => BlockId -> TrackNum -> Block.Track -> m ()
insert_track block_id tracknum track = do
    block <- get_block block_id
    views <- views_of block_id
    whenJust (Block.track_id track) $ \track_id -> do
        track_ids <- track_ids_of block_id
        when (track_id `elem` track_ids) $
            throw $ "insert_track: block " <> showt block_id
                <> " already contains " <> showt track_id
    -- You can only put a ruler in tracknum 0.
    unless (tracknum > 0 || is_ruler track) $
        throw $ "non-ruler track can't go at tracknum " <> showt tracknum
            <> ": " <> pretty track
    let tracks = Lists.insertAt tracknum track (Block.block_tracks block)
        -- Make sure the views are up to date.
        views' = Map.map (insert_into_view block tracknum) views
    skel <- case Block.config_skeleton (Block.block_config block) of
        Block.Explicit -> return $
            Skeleton.insert tracknum (Block.block_skeleton block)
        Block.Implicit -> infer_skeleton tracks
    set_block block_id $ block
        { Block.block_tracks = tracks
        , Block.block_skeleton = skel
        }
    unsafe_modify $ \st ->
        st { state_views = Map.union views' (state_views st) }
    mapM_ damage_view (Map.keys views')
    where
    is_ruler t = case Block.tracklike_id t of
        Block.RId {} -> True
        _ -> False

-- | Remove the track at the given tracknum.
remove_track :: M m => BlockId -> TrackNum -> m ()
remove_track block_id tracknum = do
    block <- get_block block_id
    let tracks = Block.block_tracks block
    unless (1 <= tracknum && tracknum < length tracks) $
        throw $ "remove_track " <> showt block_id <> " " <> showt tracknum
            <> " out of range 1--" <> showt (length tracks)
    views <- Map.map (remove_from_view block tracknum) <$> views_of block_id
    set_block block_id $ block
        { Block.block_tracks = Lists.removeAt tracknum tracks
        , Block.block_skeleton =
            Skeleton.remove tracknum (Block.block_skeleton block)
        }
    unsafe_modify $ \st ->
        st { state_views = Map.union views (state_views st) }
    mapM_ damage_view (Map.keys views)
    -- Clear any orphaned integration destinations.
    fix_integrated_tracks block_id =<< get_block block_id
    return ()

-- | Move a track from one tracknum to another.
move_track :: M m => BlockId -> TrackNum -> TrackNum -> m ()
move_track block_id from to = do
    block <- get_block block_id
    let msg = "move_track: from " <> showt from <> " to " <> showt to
            <> " out of range"
    tracks <- require msg $ do
        -- Things get generally messed up if you try to move an event track to
        -- the ruler spot.
        guard (from /= 0 && to /= 0)
        Lists.move from to (Block.block_tracks block)
    skel <- case Block.config_skeleton (Block.block_config block) of
        Block.Explicit -> return $
            Skeleton.move from to (Block.block_skeleton block)
        Block.Implicit -> infer_skeleton tracks
    modify_block block_id $ const $ block
        { Block.block_tracks = tracks, Block.block_skeleton = skel }

infer_skeleton :: M m => [Block.Track] -> m Skeleton.Skeleton
infer_skeleton tracks = do
    titles <- traverse (traverse get_track_title) $
        Lists.mapMaybeSnd Block.track_id (zip [0..] tracks)
    return $ ParseSkeleton.default_parser $
        map (uncurry ParseSkeleton.Track) titles

update_skeleton :: M m => BlockId -> m ()
update_skeleton block_id = do
    block <- get_block block_id
    case Block.config_skeleton (Block.block_config block) of
        Block.Implicit -> _set_skeleton block_id block
            =<< infer_skeleton (Block.block_tracks block)
        _ -> return ()

-- *** tracks by TrackNum

-- | Number of tracks in the block.  This includes the ruler, so subtract 1 if
-- you want all non-ruler tracks.
track_count :: M m => BlockId -> m TrackNum
track_count block_id = do
    block <- get_block block_id
    return $ length (Block.block_tracks block)

-- | Get the Track at @tracknum@, or Nothing if its out of range.
block_track_at :: M m => BlockId -> TrackNum -> m (Maybe Block.Track)
block_track_at block_id tracknum
    | tracknum < 0 =
        throw $ "block_track_at: negative tracknum: " <> showt tracknum
    | otherwise = do
        block <- get_block block_id
        return $ Lists.at (Block.block_tracks block) tracknum

get_block_track_at :: M m => BlockId -> TrackNum -> m Block.Track
get_block_track_at block_id tracknum =
    tracknum_in_range block_id tracknum =<< block_track_at block_id tracknum
    where
    tracknum_in_range block_id tracknum Nothing = do
        count <- track_count block_id
        throw $ "track " <> pretty (Track block_id tracknum)
            <> " out of range 0--" <> showt count
    tracknum_in_range _ _ (Just a) = return a

track_at :: M m => BlockId -> TrackNum -> m (Maybe Block.TracklikeId)
track_at block_id tracknum = do
    maybe_track <- block_track_at block_id tracknum
    return $ fmap Block.tracklike_id maybe_track

-- | Like 'track_at', but only for event tracks.
event_track_at :: M m => BlockId -> TrackNum -> m (Maybe TrackId)
event_track_at block_id tracknum = do
    maybe_track <- track_at block_id tracknum
    return $ Block.track_id_of =<< maybe_track

-- | Like 'event_track_at' but throws if it's not there or not an event track.
get_event_track_at :: M m => BlockId -> TrackNum -> m TrackId
get_event_track_at block_id tracknum = do
    track <- get_block_track_at block_id tracknum
    require ("track " <> pretty (Track block_id tracknum)
            <> " not an event track") $
        Block.track_id track

-- | Get the RulerId of an event or ruler track, or Nothing if the tracknum is
-- out of range or doesn't have a ruler.
ruler_track_at :: M m => BlockId -> TrackNum -> m (Maybe RulerId)
ruler_track_at block_id tracknum = do
    maybe_track <- track_at block_id tracknum
    return $ Block.ruler_id_of =<< maybe_track

-- | 0 is the conventional ruler tracknum.
block_ruler :: M m => BlockId -> m RulerId
block_ruler block_id = fromMaybe no_ruler <$> ruler_track_at block_id 0

-- *** tracks by TrackId

-- | Get all TrackIds of the given block.
track_ids_of :: M m => BlockId -> m [TrackId]
track_ids_of block_id = Block.block_track_ids <$> get_block block_id

-- | Get all TrackIds of the given block, along with their tracknums.
tracknums_of :: M m => BlockId -> m [(TrackId, TrackNum)]
tracknums_of = fmap (Lists.mapMaybeFst Block.track_id) . block_tracknums

-- | Get tracks along with their TrackNums.
block_tracknums :: M m => BlockId -> m [(Block.Track, TrackNum)]
block_tracknums = fmap (flip zip [0..] . Block.block_tracks) . get_block

-- | There can only be one TrackId per block, which allows TrackNums and
-- TrackIds to be interchangeable.  This is enforced by 'insert_track'.
--
-- The inverse is 'event_track_at'.
tracknum_of :: M m => BlockId -> TrackId -> m (Maybe TrackNum)
tracknum_of block_id tid = lookup tid <$> tracknums_of block_id

get_tracknum_of :: M m => BlockId -> TrackId -> m TrackNum
get_tracknum_of block_id tid =
    require ("tracknum_of: track " <> showt tid <> " not in " <> showt block_id)
        =<< tracknum_of block_id tid

-- *** block track

set_track_width :: M m => BlockId -> TrackNum -> Types.Width -> m ()
set_track_width block_id tracknum width =
    modify_block_track block_id tracknum $ \btrack ->
        btrack { Block.track_width = width }

set_track_suggested_width :: M m => BlockId -> TrackNum -> Types.Width -> m ()
set_track_suggested_width block_id tracknum width =
    modify_block_track block_id tracknum $ \btrack ->
        btrack { Block.track_suggested_width = width }

track_flags :: M m => BlockId -> TrackNum -> m (Set Block.TrackFlag)
track_flags block_id tracknum =
    Block.track_flags <$> get_block_track_at block_id tracknum

track_collapsed :: M m => BlockId -> TrackNum -> m Bool
track_collapsed block_id tracknum =
    Block.is_collapsed . Block.track_flags <$>
        get_block_track_at block_id tracknum

toggle_track_flag :: M m => BlockId -> TrackNum -> Block.TrackFlag -> m ()
toggle_track_flag block_id tracknum flag =
    modify_track_flags block_id tracknum toggle
    where
    toggle flags
        | flag `Set.member` flags = Set.delete flag flags
        | otherwise = Set.insert flag flags

add_track_flag, remove_track_flag
    :: M m => BlockId -> TrackNum -> Block.TrackFlag -> m ()
add_track_flag block_id tracknum flag =
    modify_track_flags block_id tracknum $ Set.insert flag
remove_track_flag block_id tracknum flag =
    modify_track_flags block_id tracknum $ case flag of
        -- Block.Merge -> Set.delete flag . Set.delete Block.Collapse
        _ -> Set.delete flag

modify_track_flags :: M m => BlockId -> TrackNum
    -> (Set Block.TrackFlag -> Set Block.TrackFlag) -> m ()
modify_track_flags block_id tracknum modify =
    modify_block_track block_id tracknum $ \btrack ->
        -- Don't allow both Merge and Collapse to be set, prefer Merge.
        -- This is so I can Collapse a bunch of tracks, and the Merged ones
        -- will stay the way they are.  Merge also has priority in
        -- Block.display_track, but having both set leads to confusing UI.
        let flags = modify (Block.track_flags btrack)
        in btrack
            { Block.track_flags = if Set.member Block.Merge flags
                then Set.delete Block.Collapse flags
                else flags
            }

set_track_ruler :: M m => BlockId -> TrackNum -> RulerId -> m ()
set_track_ruler block_id tracknum ruler_id = do
    _ <- get_ruler ruler_id -- Throw if it doesn't exist.
    modify_block_track block_id tracknum $
        Block.modify_id (Block.set_ruler_id ruler_id)

-- | Merge the @from@ tracknum into the @to@ tracknum and collapse @from@.
merge_track :: M m => BlockId -> TrackNum -> TrackNum -> m ()
merge_track block_id to from = do
    from_id <- get_event_track_at block_id from
    modify_block_track block_id to $ \btrack -> btrack
        { Block.track_merged = Set.insert from_id (Block.track_merged btrack) }
    add_track_flag block_id from Block.Merge

-- | Reverse 'merge_track': remove the merged tracks and expand their
-- occurrances in the given block.  \"Unmerge\" is not a graceful term, but at
-- least it's obviously the opposite of \"merge\".
unmerge_track :: M m => BlockId -> TrackNum -> m ()
unmerge_track block_id tracknum = do
    track_ids <- Block.track_merged <$> get_block_track_at block_id tracknum
    unmerged_tracknums <- mapMaybeM (tracknum_of block_id)
        (Set.toList track_ids)
    forM_ unmerged_tracknums $ \tracknum ->
        remove_track_flag block_id tracknum Block.Merge
    set_merged_tracks block_id tracknum mempty

set_merged_tracks :: M m => BlockId -> TrackNum -> Set TrackId -> m ()
set_merged_tracks block_id tracknum merged =
    modify_block_track block_id tracknum $ \btrack ->
        btrack { Block.track_merged = merged }

track_merged :: M m => BlockId -> TrackNum -> m Bool
track_merged block_id tracknum = not . Set.null . Block.track_merged <$>
    get_block_track_at block_id tracknum

-- | Set rulers, one per track.
set_ruler_ids :: M m => BlockId -> [Maybe RulerId] -> m ()
set_ruler_ids block_id ruler_ids = modify_block block_id $ \block -> block
    { Block.block_tracks =
        zipWith set (Block.block_tracks block) (ruler_ids ++ repeat Nothing)
    }
    where
    set track (Just ruler_id) = track
        { Block.tracklike_id =
            Block.set_ruler_id ruler_id (Block.tracklike_id track)
        }
    set track Nothing = track

set_ruler_id :: M m => BlockId -> RulerId -> m ()
set_ruler_id block_id ruler_id = modify_ruler_id block_id (const ruler_id)

-- | Replace one RulerId with another on the given block.
--
-- It's more convenient to do here than removing and inserting tracks, and easy
-- since there's no "one per block" invariant to maintain with ruler ids.
replace_ruler_id :: M m => BlockId -> RulerId -> RulerId -> m ()
replace_ruler_id block_id from to =
    modify_ruler_id block_id $ \rid -> if rid == from then to else from

modify_ruler_id :: M m => BlockId -> (RulerId -> RulerId) -> m ()
modify_ruler_id block_id modify = modify_block block_id $ \block ->
    block { Block.block_tracks = map replace_track (Block.block_tracks block) }
    where
    replace_track track = track
        { Block.tracklike_id = replace (Block.tracklike_id track) }
    replace tlike_id = case Block.ruler_id_of tlike_id of
        Just rid -> Block.set_ruler_id (modify rid) tlike_id
        Nothing -> tlike_id

-- | Resolve a TracklikeId to a Tracklike.
get_tracklike :: M m => Block.TracklikeId -> m Block.Tracklike
get_tracklike track = case track of
    Block.TId tid rid -> Block.T <$> get_track tid <*> get_ruler rid
    Block.RId rid -> Block.R <$> get_ruler rid
    Block.DId divider -> return (Block.D divider)

modify_block_track :: M m => BlockId -> TrackNum
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
        Map.mapMaybeWithKey (remove_from_selection block tracknum)
            (Block.view_selections view)
    }

-- | If tracknum is before or at the selection, push it to the right.  If it's
-- inside, extend it.  If it's to the right, do nothing.
insert_into_selection :: Block.Block -> TrackNum -> Sel.Selection
    -> Sel.Selection
insert_into_selection block tracknum sel
    | tracknum <= low = shift_selection True block 1 sel
    | tracknum <= high = Sel.expand_tracks 1 sel
    | otherwise = sel
    where (low, high) = Sel.track_range sel

-- | Remove the given track from the selection.  The selection will be moved or
-- shrunk as per 'insert_into_selection', possibly to nothing if the selection
-- was only on the deleted track.  Config.insert_selnum is an exception, it
-- moves one track to the left, if possible.  That's because it's convenient to
-- delete consecutive tracks.
remove_from_selection :: Block.Block -> TrackNum
    -> Sel.Num -> Sel.Selection -> Maybe Sel.Selection
remove_from_selection block tracknum selnum sel
    | tracknum < low = Just $ shift_selection True block (-1) sel
    | tracknum == high && high == low =
        if selnum == Config.insert_selnum
        then Just $ shift_selection True block (-1) sel
        else Nothing
    | tracknum <= high = Just $ Sel.expand_tracks (-1) sel
    | otherwise = Just sel
    where (low, high) = Sel.track_range sel

-- | Shift the selection, clipping if it's out of range.  While the
-- sel_cur_track won't be on a non-selectable track after this, the selection
-- may still include one.
shift_selection :: Bool -- ^ skip unselectable tracks
    -> Block.Block -> TrackNum -> Sel.Selection -> Sel.Selection
shift_selection skip_unselectable block shift sel =
    Sel.modify_tracks (+shift2) sel
    where
    shift2
        | skip_unselectable =
            skip_unselectable_tracks block (Sel.cur_track sel) shift
                - Sel.cur_track sel
        | otherwise = shift

-- | Shift a tracknum to another track, skipping unselectable tracks.
skip_unselectable_tracks :: Block.Block -> TrackNum -> Int -> TrackNum
skip_unselectable_tracks block tracknum shift
    | shift == 0 = tracknum
    | shift > 0 = find_track (dropWhile (<tracknum) selectable)
    | otherwise = find_track (dropWhile (>tracknum) (List.reverse selectable))
    where
    selectable = selectable_tracks block
    find_track [] = tracknum
    find_track tracks@(first:_) =
        fromMaybe tracknum $ Lists.head $ drop abs_shift tracks
        where
        abs_shift = if tracknum /= first then abs shift - 1 else abs shift

-- | Get the tracknums from a block that should be selectable.
selectable_tracks :: Block.Block -> [TrackNum]
selectable_tracks block =
    [ tracknum | (tracknum, track) <- zip [0..] (Block.block_tracks block)
    , Block.track_selectable track
    ]

-- ** util

modify_block :: M m => BlockId -> (Block.Block -> Block.Block) -> m ()
modify_block block_id f = do
    block <- get_block block_id
    set_block block_id (f block)

set_block :: M m => BlockId -> Block.Block -> m ()
set_block block_id block = do
    unsafe_modify $ \st -> st
        { state_blocks = Map.adjust (const block) block_id (state_blocks st) }
    damage_block block_id

-- * track

get_track :: M m => TrackId -> m Track.Track
get_track track_id = get >>= lookup_id track_id . state_tracks

lookup_track :: M m => TrackId -> m (Maybe Track.Track)
lookup_track track_id = gets (Map.lookup track_id . state_tracks)

all_track_ids :: M m => m [TrackId]
all_track_ids = gets (Map.keys . state_tracks)

-- | Insert the given track with the given ID.
--
-- Throw if the TrackId already exists.
create_track :: M m => Id.Id -> Track.Track -> m TrackId
create_track id track =
    insert (Id.TrackId id) track damage_track state_tracks $ \tracks st ->
        st { state_tracks = tracks }

-- | Destroy the track and remove it from all the blocks it's in.  No-op if
-- the TrackId doesn't exist.
destroy_track :: M m => TrackId -> m ()
destroy_track track_id = do
    blocks <- blocks_with_track_id track_id
    forM_ blocks $ \(block_id, tracks) -> forM_ tracks $ \(tracknum, _) ->
        remove_track block_id tracknum
    unsafe_modify $ \st ->
        st { state_tracks = Map.delete track_id (state_tracks st) }
    damage_track track_id

get_track_title :: M m => TrackId -> m Text
get_track_title = (Track.track_title <$>) . get_track

set_track_title :: M m => TrackId -> Text -> m ()
set_track_title track_id text = modify_track_title track_id (const text)

modify_track_title :: M m => TrackId -> (Text -> Text) -> m ()
modify_track_title track_id modify = do
    modify_track track_id $ \track ->
        track { Track.track_title = modify (Track.track_title track) }
    block_ids <- map fst <$> blocks_with_track_id track_id
    mapM_ update_skeleton block_ids

set_track_bg :: M m => TrackId -> Color.Color -> m ()
set_track_bg track_id color = modify_track track_id $ \track ->
    track { Track.track_bg = color }

set_render_style :: M m => Track.RenderStyle -> TrackId -> m ()
set_render_style style track_id = modify_track_render track_id $
    \render -> render { Track.render_style = style }

modify_track_render :: M m => TrackId
    -> (Track.RenderConfig -> Track.RenderConfig) -> m ()
modify_track_render track_id modify = modify_track track_id $ \track ->
    track { Track.track_render = modify (Track.track_render track) }

modify_waveform :: M m => TrackId -> (Bool -> Bool) -> m ()
modify_waveform track_id modify = modify_track track_id $ \track ->
    track { Track.track_waveform = modify (Track.track_waveform track) }

-- | Find @track_id@ in all the blocks it exists in, and return the track info
-- for each tracknum at which @track_id@ lives.  Blocks with no matching tracks
-- won't be returned, so the return track lists will always be non-null.
blocks_with_track_id :: M m =>
    TrackId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
blocks_with_track_id track_id =
    find_tracks ((== Just track_id) . Block.track_id_of) <$> gets state_blocks

-- ** events

{- There are two interpretations of a range: the strict one is that when
    start==end nothing can be selected.  A more relaxed one is that start==end
    will still select an event at start.  The relaxed one is often convenient
    for commands, so there are typically three variants of each ranged command:
    select events in the strict half-open range (functions end with _range),
    select an event at a certain point (functions use the singular), and select
    events in the relaxed half-open range (functions use the plural).
-}

-- | Insert events into track_id as per 'Events.insert'.
insert_events :: M m => TrackId -> [Event.Event] -> m ()
insert_events track_id events_ = _modify_events track_id $ \old_events ->
    (Events.insert events old_events, events_range events)
    where
    events = map clip_negative $ dropWhile ((<0) . Event.start) events_
    clip_negative event
        | Event.end event < 0 = Event.set_end 0 event
        | otherwise = event

{- | Like 'insert_events', but clip the events to the end of a block.

    This is necessarily block specific, because block duration is defined by its
    ruler.  Still, you should use this in preference to 'insert_events'.

    This uses 'block_end', which means that if events don't already go past the
    end of the ruler, they won't after this is called.  If they are already
    past (e.g. there is no ruler), then they will only be clipped if they move
    to later in time.  This might be confusing, but it seems generally
    convenient to not have to constantly manually trim events when they get
    moved past the end of the ruler, but definitely inconvenient for events to
    just disappear when there is no ruler.
-}
insert_block_events :: M m => BlockId -> TrackId -> [Event.Event] -> m ()
insert_block_events block_id track_id events = do
    end <- block_end block_id
    -- allow_zero=True because zero-dur events at the end of a block are used
    -- for negative/final notes.
    insert_events track_id (Events.clip_list True end events)

insert_event :: M m => TrackId -> Event.Event -> m ()
insert_event track_id event = insert_events track_id [event]

get_events :: M m => TrackId -> m Events.Events
get_events track_id = Track.track_events <$> get_track track_id

-- | Modify the events on a track, and assume the entire track has been
-- damaged.
modify_events :: M m => TrackId -> (Events.Events -> Events.Events) -> m ()
modify_events track_id f = _modify_events track_id $ \events ->
    (f events, Ranges.everything)

modify_events_range :: M m => TrackId -> Events.Range
    -> (Events.Events -> Events.Events) -> m ()
modify_events_range track_id range modify = _modify_events track_id $ \events ->
    (process events, uncurry Ranges.range (Events.range_times range))
    where
    -- A range to the end should be inclusive, because I frequently have a
    -- positive event at the end.
    process events = (pre <> modify within <> post)
        where (pre, within, post) = Events.split_range range events

modify_events_from :: M m => TrackId -> TrackTime
    -> (Events.Events -> Events.Events) -> m ()
modify_events_from track_id start modify = do
    range <- range_from track_id start
    modify_events_range track_id range modify

-- | Just like 'modify_events', except that it expects you only modified a few
-- events, and will only emit damage for the changed parts.
modify_some_events :: M m => TrackId -> (Events.Events -> Events.Events)
    -> m ()
modify_some_events track_id f = _modify_events track_id $ \events ->
    let new_events = f events
    in (new_events, calculate_damage events new_events)

calculate_damage :: Events.Events -> Events.Events -> Ranges.Ranges TrackTime
calculate_damage old new =
    Ranges.sorted_ranges $ foldr f [] $
        Lists.pairSortedOn1 Event.start
            (Events.ascending old) (Events.ascending new)
    where
    f (Lists.Second new) ranges = Event.range new : ranges
    f (Lists.First old) ranges = Event.range old : ranges
    f (Lists.Both old new) ranges
        | old == new = ranges
        | otherwise =
            (Event.start old, max (Event.end old) (Event.end new)) : ranges

-- | Remove a single event by start and orientation.
-- TODO I think 'remove_events_range' is now just as expressive and can be just
-- as efficient
remove_event :: M m => TrackId -> Event.Event -> m ()
remove_event track_id event = _modify_events track_id $ \events ->
    case Events.at t (Event.orientation event) events of
        Nothing -> (events, Ranges.nothing)
        Just event ->
            ( Events.remove (Events.Point t (Event.orientation event)) events
            , events_range [event]
            )
    where t = Event.start event

-- | Just like @mapM_ (remove_event track_id)@ but more efficient.
-- TODO at least I hope, it got sort of complicated.
remove_events :: M m => TrackId -> [Event.Event] -> m ()
remove_events _ [] = return ()
remove_events track_id [event] = remove_event track_id event
remove_events track_id events = do
    remove_events_range track_id $
        Events.Range (Event.min first) (Event.max last)
    when (Event.is_negative first) $
        remove_event track_id first
    when (Event.is_positive last) $
        remove_event track_id last
    where
    -- Events is non-empty due to the pattern match above.
    Just first = Lists.minimumOn Event.start events
    Just last = Lists.maximumOn Event.start events

remove_events_range :: M m => TrackId -> Events.Range -> m ()
remove_events_range track_id range =
    modify_events_range track_id range (const mempty)

-- | Get the end of the last event of the block.
track_event_end :: M m => TrackId -> m TrackTime
track_event_end = fmap Events.time_end . get_events

range_from :: M m => TrackId -> TrackTime -> m Events.Range
range_from track_id start =
    Events.Range start . (+1) <$> track_event_end track_id
    -- +1 to get a final 0 dur positive event.

-- ** util

-- | Don't use this to modify the events, because it won't create damage.
-- TODO should I try to protect against that?
-- TODO except now there's only one kind of track damage, so it doesn't matter
modify_track :: M m => TrackId -> (Track.Track -> Track.Track) -> m ()
modify_track track_id f = do
    get_track track_id -- Throw if track_id doesn't exist.
    unsafe_modify $ \st ->
        st { state_tracks = Map.adjust f track_id (state_tracks st) }
    damage_track track_id

-- | Low level modify events.  The function can modify Events however it
-- pleases, but should return Ranges indicating where the modification
-- happened.  This must be done manually, and there is no enforcement, but if
-- you get it wrong, 'Update.UiDamage' will be wrong, which will mess up UI
-- updates, undo, rederivation, etc.
--
-- TODO could figure this out automatically if Events supported efficient diff.
_modify_events :: M m => TrackId
    -> (Events.Events -> (Events.Events, Ranges.Ranges TrackTime))
    -> m ()
_modify_events track_id f = do
    track <- get_track track_id
    let (new_events, ranges) = f (Track.track_events track)
        new_track = track { Track.track_events = new_events }
    unsafe_modify $ \st ->
        st { state_tracks = Map.insert track_id new_track (state_tracks st) }
    -- Force out whatever transformations might be in the new events.  The
    -- main reason is to force out any IO exceptions that might be hiding in
    -- REPL expressions, but it seems better for memory in general to keep
    -- State in normal form.
    DeepSeq.deepseq new_events $ damage $ Update.track_damage track_id ranges

events_range :: [Event.Event] -> Ranges.Ranges TrackTime
events_range events = case minmax events of
    Just (emin, emax) -> Ranges.range emin emax
    Nothing -> Ranges.nothing
    where
    minmax (e:es) = Just $ go (Event.min e) (Event.max e) es
    minmax [] = Nothing
    go !emin !emax (e:es) =
        go (min emin (Event.min e)) (max emax (Event.max e)) es
    go emin emax [] = (emin, emax)

-- * ruler

get_ruler :: M m => RulerId -> m Ruler.Ruler
get_ruler ruler_id
    | ruler_id == no_ruler = return Ruler.empty
    | otherwise = get >>= lookup_id ruler_id . state_rulers

lookup_ruler :: M m => RulerId -> m (Maybe Ruler.Ruler)
lookup_ruler ruler_id = get >>= return . Map.lookup ruler_id . state_rulers

all_ruler_ids :: M m => m [RulerId]
all_ruler_ids = gets (Map.keys . state_rulers)

-- | Insert the given ruler with the given ID.
--
-- Throw if the RulerId already exists.
create_ruler :: M m => Id.Id -> Ruler.Ruler -> m RulerId
create_ruler id ruler
    -- no_ruler is global and assumed to always exist.
    | id == Id.unpack_id no_ruler = throw "can't insert no-ruler"
    | otherwise = insert (Id.RulerId id) ruler damage_ruler state_rulers $
        \rulers st -> st { state_rulers = rulers }

-- | Destroy the ruler and remove it from all the blocks it's in.
destroy_ruler :: M m => RulerId -> m ()
destroy_ruler ruler_id = do
    blocks <- blocks_with_ruler_id ruler_id
    forM_ blocks $ \(block_id, tracks) -> do
        let tracknums = map fst tracks
            setr i = if i `elem` tracknums
                then Block.set_ruler_id no_ruler else id
            deruler (i, track) = Block.modify_id (setr i) track
        modify_block block_id $ \block -> block { Block.block_tracks =
            map deruler (Lists.enumerate (Block.block_tracks block)) }
    unsafe_modify $ \st ->
        st { state_rulers = Map.delete ruler_id (state_rulers st) }
    damage_ruler ruler_id

modify_ruler :: M m => RulerId -> (Ruler.Ruler -> Either Text Ruler.Ruler)
    -> m ()
modify_ruler ruler_id modify = do
    when (ruler_id == no_ruler) $
        throw "can't modify no_ruler"
    ruler <- get_ruler ruler_id
    let msg = "modify_ruler " <> pretty ruler_id <> ": "
    modified <- require_right (msg<>) $ modify ruler
    unsafe_modify $ \st ->
        st { state_rulers = Map.insert ruler_id modified (state_rulers st) }
    damage_ruler ruler_id

ruler_of :: M m => BlockId -> m RulerId
ruler_of block_id = require ("no ruler in " <> showt block_id)
    =<< Lists.head . Block.block_ruler_ids <$> get_block block_id

rulers_of :: M m => BlockId -> m [RulerId]
rulers_of block_id = Lists.unique . Block.block_ruler_ids <$> get_block block_id

-- | Just like 'blocks_with_track_id' except for ruler_id.
blocks_with_ruler_id :: M m =>
    RulerId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
blocks_with_ruler_id ruler_id =
    find_tracks ((== Just ruler_id) . Block.ruler_id_of) <$> gets state_blocks

-- | Since all TracklikeIds must have a ruler, all States have a special empty
-- ruler that can be used in a \"no ruler\" situation.
--
-- This RulerId is implicitly present in every block.  It's not actually in
-- 'state_rulers' to avoid it getting renamed or deleted, but 'get_ruler' will
-- pretend it exists.  As long as everyone that cares about no_ruler (which is
-- only 'verify' and 'get_tracklike' for "Ui.Sync") uses 'get_ruler' then
-- they won't be confused by tracks that have no_ruler.
no_ruler :: RulerId
no_ruler = Id.RulerId (Id.global "-no-ruler-")


-- * util

find_tracks :: (Block.TracklikeId -> Bool) -> Map BlockId Block.Block
    -> [(BlockId, [(TrackNum, Block.TracklikeId)])]
find_tracks f blocks = do
    (bid, b) <- Map.assocs blocks
    let tracks = get_tracks b
    guard (not (null tracks))
    return (bid, tracks)
    where
    all_tracks block = Lists.enumerate (Block.block_tracks block)
    get_tracks block =
        [ (tracknum, Block.tracklike_id track)
        | (tracknum, track) <- all_tracks block, f (Block.tracklike_id track)
        ]

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (CallStack.Stack, Ord k, Show k, M m) => k -> Map k a -> m a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "State.lookup: unknown " <> showt key
    Just val -> return val

-- | Insert @val@ at @key@ in @get_map state@, throwing if it already exists.
-- Put the map back into @state@ by applying @set_map new_map state@ to it.
insert :: (M m, Ord k, Show k)
    => k -> a
    -> (k -> m ())
    -> (State -> Map k a)
    -> (Map k a -> State -> State)
    -> m k
insert key val updated get_map set_map = do
    state <- get
    when (key `Map.member` get_map state) $
        throw $ showt key <> " already exists"
    unsafe_put (set_map (Map.insert key val (get_map state)) state)
    updated key
    return key

-- | Modify the @i@th element of @xs@ by applying @f@ to it.
modify_at :: M m => Text -> [a] -> Int -> (a -> a) -> m [a]
modify_at msg xs i f = case post of
    [] -> throw $ msg <> ": can't replace index " <> showt i
        <> " of list with length " <> showt (length xs)
    (elt:rest) -> return (pre ++ f elt : rest)
    where (pre, post) = splitAt i xs

{-
-- | Modify the @i@th element of @xs@ by applying @f@ to it.
--
-- As an optimization, this returns Nothing if the modification had no effect.
-- This assumes that comparing before and after is cheaper than going through
-- with the modification.  I don't know if it is.  Probably it doesn't matter.
modify_at :: (M m, Eq a) => Text -> [a] -> Int -> (a -> a) -> m (Maybe [a])
modify_at msg xs i f = case post of
    [] -> throw $ msg <> ": can't replace index " <> showt i
        <> " of list with length " <> showt (length xs)
    elt : rest
        | elt == after -> return Nothing
        | otherwise -> return $ Just $ pre ++ after : rest
        where after = f elt
    where (pre, post) = splitAt i xs
-}

-- * verify

-- | Run a @fix_*@ function, and throw an error if it found problems.
require_valid :: M m => Text -> StateId [Text] -> m ()
require_valid caller verify = do
    state <- get
    case run_id state verify of
        Left err -> throw $ caller <> ": error validating: " <> showt err
        Right (errs, state, _)
            | null errs -> return ()
            | otherwise -> do
                -- The exception should cause the state to be rolled back, but
                -- I might as well not let a known broken state stick around.
                put state
                throw $ caller <> ": aborted due to validation actions: "
                    <> Text.intercalate "; " errs

-- | Unfortunately there are some invariants to protect within State.
-- They can all be fixed by dropping things, so this will fix them and return
-- a list of warnings.
verify :: State -> (State, [Text])
verify state = case run_id state fix_state of
    Left err -> (state, ["exception: " <> pretty err])
    Right (errs, state, _) -> (state, errs)

-- | This is like 'verify', but less complete.  It returns Left if it wants
-- you to reject the new state entirely.
--
-- 'verify' is better, but more expensive, so I'm reluctant to run it on every
-- single cmd.  If I run 'verify' before unsafe puts and trust this module to
-- maintain invariants then I don't need to, but I don't fully trust this
-- module.
--
-- TODO a better approach would be to make sure Sync can't be broken by State.
quick_verify :: Update.UiDamage -> State -> Either String (State, [Text])
quick_verify damage state = case run_id state quick_fix of
    Left err -> Left $ prettys err
    Right (errs, state, _) -> Right (state, errs)
    where
    quick_fix = do
        st <- get
        mapM_ verify_block $ Map.elems $
            Map.restrictKeys (state_blocks st) (Update._blocks damage)
        -- Disappearing views can happen if you undo past a block rename.
        -- In that case I should track the rename rather than disappearing
        -- the view, but in any case I don't want dangling ViewIds and
        -- disappearing the view is relatively harmless.
        concatMapM (uncurry verify_view) $ Map.toList $
            Map.restrictKeys (state_views st) (Update._views damage)
    verify_block block = do
        mapM_ get_track (Block.block_track_ids block)
        mapM_ get_ruler (Block.block_ruler_ids block)

fix_state :: M m => m [Text]
fix_state = do
    views <- gets (Map.toList . state_views)
    view_errs <- concatMapM (uncurry verify_view) views
    blocks <- gets (Map.toList . state_blocks)
    block_errs <- concatMapM (uncurry fix_block) blocks
    return $ view_errs ++ block_errs

-- | Drop views with invalid BlockIds.
verify_view :: M m => ViewId -> Block.View -> m [Text]
verify_view view_id view = do
    block <- lookup_block (Block.view_block view)
    case block of
        Just _ -> return []
        Nothing -> do
            destroy_view view_id
            return [showt view_id <> ": dropped because of invalid "
                <> showt (Block.view_block view)]

fix_block :: M m => BlockId -> Block.Block -> m [Text]
fix_block block_id block =
    map ((showt block_id <> ": ") <>) . mconcat <$> sequence
        [ fix_track_ids block_id block
        , unique_track_ids block_id block
        , fix_ruler_ids block_id block
        , fix_skeleton block_id block
        , concatMapM (fix_merged block_id) tracks
        , fix_integrated_block block_id block
        , fix_integrated_tracks block_id block
        ]
    where tracks = zip [0..] (Block.block_tracks block)

-- | Drop invalid track ids.
fix_track_ids :: M m => BlockId -> Block.Block -> m [Text]
fix_track_ids block_id block = do
    all_track_ids <- gets state_tracks
    let is_valid = (`Map.member` all_track_ids)
    let invalid = filter (not . is_valid . snd) (block_event_tracknums block)
    mapM_ (remove_track block_id . fst) invalid
    return ["tracknum " <> showt tracknum <> ": dropped invalid "
        <> showt track_id | (tracknum, track_id) <- invalid]

-- | Replace invalid ruler ids with no_ruler.
fix_ruler_ids :: M m => BlockId -> Block.Block -> m [Text]
fix_ruler_ids _block_id _block = return [] -- TODO

-- | Each TrackId of a block is unique.
unique_track_ids :: M m => BlockId -> Block.Block -> m [Text]
unique_track_ids block_id block = do
    let invalid = concatMap snd $ snd $
            Lists.partitionDups snd (block_event_tracknums block)
    mapM_ (remove_track block_id . fst) invalid
    return ["tracknum " <> showt tracknum <> ": dropped duplicate "
        <> showt track_id | (tracknum, track_id) <- invalid]

-- | Skeleton tracknums in range.
fix_skeleton :: M m => BlockId -> Block.Block -> m [Text]
fix_skeleton _block_id _block = return [] -- TODO

-- | Strip invalid Block.track_merged.
fix_merged :: M m => BlockId -> (TrackNum, Block.Track) -> m [Text]
fix_merged block_id (tracknum, track) = do
    all_track_ids <- gets state_tracks
    let is_valid = (`Map.member` all_track_ids)
    let (valid, invalid) = Set.partition is_valid (Block.track_merged track)
    unless (Set.null invalid) $
        modify_block_track block_id tracknum
            (const $ track { Block.track_merged = valid })
    return ["tracknum " <> showt tracknum <> ": stripped invalid merged "
        <> showt track_id | track_id <- Set.toList invalid]

-- | Drop block_integrated if the source BlockId doesn't exist, and strip out
-- TrackDestinations whose TrackIds aren't in this block.
-- 'Event.clear_integration' on any events that no longer have a source.
fix_integrated_block :: M m => BlockId -> Block.Block -> m [Text]
fix_integrated_block block_id block = do
    blocks <- gets state_blocks
    let (source_gone, integrated, errs) =
            fix blocks (Block.block_integrated block)
    unless (null errs) $
        modify_block block_id $ \block -> block
            { Block.block_integrated = integrated }
    when source_gone $
        mapM_ clear_integration (Block.block_track_ids block)
    return errs
    where
    track_ids = Block.block_track_ids block
    fix _ Nothing = (False, Nothing, [])
    fix blocks (Just (source_id, dests)) = case Map.lookup source_id blocks of
        Nothing ->
            ( True
            , Nothing
            , ["removed invalid integrated block: " <> showt source_id]
            )
        Just source -> (False, Just (source_id, valid), errs)
            where
            (valid, errs) = fix_track_destinations
                ("block of " <> showt source_id)
                (Block.block_track_ids source) track_ids dests

-- | Drop integrated tracks whose source TrackId isn't in this block, and
-- TrackDestinations whose TrackIds aren't in this block.
-- 'Event.clear_integration' on any events that no longer have a source.
--
-- TODO
-- - No TrackIds duplicated between DeriveDestinations.
-- - No TrackIds duplicated across integrated tracks.
fix_integrated_tracks :: M m => BlockId -> Block.Block -> m [Text]
fix_integrated_tracks block_id block = do
    let gone = concat
            [ Block.dest_track_ids dests
            | (track_id, dests) <- Block.block_integrated_tracks block
            , track_id `notElem` track_ids
            ]
    let (dests, errs) = bimap Maybe.catMaybes concat $ unzip $
            map fix (Block.block_integrated_tracks block)
    unless (null errs) $ do
        modify_block block_id $ \block -> block
            { Block.block_integrated_tracks = dests }
        mapM_ clear_integration gone
    return errs
    where
    track_ids = Block.block_track_ids block
    fix (track_id, dests)
        | track_id `notElem` track_ids =
            (Nothing, ["removed invalid integrated track: " <> showt track_id])
        | otherwise = (Just (track_id, valid), errs)
        where
        (valid, errs) = fix_track_destinations ("track of " <> showt track_id)
            track_ids track_ids dests

fix_track_destinations :: Text -> [TrackId] -> [TrackId]
    -> Block.TrackDestinations -> (Block.TrackDestinations, [Text])
fix_track_destinations err_msg source_track_ids track_ids d = case d of
    Block.DeriveDestinations dests ->
        ( Block.DeriveDestinations valid
        , errs (map Block.note_dest_track_ids invalid)
        )
        where (valid, invalid) = List.partition derive_valid dests
    Block.ScoreDestinations dests ->
        (Block.ScoreDestinations valid, errs (map score_track_ids invalid))
        where (valid, invalid) = List.partition score_valid dests
    where
    errs invalid = ["integrated " <> err_msg
        <> ": track destination has track ids not in the right block: "
        <> pretty dest | dest <- invalid]
    score_track_ids (source_id, (dest_id, _)) = (source_id, dest_id)
    derive_valid = all (`elem` track_ids) . Block.note_dest_track_ids
    score_valid (source_id, (dest_id, _index)) =
        source_id `elem` source_track_ids && dest_id `elem` track_ids

clear_integration :: M m => TrackId -> m ()
clear_integration track_id = modify_events track_id $
    Events.map_events Event.clear_integration

block_event_tracknums :: Block.Block -> [(TrackNum, TrackId)]
block_event_tracknums block =
    [(tracknum, track_id) | (tracknum, Just track_id) <- zip [0..] track_ids]
    where track_ids = map Block.track_id (Block.block_tracks block)

-- * IDs

-- | Read an ID of the form \"namespace/name\", or just \"name\", filling in
-- the current namespace if it's not present.
read_id :: (CallStack.Stack, Id.Ident a, M m) => Text -> m a
read_id name = do
    ns <- get_namespace
    require ("invalid characters in id name: " <> showt name) $
        Id.make $ Id.id ns name

namespace :: M m => Text -> m Id.Namespace
namespace ns = do
    unless (Id.valid_symbol ns) $
        throw $ "invalid characters in namespace: " <> showt ns
    return $ Id.namespace ns

-- * damage

damage_view :: M m => ViewId -> m ()
damage_view = damage . Update.view_damage

damage_block :: M m => BlockId -> m ()
damage_block = damage . Update.block_damage

damage_track :: M m => TrackId -> m ()
damage_track track_id = damage $ Update.track_damage track_id Ranges.everything

damage_ruler :: M m => RulerId -> m ()
damage_ruler = damage . Update.ruler_damage

damage_score :: M m => m ()
damage_score = damage $ mempty { Update._score_damage = True }
