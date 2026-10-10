-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
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
    , empty, create
    -- * address types
    , Track(..), Range(..), TrackInfo(..)
    -- * StateT monad
    , M, StateT, StateId, get, unsafePut, damage, getDamage
    , throwError, throw, throw_
    , run, runId, eval, evalRethrow, exec, execRethrow
    , gets, unsafeModify, put, modify
    , updateAll
    -- ** errors
    , Error(..)
    , require, requireRight

    -- * config
    , getNamespace, setNamespace
    , getDefault, modifyDefault, getRootId, lookupRootId, setRootId
    , modifyConfig, getConfig, withConfig
    , modifyMeta
    , modifyAllocation, allocation

    -- * view
    , getView, lookupView, allViewIds
    , createView, destroyView, putViews
    , setViewStatus
    -- ** zoom and track scroll
    , getZoom, modifyZoom, setTrackScroll, setViewRect
    , setViewPadding
    -- ** selections
    , getSelection, setSelection
    , shiftSelection, skipUnselectableTracks
    , selectableTracks

    -- * block
    , getBlock, lookupBlock, allBlockIds, allBlockTrackIds
    , createConfigBlock, createBlock, destroyBlock
    , blockOf, blockIdOf, viewsOf
    , getBlockTitle, setBlockTitle
    , modifyBlockMeta
    , setIntegratedBlock, modifyIntegratedTracks, setIntegratedManual
    , setEditBox, setPlayBox
    , blockRulerEnd, blockEventEnd, blockEnd, blockLogicalRange
    -- ** skeleton
    , setSkeletonConfig
    , hasExplicitSkeleton
    , getSkeleton, setSkeleton, modifySkeleton
    , toggleSkeletonEdge, addEdges, removeEdges
    , spliceSkeletonAbove, spliceSkeletonBelow
    -- ** tracks
    , insertTrack, removeTrack, moveTrack
    -- *** tracks by tracknum
    , trackCount
    , blockTrackAt, getBlockTrackAt, trackAt
    , eventTrackAt, getEventTrackAt
    , rulerTrackAt, blockRuler
    -- *** tracks by TrackId
    , trackIdsOf, tracknumsOf, blockTracknums
    , tracknumOf, getTracknumOf
    -- *** block track
    , setTrackWidth, setTrackSuggestedWidth
    , trackFlags, trackCollapsed
    , toggleTrackFlag, addTrackFlag, removeTrackFlag
    , modifyTrackFlags
    , setTrackRuler
    , mergeTrack, unmergeTrack, setMergedTracks
    , trackMerged
    , setRulerIds, replaceRulerId, setRulerId
    , getTracklike

    -- * track
    , getTrack, lookupTrack, allTrackIds
    , createTrack, destroyTrack
    , getTrackTitle, setTrackTitle, modifyTrackTitle
    , setTrackBg
    , modifyTrackRender, setRenderStyle, modifyWaveform
    , blocksWithTrackId
    -- ** events
    , insertEvents, insertBlockEvents, insertEvent
    , getEvents, modifyEvents, modifyEventsRange, modifyEventsFrom
    , modifySomeEvents, calculateDamage
    , removeEvent, removeEvents, removeEventsRange
    , trackEventEnd, rangeFrom

    -- * ruler
    , getRuler, lookupRuler, allRulerIds
    , createRuler, destroyRuler, modifyRuler
    , rulerOf, rulersOf
    , blocksWithRulerId
    , noRuler

    -- * util
    , findTracks

    -- * verify
    , quickVerify, verify -- TODO should be done automatically by put
    , fixState

    -- * ID
    , readId, namespace
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
import           GHC.Stack (HasCallStack)

import qualified Util.CallStack as CallStack
import qualified Util.Lens as Lens
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect

import qualified App.Config as Config
import qualified Derive.Parse as Parse
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
data State = State
    { stateViews :: Map ViewId Block.View
    , stateBlocks :: Map BlockId Block.Block
    , stateTracks :: Map TrackId Track.Track
    , stateRulers :: Map RulerId Ruler.Ruler
    , stateConfig :: UiConfig.Config
    } deriving (Eq, Show)

views :: Lens.Lens State (Map ViewId Block.View)
views = Lens.lens stateViews (\r a -> r { stateViews = a })

blocks :: Lens.Lens State (Map BlockId Block.Block)
blocks = Lens.lens stateBlocks (\r a -> r { stateBlocks = a })

tracks :: Lens.Lens State (Map TrackId Track.Track)
tracks = Lens.lens stateTracks (\r a -> r { stateTracks = a })

rulers :: Lens.Lens State (Map RulerId Ruler.Ruler)
rulers = Lens.lens stateRulers (\r a -> r { stateRulers = a })

config :: Lens.Lens State UiConfig.Config
config = Lens.lens stateConfig (\r a -> r { stateConfig = a })

empty :: State
empty = State
    { stateViews = Map.empty
    , stateBlocks = Map.empty
    , stateTracks = Map.empty
    , stateRulers = Map.empty
    , stateConfig = UiConfig.emptyConfig
    }

-- | Like 'empty', but the state is initialized with the current creation time.
create :: IO State
create = do
    now <- Time.getCurrentTime
    pure $ (config#UiConfig.meta#UiConfig.creation #= now) empty

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
data Track = Track BlockId TrackNum
    deriving (Eq, Show)

instance Pretty Track where
    pretty (Track blockId tracknum) =
        pretty blockId <> "/" <> showt tracknum

-- | A position on a track that can be indicated on the UI.  Its Pretty
-- instance emits a string, which if logged or copy-pasted into the REPL, will
-- cause that section of score to be highlighted.
data Range = Range (Maybe BlockId) TrackId TrackTime TrackTime
    deriving (Eq, Show)

instance Pretty Range where
    pretty (Range maybeBlockId trackId start end) =
        Stack.logUiFrame (maybeBlockId, Just trackId, Just (start, end))

-- * other types

-- | Summary information on a Track.
data TrackInfo = TrackInfo
    { trackTitle :: Text
    , trackId :: TrackId
    , trackTracknum :: TrackNum
    , trackBlock :: Block.Track
    } deriving (Eq, Show)

instance Pretty TrackInfo where
    pretty (TrackInfo title trackId tracknum _) =
        "(" <> Text.unwords
            ["TrackInfo", showt title, showt trackId, showt tracknum]
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
    unsafePut :: State -> m ()
    damage :: Update.UiDamage -> m ()
    getDamage :: m Update.UiDamage
    throwError :: Error -> m a

instance Monad m => M (StateT m) where
    get = StateT State.get
    unsafePut st = StateT (State.put st)
    damage upd = (StateT . lift) (State.modify' (upd<>))
    getDamage = (StateT . lift) State.get
    throwError = StateT . lift . lift . Except.throwError

-- Basic level membership in the MTL club.
instance M m => M (State.StateT state m) where
    get = lift get
    unsafePut = lift . unsafePut
    damage = lift . damage
    getDamage = lift getDamage
    throwError = lift . throwError

instance M m => M (Except.ExceptT exc m) where
    get = lift get
    unsafePut = lift . unsafePut
    damage = lift . damage
    getDamage = lift getDamage
    throwError = lift . throwError

throw :: (HasCallStack, M m) => Text -> m a
throw msg = throwError $ Error GHC.Stack.callStack msg

throw_ :: M m => Text -> m a
throw_ msg = throwError $ Error GHC.Stack.callStack msg

gets :: M m => (State -> a) -> m a
gets f = fmap f get

-- | As with 'unsafe_put', this directly modifies the state.  'modify' is
-- the safe version.
unsafeModify :: M m => (State -> State) -> m ()
unsafeModify f = do
    state <- get
    unsafePut $! f state

-- | TODO verify
--
-- This updates all tracks because I don't know what you modified in there.
put :: M m => State -> m ()
put state = unsafePut state >> updateAll

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
updateAll :: M m => m ()
updateAll = do
    st <- get
    damage $ mempty { Update._blocks = Map.keysSet (stateBlocks st) }

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
    pure $ case res of
        Left err -> Left err
        Right ((val, state), damage) ->
            Right (val, state, blockToViewDamage (stateViews state) damage)

runId :: State -> StateId a -> Either Error (a, State, Update.UiDamage)
runId state m = Identity.runIdentity (run state m)

-- | A form of 'run' that returns only the val and automatically runs in
-- Identity.
eval :: State -> StateId a -> Either Error a
eval state m = case result of
        Left err -> Left err
        Right (val, _, _) -> Right val
    where result = Identity.runIdentity (run state m)

evalRethrow :: M m => Text -> State -> StateId a -> m a
evalRethrow msg state =
    requireRight (((msg <> ": ") <>) . pretty) . eval state

exec :: State -> StateId a -> Either Error State
exec state m = case result of
        Left err -> Left err
        Right (_, state', _) -> Right state'
    where result = Identity.runIdentity (run state m)

execRethrow :: M m => Text -> State -> StateId a -> m State
execRethrow msg state =
    requireRight (((msg <> ": ") <>) . pretty) . exec state

-- | Promote block damage to damage on all of that block's views.  This is run
-- before returning the UiDamage out of the Ui monad.  Otherwise, Diff isn't
-- smart enough to update views when the underlying blocks change.
blockToViewDamage :: Map ViewId Block.View -> Update.UiDamage
    -> Update.UiDamage
blockToViewDamage views damage
    | null viewIds = damage
    | otherwise = damage
        { Update._views = Update._views damage <> Set.fromList viewIds }
    where
    -- This is O(blocks*views) because I have no index BlockId -> [ViewId].
    -- But damaged blocks and views should both be small.
    viewIds = mconcatMap viewsOf (Set.toList (Update._blocks damage))
    viewsOf blockId = map fst $
        filter ((==blockId) . Block.viewBlock . snd) $ Map.toList views


-- ** error

-- | Abort is used by Cmd, so don't throw it from here.  This isn't exactly
-- modular, but ErrorT can't be composed and extensible exceptions are too
-- much bother at the moment.
data Error = Error GHC.Stack.CallStack Text | Abort deriving (Show)

instance Pretty Error where
    pretty (Error stack msg) =
        CallStack.showCaller (CallStack.caller stack) <> " " <> msg
    pretty Abort = "(abort)"

require :: (HasCallStack, M m) => Text -> Maybe a -> m a
require err = maybe (throw err) pure

requireRight :: (HasCallStack, M m) => (err -> Text) -> Either err a -> m a
requireRight fmtErr = either (throw . fmtErr) pure

-- * config

getNamespace :: M m => m Id.Namespace
getNamespace = getConfig UiConfig.configNamespace

setNamespace :: M m => Id.Namespace -> m ()
setNamespace ns = modifyConfig $ \st -> st { UiConfig.configNamespace = ns }

getDefault :: M m => (UiConfig.Default -> a) -> m a
getDefault f = f <$> getConfig UiConfig.configDefault

modifyDefault :: M m => (UiConfig.Default -> UiConfig.Default) -> m ()
modifyDefault f = modifyConfig $ \st ->
    st { UiConfig.configDefault = f (UiConfig.configDefault st) }

getRootId :: M m => m BlockId
getRootId = require "no root root_id" =<< lookupRootId

lookupRootId :: M m => m (Maybe BlockId)
lookupRootId = getConfig UiConfig.configRoot

setRootId :: M m => BlockId -> m ()
setRootId blockId =
    modifyConfig $ \st -> st { UiConfig.configRoot = Just blockId }

-- | Unlike other State fields, you can modify Config freely without worrying
-- about breaking invariants.  TODO except allocations have invariants.
modifyConfig :: M m => (UiConfig.Config -> UiConfig.Config) -> m ()
modifyConfig f = unsafeModify $ \st ->
    st { stateConfig = f (stateConfig st) }

modifyMeta :: M m => (UiConfig.Meta -> UiConfig.Meta) -> m ()
modifyMeta f = modifyConfig $ UiConfig.meta %= f

modifyAllocation :: M m => ScoreT.Instrument
    -> (UiConfig.Allocation -> UiConfig.Allocation) -> m ()
modifyAllocation inst modify = do
    allocs <- config#UiConfig.allocations <#> get
    allocs <- requireRight (("modify " <> pretty inst <> ": ")<>) $
        UiConfig.modifyAllocation inst (Right . modify) allocs
    modifyConfig $ UiConfig.allocations #= allocs

getConfig :: M m => (UiConfig.Config -> a) -> m a
getConfig f = gets (f . stateConfig)

-- | Run the action with a modified state, and restore it.
withConfig :: M m => (UiConfig.Config -> UiConfig.Config) -> m a -> m a
withConfig f action = do
    old <- getConfig id
    modifyConfig f
    -- I think this is exception safe because the state is reverted after an
    -- exception, and there's no way to catch an exception.
    result <- action
    modifyConfig $ const old
    pure result

-- | TODO use this for read only.  If used for write it bypasses
-- 'UiConfig.allocate'.
allocation :: ScoreT.Instrument -> Lens State (Maybe UiConfig.Allocation)
allocation inst = config # UiConfig.allocationsMap # Lens.map inst

-- * view

getView :: M m => ViewId -> m Block.View
getView viewId = lookupId viewId . stateViews =<< get

lookupView :: M m => ViewId -> m (Maybe Block.View)
lookupView viewId = gets (Map.lookup viewId . stateViews)

-- | All ViewIds, in sorted order.
allViewIds :: M m => m [ViewId]
allViewIds = gets (Map.keys . stateViews)

-- | Create a new view.  Block.view_tracks can be left empty, since it will
-- be replaced by views generated from the the block.  If the caller uses the
-- 'Block.view' constructor, it won't have to worry about this.
--
-- Throw if the ViewId already exists.
createView :: M m => Id.Id -> Block.View -> m ViewId
createView id view = do
    view <- _updateViewStatus view
    insert (Id.ViewId id) view damageView stateViews $ \views st ->
        st { stateViews = views }

destroyView :: M m => ViewId -> m ()
destroyView viewId = do
    unsafeModify $ \st ->
        st { stateViews = Map.delete viewId (stateViews st) }
    damageView viewId

putViews :: M m => Map ViewId Block.View -> m ()
putViews viewMap = do
    let (viewIds, views) = unzip (Map.toList viewMap)
    views <- mapM _updateViewStatus views
    unsafeModify $ \st -> st
        { stateViews = Map.fromList (zip viewIds views) }
    mapM_ damageView viewIds

-- | Set a status variable on a view.
setViewStatus :: M m => ViewId -> (Int, Text) -> Maybe Text -> m ()
setViewStatus viewId key val = modifyView viewId $ \view -> view
    { Block.viewStatus = Map.alter (const val) key (Block.viewStatus view) }

_updateViewStatus :: M m => Block.View -> m Block.View
_updateViewStatus view = do
    block <- getBlock (Block.viewBlock view)
    pure $ case Block.blockIntegrated block of
        Just (sourceBlock, _) -> view
            { Block.viewStatus = Map.insert Config.statusIntegrateSource
                (Id.identText sourceBlock) (Block.viewStatus view)
            }
        Nothing -> view

-- ** zoom and track scroll

getZoom :: M m => ViewId -> m Zoom.Zoom
getZoom = fmap Block.viewZoom . getView

modifyZoom :: M m => ViewId -> (Zoom.Zoom -> Zoom.Zoom) -> m ()
modifyZoom viewId modify = modifyView viewId $ \view ->
    view { Block.viewZoom = clamp $ modify $ Block.viewZoom view }
    where clamp zoom = zoom { Zoom.offset = max 0 (Zoom.offset zoom) }

setTrackScroll :: M m => ViewId -> Types.Width -> m ()
setTrackScroll viewId offset =
    modifyView viewId (\view -> view { Block.viewTrackScroll = offset })

setViewRect :: M m => ViewId -> Rect.Rect -> m ()
setViewRect viewId rect =
    modifyView viewId (\view -> view { Block.viewRect = rect })

-- | Only 'Cmd.Cmd.ui_update' is supposed to call this, because the UI is
-- responsible for the padding.
setViewPadding :: M m => ViewId -> Block.Padding -> m ()
setViewPadding viewId padding = modifyView viewId $ \view ->
    view { Block.viewPadding = padding }

-- ** selections

-- | Get @view_id@'s selection at @selnum@, or Nothing if there is none.
getSelection :: M m => ViewId -> Sel.Num -> m (Maybe Sel.Selection)
getSelection viewId selnum = do
    view <- getView viewId
    pure (Map.lookup selnum (Block.viewSelections view))

-- | Replace any selection on @view_id@ at @selnum@ with @sel@.
setSelection :: M m => ViewId -> Sel.Num -> Maybe Sel.Selection -> m ()
setSelection viewId selnum maybeSel = do
    view <- getView viewId
    updateView viewId $ view
        { Block.viewSelections =
            maybe (Map.delete selnum) (Map.insert selnum) maybeSel
                (Block.viewSelections view)
        }

-- ** util

modifyView :: M m => ViewId -> (Block.View -> Block.View) -> m ()
modifyView viewId f = do
    view <- getView viewId
    updateView viewId (f view)

updateView :: M m => ViewId -> Block.View -> m ()
updateView viewId view = do
    unsafeModify $ \st -> st
        { stateViews = Map.adjust (const view) viewId (stateViews st) }
    damageView viewId

-- * block

getBlock :: M m => BlockId -> m Block.Block
getBlock blockId = get >>= lookupId blockId . stateBlocks

lookupBlock :: M m => BlockId -> m (Maybe Block.Block)
lookupBlock blockId = get >>= pure . Map.lookup blockId . stateBlocks

allBlockIds :: M m => m [BlockId]
allBlockIds = gets (Map.keys . stateBlocks)

-- | Get all blocks along with their tracks.
allBlockTrackIds :: M m => m [(BlockId, [TrackId])]
allBlockTrackIds =
    map (second Block.blockTrackIds) <$> gets (Map.toList . stateBlocks)

-- | Make a new block.  If it's the first one, it will be set as the root.
-- This is the low level version, you probably want to use 'create_block'.
--
-- Throw if the BlockId already exists.
createConfigBlock :: M m => Id.Id -> Block.Block -> m BlockId
createConfigBlock id block = do
    bid <- insert (Id.BlockId id) block damageBlock stateBlocks $
        \blocks st -> st
            { stateBlocks = blocks
            , stateConfig = let c = stateConfig st
                in c { UiConfig.configRoot = if Map.size blocks == 1
                    then Just (Id.BlockId id) else UiConfig.configRoot c }
            }
    updateSkeleton bid
    pure bid

-- | Make a new block with the default 'Block.Config'.
createBlock :: M m => Id.Id -> Text -> [Block.Track] -> m BlockId
createBlock blockId title tracks =
    createConfigBlock blockId (Block.block Block.defaultConfig title tracks)

-- | Destroy the block and all the views that display it.  If the block was
-- the root, it will be be unset.  The block's tracks are left intact.
destroyBlock :: M m => BlockId -> m ()
destroyBlock blockId = do
    views <- viewsOf blockId
    mapM_ destroyView (Map.keys views)
    unsafeModify $ \st -> st
        { stateBlocks = Map.delete blockId (stateBlocks st)
        , stateConfig = let c = stateConfig st in c
            { UiConfig.configRoot = if UiConfig.configRoot c == Just blockId
                then Nothing else UiConfig.configRoot c
            }
        }
    damageBlock blockId
    blocks <- gets (Map.toList . stateBlocks)
    -- Remove integration destinations of any blocks that were generated from
    -- this one.
    mapM_ (uncurry fixIntegratedBlock) blocks

blockOf :: M m => ViewId -> m Block.Block
blockOf viewId = getBlock . Block.viewBlock =<< getView viewId

blockIdOf :: M m => ViewId -> m BlockId
blockIdOf viewId = Block.viewBlock <$> getView viewId

-- | Get all views of a given block.
viewsOf :: M m => BlockId -> m (Map ViewId Block.View)
viewsOf blockId = do
    views <- gets stateViews
    pure $ Map.filter ((==blockId) . Block.viewBlock) views

getBlockTitle :: M m => BlockId -> m Text
getBlockTitle = fmap Block.blockTitle . getBlock

setBlockTitle :: M m => BlockId -> Text -> m ()
setBlockTitle blockId title =
    modifyBlock blockId (\block -> block { Block.blockTitle = title })

modifyBlockMeta :: M m => BlockId -> (Block.Meta -> Block.Meta) -> m ()
modifyBlockMeta blockId f = modifyBlock blockId $ \block ->
    block { Block.blockMeta = f (Block.blockMeta block) }

-- | Set or clear this block as an integrate destination.  The automatic
-- integration system will update it from the given source block.
setIntegratedBlock :: M m => BlockId
    -> Maybe (BlockId, Block.TrackDestinations) -> m ()
setIntegratedBlock blockId integrated = do
    modifyBlock blockId $ \block ->
        block { Block.blockIntegrated = integrated }
    block <- getBlock blockId
    requireValid ("set_integrated_block " <> pretty blockId)
        (fixIntegratedBlock blockId block)

modifyIntegratedTracks :: M m => BlockId
    -> ([(TrackId, Block.TrackDestinations)]
        -> [(TrackId, Block.TrackDestinations)])
    -> m ()
modifyIntegratedTracks blockId modify = do
    pre <- destTrackIds <$> getBlock blockId
    modifyBlock blockId $ \block -> block
        { Block.blockIntegratedTracks =
            modify (Block.blockIntegratedTracks block)
        }
    block <- getBlock blockId
    requireValid ("modify_integrated_tracks " <> pretty blockId) $
        fixIntegratedTracks blockId block
    -- fix_integrated_tracks will detect integrates where the track is gone.
    -- If the track remains but a previously existing integration was deleted,
    -- clear out the corresponding Event.stacks.
    let post = destTrackIds block
    let gone = filter (`notElem` post) pre
    mapM_ clearIntegration gone
    where
    -- TODO This also gets derive integration, is that correct?
    destTrackIds = concatMap (Block.destTrackIds . snd)
        . Block.blockIntegratedTracks

-- | Set or clear the block's manual integration 'Block.NoteDestination's.
-- This just attaches (or removes) the integrate information to the block so
-- a future integration can use it to merge, and then call this function again.
setIntegratedManual :: M m => BlockId -> Block.SourceKey
    -> Maybe [Block.NoteDestination] -> m ()
setIntegratedManual blockId key dests =
    modifyBlock blockId $ \block -> block
        { Block.blockIntegratedManual =
            maybe (Map.delete key) (Map.insert key) dests
                (Block.blockIntegratedManual block)
        }
    -- TODO require_valid?

modifyBlockConfig :: M m => BlockId -> (Block.Config -> Block.Config) -> m ()
modifyBlockConfig blockId modify = modifyBlock blockId $
    \block -> block { Block.blockConfig = modify (Block.blockConfig block) }

setEditBox :: M m => BlockId -> Block.Box -> Block.Box -> m ()
setEditBox blockId skel track = modifyBlockConfig blockId $
    \config -> config
        { Block.configSkelBox = skel
        , Block.configTrackBox = track
        }

-- | The play box doesn't use a char, so I leave that out.
setPlayBox :: M m => BlockId -> Color.Color -> m ()
setPlayBox blockId color = modifyBlockConfig blockId $ \config -> config
    { Block.configSbBox = Block.Box color ' ' }

-- | Get the end of the block according to the ruler.  This means that if the
-- block has no rulers (e.g. a clipboard block) then block_ruler_end will be 0.
blockRulerEnd :: M m => BlockId -> m TrackTime
blockRulerEnd blockId = do
    block <- getBlock blockId
    case Block.blockRulerIds block of
        [] -> pure 0
        rulerId : _ -> Ruler.timeEnd <$> getRuler rulerId

-- | Get the end of the block according to the last event of the block.
blockEventEnd :: M m => BlockId -> m TrackTime
blockEventEnd blockId = do
    block <- getBlock blockId
    trackEnds <- mapM trackEventEnd (Block.blockTrackIds block)
    pure $ maximum (0 : trackEnds)

-- | Get the maximum of ruler end and event end.  The end may still be 0 if the
-- block is totally empty.
blockEnd :: M m => BlockId -> m TrackTime
blockEnd blockId =
    max <$> blockRulerEnd blockId <*> blockEventEnd blockId

-- | The logical range is defined by 'Ruler.bounds_of' and is intended to
-- correspond to the \"note\" that this block defines.
blockLogicalRange :: M m => BlockId -> m (TrackTime, TrackTime)
blockLogicalRange blockId = do
    block <- getBlock blockId
    case Block.blockRulerIds block of
        [] -> (,) 0 <$> blockEventEnd blockId
        rulerId : _ -> do
            (start, end) <- Ruler.boundsOf <$> getRuler rulerId
            end <- maybe (blockEventEnd blockId) pure end
            pure (start, end)

-- ** skeleton

setSkeletonConfig :: M m => BlockId -> Block.Skeleton -> m ()
setSkeletonConfig blockId skel = do
    modifyBlockConfig blockId $ \config -> config
        { Block.configSkeleton = skel }
    updateSkeleton blockId
    damageScore

hasExplicitSkeleton :: M m => BlockId -> m Bool
hasExplicitSkeleton =
    fmap ((==Block.Explicit) . Block.configSkeleton . Block.blockConfig)
        . getBlock

getSkeleton :: M m => BlockId -> m Skeleton.Skeleton
getSkeleton blockId = Block.blockSkeleton <$> getBlock blockId

setSkeleton :: M m => BlockId -> Skeleton.Skeleton -> m ()
setSkeleton blockId skel = modifySkeleton blockId (const skel)

modifySkeleton :: M m => BlockId
    -> (Skeleton.Skeleton -> Skeleton.Skeleton) -> m ()
modifySkeleton blockId modify = do
    block <- getBlock blockId
    let config = Block.configSkeleton (Block.blockConfig block)
    unless (config == Block.Explicit) $
        throw $ "can't modify skeleton of " <> pretty blockId
            <> ", it's not explicit: " <> showt config
    _setSkeleton blockId block $ modify (Block.blockSkeleton block)

_setSkeleton :: M m => BlockId -> Block.Block -> Skeleton.Skeleton -> m ()
_setSkeleton blockId block skel = do
    forM_ (Skeleton.flatten skel) $ \(parent, child) ->
        unless (Num.inRange 1 tracks parent && Num.inRange 1 tracks child) $
            throw $ "modify_skeleton: edge " <> showt (parent, child)
                <> " out of range for " <> showt blockId
    modifyBlock blockId $ \block -> block { Block.blockSkeleton = skel }
    where tracks = length $ Block.blockTracks block

-- | Toggle the given edge in the block's skeleton.  If a cycle would be
-- created, refuse to add the edge and return False.  The edge is in (parent,
-- child) order.
toggleSkeletonEdge :: M m => Bool
    -- ^ If not true, the child's existing parents will be unlinked.
    -- While a track with multiple parents is possible, and is a way to
    -- express the same score derived under different conditions, in practice
    -- I never do that.
    -> BlockId -> Skeleton.Edge -> m Bool
toggleSkeletonEdge allowMultipleParents blockId edge@(_, child) = do
    block <- getBlock blockId
    whenJust (edgesInRange block edge) (throw . ("toggle: "<>))
    let skel = dropParents (Block.blockSkeleton block)
    case Skeleton.toggleEdge edge skel of
        Nothing -> pure False
        Just newSkel -> do
            setSkeleton blockId newSkel
            pure True
    where
    dropParents skel
        | allowMultipleParents || Skeleton.hasEdge skel edge = skel
        | otherwise = Skeleton.removeEdges parents skel
        where parents = map (, child) (Skeleton.parents skel child)

-- | Add the edges to the skeleton.  Throw if they would produce a cycle.
addEdges :: M m => BlockId -> [Skeleton.Edge] -> m ()
addEdges blockId edges = do
    skel <- getSkeleton blockId
    block <- getBlock blockId
    whenJust (msum (map (edgesInRange block) edges))
        (throw . ("add_edges: " <>))
    maybe (throw $ "add_edges " <> showt edges <> " to " <> showt skel
            <> " would have caused a cycle")
        (setSkeleton blockId) (Skeleton.addEdges edges skel)

removeEdges :: M m => BlockId -> [Skeleton.Edge] -> m ()
removeEdges blockId edges =
    modifySkeleton blockId (Skeleton.removeEdges edges)

-- | The first tracknum is spliced above the second.
spliceSkeletonAbove :: M m => BlockId -> TrackNum -> TrackNum -> m ()
spliceSkeletonAbove = _spliceSkeleton True

-- | The first tracknum is spliced below the second.
spliceSkeletonBelow :: M m => BlockId -> TrackNum -> TrackNum -> m ()
spliceSkeletonBelow = _spliceSkeleton False

-- | Splice the given tracknum into the skeleton, either above or below
-- the @to@ tracknum.  What this means exactly is documented in
-- 'Graphs.splice_above' and 'Graphs.slice_below'.
_spliceSkeleton :: M m => Bool -> BlockId -> TrackNum -> TrackNum -> m ()
_spliceSkeleton above blockId new to = do
    block <- getBlock blockId
    whenJust (msum (map (edgeInRange block) [new, to]))
        (throw . ("splice: " <>))
    let splice = if above then Skeleton.spliceAbove else Skeleton.spliceBelow
    maybe (throw $ "splice_skeleton: " <> showt (new, to)
            <> " would have caused a cycle")
        (setSkeleton blockId) (splice new to (Block.blockSkeleton block))

edgeInRange :: Block.Block -> TrackNum -> Maybe Text
edgeInRange block tracknum =
    case Lists.at (Block.blockTracks block) tracknum of
        Nothing -> Just $ "tracknum out of range: " <> showt tracknum
        Just t -> case Block.tracklikeId t of
            Block.TId {} -> Nothing
            _ -> Just $ "edge points to non-event track: " <> showt t

edgesInRange :: Block.Block -> Skeleton.Edge -> Maybe Text
edgesInRange block (from, to) =
    edgeInRange block from <|> edgeInRange block to

-- ** tracks

-- | Insert a track at the given TrackNum.  The TrackNum can be out of range to
-- insert a track at the beginning or append it to the end.
--
-- This will throw if it's an event track and the block already contains that
-- TrackId.  This invariant ensures that a (BlockId, TrackNum) is
-- interchangeable with a TrackId.
insertTrack :: M m => BlockId -> TrackNum -> Block.Track -> m ()
insertTrack blockId tracknum track = do
    block <- getBlock blockId
    views <- viewsOf blockId
    whenJust (Block.trackId track) $ \trackId -> do
        trackIds <- trackIdsOf blockId
        when (trackId `elem` trackIds) $
            throw $ "insert_track: block " <> showt blockId
                <> " already contains " <> showt trackId
    -- You can only put a ruler in tracknum 0.
    unless (tracknum > 0 || isRuler track) $
        throw $ "non-ruler track can't go at tracknum " <> showt tracknum
            <> ": " <> pretty track
    let tracks = Lists.insertAt tracknum track (Block.blockTracks block)
        -- Make sure the views are up to date.
        views' = Map.map (insertIntoView block tracknum) views
    skel <- case Block.configSkeleton (Block.blockConfig block) of
        Block.Explicit -> pure $
            Skeleton.insert tracknum (Block.blockSkeleton block)
        Block.Implicit -> inferSkeleton tracks
    setBlock blockId $ block
        { Block.blockTracks = tracks
        , Block.blockSkeleton = skel
        }
    unsafeModify $ \st ->
        st { stateViews = Map.union views' (stateViews st) }
    mapM_ damageView (Map.keys views')
    where
    isRuler t = case Block.tracklikeId t of
        Block.RId {} -> True
        _ -> False

-- | Remove the track at the given tracknum.
removeTrack :: M m => BlockId -> TrackNum -> m ()
removeTrack blockId tracknum = do
    block <- getBlock blockId
    let tracks = Block.blockTracks block
    unless (1 <= tracknum && tracknum < length tracks) $
        throw $ "remove_track " <> showt blockId <> " " <> showt tracknum
            <> " out of range 1--" <> showt (length tracks)
    views <- Map.map (removeFromView block tracknum) <$> viewsOf blockId
    setBlock blockId $ block
        { Block.blockTracks = Lists.removeAt tracknum tracks
        , Block.blockSkeleton =
            Skeleton.remove tracknum (Block.blockSkeleton block)
        }
    unsafeModify $ \st ->
        st { stateViews = Map.union views (stateViews st) }
    mapM_ damageView (Map.keys views)
    -- Clear any orphaned integration destinations.
    fixIntegratedTracks blockId =<< getBlock blockId
    pure ()

-- | Move a track from one tracknum to another.
moveTrack :: M m => BlockId -> TrackNum -> TrackNum -> m ()
moveTrack blockId from to = do
    block <- getBlock blockId
    let msg = "move_track: from " <> showt from <> " to " <> showt to
            <> " out of range"
    tracks <- require msg $ do
        -- Things get generally messed up if you try to move an event track to
        -- the ruler spot.
        guard (from /= 0 && to /= 0)
        Lists.move from to (Block.blockTracks block)
    skel <- case Block.configSkeleton (Block.blockConfig block) of
        Block.Explicit -> pure $
            Skeleton.move from to (Block.blockSkeleton block)
        Block.Implicit -> inferSkeleton tracks
    modifyBlock blockId $ const $ block
        { Block.blockTracks = tracks, Block.blockSkeleton = skel }

inferSkeleton :: M m => [Block.Track] -> m Skeleton.Skeleton
inferSkeleton tracks = do
    titles <- traverse (traverse (fmap strip . getTrackTitle)) $
        Lists.mapMaybeSnd Block.trackId (zip [0..] tracks)
    pure $ ParseSkeleton.defaultParser $
        map (uncurry ParseSkeleton.Track) titles
    where
    -- Treat commented out tracks as if they weren't commented.  This is so
    -- I can comment out a track and it'll disable the contents and not change
    -- skeleton.
    strip title
        | Just s <- remove Parse.comment = s
        | otherwise = title
        where remove p = Text.stripStart <$> Text.stripPrefix p title

updateSkeleton :: M m => BlockId -> m ()
updateSkeleton blockId = do
    block <- getBlock blockId
    case Block.configSkeleton (Block.blockConfig block) of
        Block.Implicit -> _setSkeleton blockId block
            =<< inferSkeleton (Block.blockTracks block)
        _ -> pure ()

-- *** tracks by TrackNum

-- | Number of tracks in the block.  This includes the ruler, so subtract 1 if
-- you want all non-ruler tracks.
trackCount :: M m => BlockId -> m TrackNum
trackCount blockId = do
    block <- getBlock blockId
    pure $ length (Block.blockTracks block)

-- | Get the Track at @tracknum@, or Nothing if its out of range.
blockTrackAt :: M m => BlockId -> TrackNum -> m (Maybe Block.Track)
blockTrackAt blockId tracknum
    | tracknum < 0 =
        throw $ "block_track_at: negative tracknum: " <> showt tracknum
    | otherwise = do
        block <- getBlock blockId
        pure $ Lists.at (Block.blockTracks block) tracknum

getBlockTrackAt :: M m => BlockId -> TrackNum -> m Block.Track
getBlockTrackAt blockId tracknum =
    tracknumInRange blockId tracknum =<< blockTrackAt blockId tracknum
    where
    tracknumInRange blockId tracknum Nothing = do
        count <- trackCount blockId
        throw $ "track " <> pretty (Track blockId tracknum)
            <> " out of range 0--" <> showt count
    tracknumInRange _ _ (Just a) = pure a

trackAt :: M m => BlockId -> TrackNum -> m (Maybe Block.TracklikeId)
trackAt blockId tracknum = do
    maybeTrack <- blockTrackAt blockId tracknum
    pure $ fmap Block.tracklikeId maybeTrack

-- | Like 'track_at', but only for event tracks.
eventTrackAt :: M m => BlockId -> TrackNum -> m (Maybe TrackId)
eventTrackAt blockId tracknum = do
    maybeTrack <- trackAt blockId tracknum
    pure $ Block.trackIdOf =<< maybeTrack

-- | Like 'event_track_at' but throws if it's not there or not an event track.
getEventTrackAt :: M m => BlockId -> TrackNum -> m TrackId
getEventTrackAt blockId tracknum = do
    track <- getBlockTrackAt blockId tracknum
    require ("track " <> pretty (Track blockId tracknum)
            <> " not an event track") $
        Block.trackId track

-- | Get the RulerId of an event or ruler track, or Nothing if the tracknum is
-- out of range or doesn't have a ruler.
rulerTrackAt :: M m => BlockId -> TrackNum -> m (Maybe RulerId)
rulerTrackAt blockId tracknum = do
    maybeTrack <- trackAt blockId tracknum
    pure $ Block.rulerIdOf =<< maybeTrack

-- | 0 is the conventional ruler tracknum.
blockRuler :: M m => BlockId -> m RulerId
blockRuler blockId = fromMaybe noRuler <$> rulerTrackAt blockId 0

-- *** tracks by TrackId

-- | Get all TrackIds of the given block.
trackIdsOf :: M m => BlockId -> m [TrackId]
trackIdsOf blockId = Block.blockTrackIds <$> getBlock blockId

-- | Get all TrackIds of the given block, along with their tracknums.
tracknumsOf :: M m => BlockId -> m [(TrackId, TrackNum)]
tracknumsOf = fmap (Lists.mapMaybeFst Block.trackId) . blockTracknums

-- | Get tracks along with their TrackNums.
blockTracknums :: M m => BlockId -> m [(Block.Track, TrackNum)]
blockTracknums = fmap (flip zip [0..] . Block.blockTracks) . getBlock

-- | There can only be one TrackId per block, which allows TrackNums and
-- TrackIds to be interchangeable.  This is enforced by 'insert_track'.
--
-- The inverse is 'event_track_at'.
tracknumOf :: M m => BlockId -> TrackId -> m (Maybe TrackNum)
tracknumOf blockId tid = lookup tid <$> tracknumsOf blockId

getTracknumOf :: M m => BlockId -> TrackId -> m TrackNum
getTracknumOf blockId tid =
    require ("tracknum_of: track " <> showt tid <> " not in " <> showt blockId)
        =<< tracknumOf blockId tid

-- *** block track

setTrackWidth :: M m => BlockId -> TrackNum -> Types.Width -> m ()
setTrackWidth blockId tracknum width =
    modifyBlockTrack blockId tracknum $ \btrack ->
        btrack { Block.trackWidth = width }

setTrackSuggestedWidth :: M m => BlockId -> TrackNum -> Types.Width -> m ()
setTrackSuggestedWidth blockId tracknum width =
    modifyBlockTrack blockId tracknum $ \btrack ->
        btrack { Block.trackSuggestedWidth = width }

trackFlags :: M m => BlockId -> TrackNum -> m (Set Block.TrackFlag)
trackFlags blockId tracknum =
    Block.trackFlags <$> getBlockTrackAt blockId tracknum

trackCollapsed :: M m => BlockId -> TrackNum -> m Bool
trackCollapsed blockId tracknum =
    Block.isCollapsed . Block.trackFlags <$>
        getBlockTrackAt blockId tracknum

toggleTrackFlag :: M m => BlockId -> TrackNum -> Block.TrackFlag -> m ()
toggleTrackFlag blockId tracknum flag =
    modifyTrackFlags blockId tracknum toggle
    where
    toggle flags
        | flag `Set.member` flags = Set.delete flag flags
        | otherwise = Set.insert flag flags

addTrackFlag, removeTrackFlag
    :: M m => BlockId -> TrackNum -> Block.TrackFlag -> m ()
addTrackFlag blockId tracknum flag =
    modifyTrackFlags blockId tracknum $ Set.insert flag
removeTrackFlag blockId tracknum flag =
    modifyTrackFlags blockId tracknum $ case flag of
        -- Block.Merge -> Set.delete flag . Set.delete Block.Collapse
        _ -> Set.delete flag

modifyTrackFlags :: M m => BlockId -> TrackNum
    -> (Set Block.TrackFlag -> Set Block.TrackFlag) -> m ()
modifyTrackFlags blockId tracknum modify =
    modifyBlockTrack blockId tracknum $ \btrack ->
        -- Don't allow both Merge and Collapse to be set, prefer Merge.
        -- This is so I can Collapse a bunch of tracks, and the Merged ones
        -- will stay the way they are.  Merge also has priority in
        -- Block.display_track, but having both set leads to confusing UI.
        let flags = modify (Block.trackFlags btrack)
        in btrack
            { Block.trackFlags = if Set.member Block.Merge flags
                then Set.delete Block.Collapse flags
                else flags
            }

setTrackRuler :: M m => BlockId -> TrackNum -> RulerId -> m ()
setTrackRuler blockId tracknum rulerId = do
    _ <- getRuler rulerId -- Throw if it doesn't exist.
    modifyBlockTrack blockId tracknum $
        Block.modifyId (Block.setRulerId rulerId)

-- | Merge the @from@ tracknum into the @to@ tracknum and collapse @from@.
mergeTrack :: M m => BlockId -> TrackNum -> TrackNum -> m ()
mergeTrack blockId to from = do
    fromId <- getEventTrackAt blockId from
    modifyBlockTrack blockId to $ \btrack -> btrack
        { Block.trackMerged = Set.insert fromId (Block.trackMerged btrack) }
    addTrackFlag blockId from Block.Merge

-- | Reverse 'merge_track': remove the merged tracks and expand their
-- occurrances in the given block.  \"Unmerge\" is not a graceful term, but at
-- least it's obviously the opposite of \"merge\".
unmergeTrack :: M m => BlockId -> TrackNum -> m ()
unmergeTrack blockId tracknum = do
    trackIds <- Block.trackMerged <$> getBlockTrackAt blockId tracknum
    unmergedTracknums <- mapMaybeM (tracknumOf blockId)
        (Set.toList trackIds)
    forM_ unmergedTracknums $ \tracknum ->
        removeTrackFlag blockId tracknum Block.Merge
    setMergedTracks blockId tracknum mempty

setMergedTracks :: M m => BlockId -> TrackNum -> Set TrackId -> m ()
setMergedTracks blockId tracknum merged =
    modifyBlockTrack blockId tracknum $ \btrack ->
        btrack { Block.trackMerged = merged }

trackMerged :: M m => BlockId -> TrackNum -> m Bool
trackMerged blockId tracknum = not . Set.null . Block.trackMerged <$>
    getBlockTrackAt blockId tracknum

-- | Set rulers, one per track.
setRulerIds :: M m => BlockId -> [Maybe RulerId] -> m ()
setRulerIds blockId rulerIds = modifyBlock blockId $ \block -> block
    { Block.blockTracks =
        zipWith set (Block.blockTracks block) (rulerIds ++ repeat Nothing)
    }
    where
    set track (Just rulerId) = track
        { Block.tracklikeId =
            Block.setRulerId rulerId (Block.tracklikeId track)
        }
    set track Nothing = track

setRulerId :: M m => BlockId -> RulerId -> m ()
setRulerId blockId rulerId = modifyRulerId blockId (const rulerId)

-- | Replace one RulerId with another on the given block.
--
-- It's more convenient to do here than removing and inserting tracks, and easy
-- since there's no "one per block" invariant to maintain with ruler ids.
replaceRulerId :: M m => BlockId -> RulerId -> RulerId -> m ()
replaceRulerId blockId from to =
    modifyRulerId blockId $ \rid -> if rid == from then to else from

modifyRulerId :: M m => BlockId -> (RulerId -> RulerId) -> m ()
modifyRulerId blockId modify = modifyBlock blockId $ \block ->
    block { Block.blockTracks = map replaceTrack (Block.blockTracks block) }
    where
    replaceTrack track = track
        { Block.tracklikeId = replace (Block.tracklikeId track) }
    replace tlikeId = case Block.rulerIdOf tlikeId of
        Just rid -> Block.setRulerId (modify rid) tlikeId
        Nothing -> tlikeId

-- | Resolve a TracklikeId to a Tracklike.
getTracklike :: M m => Block.TracklikeId -> m Block.Tracklike
getTracklike track = case track of
    Block.TId tid rid -> Block.T <$> getTrack tid <*> getRuler rid
    Block.RId rid -> Block.R <$> getRuler rid
    Block.DId divider -> pure (Block.D divider)

modifyBlockTrack :: M m => BlockId -> TrackNum
    -> (Block.Track -> Block.Track) -> m ()
modifyBlockTrack blockId tracknum modify = do
    block <- getBlock blockId
    btracks <- modifyAt "modify_block_track"
        (Block.blockTracks block) tracknum modify
    modifyBlock blockId $ \b -> b { Block.blockTracks = btracks }

-- *** track util

-- | Insert a new track into Block.view_tracks, moving selections as
-- appropriate.  @tracknum@ is clipped to be in range.
insertIntoView :: Block.Block -> TrackNum -> Block.View -> Block.View
insertIntoView block tracknum view = view
    { Block.viewSelections = Map.map (insertIntoSelection block tracknum)
        (Block.viewSelections view)
    }

-- | Remove @tracknum@ from Block.view_tracks, moving selections as
-- appropriate.  Ignored if @tracknum@ is out of range.
removeFromView :: Block.Block -> TrackNum -> Block.View -> Block.View
removeFromView block tracknum view = view
    { Block.viewSelections =
        Map.mapMaybeWithKey (removeFromSelection block tracknum)
            (Block.viewSelections view)
    }

-- | If tracknum is before or at the selection, push it to the right.  If it's
-- inside, extend it.  If it's to the right, do nothing.
insertIntoSelection :: Block.Block -> TrackNum -> Sel.Selection
    -> Sel.Selection
insertIntoSelection block tracknum sel
    | tracknum <= low = shiftSelection True block 1 sel
    | tracknum <= high = Sel.expandTracks 1 sel
    | otherwise = sel
    where (low, high) = Sel.trackRange sel

-- | Remove the given track from the selection.  The selection will be moved or
-- shrunk as per 'insert_into_selection', possibly to nothing if the selection
-- was only on the deleted track.  Config.insert_selnum is an exception, it
-- moves one track to the left, if possible.  That's because it's convenient to
-- delete consecutive tracks.
removeFromSelection :: Block.Block -> TrackNum
    -> Sel.Num -> Sel.Selection -> Maybe Sel.Selection
removeFromSelection block tracknum selnum sel
    | tracknum < low = Just $ shiftSelection True block (-1) sel
    | tracknum == high && high == low =
        if selnum == Config.insertSelnum
        then Just $ shiftSelection True block (-1) sel
        else Nothing
    | tracknum <= high = Just $ Sel.expandTracks (-1) sel
    | otherwise = Just sel
    where (low, high) = Sel.trackRange sel

-- | Shift the selection, clipping if it's out of range.  While the
-- sel_cur_track won't be on a non-selectable track after this, the selection
-- may still include one.
shiftSelection :: Bool -- ^ skip unselectable tracks
    -> Block.Block -> TrackNum -> Sel.Selection -> Sel.Selection
shiftSelection skipUnselectable block shift sel =
    Sel.modifyTracks (+shift2) sel
    where
    shift2
        | skipUnselectable =
            skipUnselectableTracks block (Sel.curTrack sel) shift
                - Sel.curTrack sel
        | otherwise = shift

-- | Shift a tracknum to another track, skipping unselectable tracks.
skipUnselectableTracks :: Block.Block -> TrackNum -> Int -> TrackNum
skipUnselectableTracks block tracknum shift
    | shift == 0 = tracknum
    | shift > 0 = findTrack (dropWhile (<tracknum) selectable)
    | otherwise = findTrack (dropWhile (>tracknum) (List.reverse selectable))
    where
    selectable = selectableTracks block
    findTrack [] = tracknum
    findTrack tracks@(first:_) =
        fromMaybe tracknum $ Lists.head $ drop absShift tracks
        where
        absShift = if tracknum /= first then abs shift - 1 else abs shift

-- | Get the tracknums from a block that should be selectable.
selectableTracks :: Block.Block -> [TrackNum]
selectableTracks block =
    [ tracknum | (tracknum, track) <- zip [0..] (Block.blockTracks block)
    , Block.trackSelectable track
    ]

-- ** util

modifyBlock :: M m => BlockId -> (Block.Block -> Block.Block) -> m ()
modifyBlock blockId f = do
    block <- getBlock blockId
    setBlock blockId (f block)

setBlock :: M m => BlockId -> Block.Block -> m ()
setBlock blockId block = do
    unsafeModify $ \st -> st
        { stateBlocks = Map.adjust (const block) blockId (stateBlocks st) }
    damageBlock blockId

-- * track

getTrack :: M m => TrackId -> m Track.Track
getTrack trackId = get >>= lookupId trackId . stateTracks

lookupTrack :: M m => TrackId -> m (Maybe Track.Track)
lookupTrack trackId = gets (Map.lookup trackId . stateTracks)

allTrackIds :: M m => m [TrackId]
allTrackIds = gets (Map.keys . stateTracks)

-- | Insert the given track with the given ID.
--
-- Throw if the TrackId already exists.
createTrack :: M m => Id.Id -> Track.Track -> m TrackId
createTrack id track =
    insert (Id.TrackId id) track damageTrack stateTracks $ \tracks st ->
        st { stateTracks = tracks }

-- | Destroy the track and remove it from all the blocks it's in.  No-op if
-- the TrackId doesn't exist.
destroyTrack :: M m => TrackId -> m ()
destroyTrack trackId = do
    blocks <- blocksWithTrackId trackId
    forM_ blocks $ \(blockId, tracks) -> forM_ tracks $ \(tracknum, _) ->
        removeTrack blockId tracknum
    unsafeModify $ \st ->
        st { stateTracks = Map.delete trackId (stateTracks st) }
    damageTrack trackId

getTrackTitle :: M m => TrackId -> m Text
getTrackTitle = (Track.trackTitle <$>) . getTrack

setTrackTitle :: M m => TrackId -> Text -> m ()
setTrackTitle trackId text = modifyTrackTitle trackId (const text)

modifyTrackTitle :: M m => TrackId -> (Text -> Text) -> m ()
modifyTrackTitle trackId modify = do
    modifyTrack trackId $ \track ->
        track { Track.trackTitle = modify (Track.trackTitle track) }
    blockIds <- map fst <$> blocksWithTrackId trackId
    mapM_ updateSkeleton blockIds

setTrackBg :: M m => TrackId -> Color.Color -> m ()
setTrackBg trackId color = modifyTrack trackId $ \track ->
    track { Track.trackBg = color }

setRenderStyle :: M m => Track.RenderStyle -> TrackId -> m ()
setRenderStyle style trackId = modifyTrackRender trackId $
    \render -> render { Track.renderStyle = style }

modifyTrackRender :: M m => TrackId
    -> (Track.RenderConfig -> Track.RenderConfig) -> m ()
modifyTrackRender trackId modify = modifyTrack trackId $ \track ->
    track { Track.trackRender = modify (Track.trackRender track) }

modifyWaveform :: M m => TrackId -> (Bool -> Bool) -> m ()
modifyWaveform trackId modify = modifyTrack trackId $ \track ->
    track { Track.trackWaveform = modify (Track.trackWaveform track) }

-- | Find @track_id@ in all the blocks it exists in, and return the track info
-- for each tracknum at which @track_id@ lives.  Blocks with no matching tracks
-- won't be returned, so the return track lists will always be non-null.
blocksWithTrackId :: M m =>
    TrackId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
blocksWithTrackId trackId =
    findTracks ((== Just trackId) . Block.trackIdOf) <$> gets stateBlocks

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
insertEvents :: M m => TrackId -> [Event.Event] -> m ()
insertEvents trackId events_ = _modifyEvents trackId $ \oldEvents ->
    (Events.insert events oldEvents, eventsRange events)
    where
    events = map clipNegative $ dropWhile ((<0) . Event.start) events_
    clipNegative event
        | Event.end event < 0 = Event.setEnd 0 event
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
insertBlockEvents :: M m => BlockId -> TrackId -> [Event.Event] -> m ()
insertBlockEvents blockId trackId events = do
    end <- blockEnd blockId
    -- allow_zero=True because zero-dur events at the end of a block are used
    -- for negative/final notes.
    insertEvents trackId (Events.clipList True end events)

insertEvent :: M m => TrackId -> Event.Event -> m ()
insertEvent trackId event = insertEvents trackId [event]

getEvents :: M m => TrackId -> m Events.Events
getEvents trackId = Track.trackEvents <$> getTrack trackId

-- | Modify the events on a track, and assume the entire track has been
-- damaged.
modifyEvents :: M m => TrackId -> (Events.Events -> Events.Events) -> m ()
modifyEvents trackId f = _modifyEvents trackId $ \events ->
    (f events, Ranges.everything)

modifyEventsRange :: M m => TrackId -> Events.Range
    -> (Events.Events -> Events.Events) -> m ()
modifyEventsRange trackId range modify = _modifyEvents trackId $ \events ->
    (process events, uncurry Ranges.range (Events.rangeTimes range))
    where
    -- A range to the end should be inclusive, because I frequently have a
    -- positive event at the end.
    process events = (pre <> modify within <> post)
        where (pre, within, post) = Events.splitRange range events

modifyEventsFrom :: M m => TrackId -> TrackTime
    -> (Events.Events -> Events.Events) -> m ()
modifyEventsFrom trackId start modify = do
    range <- rangeFrom trackId start
    modifyEventsRange trackId range modify

-- | Just like 'modify_events', except that it expects you only modified a few
-- events, and will only emit damage for the changed parts.
modifySomeEvents :: M m => TrackId -> (Events.Events -> Events.Events)
    -> m ()
modifySomeEvents trackId f = _modifyEvents trackId $ \events ->
    let newEvents = f events
    in (newEvents, calculateDamage events newEvents)

calculateDamage :: Events.Events -> Events.Events -> Ranges.Ranges TrackTime
calculateDamage old new =
    Ranges.sortedRanges $ foldr f [] $
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
removeEvent :: M m => TrackId -> Event.Event -> m ()
removeEvent trackId event = _modifyEvents trackId $ \events ->
    case Events.at t (Event.orientation event) events of
        Nothing -> (events, Ranges.nothing)
        Just event ->
            ( Events.remove (Events.Point t (Event.orientation event)) events
            , eventsRange [event]
            )
    where t = Event.start event

-- | Just like @mapM_ (remove_event track_id)@ but more efficient.
-- TODO at least I hope, it got sort of complicated.
removeEvents :: M m => TrackId -> [Event.Event] -> m ()
removeEvents _ [] = pure ()
removeEvents trackId [event] = removeEvent trackId event
removeEvents trackId events = do
    removeEventsRange trackId $
        Events.Range (Event.min first) (Event.max last)
    when (Event.isNegative first) $
        removeEvent trackId first
    when (Event.isPositive last) $
        removeEvent trackId last
    where
    -- Events is non-empty due to the pattern match above.
    Just first = Lists.minimumOn Event.start events
    Just last = Lists.maximumOn Event.start events

removeEventsRange :: M m => TrackId -> Events.Range -> m ()
removeEventsRange trackId range =
    modifyEventsRange trackId range (const mempty)

-- | Get the end of the last event of the block.
trackEventEnd :: M m => TrackId -> m TrackTime
trackEventEnd = fmap Events.timeEnd . getEvents

rangeFrom :: M m => TrackId -> TrackTime -> m Events.Range
rangeFrom trackId start =
    Events.Range start . (+1) <$> trackEventEnd trackId
    -- +1 to get a final 0 dur positive event.

-- ** util

-- | Don't use this to modify the events, because it won't create damage.
-- TODO should I try to protect against that?
-- TODO except now there's only one kind of track damage, so it doesn't matter
modifyTrack :: M m => TrackId -> (Track.Track -> Track.Track) -> m ()
modifyTrack trackId f = do
    getTrack trackId -- Throw if track_id doesn't exist.
    unsafeModify $ \st ->
        st { stateTracks = Map.adjust f trackId (stateTracks st) }
    damageTrack trackId

-- | Low level modify events.  The function can modify Events however it
-- pleases, but should return Ranges indicating where the modification
-- happened.  This must be done manually, and there is no enforcement, but if
-- you get it wrong, 'Update.UiDamage' will be wrong, which will mess up UI
-- updates, undo, rederivation, etc.
--
-- TODO could figure this out automatically if Events supported efficient diff.
_modifyEvents :: M m => TrackId
    -> (Events.Events -> (Events.Events, Ranges.Ranges TrackTime))
    -> m ()
_modifyEvents trackId f = do
    track <- getTrack trackId
    let (newEvents, ranges) = f (Track.trackEvents track)
        newTrack = track { Track.trackEvents = newEvents }
    unsafeModify $ \st ->
        st { stateTracks = Map.insert trackId newTrack (stateTracks st) }
    -- Force out whatever transformations might be in the new events.  The
    -- main reason is to force out any IO exceptions that might be hiding in
    -- REPL expressions, but it seems better for memory in general to keep
    -- State in normal form.
    DeepSeq.deepseq newEvents $ damage $ Update.trackDamage trackId ranges

eventsRange :: [Event.Event] -> Ranges.Ranges TrackTime
eventsRange events = case minmax events of
    Just (emin, emax) -> Ranges.range emin emax
    Nothing -> Ranges.nothing
    where
    minmax (e:es) = Just $ go (Event.min e) (Event.max e) es
    minmax [] = Nothing
    go !emin !emax (e:es) =
        go (min emin (Event.min e)) (max emax (Event.max e)) es
    go emin emax [] = (emin, emax)

-- * ruler

getRuler :: M m => RulerId -> m Ruler.Ruler
getRuler rulerId
    | rulerId == noRuler = pure Ruler.empty
    | otherwise = get >>= lookupId rulerId . stateRulers

lookupRuler :: M m => RulerId -> m (Maybe Ruler.Ruler)
lookupRuler rulerId = get >>= pure . Map.lookup rulerId . stateRulers

allRulerIds :: M m => m [RulerId]
allRulerIds = gets (Map.keys . stateRulers)

-- | Insert the given ruler with the given ID.
--
-- Throw if the RulerId already exists.
createRuler :: M m => Id.Id -> Ruler.Ruler -> m RulerId
createRuler id ruler
    -- no_ruler is global and assumed to always exist.
    | id == Id.unpackId noRuler = throw "can't insert no-ruler"
    | otherwise = insert (Id.RulerId id) ruler damageRuler stateRulers $
        \rulers st -> st { stateRulers = rulers }

-- | Destroy the ruler and remove it from all the blocks it's in.
destroyRuler :: M m => RulerId -> m ()
destroyRuler rulerId = do
    blocks <- blocksWithRulerId rulerId
    forM_ blocks $ \(blockId, tracks) -> do
        let tracknums = map fst tracks
            setr i = if i `elem` tracknums
                then Block.setRulerId noRuler else id
            deruler (i, track) = Block.modifyId (setr i) track
        modifyBlock blockId $ \block -> block { Block.blockTracks =
            map deruler (Lists.enumerate (Block.blockTracks block)) }
    unsafeModify $ \st ->
        st { stateRulers = Map.delete rulerId (stateRulers st) }
    damageRuler rulerId

modifyRuler :: M m => RulerId -> (Ruler.Ruler -> Either Text Ruler.Ruler)
    -> m ()
modifyRuler rulerId modify = do
    when (rulerId == noRuler) $
        throw "can't modify no_ruler"
    ruler <- getRuler rulerId
    let msg = "modify_ruler " <> pretty rulerId <> ": "
    modified <- requireRight (msg<>) $ modify ruler
    unsafeModify $ \st ->
        st { stateRulers = Map.insert rulerId modified (stateRulers st) }
    damageRuler rulerId

rulerOf :: M m => BlockId -> m RulerId
rulerOf blockId = require ("no ruler in " <> showt blockId)
    =<< Lists.head . Block.blockRulerIds <$> getBlock blockId

rulersOf :: M m => BlockId -> m [RulerId]
rulersOf blockId = Lists.unique . Block.blockRulerIds <$> getBlock blockId

-- | Just like 'blocks_with_track_id' except for ruler_id.
blocksWithRulerId :: M m =>
    RulerId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
blocksWithRulerId rulerId =
    findTracks ((== Just rulerId) . Block.rulerIdOf) <$> gets stateBlocks

-- | Since all TracklikeIds must have a ruler, all States have a special empty
-- ruler that can be used in a \"no ruler\" situation.
--
-- This RulerId is implicitly present in every block.  It's not actually in
-- 'state_rulers' to avoid it getting renamed or deleted, but 'get_ruler' will
-- pretend it exists.  As long as everyone that cares about no_ruler (which is
-- only 'verify' and 'get_tracklike' for "Ui.Sync") uses 'get_ruler' then
-- they won't be confused by tracks that have no_ruler.
noRuler :: RulerId
noRuler = Id.RulerId (Id.global "-no-ruler-")


-- * util

findTracks :: (Block.TracklikeId -> Bool) -> Map BlockId Block.Block
    -> [(BlockId, [(TrackNum, Block.TracklikeId)])]
findTracks f blocks = do
    (bid, b) <- Map.assocs blocks
    let tracks = getTracks b
    guard (not (null tracks))
    pure (bid, tracks)
    where
    allTracks block = Lists.enumerate (Block.blockTracks block)
    getTracks block =
        [ (tracknum, Block.tracklikeId track)
        | (tracknum, track) <- allTracks block, f (Block.tracklikeId track)
        ]

-- | Lookup @map!key@, throwing if it doesn't exist.
lookupId :: (HasCallStack, Ord k, Show k, M m) => k -> Map k a -> m a
lookupId key map = case Map.lookup key map of
    Nothing -> throw $ "State.lookup: unknown " <> showt key
    Just val -> pure val

-- | Insert @val@ at @key@ in @get_map state@, throwing if it already exists.
-- Put the map back into @state@ by applying @set_map new_map state@ to it.
insert :: (M m, Ord k, Show k)
    => k -> a
    -> (k -> m ())
    -> (State -> Map k a)
    -> (Map k a -> State -> State)
    -> m k
insert key val updated getMap setMap = do
    state <- get
    when (key `Map.member` getMap state) $
        throw $ showt key <> " already exists"
    unsafePut (setMap (Map.insert key val (getMap state)) state)
    updated key
    pure key

-- | Modify the @i@th element of @xs@ by applying @f@ to it.
modifyAt :: M m => Text -> [a] -> Int -> (a -> a) -> m [a]
modifyAt msg xs i f = case post of
    [] -> throw $ msg <> ": can't replace index " <> showt i
        <> " of list with length " <> showt (length xs)
    (elt:rest) -> pure (pre ++ f elt : rest)
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
requireValid :: M m => Text -> StateId [Text] -> m ()
requireValid caller verify = do
    state <- get
    case runId state verify of
        Left err -> throw $ caller <> ": error validating: " <> showt err
        Right (errs, state, _)
            | null errs -> pure ()
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
verify state = case runId state fixState of
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
quickVerify :: Update.UiDamage -> State -> Either String (State, [Text])
quickVerify damage state = case runId state quickFix of
    Left err -> Left $ prettys err
    Right (errs, state, _) -> Right (state, errs)
    where
    quickFix = do
        st <- get
        mapM_ verifyBlock $ Map.elems $
            Map.restrictKeys (stateBlocks st) (Update._blocks damage)
        -- Disappearing views can happen if you undo past a block rename.
        -- In that case I should track the rename rather than disappearing
        -- the view, but in any case I don't want dangling ViewIds and
        -- disappearing the view is relatively harmless.
        concatMapM (uncurry verifyView) $ Map.toList $
            Map.restrictKeys (stateViews st) (Update._views damage)
    verifyBlock block = do
        mapM_ getTrack (Block.blockTrackIds block)
        mapM_ getRuler (Block.blockRulerIds block)

fixState :: M m => m [Text]
fixState = do
    views <- gets (Map.toList . stateViews)
    viewErrs <- concatMapM (uncurry verifyView) views
    blocks <- gets (Map.toList . stateBlocks)
    blockErrs <- concatMapM (uncurry fixBlock) blocks
    pure $ viewErrs ++ blockErrs

-- | Drop views with invalid BlockIds.
verifyView :: M m => ViewId -> Block.View -> m [Text]
verifyView viewId view = do
    block <- lookupBlock (Block.viewBlock view)
    case block of
        Just _ -> pure []
        Nothing -> do
            destroyView viewId
            pure [showt viewId <> ": dropped because of invalid "
                <> showt (Block.viewBlock view)]

fixBlock :: M m => BlockId -> Block.Block -> m [Text]
fixBlock blockId block =
    map ((showt blockId <> ": ") <>) . mconcat <$> sequence
        [ fixTrackIds blockId block
        , uniqueTrackIds blockId block
        , fixRulerIds blockId block
        , fixSkeleton blockId block
        , concatMapM (fixMerged blockId) tracks
        , fixIntegratedBlock blockId block
        , fixIntegratedTracks blockId block
        ]
    where tracks = zip [0..] (Block.blockTracks block)

-- | Drop invalid track ids.
fixTrackIds :: M m => BlockId -> Block.Block -> m [Text]
fixTrackIds blockId block = do
    allTrackIds <- gets stateTracks
    let isValid = (`Map.member` allTrackIds)
    let invalid = filter (not . isValid . snd) (blockEventTracknums block)
    mapM_ (removeTrack blockId . fst) invalid
    pure ["tracknum " <> showt tracknum <> ": dropped invalid "
        <> showt trackId | (tracknum, trackId) <- invalid]

-- | Replace invalid ruler ids with no_ruler.
fixRulerIds :: M m => BlockId -> Block.Block -> m [Text]
fixRulerIds _blockId _block = pure [] -- TODO

-- | Each TrackId of a block is unique.
uniqueTrackIds :: M m => BlockId -> Block.Block -> m [Text]
uniqueTrackIds blockId block = do
    let invalid = concatMap snd $ snd $
            Lists.partitionDups snd (blockEventTracknums block)
    mapM_ (removeTrack blockId . fst) invalid
    pure ["tracknum " <> showt tracknum <> ": dropped duplicate "
        <> showt trackId | (tracknum, trackId) <- invalid]

-- | Skeleton tracknums in range.
fixSkeleton :: M m => BlockId -> Block.Block -> m [Text]
fixSkeleton _blockId _block = pure [] -- TODO

-- | Strip invalid Block.track_merged.
fixMerged :: M m => BlockId -> (TrackNum, Block.Track) -> m [Text]
fixMerged blockId (tracknum, track) = do
    allTrackIds <- gets stateTracks
    let isValid = (`Map.member` allTrackIds)
    let (valid, invalid) = Set.partition isValid (Block.trackMerged track)
    unless (Set.null invalid) $
        modifyBlockTrack blockId tracknum
            (const $ track { Block.trackMerged = valid })
    pure ["tracknum " <> showt tracknum <> ": stripped invalid merged "
        <> showt trackId | trackId <- Set.toList invalid]

-- | Drop block_integrated if the source BlockId doesn't exist, and strip out
-- TrackDestinations whose TrackIds aren't in this block.
-- 'Event.clear_integration' on any events that no longer have a source.
fixIntegratedBlock :: M m => BlockId -> Block.Block -> m [Text]
fixIntegratedBlock blockId block = do
    blocks <- gets stateBlocks
    let (sourceGone, integrated, errs) =
            fix blocks (Block.blockIntegrated block)
    unless (null errs) $
        modifyBlock blockId $ \block -> block
            { Block.blockIntegrated = integrated }
    when sourceGone $
        mapM_ clearIntegration (Block.blockTrackIds block)
    pure errs
    where
    trackIds = Block.blockTrackIds block
    fix _ Nothing = (False, Nothing, [])
    fix blocks (Just (sourceId, dests)) = case Map.lookup sourceId blocks of
        Nothing ->
            ( True
            , Nothing
            , ["removed invalid integrated block: " <> showt sourceId]
            )
        Just source -> (False, Just (sourceId, valid), errs)
            where
            (valid, errs) = fixTrackDestinations
                ("block of " <> showt sourceId)
                (Block.blockTrackIds source) trackIds dests

-- | Drop integrated tracks whose source TrackId isn't in this block, and
-- TrackDestinations whose TrackIds aren't in this block.
-- 'Event.clear_integration' on any events that no longer have a source.
--
-- TODO
-- - No TrackIds duplicated between DeriveDestinations.
-- - No TrackIds duplicated across integrated tracks.
fixIntegratedTracks :: M m => BlockId -> Block.Block -> m [Text]
fixIntegratedTracks blockId block = do
    let gone = concat
            [ Block.destTrackIds dests
            | (trackId, dests) <- Block.blockIntegratedTracks block
            , trackId `notElem` trackIds
            ]
    let (dests, errs) = bimap Maybe.catMaybes concat $ unzip $
            map fix (Block.blockIntegratedTracks block)
    unless (null errs) $ do
        modifyBlock blockId $ \block -> block
            { Block.blockIntegratedTracks = dests }
        mapM_ clearIntegration gone
    pure errs
    where
    trackIds = Block.blockTrackIds block
    fix (trackId, dests)
        | trackId `notElem` trackIds =
            (Nothing, ["removed invalid integrated track: " <> showt trackId])
        | otherwise = (Just (trackId, valid), errs)
        where
        (valid, errs) = fixTrackDestinations ("track of " <> showt trackId)
            trackIds trackIds dests

fixTrackDestinations :: Text -> [TrackId] -> [TrackId]
    -> Block.TrackDestinations -> (Block.TrackDestinations, [Text])
fixTrackDestinations errMsg sourceTrackIds trackIds d = case d of
    Block.DeriveDestinations dests ->
        ( Block.DeriveDestinations valid
        , errs (map Block.noteDestTrackIds invalid)
        )
        where (valid, invalid) = List.partition deriveValid dests
    Block.ScoreDestinations dests ->
        (Block.ScoreDestinations valid, errs (map scoreTrackIds invalid))
        where (valid, invalid) = List.partition scoreValid dests
    where
    errs invalid = ["integrated " <> errMsg
        <> ": track destination has track ids not in the right block: "
        <> pretty dest | dest <- invalid]
    scoreTrackIds (sourceId, (destId, _)) = (sourceId, destId)
    deriveValid = all (`elem` trackIds) . Block.noteDestTrackIds
    scoreValid (sourceId, (destId, _index)) =
        sourceId `elem` sourceTrackIds && destId `elem` trackIds

clearIntegration :: M m => TrackId -> m ()
clearIntegration trackId = modifyEvents trackId $
    Events.mapEvents Event.clearIntegration

blockEventTracknums :: Block.Block -> [(TrackNum, TrackId)]
blockEventTracknums block =
    [(tracknum, trackId) | (tracknum, Just trackId) <- zip [0..] trackIds]
    where trackIds = map Block.trackId (Block.blockTracks block)

-- * IDs

-- | Read an ID of the form \"namespace/name\", or just \"name\", filling in
-- the current namespace if it's not present.
readId :: (HasCallStack, Id.Ident a, M m) => Text -> m a
readId name = do
    ns <- getNamespace
    require ("invalid characters in id name: " <> showt name) $
        Id.make $ Id.id ns name

namespace :: M m => Text -> m Id.Namespace
namespace ns = do
    unless (Id.validSymbol ns) $
        throw $ "invalid characters in namespace: " <> showt ns
    pure $ Id.namespace ns

-- * damage

damageView :: M m => ViewId -> m ()
damageView = damage . Update.viewDamage

damageBlock :: M m => BlockId -> m ()
damageBlock = damage . Update.blockDamage

damageTrack :: M m => TrackId -> m ()
damageTrack trackId = damage $ Update.trackDamage trackId Ranges.everything

damageRuler :: M m => RulerId -> m ()
damageRuler = damage . Update.rulerDamage

damageScore :: M m => m ()
damageScore = damage $ mempty { Update._scoreDamage = True }
