-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This module is sister to "Derive.Deriver.Lib", except that it contains
    functions which are normally only used by the built-in track derivation
    scheme, and are not used when writing most normal calls.
-}
module Derive.Deriver.Internal where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)

import qualified Util.Log as Log
import qualified Util.Seed as Seed
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Warp as Warp

import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Derive.Deriver.Monad
import           Global
import           Types


-- * generic state access

getDynamic :: (Dynamic -> a) -> Deriver a
getDynamic f = gets (f . stateDynamic)

getConstant :: (Constant -> a) -> Deriver a
getConstant f = gets (f . stateConstant)

-- | This is a little different from Reader.local because only a portion of
-- the state is used Reader-style.
--
-- Note that this doesn't restore the state on an exception.  I think this
-- is ok because exceptions are always \"caught\" at the event evaluation
-- level since it runs each one separately.  Since the state dynamic state
-- (i.e. except Collect) from the sub derivation is discarded, whatever state
-- it's in after the exception shouldn't matter.
local :: (Dynamic -> Dynamic) -> Deriver a -> Deriver a
local modifyDynamic = localm (pure . modifyDynamic)

localm :: (Dynamic -> Deriver Dynamic) -> Deriver a -> Deriver a
localm modifyDynamic deriver = do
    st <- get
    new <- modifyDynamic (stateDynamic st)
    put $ st { stateDynamic = new }
    result <- deriver
    modify $ \new -> new { stateDynamic = stateDynamic st }
    pure result

-- | A version of 'local' that catches exceptions and ignores any changes to
-- Collect.  This is appropriate for sub-calls that are below normal track
-- derivation.
detachedLocal :: (Dynamic -> Dynamic) -> Deriver a -> Deriver (Either Error a)
detachedLocal modifyDynamic deriver = do
    st <- get
    let (result, _, logs) = run
            (st { stateDynamic = modifyDynamic (stateDynamic st) }) deriver
    mapM_ Log.write logs
    pure result

setThreaded :: Threaded -> Deriver ()
setThreaded threaded = modify $ \st -> st { stateThreaded = threaded }

-- * Collect

-- | Collect is only ever accumulated.
--
-- Direct modification would be potentially more efficient, but according to
-- profiling it doesn't make a difference.
mergeCollect :: Collect -> Deriver ()
mergeCollect c = modify $ \st -> st { stateCollect = stateCollect st <> c }
    -- I append the Collect, which means that I wind up with the first instance
    -- for Maps with duplicate keys.  This seems a bit more intuitive than the
    -- last one.

-- | Run with an empty Collect, restore the original Collect, and return the
-- sub-deriver's Collect.
localCollect :: Deriver a -> Deriver (a, Collect)
localCollect deriver = do
    old <- gets stateCollect
    modify $ \st -> st { stateCollect = mempty }
    result <- deriver
    subCollect <- gets stateCollect
    modify $ \st -> st { stateCollect = old }
    pure (result, subCollect)

-- | Modify the 'collect_warp_map' to reduce the start and end by the given
-- times.  This is useful if you're going to clip off some events.  The
-- TrackWarps, and hence playback cursor, can't know you're going to do this,
-- so you have to tell it.
trimTrackWarps :: Maybe RealTime -> Maybe RealTime -> Deriver a -> Deriver a
trimTrackWarps start end = withCollect $ \st -> st
    { collectWarpMap = trim <$> collectWarpMap st }
    where
    trim (TrackWarp.Track s e warp blockId trackId) =
        TrackWarp.Track (maybe s (max s) start) (maybe e (min e) end)
            warp blockId trackId

-- | Run the deriver and modify the Collect it returns.
withCollect :: (Collect -> Collect) -> Deriver a -> Deriver a
withCollect modify deriver = do
    (a, collect) <- localCollect deriver
    mergeCollect (modify collect)
    pure a

-- | TODO this is sketchy, you're supposed to use 'merge_collect'.
modifyCollect :: (Collect -> Collect) -> Deriver ()
modifyCollect f = modify $ \st -> st { stateCollect = f (stateCollect st) }

-- * environ

getEnviron :: Deriver DeriveT.Environ
getEnviron = getDynamic stateEnviron

-- | Figure out the current block and track, and record the current environ
-- in the Collect.  It only needs to be recorded once per track.
recordTrackDynamic :: Dynamic -> Maybe TrackDynamic
recordTrackDynamic dyn = case Stack.blockTrackOf (stateStack dyn) of
    Just (bid, tid) -> Just $! Map.singleton (bid, tid) $!
        -- If I don't clear the inversion state, any inverting call that uses
        -- this dynamic will throw a double inversion error.  Also the function
        -- closure probably causes drag.
        dyn { stateInversion = NotInverted }
    Nothing -> Nothing

-- | 'record_track_dynamic' for when I already know BlockId and TrackId.
recordTrackDynamicFor :: BlockId -> TrackId -> Deriver ()
recordTrackDynamicFor blockId trackId = do
    dynamic <- gets stateDynamic
    mergeCollect $ mempty
        { collectTrackDynamic = Map.singleton (blockId, trackId) dynamic }

-- * misc Dynamic state

withDefaultMerge :: Map ScoreT.Control Merger -> Deriver a -> Deriver a
withDefaultMerge defaults = local $ \st -> st
    { stateControlMergeDefaults =
        defaults <> stateControlMergeDefaults st
    }

-- * cache

withControlDamage :: ControlDamage -> Deriver derived -> Deriver derived
withControlDamage damage = local $ \st ->
    st { stateControlDamage = damage }

addBlockDep :: BlockId -> Deriver ()
addBlockDep blockId = mergeCollect $ mempty
    { collectBlockDeps = BlockDeps (Set.singleton blockId) }

-- * ui state

getUiState :: (Ui.State -> a) -> Deriver a
getUiState f = gets (f . stateUi . stateConstant)

getUiConfig :: (UiConfig.Config -> a) -> Deriver a
getUiConfig f = getUiState (f . Ui.stateConfig)

-- | Because Deriver is not a UiStateMonad.
--
-- TODO I suppose it could be, but then I'd be tempted to make
-- a ReadOnlyUiStateMonad.  And I'd have to merge the exceptions.
-- Or just rethrow, right?
getTrack :: TrackId -> Deriver Track.Track
getTrack trackId = lookupId trackId =<< getUiState Ui.stateTracks

getBlock :: BlockId -> Deriver Block.Block
getBlock blockId = lookupId blockId =<< getUiState Ui.stateBlocks

-- | Evaluate a Ui.M computation, rethrowing any errors.
evalUi :: HasCallStack => Ui.StateId a -> Deriver a
evalUi action = do
    uiState <- getUiState id
    let rethrow exc = throw $ pretty exc
    either rethrow pure (Ui.eval uiState action)

-- | Lookup @map!key@, throwing if it doesn't exist.
lookupId :: (Ord k, Show k) => k -> Map k a -> Deriver a
lookupId key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " <> showt key
    Just val -> pure val

-- * stack

lookupCurrentBlockId :: Deriver (Maybe BlockId)
lookupCurrentBlockId = do
    stack <- getStack
    pure $ case [bid | Stack.Block bid <- Stack.innermost stack] of
        [] -> Nothing
        bid : _ -> Just bid

getCurrentBlockId :: Deriver BlockId
getCurrentBlockId =
    maybe (throw "get_current_block_id: no blocks in stack") pure
        =<< lookupCurrentBlockId

lookupCurrentTracknum :: Deriver (Maybe (BlockId, TrackNum))
lookupCurrentTracknum = do
    stack <- getStack
    case Stack.blockTrackOf stack of
        Nothing -> pure Nothing
        Just (blockId, trackId) -> do
            tracknum <- evalUi $ Ui.getTracknumOf blockId trackId
            pure $ Just (blockId, tracknum)

getCurrentTracknum :: Deriver (BlockId, TrackNum)
getCurrentTracknum =
    maybe (throw "get_current_tracknum") pure =<< lookupCurrentTracknum

-- | Make a quick trick block stack.
{-# SCC withStackBlock #-}
withStackBlock :: BlockId -> Deriver a -> Deriver a
withStackBlock = withStack . Stack.Block

-- | Make a quick trick track stack.
{-# SCC withStackTrack #-}
withStackTrack :: TrackId -> Deriver a -> Deriver a
withStackTrack = withStack . Stack.Track

{-# SCC withStackRegion #-}
withStackRegion :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
withStackRegion s e = withStack (Stack.Region s e)

{-# SCC withStackCall #-}
withStackCall :: CallName -> Deriver a -> Deriver a
withStackCall (CallName name) = withStack (Stack.Call name)

{-# SCC withStackSerial #-}
withStackSerial :: Int -> Deriver a -> Deriver a
withStackSerial = withStack . Stack.Serial

withStack :: Stack.Frame -> Deriver a -> Deriver a
withStack frame = localm $ \st -> do
    stack <- getStack
    when (Stack.length stack >= maxDepth) $
        throw $ "call stack too deep: " <> pretty frame
    pure $ addStackFrame frame st
    where
    -- A recursive loop will result in an unfriendly hang.  So limit the total
    -- nesting depth to catch those.  I could disallow all recursion, but this
    -- is more general.
    maxDepth = 100

{- | Add a new stack frame and hash it with the random seed.

    I skip Stack.Call for seed changes.  This is so I can use calls like
    log-seed to record the seed to hardcode it later, which is the whole point
    of doing this thing where I hash on every stack frame.  Otherwise, the
    presence of the log-seed call itself would be enough to change the seed.

    In addition, calculating the seed eagerly this way is actually pretty
    expensive, since the stack is constantly being updated, so updating it
    less frequently is good for performance.  It's possible I could update
    the seed lazily to reduce this cost, but not doing something at all
    is still cheaper than lazy, and can't be accidentally forced.

    The risk is that this makes the seed too stable, and things which should
    be randomized become identical.
-}
addStackFrame :: Stack.Frame -> Dynamic -> Dynamic
addStackFrame frame st = st
    { stateStack = Stack.add frame (stateStack st)
    , stateEnviron = (if shouldUpdateSeed frame then updateSeed else id)
        (stateEnviron st)
    }
    where
    shouldUpdateSeed (Stack.Call {}) = False
    shouldUpdateSeed _ = True
    updateSeed env =
        DeriveT.insert EnvKey.seed (DeriveT.num (update old)) env
        where
        old = maybe 0 ScoreT.valOf $
            DeriveT.constantVal =<< DeriveT.lookup EnvKey.seed env
    update :: Double -> Double
    update n = i2d (Seed.toSeed (floor n) frame)
    -- A Double should be able to hold up to 2^52, but that's still an
    -- annoyingly large number to write in a score, so restrict it further.
    i2d :: Int -> Double
    i2d i = fromIntegral (i `mod` 999)

getStack :: Deriver Stack.Stack
getStack = getDynamic stateStack

-- ** time and duration

-- | Times are types that can be converted to RealTime and ScoreTime.
class Time a where
    real :: a -> Deriver RealTime
    score :: a -> Deriver ScoreTime
    toDuration :: a -> DeriveT.Duration

instance Time ScoreTime where
    real = scoreToReal
    score = pure
    toDuration = DeriveT.ScoreDuration

instance Time RealTime where
    real = pure
    score = realToScore
    toDuration = DeriveT.RealDuration

instance Time DeriveT.Duration where
    real (DeriveT.RealDuration t) = real t
    real (DeriveT.ScoreDuration t) = real t
    score (DeriveT.RealDuration t) = score t
    score (DeriveT.ScoreDuration t) = score t
    toDuration = id

-- * warp

inRealTime :: Deriver a -> Deriver a
inRealTime = withWarp (const Warp.identity)

withWarp :: (Warp.Warp -> Warp.Warp) -> Deriver a -> Deriver a
withWarp f = local $ \st -> st { stateWarp = f (stateWarp st) }

getWarp :: Deriver Warp.Warp
getWarp = getDynamic stateWarp

at :: ScoreTime -> Deriver a -> Deriver a
at shift = withWarp $ Warp.shift shift

stretch :: ScoreTime -> Deriver a -> Deriver a
stretch factor = withWarp $ Warp.stretch factor

-- | 'at' and 'stretch' in one.  It's a little more efficient than using them
-- separately.  The order is stretch, then shift.
place :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
place shift stretch = withWarp $ Warp.stretch stretch . Warp.shift shift
    -- Warp.stretch and Warp.shift look like they're in the wrong order here,
    -- but they're not.  "Derive.Warp" for details.

-- | Compose warps.
warp :: Warp.Warp -> Deriver a -> Deriver a
warp w
    | Warp.isIdentity w = id
    | otherwise = withWarp (`Warp.compose` w)

-- ** track warp

-- | Start a new track warp for the current block_id.
--
-- This must be called for each block, and it must be called after the tempo is
-- warped for that block so it can install the new warp.
addNewTrackWarp :: Maybe TrackId -> Deriver ()
addNewTrackWarp maybeTrackId = do
    stack <- getStack
    blockId <- getCurrentBlockId
    start <- scoreToReal 0
    -- Use block_event_end instead of block_logical_range.  Otherwise, the play
    -- monitor can't go past the end of the ruler, while the player is
    -- perfectly happy to do so.
    end <- real =<< blockEventEnd blockId
    warp <- getWarp
    let track = TrackWarp.Track start end warp blockId maybeTrackId
    mergeCollect $ mempty { collectWarpMap = Map.singleton stack track }

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  This is therefore the logical
-- duration of the block, which may be shorter or lorger than the end of the
-- last event, or the ruler.
blockLogicalRange :: BlockId -> Deriver (TrackTime, TrackTime)
blockLogicalRange = evalUi . Ui.blockLogicalRange

-- | Get the duration of the block according to the last event.
blockEventEnd :: BlockId -> Deriver ScoreTime
blockEventEnd = evalUi . Ui.blockEventEnd


-- * track

-- | The deriver strips out tracks that can't be derived because they have no
-- notes.  But that means the track warps and track dynamics aren't recorded,
-- which means they don't have tempo or a playback monitor, which makes them
-- annoying.
recordEmptyTracks :: [TrackId] -> Deriver ()
recordEmptyTracks [] = pure ()
recordEmptyTracks trackIds = do
    blockId <- getCurrentBlockId
    mapM_ (recordEmptyTrack blockId) trackIds

recordEmptyTrack :: BlockId -> TrackId -> Deriver ()
recordEmptyTrack blockId trackId =
    recordTrackDynamicFor blockId trackId

-- * ControlFunction

getControlFunctionDynamic :: Deriver DeriveT.Dynamic
getControlFunctionDynamic = do
    ruler <- getRuler
    state <- get
    pure $ convertDynamic ruler (stateDynamic state)
        (stateEventSerial (stateThreaded state))

convertDynamic :: Ruler.Marklists -> Dynamic -> Stack.Serial
    -> DeriveT.Dynamic
convertDynamic ruler dyn serial = DeriveT.Dynamic
    { dynPitch = statePitch dyn
    , dynEnviron = stateEnviron dyn
    , dynEventSerial = serial
    , dynWarp = stateWarp dyn
    , dynRuler = ruler
    }

-- | Get the 'Ruler.meter' marklists, if there is a ruler track here.  This
-- is called in all contexts, due to 'control_at', so it has to be careful
-- to not require a ruler.
getRuler :: Deriver Ruler.Marklists
getRuler = lookupCurrentTracknum >>= \case
    Nothing -> pure mempty
    Just (blockId, tracknum) -> do
        state <- getUiState id
        pure $ either (const mempty) id $ Ui.eval state $ do
            rulerId <- fromMaybe Ui.noRuler <$>
                Ui.rulerTrackAt blockId tracknum
            Ruler.rulerMarklists <$> Ui.getRuler rulerId


-- * Threaded

modifyThreaded :: (Threaded -> Threaded) -> Deriver ()
modifyThreaded f = modify $ \state -> state
    { stateThreaded = f (stateThreaded state) }

-- | Increment 'state_event_serial'.
incrementEventSerial :: Deriver ()
incrementEventSerial = modifyThreaded $ \threaded -> threaded
    { stateEventSerial = stateEventSerial threaded + 1 }


-- * misc

-- | Am I deriving the toplevel block?
isRootBlock :: Deriver Bool
isRootBlock = isRootBlockStack <$> getStack
    where
    isRootBlockStack stack =
        case [bid | Stack.Block bid <- Stack.outermost stack] of
            [] -> True
            [_] -> True
            _ -> False
