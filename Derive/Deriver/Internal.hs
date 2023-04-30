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

import qualified Util.CallStack as CallStack
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

get_dynamic :: (Dynamic -> a) -> Deriver a
get_dynamic f = gets (f . state_dynamic)

get_constant :: (Constant -> a) -> Deriver a
get_constant f = gets (f . state_constant)

-- | This is a little different from Reader.local because only a portion of
-- the state is used Reader-style.
--
-- Note that this doesn't restore the state on an exception.  I think this
-- is ok because exceptions are always \"caught\" at the event evaluation
-- level since it runs each one separately.  Since the state dynamic state
-- (i.e. except Collect) from the sub derivation is discarded, whatever state
-- it's in after the exception shouldn't matter.
local :: (Dynamic -> Dynamic) -> Deriver a -> Deriver a
local modify_dynamic = localm (return . modify_dynamic)

localm :: (Dynamic -> Deriver Dynamic) -> Deriver a -> Deriver a
localm modify_dynamic deriver = do
    st <- get
    new <- modify_dynamic (state_dynamic st)
    put $ st { state_dynamic = new }
    result <- deriver
    modify $ \new -> new { state_dynamic = state_dynamic st }
    return result

-- | A version of 'local' that catches exceptions and ignores any changes to
-- Collect.  This is appropriate for sub-calls that are below normal track
-- derivation.
detached_local :: (Dynamic -> Dynamic) -> Deriver a -> Deriver (Either Error a)
detached_local modify_dynamic deriver = do
    st <- get
    let (result, _, logs) = run
            (st { state_dynamic = modify_dynamic (state_dynamic st) }) deriver
    mapM_ Log.write logs
    return result

set_threaded :: Threaded -> Deriver ()
set_threaded threaded = modify $ \st -> st { state_threaded = threaded }

-- * Collect

-- | Collect is only ever accumulated.
--
-- Direct modification would be potentially more efficient, but according to
-- profiling it doesn't make a difference.
merge_collect :: Collect -> Deriver ()
merge_collect c = modify $ \st -> st { state_collect = state_collect st <> c }
    -- I append the Collect, which means that I wind up with the first instance
    -- for Maps with duplicate keys.  This seems a bit more intuitive than the
    -- last one.

-- | Run with an empty Collect, restore the original Collect, and return the
-- sub-deriver's Collect.
local_collect :: Deriver a -> Deriver (a, Collect)
local_collect deriver = do
    old <- gets state_collect
    modify $ \st -> st { state_collect = mempty }
    result <- deriver
    sub_collect <- gets state_collect
    modify $ \st -> st { state_collect = old }
    return (result, sub_collect)

-- | Modify the 'collect_warp_map' to reduce the start and end by the given
-- times.  This is useful if you're going to clip off some events.  The
-- TrackWarps, and hence playback cursor, can't know you're going to do this,
-- so you have to tell it.
trim_track_warps :: Maybe RealTime -> Maybe RealTime -> Deriver a -> Deriver a
trim_track_warps start end = with_collect $ \st -> st
    { collect_warp_map = trim <$> collect_warp_map st }
    where
    trim (TrackWarp.Track s e warp block_id track_id) =
        TrackWarp.Track (maybe s (max s) start) (maybe e (min e) end)
            warp block_id track_id

-- | Run the deriver and modify the Collect it returns.
with_collect :: (Collect -> Collect) -> Deriver a -> Deriver a
with_collect modify deriver = do
    (a, collect) <- local_collect deriver
    merge_collect (modify collect)
    return a

-- | TODO this is sketchy, you're supposed to use 'merge_collect'.
modify_collect :: (Collect -> Collect) -> Deriver ()
modify_collect f = modify $ \st -> st { state_collect = f (state_collect st) }

-- * environ

get_environ :: Deriver DeriveT.Environ
get_environ = get_dynamic state_environ

-- | Figure out the current block and track, and record the current environ
-- in the Collect.  It only needs to be recorded once per track.
record_track_dynamic :: Dynamic -> Maybe TrackDynamic
record_track_dynamic dyn = case Stack.block_track_of (state_stack dyn) of
    Just (bid, tid) -> Just $! Map.singleton (bid, tid) $!
        -- If I don't clear the inversion state, any inverting call that uses
        -- this dynamic will throw a double inversion error.  Also the function
        -- closure probably causes drag.
        dyn { state_inversion = NotInverted }
    Nothing -> Nothing

-- | 'record_track_dynamic' for when I already know BlockId and TrackId.
record_track_dynamic_for :: BlockId -> TrackId -> Deriver ()
record_track_dynamic_for block_id track_id = do
    dynamic <- gets state_dynamic
    merge_collect $ mempty
        { collect_track_dynamic = Map.singleton (block_id, track_id) dynamic }

-- * misc Dynamic state

with_default_merge :: Map ScoreT.Control Merger -> Deriver a -> Deriver a
with_default_merge defaults = local $ \st -> st
    { state_control_merge_defaults =
        defaults <> state_control_merge_defaults st
    }

-- * cache

with_control_damage :: ControlDamage -> Deriver derived -> Deriver derived
with_control_damage damage = local $ \st ->
    st { state_control_damage = damage }

add_block_dep :: BlockId -> Deriver ()
add_block_dep block_id = merge_collect $ mempty
    { collect_block_deps = BlockDeps (Set.singleton block_id) }

-- * ui state

get_ui_state :: (Ui.State -> a) -> Deriver a
get_ui_state f = gets (f . state_ui . state_constant)

get_ui_config :: (UiConfig.Config -> a) -> Deriver a
get_ui_config f = get_ui_state (f . Ui.state_config)

-- | Because Deriver is not a UiStateMonad.
--
-- TODO I suppose it could be, but then I'd be tempted to make
-- a ReadOnlyUiStateMonad.  And I'd have to merge the exceptions.
-- Or just rethrow, right?
get_track :: TrackId -> Deriver Track.Track
get_track track_id = lookup_id track_id =<< get_ui_state Ui.state_tracks

get_block :: BlockId -> Deriver Block.Block
get_block block_id = lookup_id block_id =<< get_ui_state Ui.state_blocks

-- | Evaluate a Ui.M computation, rethrowing any errors.
eval_ui :: CallStack.Stack => Ui.StateId a -> Deriver a
eval_ui action = do
    ui_state <- get_ui_state id
    let rethrow exc = throw $ pretty exc
    either rethrow return (Ui.eval ui_state action)

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k) => k -> Map k a -> Deriver a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " <> showt key
    Just val -> return val

-- * stack

lookup_current_block_id :: Deriver (Maybe BlockId)
lookup_current_block_id = do
    stack <- get_stack
    return $ case [bid | Stack.Block bid <- Stack.innermost stack] of
        [] -> Nothing
        bid : _ -> Just bid

get_current_block_id :: Deriver BlockId
get_current_block_id =
    maybe (throw "get_current_block_id: no blocks in stack") return
        =<< lookup_current_block_id

lookup_current_tracknum :: Deriver (Maybe (BlockId, TrackNum))
lookup_current_tracknum = do
    stack <- get_stack
    case Stack.block_track_of stack of
        Nothing -> return Nothing
        Just (block_id, track_id) -> do
            tracknum <- eval_ui $ Ui.get_tracknum_of block_id track_id
            return $ Just (block_id, tracknum)

get_current_tracknum :: Deriver (BlockId, TrackNum)
get_current_tracknum =
    maybe (throw "get_current_tracknum") return =<< lookup_current_tracknum

-- | Make a quick trick block stack.
{-# SCC with_stack_block #-}
with_stack_block :: BlockId -> Deriver a -> Deriver a
with_stack_block = with_stack . Stack.Block

-- | Make a quick trick track stack.
{-# SCC with_stack_track #-}
with_stack_track :: TrackId -> Deriver a -> Deriver a
with_stack_track = with_stack . Stack.Track

{-# SCC with_stack_region #-}
with_stack_region :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
with_stack_region s e = with_stack (Stack.Region s e)

{-# SCC with_stack_call #-}
with_stack_call :: CallName -> Deriver a -> Deriver a
with_stack_call (CallName name) = with_stack (Stack.Call name)

{-# SCC with_stack_serial #-}
with_stack_serial :: Int -> Deriver a -> Deriver a
with_stack_serial = with_stack . Stack.Serial

with_stack :: Stack.Frame -> Deriver a -> Deriver a
with_stack frame = localm $ \st -> do
    stack <- get_stack
    when (Stack.length stack >= max_depth) $
        throw $ "call stack too deep: " <> pretty frame
    return $ add_stack_frame frame st
    where
    -- A recursive loop will result in an unfriendly hang.  So limit the total
    -- nesting depth to catch those.  I could disallow all recursion, but this
    -- is more general.
    max_depth = 100

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
add_stack_frame :: Stack.Frame -> Dynamic -> Dynamic
add_stack_frame frame st = st
    { state_stack = Stack.add frame (state_stack st)
    , state_environ = (if should_update_seed frame then update_seed else id)
        (state_environ st)
    }
    where
    should_update_seed (Stack.Call {}) = False
    should_update_seed _ = True
    update_seed env =
        DeriveT.insert EnvKey.seed (DeriveT.num (update old)) env
        where
        old = maybe 0 ScoreT.val_of $
            DeriveT.constant_val =<< DeriveT.lookup EnvKey.seed env
    update :: Double -> Double
    update n = i2d (Seed.to_seed (floor n) frame)
    -- A Double should be able to hold up to 2^52, but that's still an
    -- annoyingly large number to write in a score, so restrict it further.
    i2d :: Int -> Double
    i2d i = fromIntegral (i `mod` 999)

get_stack :: Deriver Stack.Stack
get_stack = get_dynamic state_stack

-- ** time and duration

-- | Times are types that can be converted to RealTime and ScoreTime.
class Time a where
    real :: a -> Deriver RealTime
    score :: a -> Deriver ScoreTime
    to_duration :: a -> DeriveT.Duration

instance Time ScoreTime where
    real = score_to_real
    score = return
    to_duration = DeriveT.ScoreDuration

instance Time RealTime where
    real = return
    score = real_to_score
    to_duration = DeriveT.RealDuration

instance Time DeriveT.Duration where
    real (DeriveT.RealDuration t) = real t
    real (DeriveT.ScoreDuration t) = real t
    score (DeriveT.RealDuration t) = score t
    score (DeriveT.ScoreDuration t) = score t
    to_duration = id

-- * warp

in_real_time :: Deriver a -> Deriver a
in_real_time = with_warp (const Warp.identity)

with_warp :: (Warp.Warp -> Warp.Warp) -> Deriver a -> Deriver a
with_warp f = local $ \st -> st { state_warp = f (state_warp st) }

get_warp :: Deriver Warp.Warp
get_warp = get_dynamic state_warp

at :: ScoreTime -> Deriver a -> Deriver a
at shift = with_warp $ Warp.shift shift

stretch :: ScoreTime -> Deriver a -> Deriver a
stretch factor = with_warp $ Warp.stretch factor

-- | 'at' and 'stretch' in one.  It's a little more efficient than using them
-- separately.  The order is stretch, then shift.
place :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
place shift stretch = with_warp $ Warp.stretch stretch . Warp.shift shift
    -- Warp.stretch and Warp.shift look like they're in the wrong order here,
    -- but they're not.  "Derive.Warp" for details.

-- | Compose warps.
warp :: Warp.Warp -> Deriver a -> Deriver a
warp w
    | Warp.is_identity w = id
    | otherwise = with_warp (`Warp.compose` w)

-- ** track warp

-- | Start a new track warp for the current block_id.
--
-- This must be called for each block, and it must be called after the tempo is
-- warped for that block so it can install the new warp.
add_new_track_warp :: Maybe TrackId -> Deriver ()
add_new_track_warp maybe_track_id = do
    stack <- get_stack
    block_id <- get_current_block_id
    start <- score_to_real 0
    -- Use block_event_end instead of block_logical_range.  Otherwise, the play
    -- monitor can't go past the end of the ruler, while the player is
    -- perfectly happy to do so.
    end <- real =<< block_event_end block_id
    warp <- get_warp
    let track = TrackWarp.Track start end warp block_id maybe_track_id
    merge_collect $ mempty { collect_warp_map = Map.singleton stack track }

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  This is therefore the logical
-- duration of the block, which may be shorter or lorger than the end of the
-- last event, or the ruler.
block_logical_range :: BlockId -> Deriver (TrackTime, TrackTime)
block_logical_range = eval_ui . Ui.block_logical_range

-- | Get the duration of the block according to the last event.
block_event_end :: BlockId -> Deriver ScoreTime
block_event_end = eval_ui . Ui.block_event_end


-- * track

-- | The deriver strips out tracks that can't be derived because they have no
-- notes.  But that means the track warps and track dynamics aren't recorded,
-- which means they don't have tempo or a playback monitor, which makes them
-- annoying.
record_empty_tracks :: [TrackId] -> Deriver ()
record_empty_tracks [] = return ()
record_empty_tracks track_ids = do
    block_id <- get_current_block_id
    mapM_ (record_empty_track block_id) track_ids

record_empty_track :: BlockId -> TrackId -> Deriver ()
record_empty_track block_id track_id =
    record_track_dynamic_for block_id track_id

-- * ControlFunction

get_control_function_dynamic :: Deriver DeriveT.Dynamic
get_control_function_dynamic = do
    ruler <- get_ruler
    state <- get
    return $ convert_dynamic ruler (state_dynamic state)
        (state_event_serial (state_threaded state))

convert_dynamic :: Ruler.Marklists -> Dynamic -> Stack.Serial
    -> DeriveT.Dynamic
convert_dynamic ruler dyn serial = DeriveT.Dynamic
    { dyn_pitch = state_pitch dyn
    , dyn_environ = state_environ dyn
    , dyn_event_serial = serial
    , dyn_warp = state_warp dyn
    , dyn_ruler = ruler
    }

-- | Get the 'Ruler.meter' marklists, if there is a ruler track here.  This
-- is called in all contexts, due to 'control_at', so it has to be careful
-- to not require a ruler.
get_ruler :: Deriver Ruler.Marklists
get_ruler = lookup_current_tracknum >>= \case
    Nothing -> return mempty
    Just (block_id, tracknum) -> do
        state <- get_ui_state id
        return $ either (const mempty) id $ Ui.eval state $ do
            ruler_id <- fromMaybe Ui.no_ruler <$>
                Ui.ruler_track_at block_id tracknum
            Ruler.ruler_marklists <$> Ui.get_ruler ruler_id


-- * Threaded

modify_threaded :: (Threaded -> Threaded) -> Deriver ()
modify_threaded f = modify $ \state -> state
    { state_threaded = f (state_threaded state) }

-- | Increment 'state_event_serial'.
increment_event_serial :: Deriver ()
increment_event_serial = modify_threaded $ \threaded -> threaded
    { state_event_serial = state_event_serial threaded + 1 }


-- * misc

-- | Am I deriving the toplevel block?
is_root_block :: Deriver Bool
is_root_block = is_root_block_stack <$> get_stack
    where
    is_root_block_stack stack =
        case [bid | Stack.Block bid <- Stack.outermost stack] of
            [] -> True
            [_] -> True
            _ -> False
