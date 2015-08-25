-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | This module is sister to "Derive.Deriver.Lib", except that it contains
    functions which are normally only used by the built-in track derivation
    scheme, and are not used when writing most normal calls.
-}
module Derive.Deriver.Internal where
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Word as Word

import qualified Util.Log as Log
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.BaseTypes as BaseTypes
import Derive.Deriver.Monad
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


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

-- | Collect is only ever accumulated.
--
-- Direct modification would be potentially more efficient, but according to
-- profiling it doesn't make a difference.
merge_collect :: Collect -> Deriver ()
merge_collect c = modify $ \st -> st { state_collect = state_collect st <> c }
    -- I append the Collect, which means that I wind up with the first instance
    -- for Maps with duplicate keys.  This seems a bit more intuitive than the
    -- last one.

modify_collect :: (Collect -> Collect) -> Deriver ()
modify_collect f = modify $ \st -> st { state_collect = f (state_collect st) }

set_threaded :: Threaded -> Deriver ()
set_threaded threaded = modify $ \st -> st { state_threaded = threaded }

-- * environ

get_environ :: Deriver BaseTypes.Environ
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

with_default_merge :: Map.Map Score.Control (Merger Signal.Control)
    -> Deriver a -> Deriver a
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

get_ui_state :: (State.State -> a) -> Deriver a
get_ui_state f = gets (f . state_ui . state_constant)

get_ui_config :: (State.Config -> a) -> Deriver a
get_ui_config f = get_ui_state (f . State.state_config)

-- | Because Deriver is not a UiStateMonad.
--
-- TODO I suppose it could be, but then I'd be tempted to make
-- a ReadOnlyUiStateMonad.  And I'd have to merge the exceptions.
get_track :: TrackId -> Deriver Track.Track
get_track track_id = lookup_id track_id =<< get_ui_state State.state_tracks

get_block :: BlockId -> Deriver Block.Block
get_block block_id = lookup_id block_id =<< get_ui_state State.state_blocks

-- | Evaluate a State.M computation, rethrowing any errors.
eval_ui :: Text -> State.StateId a -> Deriver a
eval_ui caller action = do
    ui_state <- get_ui_state id
    let rethrow exc = throw $ caller <> ": " <> pretty exc
    either rethrow return (State.eval ui_state action)

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k) => k -> Map.Map k a -> Deriver a
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
            tracknum <- eval_ui "lookup_current_tracknum" $
                State.get_tracknum_of block_id track_id
            return $ Just (block_id, tracknum)

get_current_tracknum :: Deriver (BlockId, TrackNum)
get_current_tracknum =
    maybe (throw "get_current_tracknum") return =<< lookup_current_tracknum

-- | Make a quick trick block stack.
with_stack_block :: BlockId -> Deriver a -> Deriver a
with_stack_block = with_stack . Stack.Block

-- | Make a quick trick track stack.
with_stack_track :: TrackId -> Deriver a -> Deriver a
with_stack_track = with_stack . Stack.Track

with_stack_region :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
with_stack_region s e = with_stack (Stack.Region s e)

with_stack_call :: Text -> Deriver a -> Deriver a
with_stack_call name = with_stack (Stack.Call name)

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

-- | Add a new stack frame and hash it with the random seed.
add_stack_frame :: Stack.Frame -> Dynamic -> Dynamic
add_stack_frame frame st = st
    { state_stack = Stack.add frame (state_stack st)
    , state_environ = update_seed (state_environ st)
    }
    where
    update_seed env = BaseTypes.insert
        EnvKey.seed (BaseTypes.VNum (Score.untyped (seed old))) env
        where
        old = case BaseTypes.lookup EnvKey.seed env of
            Just (BaseTypes.VNum n) -> Score.typed_val n
            _ -> 0
    seed :: Double -> Double
    seed n = i2d (CRC32.crc32Update (floor n) frame)
    -- A Double should be able to hold up to 2^52, but that's still an
    -- annoyingly large number to write in a score, so restrict it further.
    i2d :: Word.Word32 -> Double
    i2d i = fromIntegral (i `mod` 999)

get_stack :: Deriver Stack.Stack
get_stack = get_dynamic state_stack

-- * warp

real_to_score :: RealTime -> Deriver ScoreTime
real_to_score pos = do
    warp <- get_warp
    return $ Score.unwarp_pos warp pos

in_real_time :: Deriver a -> Deriver a
in_real_time = with_warp (const Score.id_warp)

with_warp :: (Score.Warp -> Score.Warp) -> Deriver a -> Deriver a
with_warp f = local $ \st -> st { state_warp = f (state_warp st) }

get_warp :: Deriver Score.Warp
get_warp = get_dynamic state_warp

-- ** time and duration

-- | Times are types that can be converted to RealTime and ScoreTime.
class Time t where
    real :: t -> Deriver RealTime
    score :: t -> Deriver ScoreTime
    to_duration :: t -> BaseTypes.Duration

instance Time ScoreTime where
    real = score_to_real
    score = return
    to_duration = BaseTypes.ScoreDuration

instance Time RealTime where
    real = return
    score = real_to_score
    to_duration = BaseTypes.RealDuration

instance Time BaseTypes.Duration where
    real (BaseTypes.RealDuration t) = real t
    real (BaseTypes.ScoreDuration t) = real t
    score (BaseTypes.RealDuration t) = score t
    score (BaseTypes.ScoreDuration t) = score t
    to_duration = id

-- ** warp

at :: ScoreTime -> Deriver a -> Deriver a
at shift = warp (Score.id_warp { Score.warp_shift = RealTime.score shift })

stretch :: ScoreTime -> Deriver a -> Deriver a
stretch factor =
    warp $ Score.id_warp { Score.warp_stretch = RealTime.score factor }

-- | 'at' and 'stretch' in one.  It's a little faster than using them
-- separately.  The order is stretch, then shift, as documented by
-- 'Score.Warp'.  TODO shouldn't the arguments go in the other order then?
place :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
place shift stretch = warp $ Score.id_warp
    { Score.warp_stretch = RealTime.score stretch
    , Score.warp_shift = RealTime.score shift
    }

-- | Low level warp function.
--
-- Previously, this would disallow <=0 stretch, but it turns out to be useful
-- to stretch events to 0, and to negative durations.
warp :: Score.Warp -> Deriver a -> Deriver a
warp score_warp deriver
    | Score.is_id_warp score_warp = deriver
    | otherwise = modify_warp (\w -> Score.compose_warps w score_warp) deriver

modify_warp :: (Score.Warp -> Score.Warp) -> Deriver a -> Deriver a
modify_warp modify = local $ \st -> st { state_warp = modify (state_warp st) }

-- | Am I deriving the toplevel block?
is_root_block :: Deriver Bool
is_root_block = do
    stack <- get_stack
    let blocks = [bid | Stack.Block bid <- Stack.outermost stack]
    return $ case blocks of
        [] -> True
        [_] -> True
        _ -> False

-- ** track warp

add_track_warp :: TrackId -> Deriver ()
add_track_warp track_id = do
    stack <- get_stack
    -- Put the track_id on the stack.  Originally this always happened
    -- implicitly because 'add_track_warp' was only called from "inside" the
    -- track (after adding the track's stack frame), but now there is orphan
    -- extraction that strips out empty tracks.  So the block deriver winds up
    -- explicitly calling 'add_track_warp' via 'record_empty_tracks' outside of
    -- the track's stack frame.
    merge_collect $ mempty
        { collect_warp_map = Map.singleton
            (Stack.add (Stack.Track track_id) stack) (Right track_id)
        }

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
    let tw = Left $ TrackWarp.TrackWarp start end warp block_id maybe_track_id
    merge_collect $ mempty { collect_warp_map = Map.singleton stack tw }

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  This is therefore the logical
-- duration of the block, which may be shorter or lorger than the end of the
-- last event, or the ruler.
block_logical_range :: BlockId -> Deriver (TrackTime, TrackTime)
block_logical_range = eval_ui "block_logical_range" . State.block_logical_range

-- | Get the duration of the block according to the last event.
block_event_end :: BlockId -> Deriver ScoreTime
block_event_end = eval_ui "block_event_end" . State.block_event_end


-- * track

-- | This does setup common to all track derivation, namely recording the
-- tempo warp, and then calls the specific track deriver.  Every track with
-- a track ID except tempo tracks should call this.
track_setup :: TrackTree.Track -> Deriver d -> Deriver d
track_setup track deriver = do
    whenJust (TrackTree.track_id track) add_track_warp
    deriver

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
record_empty_track block_id track_id = do
    add_track_warp track_id
    record_track_dynamic_for block_id track_id

-- * ControlFunction

get_control_function_dynamic :: Deriver BaseTypes.Dynamic
get_control_function_dynamic = do
    ruler <- get_ruler
    get_dynamic (convert_dynamic ruler)

convert_dynamic :: Ruler.Marklists -> Dynamic -> BaseTypes.Dynamic
convert_dynamic ruler dyn = BaseTypes.Dynamic
    { BaseTypes.dyn_controls = state_controls dyn
    , BaseTypes.dyn_control_functions = state_control_functions dyn
    , BaseTypes.dyn_pitches = state_pitches dyn
    , BaseTypes.dyn_pitch = state_pitch dyn
    , BaseTypes.dyn_environ = state_environ dyn
    , BaseTypes.dyn_warp = state_warp dyn
    , BaseTypes.dyn_ruler = ruler
    }

-- | Get the 'Ruler.meter' marklists, if there is a ruler track here.  This
-- is called in all contexts, due to 'control_at', so it has to be careful
-- to not require a ruler.
get_ruler :: Deriver Ruler.Marklists
get_ruler = lookup_current_tracknum >>= \x -> case x of
    Nothing -> return mempty
    Just (block_id, tracknum) -> do
        state <- get_ui_state id
        return $ either (const mempty) id $ State.eval state $ do
            ruler_id <- fromMaybe State.no_ruler <$>
                State.ruler_track_at block_id tracknum
            Ruler.ruler_marklists <$> State.get_ruler ruler_id
