{- | This module is sister to "Derive.Deriver.Lib", except that it contains
    functions which are normally only used by the built-in track derivation
    scheme, and are not so useful when writing calls.
-}
module Derive.Deriver.Internal where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.State as State
import Derive.Deriver.Monad
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


-- * generic state access

get_dynamic :: (Dynamic -> a) -> Deriver a
get_dynamic f = gets (f . state_dynamic)

-- | This is a little different from Reader.local because only a portion of
-- the state is used Reader-style.
--
-- Note that this doesn't restore the state on an exception.  I think this
-- is ok because exceptions are always \"caught\" at the event evaluation
-- level since it runs each one separately.  Since the state dynamic state
-- (i.e. except Collect) from the sub derivation is discarded, whatever state
-- it's in after the exception shouldn't matter.
local :: (Dynamic -> b) -> (b -> Dynamic -> Dynamic)
    -> (Dynamic -> Deriver Dynamic) -> Deriver a -> Deriver a
local from_state restore_state modify_state deriver = do
    st <- get
    let old = from_state (state_dynamic st)
    new <- modify_state (state_dynamic st)
    put $ st { state_dynamic = new }
    result <- deriver
    modify $ \st -> st { state_dynamic =
        restore_state old (state_dynamic st) }
    return result

-- | Collect is only ever accumulated.
merge_collect :: Collect -> Deriver ()
merge_collect c = modify $ \st -> st { state_collect = c <> state_collect st }

-- * environ

insert_environ :: (TrackLang.Typecheck val) => TrackLang.ValName
    -> val -> TrackLang.Environ -> Deriver TrackLang.Environ
insert_environ name val environ =
    case TrackLang.put_val name val environ of
        Left typ -> throw $ "can't set " ++ show name ++ " to "
            ++ Pretty.pretty (TrackLang.to_val val)
            ++ ", expected " ++ Pretty.pretty typ
        Right environ2 -> return environ2

-- | Figure out the current block and track, and record the current environ
-- in the Collect.  It only need be recorded once per track.
record_track_environ :: State -> Collect
record_track_environ state = case stack of
        Stack.Track tid : Stack.Block bid : _ ->
            mempty { collect_track_environ = Map.singleton (bid, tid)
                (state_environ (state_dynamic state)) }
        _ -> mempty
    where
    -- Strip the stack down to the most recent track and block, since it will
    -- look like [tid, tid, tid, bid, ...].
    stack = Seq.drop_dups is_track $ filter track_or_block $
        Stack.innermost (state_stack (state_dynamic state))
    track_or_block (Stack.Track _) = True
    track_or_block (Stack.Block _) = True
    track_or_block _ = False
    is_track (Stack.Track _) = True
    is_track _ = False


-- * cache

with_control_damage :: ControlDamage -> Deriver derived -> Deriver derived
with_control_damage damage = local
    state_control_damage
    (\old st -> st { state_control_damage = old })
    (\st -> return $ st { state_control_damage = damage })

add_block_dep :: BlockId -> Deriver ()
add_block_dep block_id = merge_collect $ mempty
    { collect_local_dep = GeneratorDep (Set.singleton block_id) }

-- | Both track warps and local deps are used as dynamic return values (aka
-- modifying a variable to \"return\" something).  When evaluating a cached
-- generator, the caller wants to know the callee's track warps and local
-- deps, without getting them mixed up with its own warps and deps.  So run
-- a deriver in an empty environment, and restore it afterwards.
with_empty_collect :: Deriver a -> Deriver (a, Collect)
with_empty_collect deriver = do
    old <- gets state_collect
    new <- (\st -> return $ st { state_collect = mempty }) =<< get
    put new
    result <- deriver
    collect <- gets state_collect
    modify (\st -> st { state_collect = old })
    return (result, collect)


-- * stack

get_current_block_id :: Deriver BlockId
get_current_block_id = do
    stack <- get_dynamic state_stack
    case [bid | Stack.Block bid <- Stack.innermost stack] of
        [] -> throw "no blocks in stack"
        block_id : _ -> return block_id

-- | Make a quick trick block stack.
with_stack_block :: BlockId -> Deriver a -> Deriver a
with_stack_block = with_stack . Stack.Block

-- | Make a quick trick track stack.
with_stack_track :: TrackId -> Deriver a -> Deriver a
with_stack_track = with_stack . Stack.Track

with_stack_region :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
with_stack_region s e = with_stack (Stack.Region s e)

with_stack_call :: String -> Deriver a -> Deriver a
with_stack_call name = with_stack (Stack.Call name)

with_stack :: Stack.Frame -> Deriver a -> Deriver a
with_stack frame = local
    state_stack (\old st -> st { state_stack = old }) $ \st -> do
        when (Stack.length (state_stack st) > max_depth) $
            throw $ "call stack too deep: " ++ Pretty.pretty frame
        return $ st { state_stack = Stack.add frame (state_stack st) }
    where max_depth = 30
    -- A recursive loop will result in an unfriendly hang.  So limit the total
    -- nesting depth to catch those.  I could disallow all recursion, but this
    -- is more general.


-- * warp

score_to_real :: ScoreTime -> Deriver RealTime
score_to_real pos = do
    warp <- get_dynamic state_warp
    return (Score.warp_pos pos warp)

real_to_score :: RealTime -> Deriver ScoreTime
real_to_score pos = do
    warp <- get_dynamic state_warp
    maybe (throw $ "real_to_score out of range: " ++ show pos) return
        (Score.unwarp_pos pos warp)

in_real_time :: Deriver a -> Deriver a
in_real_time = with_warp (const Score.id_warp)

with_warp :: (Score.Warp -> Score.Warp) -> Deriver a -> Deriver a
with_warp f = local state_warp (\w st -> st { state_warp = w }) $ \st ->
    return $ st { state_warp = f (state_warp st) }

-- ** tempo

d_at :: ScoreTime -> Deriver a -> Deriver a
d_at shift = d_warp (Score.id_warp { Score.warp_shift = shift })

d_stretch :: ScoreTime -> Deriver a -> Deriver a
d_stretch factor = d_warp (Score.id_warp { Score.warp_stretch = factor })

-- | 'd_at' and 'd_stretch' in one.  It's a little faster than using them
-- separately.
d_place :: ScoreTime -> ScoreTime -> Deriver a -> Deriver a
d_place shift stretch = d_warp
    (Score.id_warp { Score.warp_stretch = stretch, Score.warp_shift = shift })

d_warp :: Score.Warp -> Deriver a -> Deriver a
d_warp warp deriver
    | Score.is_id_warp warp = deriver
    | Score.warp_stretch warp <= 0 =
        throw $ "stretch <= 0: " ++ show (Score.warp_stretch warp)
    | otherwise = local state_warp (\w st -> st { state_warp = w })
        (\st -> return $
            st { state_warp = Score.compose_warps (state_warp st) warp })
        deriver

-- | Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo
-- implies, which is integral (1/tempo).

-- | Warp a block with the given deriver with the given signal.
--
-- TODO what to do about blocks with multiple tempo tracks?  I think it would
-- be best to stretch the block to the first one.  I could break out
-- stretch_to_1 and have compile apply it to only the first tempo track.
d_tempo :: ScoreTime
    -- ^ Used to stretch the block to a length of 1, regardless of the tempo.
    -- This means that when the calling block stretches it to the duration of
    -- the event it winds up being the right length.  This is skipped for the
    -- top level block or all pieces would last exactly 1 second.  This is
    -- another reason every block must have a 'd_tempo' at the top.
    --
    -- TODO relying on the stack seems a little implicit, would it be better
    -- to pass Maybe BlockId or Maybe ScoreTime?
    --
    -- 'Derive.Call.Block.d_block' might seem like a better place to do this,
    -- but it doesn't have the local warp yet.
    -> Maybe TrackId
    -- ^ Needed to record this track in TrackWarps.  It's optional because if
    -- there's no explicit tempo track there's an implicit tempo around the
    -- whole block, but the implicit one doesn't have a track of course.
    -> Signal.Tempo -> Deriver a -> Deriver a
d_tempo block_dur maybe_track_id signal deriver = do
    let warp = tempo_to_warp signal
    root <- is_root_block
    stretch_to_1 <- if root then return id
        else do
            real_dur <- with_warp (const warp) (score_to_real block_dur)
            -- Log.debug $ "dur, global dur "
            --     ++ show (block_id, block_dur, real_dur)
            when (block_dur == 0) $
                throw "can't derive a block with zero duration"
            return (d_stretch (1 / RealTime.to_score real_dur))
    stretch_to_1 $ d_warp warp $ do
        add_new_track_warp maybe_track_id
        deriver

tempo_to_warp :: Signal.Tempo -> Score.Warp
tempo_to_warp sig
    -- Optimize for a constant (or missing) tempo.
    | Signal.is_constant sig =
        let stretch = 1 / max min_tempo (Signal.at 0 sig)
        in Score.Warp Score.id_warp_signal 0 (Signal.y_to_score stretch)
    | otherwise = Score.Warp warp_sig 0 1
    where
    warp_sig = Signal.integrate Signal.tempo_srate $ Signal.map_y (1/) $
         Signal.clip_min min_tempo sig

min_tempo :: Signal.Y
min_tempo = 0.001

is_root_block :: Deriver Bool
is_root_block = do
    stack <- get_dynamic state_stack
    let blocks = [bid | Stack.Block bid <- Stack.outermost stack]
    return $ case blocks of
        [] -> True
        [_] -> True
        _ -> False

-- ** track warp

add_track_warp :: TrackId -> Deriver ()
add_track_warp track_id = do
    stack <- get_dynamic state_stack
    merge_collect $ mempty
        { collect_warp_map = Map.singleton stack (Right track_id) }

-- | Start a new track warp for the current block_id.
--
-- This must be called for each block, and it must be called after the tempo is
-- warped for that block so it can install the new warp.
add_new_track_warp :: Maybe TrackId -> Deriver ()
add_new_track_warp track_id = do
    stack <- get_dynamic state_stack
    block_id <- get_current_block_id
    start <- score_to_real 0
    end <- score_to_real =<< get_block_dur block_id
    warp <- get_dynamic state_warp
    let tw = Left $ TrackWarp.TrackWarp (start, end, warp, block_id, track_id)
    merge_collect $ mempty { collect_warp_map = Map.singleton stack tw }

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  'State.block_event_end' seems the
-- most intuitive, but then you can't make blocks with trailing space.  You
-- can work around it though by appending a comment dummy event.
get_block_dur :: BlockId -> Deriver ScoreTime
get_block_dur block_id = do
    ui_state <- gets (state_ui . state_constant)
    either (throw . ("get_block_dur: "++) . show) return
        (State.eval ui_state (State.block_event_end block_id))


-- * track

-- | This does setup common to all track derivation, namely recording the
-- tempo warp, and then calls the specific track deriver.  Every track with
-- a track ID except tempo tracks should call this.
track_setup :: State.TrackEvents -> Deriver d -> Deriver d
track_setup track deriver = do
    when_just (State.tevents_track_id track) add_track_warp
    deriver

-- | This is a version of 'track_setup' for the tempo track.  It doesn't
-- record the track warp, see 'd_tempo' for why.
setup_without_warp :: Deriver d -> Deriver d
setup_without_warp = in_real_time
