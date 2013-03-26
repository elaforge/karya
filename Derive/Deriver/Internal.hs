{- | This module is sister to "Derive.Deriver.Lib", except that it contains
    functions which are normally only used by the built-in track derivation
    scheme, and are not so useful when writing calls.
-}
module Derive.Deriver.Internal where
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Word as Word

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import Derive.Deriver.Monad
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
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
local modify_state = localm (return . modify_state)

localm :: (Dynamic -> Deriver Dynamic) -> Deriver a -> Deriver a
localm modify_state deriver = do
    st <- get
    new <- modify_state (state_dynamic st)
    put $ st { state_dynamic = new }
    result <- deriver
    modify $ \new -> new { state_dynamic = state_dynamic st }
    return result

-- | A version of 'local' that catches exceptions and ignores any changes to
-- Collect.  This is appropriate for sub-calls that are below normal track
-- derivation.
detached_local :: (Dynamic -> Dynamic) -> Deriver a -> Deriver (Either Error a)
detached_local modify_state deriver = do
    st <- get
    let (result, _, logs) = run
            (st { state_dynamic = modify_state (state_dynamic st) }) deriver
    mapM_ Log.write logs
    return result

-- | Collect is only ever accumulated.
--
-- Direct modification would be potentially more efficient, but according to
-- profiling it doesn't make a difference.
merge_collect :: Collect -> Deriver ()
merge_collect c = modify $ \st -> st { state_collect = c <> state_collect st }

modify_collect :: (Collect -> Collect) -> Deriver ()
modify_collect f = modify $ \st -> st { state_collect = f (state_collect st) }

-- * environ

insert_environ :: (TrackLang.Typecheck val) => TrackLang.ValName
    -> val -> TrackLang.Environ -> Deriver TrackLang.Environ
insert_environ name val environ =
    case TrackLang.put_val name val environ of
        Left typ -> throw $ "can't set " ++ show name ++ " to "
            ++ TrackLang.show_val (TrackLang.to_val val)
            ++ ", expected " ++ Pretty.pretty typ
        Right environ2 -> return environ2

-- | Figure out the current block and track, and record the current environ
-- in the Collect.  It only needs to be recorded once per track.
record_track_dynamic :: State -> Collect
record_track_dynamic state = case stack of
        Stack.Track tid : Stack.Block bid : _ -> mempty
            { collect_track_dynamic =
                Map.singleton (bid, tid) (state_dynamic state)
            }
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

-- | 'record_track_dynamic' for when I already know BlockId and TrackId.
record_track_dynamic_for :: BlockId -> TrackId -> Deriver ()
record_track_dynamic_for block_id track_id = do
    dynamic <- gets state_dynamic
    merge_collect $ mempty
        { collect_track_dynamic = Map.singleton (block_id, track_id) dynamic }


-- * cache

with_control_damage :: ControlDamage -> Deriver derived -> Deriver derived
with_control_damage damage = local $ \st ->
    st { state_control_damage = damage }

add_block_dep :: BlockId -> Deriver ()
add_block_dep block_id = merge_collect $ mempty
    { collect_local_dep = GeneratorDep (Set.singleton block_id) }

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
eval_ui :: String -> State.StateId a -> Deriver a
eval_ui caller action = do
    ui_state <- get_ui_state id
    let rethrow exc = throw $ caller ++ ": " ++ show exc
    either rethrow return (State.eval ui_state action)

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k) => k -> Map.Map k a -> Deriver a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- * stack

get_current_block_id :: Deriver BlockId
get_current_block_id = do
    stack <- get_stack
    case [bid | Stack.Block bid <- Stack.innermost stack] of
        [] -> throw "no blocks in stack"
        bid : _ -> return bid

get_current_tracknum :: Deriver (BlockId, TrackNum)
get_current_tracknum = do
    stack <- get_stack
    track_id <- case [tid | Stack.Track tid <- Stack.innermost stack] of
        [] -> throw "no tracks in stack"
        tid : _ -> return tid
    block_id <- get_current_block_id
    tracknum <- eval_ui "get_current_tracknum" $
        State.get_tracknum_of block_id track_id
    return (block_id, tracknum)

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
with_stack frame = localm $ \st -> do
    stack <- get_stack
    when (Stack.length stack >= max_depth) $
        throw $ "call stack too deep: " ++ Pretty.pretty frame
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
    update_seed env = TrackLang.insert_val
        TrackLang.v_seed (TrackLang.num (seed old)) env
        where old = fromMaybe 0 (TrackLang.maybe_val TrackLang.v_seed env)
    seed :: Double -> Double
    seed n = i2d (CRC32.crc32Update (floor n) frame)
    -- A Double should be able to hold up to 2^52, but that's still an
    -- annoyingly large number to write in a score, so restrict it further.
    i2d :: Word.Word32 -> Double
    i2d i = fromIntegral (i `mod` 999)

get_stack :: Deriver Stack.Stack
get_stack = get_dynamic state_stack

-- * warp

real :: ScoreTime -> Deriver RealTime
real pos = do
    warp <- get_dynamic state_warp
    return (Score.warp_pos pos warp)

score :: RealTime -> Deriver ScoreTime
score pos = do
    warp <- get_dynamic state_warp
    maybe (throw $ "score: out of range: " ++ show pos) return
        (Score.unwarp_pos pos warp)

in_real_time :: Deriver a -> Deriver a
in_real_time = with_warp (const Score.id_warp)

with_warp :: (Score.Warp -> Score.Warp) -> Deriver a -> Deriver a
with_warp f = local $ \st -> st { state_warp = f (state_warp st) }

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
    -- Originally a 0 stretch was also illegal, the idea being that it's
    -- probably a bug.  However, 0 duration events are legal, and if I want to
    -- multiply their stretch then of course it winds up being 0.
    | Score.warp_stretch warp < 0 =
        throw $ "stretch < 0: " ++ Pretty.pretty (Score.warp_stretch warp)
            ++ " (shift: " ++ Pretty.pretty (Score.warp_shift warp) ++ ")"
    | otherwise = local
        (\st -> st { state_warp = Score.compose_warps (state_warp st) warp })
        deriver

-- | Warp a block with the given deriver with the given signal.

-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo
-- implies, which is integral (1/tempo).
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
    stretch_to_1 <- if root then return id else do
        real_dur <- with_warp (const warp) (real block_dur)
        return $ if block_dur == 0 then id
            else if real_dur == 0
            then const $ throw $ "real time of non-zero block dur "
                ++ show block_dur ++ " was zero"
            else d_stretch (1 / RealTime.to_score real_dur)
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
         Signal.scalar_min min_tempo sig

min_tempo :: Signal.Y
min_tempo = 0.001

-- | Is this the toplevel block?
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
add_new_track_warp track_id = do
    stack <- get_stack
    block_id <- get_current_block_id
    start <- real 0
    -- Use get_total_block_dur instead get_block_dur.  Otherwise, the play
    -- monitor can't go past the end of the ruler, while the player is
    -- perfectly happy to do so.
    end <- real =<< get_total_block_dur block_id
    warp <- get_dynamic state_warp
    let tw = Left $ TrackWarp.TrackWarp (start, end, warp, block_id, track_id)
    merge_collect $ mempty { collect_warp_map = Map.singleton stack tw }

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  Using 'State.block_ruler_end' which
-- makes the ruler the decider of block length, as it does at the Cmd layer.
get_block_dur :: BlockId -> Deriver ScoreTime
get_block_dur block_id = do
    ui_state <- gets (state_ui . state_constant)
    either (throw . ("get_block_dur: "++) . show) return
        (State.eval ui_state (State.block_ruler_end block_id))

-- | Get the duration of the block according to the last event.
get_total_block_dur :: BlockId -> Deriver ScoreTime
get_total_block_dur block_id = do
    ui_state <- gets (state_ui . state_constant)
    either (throw . ("get_total_block_dur: "++) . show) return
        (State.eval ui_state (State.block_event_end block_id))


-- * track

-- | This does setup common to all track derivation, namely recording the
-- tempo warp, and then calls the specific track deriver.  Every track with
-- a track ID except tempo tracks should call this.
track_setup :: TrackTree.TrackEvents -> Deriver d -> Deriver d
track_setup track deriver = do
    when_just (TrackTree.tevents_track_id track) add_track_warp
    deriver

-- | This is a version of 'track_setup' for the tempo track.  It doesn't
-- record the track warp, see 'd_tempo' for why.
setup_without_warp :: Deriver d -> Deriver d
setup_without_warp = in_real_time

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
