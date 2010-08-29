{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-} -- for super-classes of Derived
{- | Main module for the deriver monad.

    Derivers are always in DeriveT, even if they don't need its facilities.
    This makes them more regular to compose.  The convention is to prepend
    deriver names with 'd_', so if the deriver is normally implemented purely,
    a d_ version can be made simply by composing 'return'.

    I have a similar sort of setup to nyquist, with a \"transformation
    environment\" that functions can look at to implement behavioral
    abstraction.  The main differences are that I don't actually generate audio
    signal, but my \"ugens\" eventually render down to MIDI or OSC (or even
    nyquist or csound source!).

    \"Stack\" handling here is kind of confusing.

    The end goal is that log messages and exceptions are tagged with the place
    they occurred.  This is called the stack, and is described in
    'Perform.Warning.Stack'.  Since the stack elements indicate positions on
    the screen, they should be in unwarped score time, not real time.

    The current stack is stored in 'state_stack' and will be added to by
    'with_stack_block', 'with_stack_track', and 'with_stack_pos' as the deriver
    processes a block, a track, and individual events respectively.
    'warn' and 'throw' will pick the current stack out of 'state_stack'.

    When 'Derive.Score.Event's are emitted they are also given the stack at the
    time of their derivation.  If there is a problem in performance, log msgs
    still have access to the stack.
-}
module Derive.Derive where
import Prelude hiding (error)
import qualified Prelude
import qualified Control.Applicative as Applicative
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as Monad.State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq
import qualified Util.SrcPos as SrcPos

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Transport as Transport

import {-# SOURCE #-} qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang


-- * DeriveT

type Deriver a = DeriveT Identity.Identity a

class (Show (Elem derived), Eq (Elem derived), Monoid.Monoid derived,
        Show derived) => Derived derived where
    type Elem derived :: *
    -- | I would prefer to have a function to a generic reified type and then
    -- use that value to index the CacheEntry, but I can't think of how to do
    -- that right now.
    from_cache_entry :: CacheEntry -> Maybe (CallType derived)
    to_cache_entry :: CallType derived -> CacheEntry
    derived_length :: derived -> Int
    derived_range :: derived -> Ranges.Ranges RealTime

instance Derived Events where
    type Elem Events = Score.Event
    from_cache_entry (CachedEvents ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedEvents
    -- TODO inefficient, this is just for debug log
    derived_length = length
    -- TODO also inefficient
    derived_range events =
        case (Seq.mhead Nothing Just events, Seq.mlast Nothing Just events) of
            (Just e1, Just e2) ->
                Ranges.range (Score.event_start e1) (Score.event_end e2)
            _ -> Ranges.nothing

instance Derived Control where
    type Elem Control = Signal.Y
    from_cache_entry (CachedControl ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedControl
    derived_length = Signal.length
    derived_range sig = case (Signal.first sig, Signal.last sig) of
        (Just (x1, _), Just (x2, _)) -> Ranges.range x1 x2
        _ -> Ranges.nothing

instance Derived Pitch where
    type Elem Pitch = PitchSignal.Y
    from_cache_entry (CachedPitch ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedPitch
    derived_length = PitchSignal.length
    derived_range sig = case (PitchSignal.first sig, PitchSignal.last sig) of
        (Just (x1, _), Just (x2, _)) -> Ranges.range x1 x2
        _ -> Ranges.nothing

data DerivedVal = Events Events | Control Control | Pitch Pitch
    deriving (Show)


-- ** events

type EventDeriver = Deriver Events
type Events = [Score.Event]

no_events :: Events
no_events = []

empty_events :: EventDeriver
empty_events = return no_events

-- ** control

type ControlDeriver = Deriver Control
type Control = Signal.Control
type DisplaySignalDeriver = Deriver [(TrackId, Signal.Display)]

no_control :: Control
no_control = Signal.empty

empty_control :: ControlDeriver
empty_control = return no_control

-- ** pitch

type PitchDeriver = Deriver PitchSignal.PitchSignal
type Pitch = PitchSignal.PitchSignal

no_pitch :: Pitch
no_pitch = PitchSignal.empty

empty_pitch :: PitchDeriver
empty_pitch = return no_pitch

-- ** state

-- TODO remove this
type TrackDeriver m e = TrackId -> DeriveT m [e]

newtype DeriveT m a = DeriveT (DeriveStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError DeriveError)
run_derive_t (DeriveT m) = m

instance (Monad m) => Applicative.Applicative (DeriveT m) where
    pure = return
    (<*>) = ap

type DeriveStack m = Error.ErrorT DeriveError
    (Monad.State.StateT State
        (Log.LogT m))

data State = State {
    -- Environment.  These form a dynamically scoped environment that applies
    -- to generated events inside its scope.

    -- | Derivers can modify it for sub-derivers, or look at it, whether to
    -- attach to an Event or to handle internally.
    state_controls :: !Score.ControlMap
    -- | Named pitch signals.
    , state_pitches :: !Score.PitchMap
    -- | Absolute pitch signal currently in scope.  This is the pitch signal
    -- that's actually applied to notes.  It's split off from pitches because
    -- it's convenient to guarentee that the main pitch signal is always
    -- present.
    , state_pitch :: !PitchSignal.PitchSignal

    , state_environ :: TrackLang.Environ
    , state_warp :: !Score.Warp
    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: Stack.Stack
    -- | This is a free-form stack which can be used to record call sequences
    -- in derivation.  It represents logical position during derivation rather
    -- than position on the score.
    , state_log_context :: ![String]

    -- | Remember the warp signal for each track.  A warp usually a applies to
    -- a set of tracks, so remembering them together will make the updater more
    -- efficient when it inverts them to get playback position.
    , state_track_warps :: [TrackWarp]
    , state_track_signals :: Track.TrackSignals

    , state_cache_state :: CacheState

    -- | Constant throughout the derivation.  Used to look up tracks and
    -- blocks.
    , state_ui :: State.State
    , state_lookup_deriver :: LookupDeriver
    , state_control_op_map :: Map.Map TrackLang.CallId ControlOp
    , state_pitch_op_map :: Map.Map TrackLang.CallId PitchOp
    , state_call_map :: CallMap
    -- | This is set if the derivation is for a signal deriver.  Signal
    -- derivers skip all special tempo treatment.  Ultimately, this is needed
    -- because of the 'add_track_warp' hack.  It might be 'add_track_warp' is
    -- too error-prone to allow to live...
    , state_ignore_tempo :: Bool
    }

initial_state cache ui_state score_damage lookup_deriver calls environ
        ignore_tempo =
    State {
    state_controls = initial_controls
    , state_pitches = Map.empty
    , state_pitch = PitchSignal.constant
        (State.state_project_scale ui_state) Pitch.middle_degree

    , state_environ = environ
    , state_warp = Score.id_warp
    , state_stack = Stack.empty
    , state_log_context = []

    , state_track_warps = []
    , state_track_signals = Map.empty

    , state_cache_state = initial_cache_state cache score_damage

    , state_ui = ui_state
    , state_lookup_deriver = lookup_deriver
    , state_control_op_map = default_control_op_map
    , state_pitch_op_map = default_pitch_op_map
    , state_call_map = calls
    , state_ignore_tempo = ignore_tempo
    }

-- | Initial control environment.
initial_controls :: Score.ControlMap
initial_controls = Map.fromList
    [(Score.c_velocity, Signal.constant default_velocity)]

-- | See 'Perform.Midi.Perform.default_velocity' for 0.79.
default_velocity :: Signal.Y
default_velocity = 0.79

data CacheState = CacheState {
    state_cache :: Cache -- modified
    , state_event_damage :: EventDamage -- appended to
    , state_score_damage :: ScoreDamage -- constant
    -- | Since ControlDamage doesn't contain the signal type, the same type
    -- can be used for pitch signals and control signals.  To make things
    -- simpler, I unify them by mapping their names through
    -- 'universal_control'.
    , state_control_damage :: ControlDamage

    -- | This is an evil hack.  Derivers must return 'EventDamage' for reasons
    -- described in its haddock.  However, damage is a concept local to the
    -- caching subsystem, and it's a hassle to have every transformer deal with
    -- a @(derived, EventDamage)@ pair instead of plain @derived@.  So instead,
    -- Cache maintains the event damage by modifying this value.  Event tracks
    -- will pull it out and merge it into the global 'state_event_damage', and
    -- control tracks will pull it out and merge it into the appropriate
    -- ControlDamage.  If they forget, then bad things happen, but this code
    -- should be restricted to the track derivers.
    --
    -- So yes, this is implicitly returning a value by modifying a global.
    , state_local_damage :: EventDamage

    -- | Similar to 'state_local_damage', this is how a call records its
    -- dependencies.  After evaluation of a deriver, this will contain the
    -- dependencies of the most recent call.  Furthermore, the type is wrong
    -- since this is also how transformers record their deps, but fortunately
    -- 'GeneratorDep' is a superset of 'TransformerDep'.  In addition, I can't
    -- record the 'gdep_prev_val' without making CacheState polymorphic, but
    -- 'Cache.cached_generator' already knows what the prev val was, so this
    -- only needs to record whether or not the call looked at the prev val.
    , state_local_dep :: LocalDep
    } deriving (Show)

empty_cache_state :: CacheState
empty_cache_state = CacheState {
    state_cache = empty_cache
    , state_event_damage = EventDamage Monoid.mempty
    , state_score_damage = Monoid.mempty
    , state_control_damage = ControlDamage Monoid.mempty
    , state_local_damage = EventDamage Monoid.mempty
    , state_local_dep = Monoid.mempty
    }

initial_cache_state :: Cache -> ScoreDamage -> CacheState
initial_cache_state cache score_damage = empty_cache_state {
    state_cache = cache
    , state_score_damage = score_damage
    }

-- | Hack, see 'state_local_dep'.
type LocalDep = GeneratorDep

-- ** calls

data CallMap = CallMap {
    calls_note :: NoteCallMap
    , calls_control :: ControlCallMap
    , calls_pitch :: PitchCallMap
    , calls_val :: ValCallMap
    } deriving (Show)
empty_call_map = CallMap Map.empty Map.empty Map.empty Map.empty

type NoteCallMap = Map.Map TrackLang.CallId NoteCall
type ControlCallMap = Map.Map TrackLang.CallId ControlCall
type PitchCallMap = Map.Map TrackLang.CallId PitchCall
type ValCallMap = Map.Map TrackLang.CallId ValCall

passed_event :: PassedArgs derived -> Event.Event
passed_event = info_event . passed_info

passed_next_events :: PassedArgs derived -> [Track.PosEvent]
passed_next_events = info_next_events . passed_info

passed_prev_events :: PassedArgs derived -> [Track.PosEvent]
passed_prev_events = info_prev_events . passed_info

passed_next_begin :: PassedArgs derived -> Maybe ScoreTime
passed_next_begin = fmap fst . relevant_event . passed_next_events

passed_prev_begin :: PassedArgs derived -> Maybe ScoreTime
passed_prev_begin = fmap fst . relevant_event . passed_prev_events

-- | Get the next \"relevant\" event beginning.  Intended to be used by calls
-- to determine their extent, especially control calls, which have no explicit
-- duration.
--
-- This will skip 'x = y' calls, which are not indended to affect note scope.
-- TODO implement that
relevant_event :: [Track.PosEvent] -> Maybe Track.PosEvent
relevant_event ((pos, evt) : _) = Just (pos, evt)
relevant_event _ = Nothing

-- | Get the previous derived val.  This is used by control derivers so they
-- can interpolate from the previous sample.
passed_prev_val :: PassedArgs derived -> Maybe (RealTime, Elem derived)
passed_prev_val args = info_prev_val (passed_info args)

-- | Additional data for a call.  This part is invariant for all calls on
-- an event.
--
-- Not used at all for val calls.
-- events not used for transform calls.
data CallInfo derived = CallInfo {
    -- The below is not used at all for val calls, and the events are not
    -- used for transform calls.  It might be cleaner to split those out, but
    -- too much bother.

    -- | Hack so control calls have access to the previous sample, since
    -- they tend to want to interpolate from that value.
    info_prev_val :: Maybe (RealTime, Elem derived)

    -- | These are warped into normalized time.
    --
    -- Calls can use this to interpret score times, which are intended to be
    -- in track score time.
    , info_event :: Event.Event
    , info_prev_events :: [Track.PosEvent]
    , info_next_events :: [Track.PosEvent]

    -- | These are not warped, so they are still in track score time.
    , info_track_pos :: (ScoreTime, ScoreTime)
    , info_track_prev :: [Track.PosEvent]
    , info_track_next :: [Track.PosEvent]
    }

-- | The deriver was stretched by the reciprocal of this number to put it
-- into normalized 0--1 time (i.e. this is the absolute value of the event's
-- duration).  Calls can multiply by this to get durations in the context of
-- the track.
--
-- Stretch for a 0 dur note is considered 1, not infinity, to avoid
-- problems with division by 0.
info_stretch :: CallInfo derived -> ScoreTime
info_stretch (CallInfo { info_track_pos = (s, e) }) =
    if start == end then 1 else end - start
    where (start, end) = (min s e, max s e)

-- | Transformer calls don't necessarily apply to any particular event, and
-- neither to generators for that matter.
dummy_call_info :: String -> CallInfo derived
dummy_call_info text = CallInfo Nothing (Event.event s 1) [] [] (0, 1) [] []
    where s = if null text then "<no-event>" else "<" ++ text ++ ">"

-- | A Call will be called as either a generator or a transformer, depending on
-- its position.  A call at the end of a compose pipeline will be called as
-- a generator while ones composed with it will be called as transformers, so
-- in @a | b@, @a@ is a transformer and @b@ is a generator.
--
-- More details on this strange setup are in the "Derive.Call" haddock.
data Call derived = Call {
    -- | Since call IDs may be rebound dynamically, each call has its own name
    -- so that error msgs are unambiguous.
    call_name :: String
    , call_generator :: Maybe (GeneratorCall derived)
    , call_transformer :: Maybe (TransformerCall derived)
    }

instance Show (Call derived) where
    show (Call name gen trans) = "<call " ++ name ++ Seq.join " " tags ++ ">"
        where
        tags = [t | (t, True) <- [("generator", Maybe.isJust gen),
            ("transformer", Maybe.isJust trans)]]

type NoteCall = Call Events
type ControlCall = Call Control
type PitchCall = Call Pitch

data ValCall = ValCall {
    vcall_name :: String
    , vcall_call :: PassedArgs TrackLang.Val
        -> Either TrackLang.TypeError (Deriver TrackLang.Val)
    }

instance Show ValCall where
    show (ValCall name _) = "<val call" ++ name ++ ">"

-- | Data passed to a 'Call'.
data PassedArgs derived = PassedArgs {
    passed_vals :: [TrackLang.Val]
    , passed_environ :: TrackLang.Environ
    , passed_call :: TrackLang.CallId
    , passed_info :: CallInfo derived
    }

-- *** generator

data GeneratorCall derived = GeneratorCall {
    gcall_func :: GeneratorFunc derived
    , gcall_type :: GeneratorType
    }

-- | args -> deriver
type GeneratorFunc derived = PassedArgs derived
    -> Either TrackLang.TypeError (Deriver derived)

generator :: (Derived derived) =>
    String -> GeneratorFunc derived -> Call derived
generator name func =
    Call name (Just (GeneratorCall func NonCachingGenerator)) Nothing

caching_generator :: (Derived derived) =>
    String -> GeneratorFunc derived -> Call derived
caching_generator name func =
    Call name (Just (GeneratorCall func CachingGenerator)) Nothing

-- *** transformer

data TransformerCall derived = TransformerCall {
    tcall_func :: TransformerFunc derived
    , tcall_type :: TransformerType
    }

-- | args -> deriver -> deriver
type TransformerFunc derived = PassedArgs derived -> Deriver derived
    -> Either TrackLang.TypeError (Deriver derived)

transformer :: (Derived derived) =>
    String -> TransformerFunc derived -> Call derived
transformer name func = Call
    name Nothing (Just (TransformerCall func NonIncremental))


-- *** misc TODO find a home

-- | The call for the whole control track.
type ControlTrackCall = BlockId -> TrackId -> PassedArgs Control
    -> Deriver Transformer

-- TODO remove?
type Transformer = EventDeriver -> EventDeriver

make_calls :: [(String, call)] -> Map.Map TrackLang.CallId call
make_calls = Map.fromList . map (first TrackLang.Symbol)

-- ** state support

-- | Since the deriver may vary based on the block, this is needed to find
-- the appropriate deriver.  It's created by 'Schema.lookup_deriver'.
type LookupDeriver = BlockId -> Either State.StateError EventDeriver

-- | Each track warp is a warp indexed by the block and tracks it covers.
-- These are used by the updater to figure out where the play position
-- indicator is at a given point in real time.
data TrackWarp = TrackWarp {
    tw_start :: RealTime
    , tw_end :: RealTime
    , tw_block :: BlockId
    , tw_tracks :: [TrackId]
    , tw_warp :: Score.Warp
    } deriving (Show)

data DeriveError = DeriveError SrcPos.SrcPos Stack.Stack String
    deriving (Eq)
instance Error.Error DeriveError where
    strMsg = DeriveError Nothing Stack.empty
instance Show DeriveError where
    show (DeriveError srcpos stack msg) =
        "<DeriveError " ++ SrcPos.show_srcpos srcpos ++ " "
        ++ Pretty.pretty stack ++ ": " ++ msg ++ ">"

error_message :: DeriveError -> String
error_message (DeriveError _ _ s) = s

instance Monad m => Log.LogMonad (DeriveT m) where
    write = DeriveT . lift . lift . Log.write


-- * monadic ops

data Result a = Result {
    r_result :: Either DeriveError a
    , r_cache :: Cache
    -- | Ranges which were rederived on this derivation.
    , r_event_damage :: EventDamage
    , r_tempo :: Transport.TempoFunction
    , r_inv_tempo :: Transport.InverseTempoFunction
    , r_track_signals :: Track.TrackSignals
    , r_logs :: [Log.Msg]
    -- | The relevant parts of the final state should be extracted into the
    -- above fields, but returning the whole state can be useful for testing.
    , r_state :: State
    }

derive :: Cache -> ScoreDamage -> LookupDeriver -> State.State -> CallMap
    -> TrackLang.Environ -> Bool -> DeriveT Identity.Identity a
    -> Result a
derive cache damage lookup_deriver ui_state calls environ ignore_tempo
        deriver =
    Result result (state_cache (state_cache_state state))
        (state_event_damage (state_cache_state state))
        tempo_func inv_tempo_func (state_track_signals state) logs state
    where
    (result, state, logs) = Identity.runIdentity $ run
        (initial_state clean_cache ui_state damage
        lookup_deriver calls environ ignore_tempo) deriver
    clean_cache = clear_damage damage cache
    track_warps = state_track_warps state
    tempo_func = make_tempo_func track_warps
    inv_tempo_func = make_inverse_tempo_func track_warps

d_block :: BlockId -> EventDeriver
d_block block_id = do
    -- Do some error checking.  These are all caught later, but if I throw here
    -- the stack doesn't include the bogus block yet and I can give more
    -- specific error msgs.
    let bthrow s = throw ("d_block " ++ show block_id ++ ": " ++ s)
    ui_state <- gets state_ui
    case Map.lookup block_id (State.state_blocks ui_state) of
        Nothing -> bthrow "block_id not found"
        _ -> return ()
    stack <- gets state_stack
    -- Since there is no branching, any recursion will be endless.
    when (Stack.Block block_id `elem` Stack.outermost stack) $
        bthrow "recursive block derivation"
    block_dur <- get_block_dur block_id
    when (block_dur <= 0) $
        bthrow "block with zero duration"
    state <- get
    let rethrow exc = bthrow $ "lookup deriver for " ++ show block_id
            ++ ": " ++ show exc
    deriver <- either rethrow return (state_lookup_deriver state block_id)
    -- Record a dependency on this block.
    add_block_dep block_id
    with_stack_block block_id deriver

-- | Run a derivation, catching and logging any exception.
d_subderive :: (Monad m) => a -> DeriveT Identity.Identity a -> DeriveT m a
d_subderive fail_val deriver = do
    state <- get
    let (res, state2, logs) = Identity.runIdentity $ run state deriver
    mapM_ Log.write logs
    case res of
        Left (DeriveError srcpos stack msg) -> do
            -- I don't use 'warn', which means I don't get the local context,
            -- but I think that's ok because the msg gets the sub-block's
            -- context.
            Log.write $
                (Log.msg_srcpos srcpos Log.Warn ("DeriveError: " ++ msg))
                { Log.msg_stack = Just stack }
            return fail_val
        Right val -> do
            -- TODO once the logging portion of the state is factored out I
            -- should copy back only that part
            modify (const state2)
            return val


run :: (Monad m) =>
    State -> DeriveT m a -> m (Either DeriveError a, State, [Log.Msg])
run derive_state m = do
    ((err, state2), logs) <- (Log.run . flip Monad.State.runStateT derive_state
        . Error.runErrorT . run_derive_t) m
    return (err, state2, logs)

make_tempo_func :: [TrackWarp] -> Transport.TempoFunction
make_tempo_func track_warps block_id track_id pos =
    map (Score.warp_pos pos) warps
    where
    warps = [tw_warp tw | tw <- track_warps, tw_block tw == block_id,
        any (==track_id) (tw_tracks tw)]

make_inverse_tempo_func :: [TrackWarp] -> Transport.InverseTempoFunction
make_inverse_tempo_func track_warps ts = do
    (block_id, track_ids, Just pos) <- track_pos
    return (block_id, [(track_id, pos) | track_id <- track_ids])
    where
    pos = Timestamp.to_real_time ts
    track_pos = [(tw_block tw, tw_tracks tw, unwarp ts (tw_warp tw)) |
            tw <- track_warps, tw_start tw <= pos && pos < tw_end tw]
    unwarp ts warp = Score.unwarp_pos (Timestamp.to_real_time ts) warp

modify :: (Monad m) => (State -> State) -> DeriveT m ()
modify f = (DeriveT . lift) (Monad.State.modify f)

get :: (Monad m) => DeriveT m State
get = (DeriveT . lift) Monad.State.get

gets :: (Monad m) => (State -> a) -> DeriveT m a
gets f = fmap f get

-- | This is a little different from Reader.local because only a portion of
-- the state is used Reader-style, i.e. 'state_track_warps' always collects.
-- TODO split State into dynamically scoped portion and use Reader for that.
local :: (Monad m) => (State -> b) -> (b -> State -> State)
    -> (State -> DeriveT m State) -> DeriveT m a -> DeriveT m a
local from_state to_state modify_state deriver = do
    old <- gets from_state
    new <- modify_state =<< get
    modify (const new)
    deriver `finally` modify (to_state old)

-- ** state access

-- | Lookup a scale_id or throw.
-- TODO merge in the static config scales.
get_scale :: (Monad m) => String -> Pitch.ScaleId -> DeriveT m Pitch.Scale
get_scale caller scale_id = do
    -- Defaulting the scale here means that relative pitch tracks don't need
    -- to mention their scale.
    scale_id <- if scale_id == Pitch.default_scale_id
        then gets (PitchSignal.sig_scale . state_pitch)
        else return scale_id
    maybe (throw (caller ++ ": unknown " ++ show scale_id)) return
        (Map.lookup scale_id Scale.scale_map)

-- ** cache

get_cache_state :: Deriver CacheState
get_cache_state = gets state_cache_state

put_cache :: Cache -> Deriver ()
put_cache cache = modify_cache_state $ \st -> st { state_cache = cache }

take_local_damage :: TrackId -> Deriver EventDamage
take_local_damage track_id = do
    old <- get_cache_state
    modify_cache_state $ \st ->
        st { state_local_damage = EventDamage Monoid.mempty }
    track_damage <- get_track_damage track_id
    return $ Monoid.mappend (state_local_damage old) track_damage

-- | Get the score damage for a track, mapped into RealTime as EventDamage
-- requires.
--
-- Local damage is obtained by recording the output of generators within the
-- score damage range.  This is essential to handle generators that produce
-- events outside of their range on the score.  However, it doesn't capture
-- events which were deleted.  Deleted events will always have score damage
-- in their former positions, but unfortunately have the same problem: if they
-- produced score events outside of the ui event range, those events won't be
-- covered under event damage after rederivation.
--
-- TODO A way to do this right would be to look at the previous score events
-- and take a diff, but at the moment I can't think of how to do that
-- efficiently.
get_track_damage :: TrackId -> Deriver EventDamage
get_track_damage track_id = do
    damage <- state_score_damage <$> get_cache_state
    case Map.lookup track_id (sdamage_tracks damage) of
        Nothing -> return Monoid.mempty
        Just ranges -> case Ranges.extract ranges of
            Nothing -> return $ EventDamage Ranges.everything
            Just pairs -> do
                realtime <- forM pairs $ \(s, e) ->
                    liftM2 (,) (score_to_real s) (score_to_real e)
                return $ EventDamage (Ranges.ranges realtime)

insert_local_damage :: EventDamage -> Deriver ()
insert_local_damage damage = modify_cache_state $ \st ->
    st { state_local_damage = Monoid.mappend damage (state_local_damage st) }

put_local_damage :: EventDamage -> Deriver ()
put_local_damage damage = modify_cache_state $ \st ->
    st { state_local_damage = damage }


insert_event_damage :: EventDamage -> Deriver ()
insert_event_damage damage = modify_cache_state $ \st ->
    st { state_event_damage = damage `Monoid.mappend` state_event_damage st }

with_control_damage :: EventDamage -> Deriver derived -> Deriver derived
with_control_damage (EventDamage damage) = local_cache_state
    state_control_damage
    (\old st -> st { state_control_damage = old })
    (\st -> st { state_control_damage = insert (state_control_damage st) })
    where
    insert (ControlDamage ranges) = ControlDamage (Monoid.mappend ranges damage)

add_block_dep :: BlockId -> Deriver ()
add_block_dep block_id = modify_cache_state $ \st ->
    st { state_local_dep = insert (state_local_dep st) }
    where
    insert (GeneratorDep blocks) = GeneratorDep (Set.insert block_id blocks)

with_local_dep :: Deriver a -> Deriver a
with_local_dep = local_cache_state
    state_local_dep (\old st -> st { state_local_dep = old })
    (\st -> st { state_local_dep = Monoid.mempty })

local_cache_state :: (CacheState -> st) -> (st -> CacheState -> CacheState)
    -> (CacheState -> CacheState)
    -> Deriver a -> Deriver a
local_cache_state from_state to_state modify_state = local
    (from_state . state_cache_state)
    (\old st -> st { state_cache_state = to_state old (state_cache_state st) })
    (\st ->
        return $ st { state_cache_state = modify_state (state_cache_state st) })

modify_cache_state :: (CacheState -> CacheState) -> Deriver ()
modify_cache_state f = modify $ \st ->
    st { state_cache_state = f (state_cache_state st) }

-- ** errors

throw :: (Monad m) => String -> DeriveT m a
throw = throw_srcpos Nothing

throw_srcpos :: (Monad m) => SrcPos.SrcPos -> String -> DeriveT m a
throw_srcpos srcpos msg = do
    stack <- gets state_stack
    context <- gets state_log_context
    Error.throwError (DeriveError srcpos stack (add_context context msg))

with_msg :: (Monad m) => String -> DeriveT m a -> DeriveT m a
with_msg msg = local state_log_context
    (\old st -> st { state_log_context = old })
    (\st -> return $ st { state_log_context = msg : state_log_context st })

-- | Catch DeriveErrors and convert them into warnings.  If an error is caught,
-- return Nothing, otherwise return Just op's value.
catch_warn :: (Monad m) => (String -> String) -> DeriveT m a
    -> DeriveT m (Maybe a)
catch_warn msg_of op = Error.catchError (fmap Just op) $
    \(DeriveError srcpos stack msg) ->
        Log.warn_stack_srcpos srcpos stack (msg_of msg) >> return Nothing

-- There' s got be a better way to get srcpos.

debug, notice, warn, error :: (Monad m) => String -> DeriveT m ()
debug = debug_srcpos Nothing
notice = notice_srcpos Nothing
warn = warn_srcpos Nothing
error = error_srcpos Nothing

debug_srcpos, notice_srcpos, warn_srcpos, error_srcpos
    :: (Monad m) => SrcPos.SrcPos -> String -> DeriveT m ()
debug_srcpos = log_msg Log.Debug
notice_srcpos = log_msg Log.Notice
warn_srcpos = log_msg Log.Warn
error_srcpos = log_msg Log.Error

log_msg :: (Monad m) => Log.Prio -> SrcPos.SrcPos -> String -> DeriveT m ()
log_msg prio srcpos text = do
    stack <- gets state_stack
    context <- gets state_log_context
    Log.write $ Log.make_msg srcpos prio (Just stack) (add_context context text)

add_context [] s = s
add_context context s = Seq.join " / " (reverse context) ++ ": " ++ s

-- ** environment

lookup_val :: forall a m. (TrackLang.Typecheck a, Monad m) =>
    TrackLang.ValName -> DeriveT m (Maybe a)
lookup_val name = do
    environ <- gets state_environ
    let return_type = TrackLang.to_type (Prelude.error "lookup_val" :: a)
    case TrackLang.lookup_val name environ of
            Left TrackLang.NotFound -> return Nothing
            Left (TrackLang.WrongType typ) ->
                throw $ "lookup_val " ++ show name ++ ": expected "
                    ++ Pretty.pretty return_type ++ " but val type is "
                    ++ Pretty.pretty typ
            Right v -> return (Just v)

-- | Like 'lookup_val', but throw if the value isn't present.
require_val :: forall a m. (TrackLang.Typecheck a, Monad m) =>
    TrackLang.ValName -> DeriveT m a
require_val name = do
    val <- lookup_val name
    maybe (throw $ "environ val not found: " ++ Pretty.pretty name) return val

put_val :: (Monad m, TrackLang.Typecheck val) => TrackLang.ValName -> val
    -> DeriveT m ()
put_val name val = do
    environ <- insert_environ name val =<< gets state_environ
    modify $ \st -> st { state_environ = environ }

-- | Set the given val dynamically within the given computation.
with_val :: (TrackLang.Typecheck val) => TrackLang.ValName -> val
    -> Deriver a -> Deriver a
with_val name val =
    local state_environ (\old st -> st { state_environ = old }) $ \st -> do
        environ <- insert_environ name val (state_environ st)
        return (st { state_environ = environ })

-- | This is like 'with_val', only it doesn't make any changes to the environ,
-- only restores it on return.
with_local_environ :: (Monad m) => DeriveT m a -> DeriveT m a
with_local_environ =
    local state_environ (\old st -> st { state_environ = old }) return

insert_environ :: (Monad m, TrackLang.Typecheck val) => TrackLang.ValName
    -> val -> TrackLang.Environ -> DeriveT m TrackLang.Environ
insert_environ name val environ =
    case TrackLang.put_val name val environ of
        Left typ -> throw $ "can't set " ++ show name ++ " to "
            ++ Pretty.pretty (TrackLang.to_val val)
            ++ ", expected " ++ Pretty.pretty typ
        Right environ2 -> return environ2

-- *** control

type ControlOp = Signal.Control -> Signal.Control -> Signal.Control
type PitchOp = PitchSignal.PitchSignal -> PitchSignal.Relative
    -> PitchSignal.PitchSignal

-- | Return an entire signal.  Remember, signals are in RealTime, so if you
-- want to index them in ScoreTime you will have to call 'score_to_real'.
-- 'control_at_score' does that for you.
get_control :: (Monad m) => Score.Control -> DeriveT m (Maybe Signal.Control)
get_control cont = Map.lookup cont <$> gets state_controls

control_at_score :: Score.Control -> ScoreTime -> Deriver (Maybe Signal.Y)
control_at_score cont pos = control_at cont =<< score_to_real pos

control_at :: Score.Control -> RealTime -> Deriver (Maybe Signal.Y)
control_at cont pos = do
    controls <- gets state_controls
    return $ fmap (\sig -> Signal.at pos sig) (Map.lookup cont controls)

pitch_at_score :: (Monad m) => ScoreTime -> DeriveT m PitchSignal.Y
pitch_at_score pos = pitch_at =<< score_to_real pos

pitch_at :: (Monad m) => RealTime -> DeriveT m PitchSignal.Y
pitch_at pos = do
    psig <- gets state_pitch
    return (PitchSignal.at pos psig)

pitch_degree_at :: (Monad m) => RealTime -> DeriveT m Pitch.Degree
pitch_degree_at pos = PitchSignal.y_to_degree <$> pitch_at pos

get_named_pitch :: (Monad m) => Score.Control
    -> DeriveT m (Maybe PitchSignal.PitchSignal)
get_named_pitch name = Map.lookup name <$> gets state_pitches

named_pitch_at :: (Monad m) => Score.Control -> RealTime
    -> DeriveT m (Maybe PitchSignal.Y)
named_pitch_at name pos = do
    maybe_psig <- get_named_pitch name
    return $ PitchSignal.at pos <$> maybe_psig

named_degree_at :: (Monad m) => Score.Control -> RealTime
    -> DeriveT m (Maybe Pitch.Degree)
named_degree_at name pos = do
    y <- named_pitch_at name pos
    return $ fmap PitchSignal.y_to_degree y

with_control :: (Monad m) =>
    Score.Control -> Signal.Control -> DeriveT m t -> DeriveT m t
with_control cont signal op = do
    controls <- gets state_controls
    -- TODO only revert the specific control
    modify $ \st ->
        st { state_controls = Map.insert cont signal controls }
    result <- op
    modify $ \st -> st { state_controls = controls }
    return result

with_control_operator :: (Monad m) => Score.Control -> TrackLang.CallId
    -> Signal.Control -> DeriveT m t -> DeriveT m t
with_control_operator cont c_op signal deriver = do
    op <- lookup_control_op c_op
    with_relative_control cont op signal deriver

with_relative_control :: (Monad m) =>
    Score.Control -> ControlOp -> Signal.Control -> DeriveT m t -> DeriveT m t
with_relative_control cont op signal deriver = do
    controls <- gets state_controls
    let msg = "relative control applied when no absolute control is in scope: "
    case Map.lookup cont controls of
        Nothing -> do
            warn (msg ++ show cont)
            deriver
        Just old_signal -> with_control cont (op old_signal signal) deriver

-- | Run the deriver in a context with the given pitch signal.  If a Control is
-- given, the pitch has that name, otherwise it's the unnamed default pitch.
with_pitch :: (Monad m) => Maybe Score.Control
    -> PitchSignal.PitchSignal -> DeriveT m t -> DeriveT m t
with_pitch = modify_pitch (flip const)

with_constant_pitch :: (Monad m) => Maybe Score.Control -> Pitch.Degree
    -> DeriveT m t -> DeriveT m t
with_constant_pitch maybe_name degree deriver = do
    pitch <- gets state_pitch
    with_pitch maybe_name
        (PitchSignal.constant (PitchSignal.sig_scale pitch) degree) deriver

with_relative_pitch :: (Monad m) => Maybe Score.Control
    -> PitchOp -> PitchSignal.Relative -> DeriveT m t -> DeriveT m t
with_relative_pitch maybe_name sig_op signal deriver = do
    old <- gets state_pitch
    if old == PitchSignal.empty
        then do
            -- This shouldn't happen normally because of the default pitch.
            warn $ "relative pitch applied when no absolute pitch is in scope"
            deriver
        else modify_pitch sig_op maybe_name signal deriver

with_pitch_operator :: (Monad m) => Maybe Score.Control
    -> TrackLang.CallId -> PitchSignal.Relative -> DeriveT m t -> DeriveT m t
with_pitch_operator maybe_name c_op signal deriver = do
    sig_op <- lookup_pitch_control_op c_op
    with_relative_pitch maybe_name sig_op signal deriver

modify_pitch :: (Monad m) =>
    (PitchSignal.PitchSignal -> PitchSignal.PitchSignal
        -> PitchSignal.PitchSignal)
    -> Maybe Score.Control -> PitchSignal.PitchSignal
    -> DeriveT m t -> DeriveT m t
modify_pitch f Nothing signal = local
    state_pitch (\old st -> st { state_pitch = old })
    (\st -> return $ st { state_pitch = f (state_pitch st) signal })
modify_pitch f (Just name) signal = local
    (Map.lookup name . ps)
    (\old st -> st { state_pitches = Map.alter (const old) name (ps st) })
    (\st -> return $ st { state_pitches = Map.alter alter name (ps st) })
    where
    ps = state_pitches
    alter Nothing = Just signal
    alter (Just old) = Just (f old signal)

-- *** specializations

velocity_at :: ScoreTime -> Deriver Signal.Y
velocity_at pos = do
    vel <- control_at Score.c_velocity =<< score_to_real pos
    return $ maybe default_velocity id vel

with_velocity :: Signal.Control -> Deriver a -> Deriver a
with_velocity = with_control Score.c_velocity


-- *** control ops

lookup_control_op :: (Monad m) => TrackLang.CallId -> DeriveT m ControlOp
lookup_control_op c_op = do
    op_map <- gets state_control_op_map
    maybe (throw ("unknown control op: " ++ show c_op)) return
        (Map.lookup c_op op_map)

-- | Default set of control operators.  Merged at runtime with the static
-- config.  TODO but not yet
default_control_op_map :: Map.Map TrackLang.CallId ControlOp
default_control_op_map = Map.fromList $ map (first TrackLang.Symbol)
    [ ("add", Signal.sig_add)
    , ("sub", Signal.sig_subtract)
    , ("mult", Signal.sig_multiply)
    , ("max", Signal.sig_max)
    , ("min", Signal.sig_min)
    ]

lookup_pitch_control_op :: (Monad m) => TrackLang.CallId -> DeriveT m PitchOp
lookup_pitch_control_op c_op = do
    op_map <- gets state_pitch_op_map
    maybe (throw ("unknown pitch op: " ++ show c_op)) return
        (Map.lookup c_op op_map)

-- | As with 'default_control_op_map', but pitch ops have a different type.
default_pitch_op_map :: Map.Map TrackLang.CallId PitchOp
default_pitch_op_map = Map.fromList $ map (first TrackLang.Symbol)
    [ ("add", PitchSignal.sig_add)
    , ("max", PitchSignal.sig_max)
    , ("min", PitchSignal.sig_min)
    ]

-- ** stack

get_current_block_id :: (Monad m) => DeriveT m BlockId
get_current_block_id = do
    stack <- gets state_stack
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
    state_stack (\old st -> st { state_stack = old })
    (\st -> return $ st { state_stack = Stack.add frame (state_stack st) })

-- ** track warps

-- | This doesn't actually take the Warp because it adds this track to the
-- existing \"open\" warp.  This is a hack to assign the same warp to many
-- tracks without having to compare warps to each other, which may be expensive
-- since they can be complicated signals.  Since many tracks should share the
-- same warp, I think grouping them should be a performance win for the
-- playback updater, but I don't know if it's necessary.
--
-- The hack should work as long as 'start_new_warp' is called when the warp is
-- set ('d_tempo' does this).  Otherwise, tracks will be grouped with the wrong
-- tempo, which will cause the playback cursor to not track properly.
add_track_warp :: (Monad m) => TrackId -> DeriveT m ()
add_track_warp track_id = do
    block_id <- get_current_block_id
    track_warps <- gets state_track_warps
    case track_warps of
            -- This happens if the initial block doesn't have a tempo track.
        [] -> start_new_warp >> add_track_warp track_id
        (tw:tws)
            | tw_block tw == block_id -> do
                let new_tws = tw { tw_tracks = track_id : tw_tracks tw } : tws
                modify $ \st -> st { state_track_warps = new_tws }
            -- start_new_warp wasn't called, either by accident or because
            -- this block doesn't have a tempo track.
            | otherwise -> start_new_warp >> add_track_warp track_id

-- | Start a new track warp for the current block_id, as in the stack.
--
-- This must be called for each block, and it must be called after the tempo is
-- warped for that block so it can install the new warp.
start_new_warp :: (Monad m) => DeriveT m ()
start_new_warp = do
    block_id <- get_current_block_id
    start <- score_to_real (ScoreTime 0)
    ui_state <- gets state_ui
    let time_end = either (const (ScoreTime 0)) id $
            State.eval ui_state (State.event_end block_id)
    end <- score_to_real time_end
    modify $ \st ->
        let tw = TrackWarp start end block_id [] (state_warp st)
        in st { state_track_warps = tw : state_track_warps st }

-- * basic derivers

-- ** tempo

-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo implies,
-- which is integral (1/tempo).

score_to_real :: (Monad m) => ScoreTime -> DeriveT m RealTime
score_to_real pos = do
    warp <- gets state_warp
    return (Score.warp_pos pos warp)

now :: (Monad m) => DeriveT m RealTime
now = score_to_real 0

real_to_score :: (Monad m) => RealTime -> DeriveT m ScoreTime
real_to_score pos = do
    warp <- gets state_warp
    maybe (throw $ "real_to_score out of range: " ++ show pos) return
        (Score.unwarp_pos pos warp)

min_tempo :: Signal.Y
min_tempo = 0.001

d_at :: (Monad m) => ScoreTime -> DeriveT m a -> DeriveT m a
d_at shift = d_warp (Score.id_warp { Score.warp_shift = shift })

d_stretch :: (Monad m) => ScoreTime -> DeriveT m a -> DeriveT m a
d_stretch factor = d_warp (Score.id_warp { Score.warp_stretch = factor })

d_warp :: (Monad m) => Score.Warp -> DeriveT m a -> DeriveT m a
d_warp warp deriver
    | Score.is_id_warp warp = deriver
    | Score.warp_stretch warp <= 0 =
        throw $ "stretch <= 0: " ++ show (Score.warp_stretch warp)
    | otherwise = local state_warp (\w st -> st { state_warp = w })
        (\st -> return $
            st { state_warp = Score.compose_warps (state_warp st) warp })
        deriver

with_warp :: (Monad m) => (Score.Warp -> Score.Warp) -> DeriveT m a
    -> DeriveT m a
with_warp f = local state_warp (\w st -> st { state_warp = w }) $ \st ->
    return $ st { state_warp = f (state_warp st) }

in_real_time :: (Monad m) => DeriveT m a -> DeriveT m a
in_real_time = with_warp (const Score.id_warp)

-- | Shift the controls of a deriver.  You're supposed to apply the warp before
-- deriving the controls, but I don't have a good solution for how to do this
-- yet, so I can leave these here for the moment.
d_control_at :: ScoreTime -> Deriver a -> Deriver a
d_control_at shift deriver = do
    real <- score_to_real shift
    local (\st -> (state_controls st, state_pitch st))
        (\(controls, pitch) st -> st { state_controls = controls,
            state_pitch = pitch })
        (\st -> return $ st
            { state_controls = nudge real (state_controls st)
            , state_pitch = nudge_pitch real (state_pitch st )})
        deriver
    where
    nudge delay = Map.map (Signal.shift delay)
    nudge_pitch = PitchSignal.shift


-- | Warp a block with the given deriver with the given signal.
--
-- The track_id passed so that the track that emitted the signal can be marked
-- as having the tempo that it emits, even though it's really derived in real
-- time, so the tempo track's play position will move at the tempo track's
-- tempo.
--
-- The block_id is used to stretch the block to a length of 1, regardless of
-- the tempo.  This means that when the calling block stretches it to the
-- duration of the event it winds up being the right length.  Obviously, this
-- is skipped for the top level block.
--
-- TODO relying on the stack seems a little implicit, would it be better
-- to pass Maybe BlockId or Maybe ScoreTime?
--
-- d_block seems like a better place to do this, but I don't have a local warp
-- yet.  This relies on every block having a d_tempo at the top, but
-- 'add_track_warp' already relies on that so Schema.compile ensures it.
--
-- TODO what to do about blocks with multiple tempo tracks?  I think it would
-- be best to stretch the block to the first one.  I could break out
-- stretch_to_1 and have compile apply it to only the first tempo track.
d_tempo :: (Monad m) => BlockId -> Maybe TrackId -> Signal.Tempo
    -> DeriveT m a -> DeriveT m a
d_tempo block_id maybe_track_id signal deriver = do
    let warp = tempo_to_warp signal
    top_level <- is_top_level_block
    stretch_to_1 <- if top_level then return id
        else do
            block_dur <- get_block_dur block_id
            real_dur <- with_warp (const warp) (score_to_real block_dur)
            -- Log.debug $ "dur, global dur "
            --     ++ show (block_id, block_dur, real_dur)
            when (block_dur == 0) $
                throw $ "can't derive a block with zero duration"
            return (d_stretch (1 / Types.real_to_score real_dur))
    stretch_to_1 $ d_warp warp $ do
        start_new_warp
        when_just maybe_track_id add_track_warp
        deriver

is_top_level_block :: (Monad m) => DeriveT m Bool
is_top_level_block = do
    stack <- gets state_stack
    let blocks = [bid | Stack.Block bid <- Stack.outermost stack]
    return $ case blocks of
        [] -> True
        [_] -> True
        _ -> False

-- | Sub-derived blocks are stretched according to their length, and this
-- function defines the length of a block.  'event_end' seems the most
-- intuitive, but then you can't make blocks with trailing space.  You can
-- work around it though by appending a comment dummy event.
get_block_dur :: (Monad m) => BlockId -> DeriveT m ScoreTime
get_block_dur block_id = do
    ui_state <- gets state_ui
    either (throw . ("get_block_dur: "++) . show) return
        (State.eval ui_state (State.event_end block_id))

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


-- ** track

-- | This does setup common to all track derivation, namely recording the tempo
-- warp and putting the track in the stack, and then calls the specific track
-- deriver.  This is because every track except tempo tracks should be wrapped
-- with this.  It doesn't actually affect the warp since that's already in
-- the environment.
track_setup :: TrackId -> Deriver d -> Deriver d
track_setup track_id deriver = do
    ignore_tempo <- gets state_ignore_tempo
    unless ignore_tempo (add_track_warp track_id)
    with_local_environ deriver

-- | This is a version of 'track_setup' for the tempo track.  It doesn't record
-- the track warp, see 'd_tempo' for why.
setup_without_warp :: Deriver d -> Deriver d
setup_without_warp deriver = with_local_environ (in_real_time deriver)

-- * utils

-- | Because DeriveT is not a UiStateMonad.
--
-- TODO I suppose it could be, but then I'd be tempted to make
-- a ReadOnlyUiStateMonad.  And I'd have to merge the exceptions.
get_track :: (Monad m) => TrackId -> DeriveT m Track.Track
get_track track_id = get >>= lookup_id track_id . State.state_tracks . state_ui

get_block :: (Monad m) => BlockId -> DeriveT m Block.Block
get_block block_id = get >>= lookup_id block_id . State.state_blocks . state_ui

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k, Monad m) => k -> Map.Map k a -> DeriveT m a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- | General purpose iterator over events.
--
-- It's like 'map_accuml_m' but sets the current event stack before operating
-- on each event, so that 'warn' can use it.  In addition, EventErrors are
-- caught and turned into warnings.  Events that threw aren't included in the
-- output.  An additional function extracts an event, so you can map over
-- things which are not themselves events.
map_events :: (Monad m) =>
    (state -> event -> DeriveT m (state, result))
    -> state -> (event -> Score.Event) -> [event] -> DeriveT m (state, [result])
map_events f state event_of xs = do
    (final_state, results) <- map_accuml_m apply state xs
    return (final_state, Maybe.catMaybes results)
    where
    apply cur_state x = with_event (event_of x) $ do
        val <- catch_warn id (f cur_state x)
        return $ case val of
            Nothing -> (cur_state, Nothing)
            Just (next_state, val) -> (next_state, Just val)

-- | So this is kind of confusing.  When events are created, they are assigned
-- their stack based on the current event_stack, which is set by the
-- with_stack_* functions.  Then, when they are processed, the stack is used
-- to *set* event_stack, which is what 'warn' and 'throw' will look at.
with_event :: (Monad m) => Score.Event -> DeriveT m a -> DeriveT m a
with_event event = local state_stack
    (\old st -> st { state_stack = old })
    (\st -> return $ st { state_stack = Score.event_stack event })

-- ** merge

d_merge :: (Monad m) => DeriveT m Events -> DeriveT m Events
    -> DeriveT m Events
d_merge = liftM2 merge_events

-- | Merge a list of EventDerivers.  The precondition is that the events
-- generated are ascending, so that the first event of each deriver is at or
-- after the first event of the next deriver.
d_merge_asc :: (Monad m) => [DeriveT m Events] -> DeriveT m Events
d_merge_asc = fmap merge_asc_events . sequence
-- d_merge_asc = foldr d_merge (return [])

merge_events :: [Score.Event] -> [Score.Event] -> [Score.Event]
merge_events = Seq.merge_on Score.event_start

merge_asc_events :: [[Score.Event]] -> [Score.Event]
merge_asc_events = Seq.merge_asc_lists Score.event_start

-- | Monoid instance for those who prefer that interface.
instance Monoid.Monoid EventDeriver where
    mempty = empty_events
    mappend = d_merge


-- * negative duration

process_negative_durations :: [Score.Event] -> [Score.Event]
process_negative_durations = id

{- TODO put this in its own module

-- TODO if I wind up going with the postproc route, this should probably become
-- bound to a special toplevel postproc symbol so it can be changed or turned
-- off

-- | Notes with negative duration have an implicit sounding duration which
-- depends on the following note.  Meanwhile (and for the last note of the
-- score), they have this sounding duration.
negative_duration_default :: RealTime
negative_duration_default = 1

-- | Post-process events to replace negative durations with positive ones.
process_negative_durations :: [Score.Event] -> [Score.Event]
process_negative_durations [] = []
process_negative_durations (evt:evts) = evt2 : process_negative_durations evts
    where
    next = find_next evt evts
    dur = calculate_duration (pos_dur evt) (fmap pos_dur next)
    evt2 = if dur == Score.event_duration evt then evt
        else evt { Score.event_duration = dur }
    pos_dur evt = (Score.event_start evt, Score.event_duration evt)

find_next :: Score.Event -> [Score.Event] -> Maybe Score.Event
find_next from = List.find (next_in_track from_stack . Score.event_stack)
    where from_stack = Score.event_stack from

-- | Is the second stack from an event that occurs later on the same track as
-- the first?  This is more complicated than it may seem at first because the
-- second event could come from a different deriver.  So it should look like
-- @same ; same ; bid same / tid same / higher ; *@.
next_in_track :: Warning.Stack -> Warning.Stack -> Bool
next_in_track (s0@(bid0, tid0, r0) : stack0) (s1@(bid1, tid1, r1) : stack1)
    | s0 == s1 = next_in_track stack0 stack1
    | bid0 == bid1 && tid0 == tid1 && r0 `before` r1 = True
    | otherwise = False
    where
    before (Just (s0, _)) (Just (s1, _)) = s0 < s1
    before _ _ = False
next_in_track _ _ = True

calculate_duration :: (RealTime, RealTime) -> Maybe (RealTime, RealTime)
    -> RealTime
calculate_duration (cur_pos, cur_dur) (Just (next_pos, next_dur))
        -- Departing notes are not changed.
    | cur_dur > 0 = cur_dur
        -- Arriving followed by arriving with a rest in between extends to
        -- the arrival of the rest.
    | next_dur <= 0 && rest > 0 = rest
        -- Arriving followed by arriving with no rest, or an arriving note
        -- followed by a departing note will sound until the next note.
    | otherwise = next_pos - cur_pos
    where
    rest = next_pos + next_dur - cur_pos
calculate_duration (_, dur) Nothing
    | dur > 0 = dur
    | otherwise = negative_duration_default

-}

-- * cache

-- I'd like to split this all off into its own module, but CacheEntry requires
-- Events, Control, etc.  Maybe I could split those off too, into a DeriveTypes
-- module or something like that?

-- instead of a stack, this could be a tree of frames
newtype Cache = Cache (Map.Map Stack.Stack CacheEntry)
    deriving (Show)

-- finding prefixes becomes trivial, but lookup is probably slower
-- or a 'Map.Map Stack.Frame (Either CacheEntry Cache)'
-- newtype Cache = Cache (Tree.Forest (Stack.Frame, CacheEntry))
--     deriving (Show)

empty_cache :: Cache
empty_cache = Cache Map.empty

-- | Since an entire track is one type but will have many different calls of
-- different types, the deriver type division goes above the call type
-- division.
data CacheEntry =
    CachedEvents (CallType Events)
    | CachedControl (CallType Control)
    | CachedPitch (CallType Pitch)
    deriving (Show)

-- | The type here should match the type of the stack it's associated with,
-- but I'm not quite up to those type gymnastics yet.
data CallType derived = CachedGenerator GeneratorDep derived
    deriving (Show)

-- ** deps

newtype GeneratorDep = GeneratorDep (Set.Set BlockId)
    deriving (Monoid.Monoid, Show, Eq)

data GeneratorType =
    CachingGenerator
    -- | This generator is so cheap it should skip the cache entirely.
    | NonCachingGenerator
    deriving (Show)

data TransformerType =
    -- | An incremental transformer can recompute a fragment of its result
    -- from a fragment of its input.  It optionally requires a number of
    -- (before, after) events or samples as context.
    Incremental (Int, Int)
    -- | Non-incremental transformers either don't evaluate their input (e.g.
    -- they simply modify the environment like a control), or they must
    -- re-process their entire input every time any part of it changes.  In
    -- either case, they bypass caching.
    | NonIncremental
    deriving (Show)

-- ** damage

type DamageRanges = Ranges.Ranges RealTime

-- | Modified ranges in the score.
data ScoreDamage = ScoreDamage {
    -- | Damaged ranges in tracks.
    sdamage_tracks :: Map.Map TrackId (Ranges.Ranges ScoreTime)
    -- | The blocks with damaged tracks.  Calls depend on blocks
    -- ('gdep_blocks') rather than tracks, so it's convenient to keep the
    -- blocks here.  This is different than block damage because a damaged
    -- block will invalidate all caches below it, but a block with damaged
    -- tracks must be called but may still have valid caches within.
    , sdamage_track_blocks :: Set.Set BlockId
    , sdamage_blocks :: Set.Set BlockId
    } deriving (Eq, Show)

instance Monoid.Monoid ScoreDamage where
    mempty = ScoreDamage Map.empty Set.empty Set.empty
    mappend (ScoreDamage tracks1 tblocks1 blocks1)
            (ScoreDamage tracks2 tblocks2 blocks2) =
        ScoreDamage (Map.mappend tracks1 tracks2)
            (Monoid.mappend tblocks1 tblocks2)
            (Monoid.mappend blocks1 blocks2)

-- | Clear the damaged portions out of the cache so they will rederive.
clear_damage :: ScoreDamage -> Cache -> Cache
clear_damage (ScoreDamage tracks _ blocks) (Cache cache) =
    Cache $ Map.filterWithKey (\stack _ -> not (rm stack)) cache
    where
    rm stack = any (`Stack.member` stack) (map Stack.Block (Set.elems blocks))
        || any (overlapping stack) (Map.assocs tracks)
    overlapping stack (track_id, ranges) =
        any (Ranges.overlapping ranges) (Stack.track_regions stack track_id)

-- | This indicates ranges of time that were rederived.
--
-- It's created by cache misses on generators, and created by transformers that
-- have to recompute or expanded by ones that can recompute incrementally.
--
-- Event tracks will append the result to a log in the derive state which can
-- be used by the performer to incrementally recompute the performance, and
-- control tracks will convert it into ControlDamage so subsequent calls
-- that depend on it can be rederived.
newtype EventDamage = EventDamage DamageRanges
    deriving (Monoid.Monoid, Eq, Show)

-- | Control damage indicates that a section of control signal has been
-- modified.  It's dynamically scoped over the same range as the control
-- itself, so that events that depend on it can be rederived.
newtype ControlDamage = ControlDamage DamageRanges
    deriving (Monoid.Monoid, Eq, Show)
