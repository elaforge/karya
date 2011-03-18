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
    Log msgs and 'throw' will pick the current stack out of 'state_stack'.

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
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text

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
import qualified Ui.Symbol as Symbol
import qualified Ui.Track as Track

import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport


-- * DeriveT

newtype DeriveT m a = DeriveT (DeriveStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError DeriveError)
run_derive_t (DeriveT m) = m

instance (Functor m, Monad m) => Applicative.Applicative (DeriveT m) where
    pure = return
    (<*>) = ap

type DeriveStack m = Error.ErrorT DeriveError
    (Monad.State.StateT State
        (Log.LogT m))

type Deriver a = DeriveT Identity.Identity a

run :: State -> Deriver a -> (Either DeriveError a, State, [Log.Msg])
run derive_state m = (err, state2, logs)
    where
    ((err, state2), logs) = (Identity.runIdentity
        . Log.run
        . flip Monad.State.runStateT derive_state
        . Error.runErrorT
        . run_derive_t) m

class (Show (Elem derived), Eq (Elem derived), Show derived) =>
        Derived derived where
    type Elem derived :: *
    -- | I would prefer to have a function to a generic reified type and then
    -- use that value to index the CacheEntry, but I can't think of how to do
    -- that right now.
    from_cache_entry :: CacheEntry -> Maybe (CallType derived)
    to_cache_entry :: CallType derived -> CacheEntry
    derived_range :: derived -> (RealTime, RealTime)
    derived_null :: derived -> Bool

type LogsDeriver d = Deriver (LEvent.LEvents d)
type Stream d = LEvent.Stream d
type EventStream d = LEvent.Stream (LEvent.LEvent d)

-- ** events

type EventDeriver = LogsDeriver Score.Event

-- | This might seem like an inefficient way to represent the Event stream, but
-- I can't think of how to make it better.
--
-- Each call generates a chunk [Event], and the chunks are then joined with
-- 'd_merge_asc'.  This means every cons is copied once, but I think this is
-- hard to avoid if I want to merge streams.
-- type Events = Events Score.Event

type Events = LEvent.LEvents Score.Event

instance Derived Score.Event where
    type Elem Score.Event = Score.Event
    from_cache_entry (CachedEvents ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedEvents
    derived_range event = (Score.event_start event, Score.event_end event)
    derived_null _ = False

-- ** control

type ControlDeriver = LogsDeriver Signal.Control
-- type Control = Signal.Control

instance Derived Signal.Control where
    type Elem Signal.Control = Signal.Y
    from_cache_entry (CachedControl ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedControl
    derived_range sig = case (Signal.first sig, Signal.last sig) of
        (Just (s, _), Just (e, _)) -> (s, e)
        _ -> (0, 0) -- TODO ummm?
    derived_null = Signal.null

-- ** pitch

type PitchDeriver = LogsDeriver PitchSignal.PitchSignal
-- type Pitch = PitchSignal.PitchSignal

instance Derived PitchSignal.PitchSignal where
    type Elem PitchSignal.PitchSignal = PitchSignal.Y
    from_cache_entry (CachedPitch ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedPitch
    derived_range sig = case (PitchSignal.first sig, PitchSignal.last sig) of
        (Just (s, _), Just (e, _)) -> (s, e)
        _ -> (0, 0)
    derived_null = PitchSignal.null

-- ** state

data State = State {
    -- Signal environment.  These form a dynamically scoped environment that
    -- applies to generated events inside its scope.

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
    , state_environ :: !TrackLang.Environ
    , state_warp :: !Score.Warp
    -- | Stack of calls currently in scope.
    , state_scopes :: ![Scope]

    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: !Stack.Stack
    -- | This is a free-form stack which can be used to prefix log msgs with
    -- a certain string.
    , state_log_context :: ![String]

    -- | This data is generally written to, and only read in special places.
    , state_collect :: !Collect
    -- | Data pertaining to the deriver cache.
    , state_cache_state :: !CacheState

    -- | This data is constant throughout the derivation.
    , state_constant :: !Constant
    }

initial_state :: [Scope] -> Cache -> ScoreDamage -> TrackLang.Environ
    -> Constant -> State
initial_state scopes cache score_damage environ constant = State
    { state_controls = initial_controls
    , state_pitches = Map.empty
    , state_pitch = PitchSignal.constant
        (State.state_default_scale (state_ui constant)) Pitch.middle_degree

    , state_environ = environ
    , state_warp = Score.id_warp
    , state_scopes = scopes
    , state_stack = Stack.empty
    , state_log_context = []

    , state_collect = mempty
    , state_cache_state = initial_cache_state cache score_damage
    , state_constant = constant
    }

type LookupCall call = TrackLang.CallId -> Deriver (Maybe call)

data Scope =
    NoteScope (LookupCall NoteCall)
    | ControlScope (LookupCall ControlCall)
    | PitchScope (LookupCall PitchCall)
    | ValScope (LookupCall ValCall)

instance Show Scope where
    show scope = case scope of
        NoteScope {} -> "((NoteScope))"
        ControlScope {} -> "((ControlScope))"
        PitchScope {} -> "((PitchScope))"
        ValScope {} -> "((ValScope))"

make_lookup :: Map.Map TrackLang.CallId call -> LookupCall call
make_lookup cmap call_id = return $ Map.lookup call_id cmap

data Constant = Constant {
    state_ui :: State.State
    , state_lookup_deriver :: LookupDeriver
    , state_control_op_map :: Map.Map TrackLang.CallId ControlOp
    , state_pitch_op_map :: Map.Map TrackLang.CallId PitchOp
    , state_lookup_scale :: LookupScale
    -- | Get the calls that should be in scope with a certain instrument.
    , state_instrument_calls :: Score.Instrument -> Maybe InstrumentCalls
    }

-- | Some ornaments only apply to a particular instrument, so each instrument
-- can bring a set of note calls and val calls into scope, via the 'Scope'
-- type.
data InstrumentCalls =
    InstrumentCalls [LookupCall NoteCall] [LookupCall ValCall]

instance Show InstrumentCalls where
    show (InstrumentCalls nlookups vlookups) = "((InstrumentCalls nlookups "
        ++ show (length nlookups) ++ " vlookups " ++ show (length vlookups)
        ++ "))"

initial_constant :: State.State -> LookupDeriver -> LookupScale
    -> (Score.Instrument -> Maybe InstrumentCalls) -> Constant
initial_constant ui_state lookup_deriver lookup_scale inst_calls = Constant
    { state_ui = ui_state
    , state_lookup_deriver = lookup_deriver
    , state_control_op_map = default_control_op_map
    , state_pitch_op_map = default_pitch_op_map
    , state_lookup_scale = lookup_scale
    , state_instrument_calls = inst_calls
    }

-- | Initial control environment.
initial_controls :: Score.ControlMap
initial_controls = Map.fromList
    [(Score.c_velocity, Signal.constant default_velocity)]

-- | See 'Perform.Midi.Perform.default_velocity' for 0.79.
default_velocity :: Signal.Y
default_velocity = 0.79

-- | These are things that collect throughout derivation, and are cached in
-- addition to the derived values.  Effectively they are return values
-- alongside the values.
--
-- 'state_local_damage' also collects, but isn't recorded by the cache since
-- by definition a cached call returns no damage.
data Collect = Collect {
    -- | Remember the warp signal for each track.  A warp usually applies to
    -- a set of tracks, so remembering them together will make the updater more
    -- efficient when it inverts them to get playback position.
    collect_warp_map :: !TrackWarp.WarpMap
    , collect_track_signals :: !Track.TrackSignals
    , collect_track_environ :: !TrackEnviron
    -- | Similar to 'state_local_damage', this is how a call records its
    -- dependencies.  After evaluation of a deriver, this will contain the
    -- dependencies of the most recent call.
    , collect_local_dep :: !LocalDep
    } deriving (Eq, Show)

instance Monoid.Monoid Collect where
    mempty = Collect mempty mempty mempty mempty
    mappend (Collect warps1 signals1 env1 deps1)
            (Collect warps2 signals2 env2 deps2) =
        Collect (warps1 <> warps2) (signals1 <> signals2) (env1 <> env2)
            (deps1 <> deps2)

-- | Snapshots of the environ at each track.  This is used by the Cmd layer to
-- figure out what the scale and instrument are for a given track.
--
-- Originally this was a map from Stacks to Environ (and only the changed
-- parts).  The idea was that I could walk up the stack to find the Environ
-- value in scope at a given point, and given Stack.Region, could even get
-- e.g. per event instruments.  Unfortunately, while it's easy to do that
-- on the Derive side, it seems really complicated and somewhat expensive to
-- try to retrace a complete stack on every cmd.  Since this implementation
-- doesn't store the entire stack, a track with a different instrument at
-- different times will wind up with the last one.
--
-- This is a much simpler solution which will hopefully work well enough in
-- practice.
type TrackEnviron = Map.Map (BlockId, TrackId) TrackLang.Environ

data CacheState = CacheState {
    state_cache :: !Cache -- modified
    , state_event_damage :: !EventDamage -- appended to
    , state_score_damage :: !ScoreDamage -- constant
    , state_control_damage :: !ControlDamage

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
    -- TODO move this to Collect?
    , state_local_damage :: !EventDamage
    } deriving (Show)

instance Monoid.Monoid CacheState where
    mempty = CacheState {
        state_cache = mempty
        , state_event_damage = EventDamage mempty
        , state_score_damage = mempty
        , state_control_damage = ControlDamage mempty
        , state_local_damage = EventDamage mempty
        }
    mappend (CacheState cache1 event1 score1 control1 local1)
            (CacheState cache2 event2 score2 control2 local2) =
        CacheState (cache1 <> cache2) (event1 <> event2) (score1 <> score2)
            (control1 <> control2) (local1 <> local2)

initial_cache_state :: Cache -> ScoreDamage -> CacheState
initial_cache_state cache score_damage = mempty {
    state_cache = cache
    , state_score_damage = score_damage
    }

-- | Hack, see 'collect_local_dep'.
type LocalDep = GeneratorDep

-- ** calls

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
passed_next_begin = fmap fst . first_event . passed_next_events

passed_prev_begin :: PassedArgs derived -> Maybe ScoreTime
passed_prev_begin = fmap fst . first_event . passed_prev_events

-- | Get the next event beginning.  Intended to be used by calls to determine
-- their extent, especially control calls, which have no explicit duration.
first_event :: [Track.PosEvent] -> Maybe Track.PosEvent
first_event ((pos, evt) : _) = Just (pos, evt)
first_event _ = Nothing

-- | Get the previous derived val.  This is used by control derivers so they
-- can interpolate from the previous sample.
passed_prev_val :: PassedArgs derived -> Maybe (RealTime, Elem derived)
passed_prev_val args = info_prev_val (passed_info args)

-- | Additional data for a call.  This part is invariant for all calls on
-- an event.
--
-- Not used at all for val calls.  The events not used for transform calls.
data CallInfo derived = CallInfo {
    -- The below is not used at all for val calls, and the events are not
    -- used for transform calls.  It might be cleaner to split those out, but
    -- too much bother.

    -- | Hack so control calls have access to the previous sample, since
    -- they tend to want to interpolate from that value.
    info_prev_val :: !(Maybe (RealTime, Elem derived))

    -- | These are warped into normalized time.
    --
    -- Calls can use this to interpret score times, which are intended to be
    -- in track score time.
    , info_event :: !Event.Event
    , info_prev_events :: [Track.PosEvent]
    , info_next_events :: [Track.PosEvent]
    -- | If there is no next event, you might want to fall back on the end of
    -- the block.
    , info_block_end :: ScoreTime

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
dummy_call_info text = CallInfo Nothing (Event.event s 1) [] [] 1 (0, 1) [] []
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
    call_name :: !String
    , call_generator :: !(Maybe (GeneratorCall derived))
    , call_transformer :: !(Maybe (TransformerCall derived))
    }

instance Show (Call derived) where
    show (Call name gen trans) =
        "((Call " ++ show name ++ " " ++ Seq.join " " tags ++ "))"
        where
        tags = [t | (t, True) <- [("generator", Maybe.isJust gen),
            ("transformer", Maybe.isJust trans)]]

type NoteCall = Call Score.Event
type ControlCall = Call Signal.Control
type PitchCall = Call PitchSignal.PitchSignal

data ValCall = ValCall {
    vcall_name :: !String
    , vcall_call :: PassedArgs TrackLang.Val -> Deriver TrackLang.Val
    }

instance Show ValCall where
    show (ValCall name _) = "((ValCall " ++ show name ++ "))"

-- | Data passed to a 'Call'.
data PassedArgs derived = PassedArgs {
    passed_vals :: ![TrackLang.Val]
    , passed_environ :: !TrackLang.Environ
    , passed_call :: !TrackLang.CallId
    , passed_info :: !(CallInfo derived)
    }

-- *** generator

data GeneratorCall derived = GeneratorCall {
    gcall_func :: GeneratorFunc derived
    , gcall_type :: GeneratorType
    -- | Block calls should put their BlockId on the stack instead of the call
    -- name.  Unfortunately by the time I get into 'd_block' it's too late.
    , gcall_block :: Maybe BlockId
    }

-- | args -> deriver
type GeneratorFunc derived = PassedArgs derived -> LogsDeriver derived

-- | Create the most common kind of generator.  The result is wrapped in
-- LEvent.Event.
generator :: (Derived derived) =>
    String -> (PassedArgs derived -> Deriver (Stream derived)) -> Call derived
generator name func = stream_generator name ((map LEvent.Event <$>) . func)

-- | Since Signals themselves are collections, there's little reason for a
-- signal generator to return a Stream of events.  So wrap the generator result
-- in a Stream singleton.
generator1 :: (Derived derived) =>
    String -> (PassedArgs derived -> Deriver derived) -> Call derived
generator1 name func = generator name ((LEvent.one <$>) . func)

-- | Like 'generator', but the deriver returns 'LEvent.LEvents' which already
-- have logs mixed in.  Useful if the generator calls a sub-deriver which will
-- already have merged the logs into the output.
stream_generator :: (Derived derived) =>
    String -> GeneratorFunc derived -> Call derived
stream_generator name func =
    Call name (Just (GeneratorCall func NonCachingGenerator Nothing)) Nothing

-- | Like 'stream_generator', but set the CachingGenerator flag, which will
-- turn on caching for this generator.
caching_generator :: (Derived derived) =>
    String -> GeneratorFunc derived -> Call derived
caching_generator name func =
    Call name (Just (GeneratorCall func CachingGenerator Nothing)) Nothing

-- *** transformer

data TransformerCall derived = TransformerCall {
    tcall_func :: TransformerFunc derived
    , tcall_type :: TransformerType
    }

-- | args -> deriver -> deriver
type TransformerFunc derived =
    PassedArgs derived -> LogsDeriver derived -> LogsDeriver derived

transformer :: (Derived derived) =>
    String -> TransformerFunc derived -> Call derived
transformer name func = Call
    name Nothing (Just (TransformerCall func NonIncremental))


-- *** misc TODO find a home

-- | The call for the whole control track.
type ControlTrackCall = BlockId -> TrackId -> PassedArgs Signal.Control
    -> Deriver (EventDeriver -> EventDeriver)

make_calls :: [(String, call)] -> Map.Map TrackLang.CallId call
make_calls = Map.fromList . map (first TrackLang.Symbol)

-- ** state support

-- | Since the deriver may vary based on the block, this is needed to find
-- the appropriate deriver.  It's created by 'Schema.lookup_deriver'.
type LookupDeriver = BlockId -> Either State.StateError EventDeriver

instance (Functor m, Monad m) => Log.LogMonad (DeriveT m) where
    write = DeriveT . lift . lift . Log.write
    initialize_msg msg = do
        -- If the msg was created by *_stack (for instance, by 'catch_warn'),
        -- it may already have a stack.
        stack <- maybe (gets state_stack) return (Log.msg_stack msg)
        context <- gets state_log_context
        return $ msg {
            Log.msg_stack = Just stack
            , Log.msg_text = add_text_context context (Log.msg_text msg)
            }

add_context :: [String] -> String -> String
add_context [] s = s
add_context context s = Seq.join " / " (reverse context) ++ ": " ++ s

-- duplicated code, ugh
add_text_context :: [String] -> Text.Text -> Text.Text
add_text_context [] s = s
add_text_context context s =
    Text.intercalate (Text.pack " / ") (map Text.pack (reverse context))
        `Text.append` (Text.pack ": ") `Text.append` s

-- * monadic ops

data Result = Result {
    r_events :: !Events
    , r_cache :: !Cache
    -- | Ranges which were rederived on this derivation.
    , r_event_damage :: !EventDamage
    , r_tempo :: !Transport.TempoFunction
    , r_closest_warp :: !Transport.ClosestWarpFunction
    , r_inv_tempo :: !Transport.InverseTempoFunction
    , r_track_signals :: !Track.TrackSignals
    , r_track_environ :: !TrackEnviron

    -- | The relevant parts of the final state should be extracted into the
    -- above fields, but returning the whole state can be useful for testing.
    , r_state :: !State
    }

-- | Kick off a derivation.
--
-- The derivation state is quite involved, so there are a lot of arguments
-- here.
derive :: Constant -> [Scope] -> Cache -> ScoreDamage -> TrackLang.Environ
    -> EventDeriver -> Result
derive constant scopes cache damage environ deriver =
    Result (merge_logs result logs) (state_cache (state_cache_state state))
        event_damage tempo_func closest_func inv_tempo_func
        (collect_track_signals collect) (collect_track_environ collect)
        state
    where
    (result, state, logs) =
        run (initial_state scopes clean_cache damage environ constant) deriver
    clean_cache = clear_damage damage cache
    collect = state_collect state
    warps = TrackWarp.collections (collect_warp_map collect)
    tempo_func = TrackWarp.tempo_func warps
    closest_func = TrackWarp.closest_warp warps
    inv_tempo_func = TrackWarp.inverse_tempo_func warps
    event_damage = state_event_damage (state_cache_state state)
        <> score_to_event_damage warps damage

-- | Convert ScoreDamage into EventDamage.
--
-- Local damage is obtained by recording the output of generators within the
-- score damage range.  This is essential to handle generators that produce
-- events outside of their range on the score.  However, it doesn't capture
-- events which were deleted, or modifications that don't produce track damage
-- ranges at all, like title changes.
--
-- Deleted events will always have score damage in their former positions, but
-- unfortunately have the same problem: if they produced score events outside
-- of the ui event range, those events won't be covered under event damage
-- after rederivation.
--
-- TODO A way to do this right would be to look at the previous score events
-- and take a diff, but at the moment I can't think of how to do that
-- efficiently.
score_to_event_damage :: TrackWarp.Collections -> ScoreDamage
    -> EventDamage
score_to_event_damage warpcs score =
    EventDamage $ Monoid.mconcat (block_ranges ++ track_ranges)
    where
    block_ranges = map block_damage (Set.elems (sdamage_blocks score))
    block_damage block_id = Ranges.ranges
        [(TrackWarp.tw_start tw, TrackWarp.tw_end tw)
            | tw <- warpcs, TrackWarp.tw_block tw == block_id]

    track_ranges = map track_damage (Map.assocs (sdamage_tracks score))
    track_damage (track_id, ranges) =
        Ranges.ranges $ case Ranges.extract ranges of
            Nothing -> [(TrackWarp.tw_start tw, TrackWarp.tw_end tw)
                | tw <- warpcs, track_id `elem` TrackWarp.tw_tracks tw]
            Just pairs -> concatMap warp pairs
        where
        warp (start, end) =
            [(Score.warp_pos start w, Score.warp_pos end w) | w <- warps]
        warps = [TrackWarp.tw_warp tw | tw <- warpcs,
            track_id `elem` TrackWarp.tw_tracks tw]

modify :: (State -> State) -> Deriver ()
modify f = (DeriveT . lift) $ do
    old <- Monad.State.get
    Monad.State.put $ f old

put :: State -> Deriver ()
put st = (DeriveT . lift) (Monad.State.put st)

-- The Monad polymorphism is required for the LogMonad instance.
get :: (Functor m, Monad m) => DeriveT m State
get = (DeriveT . lift) Monad.State.get

gets :: (Functor m, Monad m) => (State -> a) -> DeriveT m a
gets f = fmap f get

-- | This is a little different from Reader.local because only a portion of
-- the state is used Reader-style.
-- TODO split State into dynamically scoped portion and use Reader for that.
local :: (State -> b) -> (b -> State -> State)
    -> (State -> Deriver State) -> Deriver a -> Deriver a
local from_state to_state modify_state deriver = do
    old <- gets from_state
    new <- modify_state =<< get
    put new
    deriver `finally` modify (to_state old)

-- ** state access

-- | Lookup a scale_id or throw.
get_scale :: Pitch.ScaleId -> Deriver Scale
get_scale scale_id = maybe (throw $ "unknown " ++ show scale_id) return
    =<< lookup_scale scale_id

lookup_scale :: Pitch.ScaleId -> Deriver (Maybe Scale)
lookup_scale scale_id = do
    -- Defaulting the scale here means that relative pitch tracks don't need
    -- to mention their scale.
    scale_id <- if scale_id == Pitch.default_scale_id
        then gets (PitchSignal.sig_scale . state_pitch)
        else return scale_id
    lookup_scale <- gets (state_lookup_scale . state_constant)
    return $ lookup_scale scale_id

-- ** collect

modify_collect :: (Collect -> Collect) -> Deriver ()
modify_collect f = modify $ \st -> st { state_collect = f (state_collect st) }

-- ** cache

get_cache_state :: Deriver CacheState
get_cache_state = gets state_cache_state

put_cache :: Cache -> Deriver ()
put_cache cache = modify_cache_state $ \st -> st { state_cache = cache }

take_local_damage :: Deriver EventDamage
take_local_damage = do
    old <- get_cache_state
    modify_cache_state $ \st ->
        st { state_local_damage = EventDamage mempty }
    return $ state_local_damage old

insert_local_damage :: EventDamage -> Deriver ()
insert_local_damage damage = modify_cache_state $ \st ->
    st { state_local_damage = damage <> state_local_damage st }

put_local_damage :: EventDamage -> Deriver ()
put_local_damage damage = modify_cache_state $ \st ->
    st { state_local_damage = damage }

insert_event_damage :: EventDamage -> Deriver ()
insert_event_damage damage = modify_cache_state $ \st ->
    st { state_event_damage = damage <> state_event_damage st }

with_control_damage :: EventDamage -> Deriver derived -> Deriver derived
with_control_damage (EventDamage damage) = local_cache_state
    state_control_damage
    (\old st -> st { state_control_damage = old })
    (\st -> st { state_control_damage = insert (state_control_damage st) })
    where
    insert (ControlDamage ranges) = ControlDamage (ranges <> damage)

add_block_dep :: BlockId -> Deriver ()
add_block_dep block_id = modify_collect $ \st ->
    st { collect_local_dep = insert (collect_local_dep st) }
    where
    insert (GeneratorDep blocks) = GeneratorDep (Set.insert block_id blocks)

-- | Both track warps and local deps are used as dynamic return values (aka
-- modifying a variable to \"return\" something).  When evaluating a cached
-- generator, the caller wants to know the callee's track warps and local deps,
-- without getting them mixed up with its own warps and deps.  So run a deriver
-- in an empty environment, and restore it afterwards.
--
-- This catches and returns any exception rather than rethrowing because the
-- caller wants the Collect regardless of whether there was an exception or
-- not.
-- TODO I would think it shouldn't need to catch because the Collect after
-- an exception should be mempty anyway, but something's not quite right
-- because not catching breaks Cache_test.test_failed_sub_track
with_empty_collect :: Deriver a -> Deriver (Either DeriveError a, Collect)
with_empty_collect deriver = do
    old <- gets state_collect
    new <- (\st -> return $ st { state_collect = mempty }) =<< get
    put new
    result <- (fmap Right deriver) `Error.catchError` (return . Left)
    collect <- gets state_collect
    modify (\st -> st { state_collect = old })
    return (result, collect)

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

data DeriveError = DeriveError SrcPos.SrcPos Stack.Stack ErrorVal
    deriving (Eq, Show)

instance Pretty.Pretty DeriveError where
    pretty (DeriveError srcpos stack val) = "<DeriveError "
        ++ SrcPos.show_srcpos srcpos ++ " " ++ Pretty.pretty stack ++ ": "
        ++ Pretty.pretty val ++ ">"

data ErrorVal = Error String | CallError CallError
    deriving (Eq, Show)

instance Pretty.Pretty ErrorVal where
    pretty (Error s) = s
    pretty (CallError err) = Pretty.pretty err

instance Error.Error DeriveError where
    strMsg _ = DeriveError Nothing Stack.empty
        (Error "Why are you calling fail?  Don't do that!")

data CallError =
    -- | arg number, arg name, expected type, received val
    TypeError Int String TrackLang.Type (Maybe TrackLang.Val)
    -- | Couldn't even call the thing because the name was not found.
    | CallNotFound TrackLang.CallId
    -- | Calling error that doesn't fit into the above categories.
    | ArgError String
    deriving (Eq, Show)

instance Pretty.Pretty CallError where
    pretty err = case err of
        TypeError argno name expected received ->
            "TypeError: arg " ++ show argno ++ "/" ++ name ++ ": expected "
            ++ Pretty.pretty expected ++ " but got "
            ++ Pretty.pretty (TrackLang.type_of <$> received)
            ++ " " ++ Pretty.pretty received
        ArgError err -> "ArgError: " ++ err
        CallNotFound call_id -> "CallNotFound: " ++ Pretty.pretty call_id

throw :: String -> Deriver a
throw msg = throw_error (Error msg)

throw_srcpos :: SrcPos.SrcPos -> String -> Deriver a
throw_srcpos srcpos msg = throw_error_srcpos srcpos (Error msg)

throw_arg_error :: String -> Deriver a
throw_arg_error = throw_arg_error_srcpos Nothing

throw_arg_error_srcpos :: SrcPos.SrcPos -> String -> Deriver a
throw_arg_error_srcpos srcpos = throw_error_srcpos srcpos . CallError . ArgError

throw_error :: ErrorVal -> Deriver a
throw_error = throw_error_srcpos Nothing

throw_error_srcpos :: SrcPos.SrcPos -> ErrorVal -> Deriver a
throw_error_srcpos srcpos err = do
    stack <- gets state_stack
    Error.throwError (DeriveError srcpos stack err)

require :: String -> Maybe a -> Deriver a
require msg = maybe (throw msg) return

with_msg :: String -> Deriver a -> Deriver a
with_msg msg = local state_log_context
    (\old st -> st { state_log_context = old })
    (\st -> return $ st { state_log_context = msg : state_log_context st })

-- | If the derive throws, turn the error into a warning and return a default
-- value.
catch_warn :: Deriver a -> Deriver a -> Deriver a
catch_warn deflt deriver = Error.catchError deriver $
    \err -> Log.write (error_to_warn err) >> deflt

error_to_warn :: DeriveError -> Log.Msg
error_to_warn (DeriveError srcpos stack val) = Log.msg_srcpos srcpos Log.Warn
    (Just stack) ("DeriveError: " ++ Pretty.pretty val)


-- ** environment

lookup_val :: forall a. (TrackLang.Typecheck a) =>
    TrackLang.ValName -> Deriver (Maybe a)
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
require_val :: forall a. (TrackLang.Typecheck a) =>
    TrackLang.ValName -> Deriver a
require_val name = do
    val <- lookup_val name
    maybe (throw $ "environ val not found: " ++ Pretty.pretty name) return val

-- | Set the given val dynamically within the given computation.  This is
-- analogous to a dynamic let.
--
-- There is intentionally no way to modify the environment via assignment.
-- It would introduce an order of execution dependency that would complicate
-- caching as well as have a confusing non-local effect.
with_val :: (TrackLang.Typecheck val) => TrackLang.ValName -> val
    -> Deriver a -> Deriver a
with_val name val =
    local state_environ (\old st -> st { state_environ = old }) $ \st -> do
        environ <- insert_environ name val (state_environ st)
        return $ st { state_environ = environ }

insert_environ :: (TrackLang.Typecheck val) => TrackLang.ValName
    -> val -> TrackLang.Environ -> Deriver TrackLang.Environ
insert_environ name val environ =
    case TrackLang.put_val name val environ of
        Left typ -> throw $ "can't set " ++ show name ++ " to "
            ++ Pretty.pretty (TrackLang.to_val val)
            ++ ", expected " ++ Pretty.pretty typ
        Right environ2 -> return environ2


-- | Figure out the current block and track, and record the current environ
-- in the Collect.  This should be called only once per track.
record_track_environ :: State -> Collect
record_track_environ state = case stack of
        Stack.Track tid : Stack.Block bid : _ ->
            collect { collect_track_environ = insert bid tid }
        _ -> collect
    where
    -- Strip the stack down to the most recent track and block, since it will
    -- look like [tid, tid, tid, bid, ...].
    stack = Seq.drop_dups is_track $ filter track_or_block $
        Stack.innermost (state_stack state)
    track_or_block (Stack.Track _) = True
    track_or_block (Stack.Block _) = True
    track_or_block _ = False
    is_track (Stack.Track _) = True
    is_track _ = False
    collect = state_collect state
    insert bid tid = Map.insert (bid, tid) (state_environ state)
        (collect_track_environ collect)

-- *** control

type ControlOp = Signal.Control -> Signal.Control -> Signal.Control
type PitchOp = PitchSignal.PitchSignal -> PitchSignal.Relative
    -> PitchSignal.PitchSignal

-- | Return an entire signal.  Remember, signals are in RealTime, so if you
-- want to index them in ScoreTime you will have to call 'score_to_real'.
-- 'control_at_score' does that for you.
get_control :: Score.Control -> Deriver (Maybe Signal.Control)
get_control cont = Map.lookup cont <$> gets state_controls

control_at_score :: Score.Control -> ScoreTime -> Deriver (Maybe Signal.Y)
control_at_score cont pos = control_at cont =<< score_to_real pos

control_at :: Score.Control -> RealTime -> Deriver (Maybe Signal.Y)
control_at cont pos = do
    controls <- gets state_controls
    return $ fmap (\sig -> Signal.at pos sig) (Map.lookup cont controls)

pitch_at_score :: ScoreTime -> Deriver PitchSignal.Y
pitch_at_score pos = pitch_at =<< score_to_real pos

pitch_at :: RealTime -> Deriver PitchSignal.Y
pitch_at pos = do
    psig <- gets state_pitch
    return (PitchSignal.at pos psig)

pitch_degree_at :: RealTime -> Deriver Pitch.Degree
pitch_degree_at pos = PitchSignal.y_to_degree <$> pitch_at pos

get_named_pitch :: Score.Control -> Deriver (Maybe PitchSignal.PitchSignal)
get_named_pitch name = Map.lookup name <$> gets state_pitches

named_pitch_at :: Score.Control -> RealTime -> Deriver (Maybe PitchSignal.Y)
named_pitch_at name pos = do
    maybe_psig <- get_named_pitch name
    return $ PitchSignal.at pos <$> maybe_psig

named_degree_at :: Score.Control -> RealTime -> Deriver (Maybe Pitch.Degree)
named_degree_at name pos = do
    y <- named_pitch_at name pos
    return $ fmap PitchSignal.y_to_degree y

with_control :: Score.Control -> Signal.Control -> Deriver a -> Deriver a
with_control cont signal =
    local (Map.lookup cont . state_controls) insert alter
    where
    insert Nothing st = st
    insert (Just sig) st = st { state_controls =
        Map.insert cont sig (state_controls st) }
    alter st = return $ st { state_controls =
        Map.insert cont signal (state_controls st) }

with_control_operator :: Score.Control -> TrackLang.CallId
    -> Signal.Control -> Deriver a -> Deriver a
with_control_operator cont c_op signal deriver = do
    op <- lookup_control_op c_op
    with_relative_control cont op signal deriver

with_relative_control :: Score.Control -> ControlOp -> Signal.Control
    -> Deriver a -> Deriver a
with_relative_control cont op signal deriver = do
    controls <- gets state_controls
    let msg = "relative control applied when no absolute control is in scope: "
    case Map.lookup cont controls of
        Nothing -> do
            Log.warn (msg ++ show cont)
            deriver
        Just old_signal -> with_control cont (op old_signal signal) deriver

-- | Run the deriver in a context with the given pitch signal.  If a Control is
-- given, the pitch has that name, otherwise it's the unnamed default pitch.
with_pitch :: Maybe Score.Control -> PitchSignal.PitchSignal
    -> Deriver a -> Deriver a
with_pitch = modify_pitch (flip const)

with_constant_pitch :: Maybe Score.Control -> Pitch.Degree
    -> Deriver a -> Deriver a
with_constant_pitch maybe_name degree deriver = do
    pitch <- gets state_pitch
    with_pitch maybe_name
        (PitchSignal.constant (PitchSignal.sig_scale pitch) degree) deriver

with_relative_pitch :: Maybe Score.Control
    -> PitchOp -> PitchSignal.Relative -> Deriver a -> Deriver a
with_relative_pitch maybe_name sig_op signal deriver = do
    old <- gets state_pitch
    if old == PitchSignal.empty
        then do
            -- This shouldn't happen normally because of the default pitch.
            Log.warn $
                "relative pitch applied when no absolute pitch is in scope"
            deriver
        else modify_pitch sig_op maybe_name signal deriver

with_pitch_operator :: Maybe Score.Control
    -> TrackLang.CallId -> PitchSignal.Relative -> Deriver a -> Deriver a
with_pitch_operator maybe_name c_op signal deriver = do
    sig_op <- lookup_pitch_control_op c_op
    with_relative_pitch maybe_name sig_op signal deriver

modify_pitch :: (PitchSignal.PitchSignal -> PitchSignal.PitchSignal
        -> PitchSignal.PitchSignal)
    -> Maybe Score.Control -> PitchSignal.PitchSignal
    -> Deriver a -> Deriver a
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

with_scopes :: [Scope] -> Deriver a -> Deriver a
with_scopes scopes = local state_scopes (\old st -> st { state_scopes = old })
    (\st -> return $ st { state_scopes = scopes ++ state_scopes st })

-- *** specializations

velocity_at :: ScoreTime -> Deriver Signal.Y
velocity_at pos = do
    vel <- control_at Score.c_velocity =<< score_to_real pos
    return $ maybe default_velocity id vel

with_velocity :: Signal.Control -> Deriver a -> Deriver a
with_velocity = with_control Score.c_velocity


-- *** control ops

lookup_control_op :: TrackLang.CallId -> Deriver ControlOp
lookup_control_op c_op = do
    op_map <- gets (state_control_op_map . state_constant)
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

lookup_pitch_control_op :: TrackLang.CallId -> Deriver PitchOp
lookup_pitch_control_op c_op = do
    op_map <- gets (state_pitch_op_map . state_constant)
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

get_current_block_id :: Deriver BlockId
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

add_track_warp :: TrackId -> Deriver ()
add_track_warp track_id = do
    stack <- gets state_stack
    modify_collect $ \st -> st { collect_warp_map =
        Map.insert stack (Right track_id) (collect_warp_map st) }

-- | Start a new track warp for the current block_id.
--
-- This must be called for each block, and it must be called after the tempo is
-- warped for that block so it can install the new warp.
add_new_track_warp :: Maybe TrackId -> Deriver ()
add_new_track_warp track_id = do
    stack <- gets state_stack
    block_id <- get_current_block_id
    start <- now
    time_end <- get_block_dur block_id
    end <- score_to_real time_end
    warp <- gets state_warp
    let tw = Left $ TrackWarp.TrackWarp (start, end, warp, block_id, track_id)
    modify_collect $ \st -> st { collect_warp_map =
        Map.insert stack tw (collect_warp_map st) }

-- * basic derivers

-- ** tempo

-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo implies,
-- which is integral (1/tempo).

score_to_real :: ScoreTime -> Deriver RealTime
score_to_real pos = do
    warp <- gets state_warp
    return (Score.warp_pos pos warp)

now :: Deriver RealTime
now = score_to_real 0

real_to_score :: RealTime -> Deriver ScoreTime
real_to_score pos = do
    warp <- gets state_warp
    maybe (throw $ "real_to_score out of range: " ++ show pos) return
        (Score.unwarp_pos pos warp)

min_tempo :: Signal.Y
min_tempo = 0.001

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

with_warp :: (Score.Warp -> Score.Warp) -> Deriver a -> Deriver a
with_warp f = local state_warp (\w st -> st { state_warp = w }) $ \st ->
    return $ st { state_warp = f (state_warp st) }

in_real_time :: Deriver a -> Deriver a
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
-- The track_id is needed to record this track in TrackWarps.  It's optional
-- because, if there's no explicit tempo track, there's an implicit tempo
-- around the whole block, but the implicit one doesn't have a track of course.
--
-- The block_id is used to stretch the block to a length of 1, regardless of
-- the tempo.  This means that when the calling block stretches it to the
-- duration of the event it winds up being the right length.  This is skipped
-- for the top level block or all pieces would last exactly 1 second.
-- This is another reason every block must have a 'd_tempo' at the top.
--
-- d_block might seem like a better place to do this, but it doesn't have the
-- local warp yet.
--
-- TODO relying on the stack seems a little implicit, would it be better
-- to pass Maybe BlockId or Maybe ScoreTime?
--
-- TODO what to do about blocks with multiple tempo tracks?  I think it would
-- be best to stretch the block to the first one.  I could break out
-- stretch_to_1 and have compile apply it to only the first tempo track.
d_tempo :: BlockId -> Maybe TrackId -> Signal.Tempo -> Deriver a -> Deriver a
d_tempo block_id maybe_track_id signal deriver = do
    let warp = tempo_to_warp signal
    root <- is_root_block
    stretch_to_1 <- if root then return id
        else do
            block_dur <- get_block_dur block_id
            real_dur <- with_warp (const warp) (score_to_real block_dur)
            -- Log.debug $ "dur, global dur "
            --     ++ show (block_id, block_dur, real_dur)
            when (block_dur == 0) $
                throw $ "can't derive a block with zero duration"
            return (d_stretch (1 / RealTime.to_score real_dur))
    stretch_to_1 $ d_warp warp $ do
        add_new_track_warp maybe_track_id
        deriver

is_root_block :: Deriver Bool
is_root_block = do
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
get_block_dur :: BlockId -> Deriver ScoreTime
get_block_dur block_id = do
    ui_state <- get_ui_state
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
track_setup track_id deriver = add_track_warp track_id >> deriver

-- | This is a version of 'track_setup' for the tempo track.  It doesn't record
-- the track warp, see 'd_tempo' for why.
setup_without_warp :: Deriver d -> Deriver d
setup_without_warp = in_real_time

-- * utils

get_ui_state :: Deriver State.State
get_ui_state = gets (state_ui . state_constant)

-- | Because DeriveT is not a UiStateMonad.
--
-- TODO I suppose it could be, but then I'd be tempted to make
-- a ReadOnlyUiStateMonad.  And I'd have to merge the exceptions.
get_track :: TrackId -> Deriver Track.Track
get_track track_id = lookup_id track_id . State.state_tracks =<< get_ui_state

get_block :: BlockId -> Deriver Block.Block
get_block block_id = lookup_id block_id . State.state_blocks =<< get_ui_state

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k) => k -> Map.Map k a -> Deriver a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- | So this is kind of confusing.  When events are created, they are assigned
-- their stack based on the current event_stack, which is set by the
-- with_stack_* functions.  Then, when they are processed, the stack is used
-- to *set* event_stack, which is what 'Log.warn' and 'throw' will look at.
with_event :: Score.Event -> Deriver a -> Deriver a
with_event event = local state_stack
    (\old st -> st { state_stack = old })
    (\st -> return $ st { state_stack = Score.event_stack event })

-- ** merge

-- | The EventDerivers run as sub-derivers and the results are mappended, which
-- lets them to interleave their work or run in parallel.
d_merge :: [EventDeriver] -> EventDeriver
d_merge [d] = d -- TODO this optimization lets exceptions through... do I care?
d_merge derivers = do
    state <- get
    -- Since track warp mappend is plain concat, if I don't clear the collect
    -- I will get duplicate entries.
    let cleared = state { state_collect = mempty }
    let (streams, collects, caches) =
            List.unzip3 (map (run_sub cleared) derivers)
    modify $ \st -> st
        { state_collect = Monoid.mconcat (state_collect state : collects)
        , state_cache_state = Monoid.mconcat caches
        }
    return (Seq.merge_lists _event_start streams)

type PureResult derived = (Stream (LEvent.LEvent derived), Collect, CacheState)

-- | Run the given deriver and return the relevant data.
run_sub :: State -> LogsDeriver derived -> PureResult derived
run_sub state deriver =
    (merge_logs result logs, state_collect state2, state_cache_state state2)
    where (result, state2, logs) = run state deriver

merge_logs :: Either DeriveError (LEvent.LEvents d) -> [Log.Msg]
    -> LEvent.LEvents d
merge_logs result logs = case result of
    Left err -> map LEvent.Log (logs ++ [error_to_warn err])
    Right events -> events ++ map LEvent.Log logs

-- | Merge sorted lists of events.  If the lists themselves are also sorted,
-- I can produce output without scanning the entire input list, so this should
-- be more efficient for a large input list than 'merge_events'.
merge_asc_events :: [Events] -> Events
merge_asc_events = Seq.merge_asc_lists _event_start

merge_events :: Events -> Events -> Events
merge_events = Seq.merge_on _event_start

-- | This will make logs always merge ahead of score events, but that should
-- be ok.
_event_start :: LEvent.LEvent Score.Event -> RealTime
_event_start (LEvent.Log _) = 0
_event_start (LEvent.Event event) = Score.event_start event

-- -- | unused monoidal interface
-- instance Monoid.Monoid EventDeriver where
--     mempty = return empty_stream
--     mappend d1 d2 = d_merge [d1, d2]
--     mconcat = d_merge


-- * negative duration

-- process_negative_durations :: Events -> Events
-- process_negative_durations = id

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
    deriving (Monoid.Monoid, Show)
    -- The monoid instance winds up being a left-biased union.  This is ok
    -- because merged caches shouldn't overlap anyway.

-- finding prefixes becomes trivial, but lookup is probably slower
-- or a 'Map.Map Stack.Frame (Either CacheEntry Cache)'
-- newtype Cache = Cache (Tree.Forest (Stack.Frame, CacheEntry))
--     deriving (Show)

-- | Since an entire track is one type but will have many different calls of
-- different types, the deriver type division goes above the call type
-- division.
data CacheEntry =
    CachedEvents !(CallType Score.Event)
    | CachedControl !(CallType Signal.Control)
    | CachedPitch !(CallType PitchSignal.PitchSignal)
    deriving (Show)

-- | The type here should match the type of the stack it's associated with,
-- but I'm not quite up to those type gymnastics yet.
data CallType derived = CachedGenerator !Collect !(LEvent.LEvents derived)
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
    -- ('GeneratorDep') rather than tracks, so it's convenient to keep the
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
            (tblocks1 <> tblocks2) (blocks1 <> blocks2)

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


-- * scale

type LookupScale = Pitch.ScaleId -> Maybe Scale
type Transpose = Pitch.Octave -> Integer -> Pitch.Note -> Maybe Pitch.Note

data Scale = Scale {
    scale_id :: Pitch.ScaleId
    -- | A pattern describing what the scale notes look like.  Used only for
    -- error msgs (i.e. parse errors) so it should be human readable and
    -- doesn't have to follow any particular syntax.  A regex is recommended
    -- though.
    , scale_pattern :: String
    -- | This is passed to the UI so it knows what to call scale degrees when
    -- rendering a pitch signal with this scale.
    , scale_map :: Track.ScaleMap

    -- | If a scale uses 'Symbol.Symbol's, it can include the definitions here
    -- so they are close to their use.  This symbol list should be loaded as
    -- soon as possible, which means program startup for hardcoded scales.
    , scale_symbols :: [Symbol.Symbol]

    -- | Transpose a Note by a given number of octaves and integral degrees.
    -- Will be nothing if the pitch is out of range, or the scale doesn't have
    -- octaves.
    , scale_transpose :: Transpose

    -- | Used by derivation.
    , scale_note_to_call :: Pitch.Note -> Maybe ValCall

    -- | Used by note input.
    , scale_input_to_note :: Pitch.InputKey -> Maybe Pitch.Note
    -- | Used by MIDI thru.  This is a shortcut for
    -- @degree_to_nn . note_to_degree . input_to_note@ but can be implemented
    -- more efficiently by the scale.
    , scale_input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber

    -- | Used by conversion before performance.
    , scale_degree_to_nn :: Pitch.Degree -> Maybe Pitch.NoteNumber
    }
