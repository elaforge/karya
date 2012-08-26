{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-} -- for super-classes of Derived
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{- | Implementation for the Deriver monad.

    This module should contain only 'Deriver' and the definitions needed to
    implement it.  Unfortunately that winds up being quite a lot.  Deriver
    carries a 'State', which is depended upon directly by all derivation, so
    it's just as complicated as derivation itself, which is to say, very.

    Since State only really requires data types, the majority of this module
    is data declarations, with the exception of a few constructors which
    are intimately concerned with the type they are constructing.  The vast
    library of functions to manipulate these types are split into
    "Derive.Deriver.Lib".

    This module is way too big.  Unfortunately it's hard to split up because
    of circular imports.  Anyone who directly or indirectly needs Deriver
    (e.g. calls) needs to import Derive.  However, anything directly or
    indirectly used by State must be imported by Derive.  Since State is the
    central type that must hold anything that persists beyond the evaluation
    of a single note, that winds up being a lot.  At one point I tried to
    reign in the madness with hs-boot files, but that's an iatrogenic cure
    since the hs-boot madness is worse.
-}
module Derive.Deriver.Monad (
    Deriver, RunResult
    , modify, get, gets, put, run

    -- * error
    , Error(..), ErrorVal(..), CallError(..)
    , throw, throw_srcpos, throw_arg_error, throw_arg_error_srcpos
    , throw_error, throw_error_srcpos

    -- * derived types
    , Derived(..)
    , LogsDeriver, Stream, EventStream

    , EventDeriver, Events
    , ControlDeriver, PitchDeriver

    -- * state
    , State(..), initial_state
    , Dynamic(..), initial_dynamic
    , initial_controls, default_dynamic

    -- ** scope
    , Scope(..), empty_scope, ScopeType(..), empty_scope_type
    , LookupCall, make_lookup

    -- ** constant
    , Constant(..), initial_constant
    , op_add, op_sub, op_mul

    -- ** instrument
    , Instrument(..), InstrumentCalls(..)

    -- ** control
    , ControlOp

    -- ** collect
    , Collect(..), Integrated(..)
    , TrackDynamic

    -- * calls
    , NoteCallMap, ControlCallMap, PitchCallMap, ValCallMap
    , CallInfo(..), dummy_call_info
    , Call(..)
    , NoteCall, ControlCall, PitchCall, ValCall(..)
    , PassedArgs(..)

    -- ** generator
    , GeneratorCall
    , generator, generator1, stream_generator

    -- ** transformer
    , TransformerCall
    , transformer

    -- ** cache types
    -- $cache_doc
    , Cache(..), Cached(..), cache_size
    , CacheEntry(..), CallType
    , GeneratorDep(..)

    -- ** damage
    , ScoreDamage(..)
    , ControlDamage(..)

    -- * scale
    -- $scale_doc
    , Scale(..)
    , LookupScale, Transpose, Enharmonics, ScaleError(..)

    -- * testing
    , invalidate_damaged
) where
import qualified Control.Applicative as Applicative
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

import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Symbol as Symbol
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


-- * Deriver internals

type Logs = [Log.Msg]

newtype Deriver a = Deriver { _runD :: forall r.
    State -> Logs -> Failure r -> Success a r -> RunResult r }

type Failure r = State -> Logs -> Error -> RunResult r
type Success a r = State -> Logs -> a -> RunResult r
type RunResult a = (Either Error a, State, Logs)

{-# INLINE returnC #-}
returnC :: a -> Deriver a
returnC a = Deriver $ \st logs _ win -> win st logs a

{-# INLINE bindC #-}
bindC :: Deriver a -> (a -> Deriver b) -> Deriver b
bindC m f = Deriver $ \st1 logs1 lose win ->
    _runD m st1 logs1 lose (\st2 logs2 a -> _runD (f a) st2 logs2 lose win)

{-# INLINE apC #-}
apC :: Deriver (a -> b) -> Deriver a -> Deriver b
apC mf ma = do
    f <- mf
    a <- ma
    return (f a)

{-# INLINE fmapC #-}
fmapC :: (a -> b) -> Deriver a -> Deriver b
fmapC f m = Deriver $ \st1 logs1 lose win ->
    _runD m st1 logs1 lose (\st2 logs2 a -> win st2 logs2 (f a))

_throw :: Error -> Deriver a
_throw err = Deriver $ \st logs lose _ -> lose st logs err

{-# INLINE modify #-}
modify :: (State -> State) -> Deriver ()
modify f = Deriver $ \st logs _ win -> win (f st) logs ()

{-# INLINE get #-}
get :: Deriver State
get = Deriver $ \st logs _ win -> win st logs st

{-# INLINE gets #-}
gets :: (State -> a) -> Deriver a
gets f = f <$> get

{-# INLINE put #-}
put :: State -> Deriver ()
put st = Deriver $ \_ logs _ win -> win st logs ()

instance Functor Deriver where
    fmap = fmapC

instance Applicative.Applicative Deriver where
    pure = returnC
    (<*>) = apC

instance Monad Deriver where
    return = returnC
    (>>=) = bindC
    fail = throw

instance Log.LogMonad Deriver where
    write msg = Deriver $ \st logs _ win -> win st (msg:logs) ()
    initialize_msg msg = do
        -- If the msg was created by *_stack (for instance, by 'catch_warn'),
        -- it may already have a stack.
        stack <- maybe (gets (Stack.to_strings . state_stack . state_dynamic))
            return (Log.msg_stack msg)
        context <- gets (state_log_context . state_dynamic)
        return $ msg {
            Log.msg_stack = Just stack
            , Log.msg_text = add_text_context context (Log.msg_text msg)
            }
        where
        add_text_context :: [String] -> Text.Text -> Text.Text
        add_text_context [] s = s
        add_text_context context s =
            Text.intercalate " / " (map Text.pack (reverse context))
                <> ": " <> s

run :: State -> Deriver a -> RunResult a
run state m = _runD m state []
    (\st logs err -> (Left err, st, reverse logs))
    (\st logs a -> (Right a, st, reverse logs))


-- * error

data Error = Error SrcPos.SrcPos Stack.Stack ErrorVal
    deriving (Show)

instance Pretty.Pretty Error where
    pretty (Error srcpos stack val) =
        SrcPos.show_srcpos srcpos ++ " " ++ Pretty.pretty stack ++ ": "
        ++ Pretty.pretty val

data ErrorVal = GenericError String | CallError CallError
    deriving (Show)

instance Pretty.Pretty ErrorVal where
    pretty (GenericError s) = s
    pretty (CallError err) = Pretty.pretty err

data CallError =
    -- | arg number, arg name, expected type, received val
    TypeError Int String TrackLang.Type (Maybe TrackLang.Val)
    -- | Couldn't even call the thing because the name was not found.
    | CallNotFound TrackLang.CallId
    -- | Calling error that doesn't fit into the above categories.
    | ArgError String
    deriving (Show)

instance Pretty.Pretty CallError where
    pretty err = case err of
        TypeError argno name expected received ->
            "TypeError: arg " ++ show argno ++ "/" ++ name ++ ": expected "
            ++ Pretty.pretty expected ++ " but got "
            ++ Pretty.pretty (TrackLang.type_of <$> received)
            ++ ": " ++ Pretty.pretty received
        ArgError err -> "ArgError: " ++ err
        CallNotFound call_id -> "CallNotFound: " ++ Pretty.pretty call_id

throw :: String -> Deriver a
throw msg = throw_error (GenericError msg)

throw_srcpos :: SrcPos.SrcPos -> String -> Deriver a
throw_srcpos srcpos msg = throw_error_srcpos srcpos (GenericError msg)

throw_arg_error :: String -> Deriver a
throw_arg_error = throw_arg_error_srcpos Nothing

throw_arg_error_srcpos :: SrcPos.SrcPos -> String -> Deriver a
throw_arg_error_srcpos srcpos =
    throw_error_srcpos srcpos . CallError . ArgError

throw_error :: ErrorVal -> Deriver a
throw_error = throw_error_srcpos Nothing

throw_error_srcpos :: SrcPos.SrcPos -> ErrorVal -> Deriver a
throw_error_srcpos srcpos err = do
    stack <- gets (state_stack . state_dynamic)
    _throw (Error srcpos stack err)


-- * derived types

-- | Cachable might be a better name.  The Elem type is only used for
-- 'info_prev_val', that could go in a separate type family, especially
-- since it also applies to TrackLang.Val calls, which are not cacheable.
class (Show (Elem derived), Show derived) => Derived derived where
    type Elem derived :: *
    -- | I would prefer to have a function to a generic reified type and then
    -- use that value to index the CacheEntry, but I can't think of how to do
    -- that right now.
    from_cache_entry :: CacheEntry -> Maybe (CallType derived)
    to_cache_entry :: CallType derived -> CacheEntry

type LogsDeriver d = Deriver (LEvent.LEvents d)
type Stream d = LEvent.Stream d
type EventStream d = LEvent.Stream (LEvent.LEvent d)

-- ** event

type EventDeriver = LogsDeriver Score.Event

-- | This might seem like an inefficient way to represent the Event stream,
-- but I can't think of how to make it better.
--
-- Each call generates a chunk [Event], and the chunks are then joined with
-- 'd_merge_asc'.  This means every cons is copied once, but I think this is
-- hard to avoid if I want to merge streams.

type Events = LEvent.LEvents Score.Event

instance Derived Score.Event where
    type Elem Score.Event = Score.Event
    from_cache_entry (CachedEvents ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedEvents

-- ** control

type ControlDeriver = LogsDeriver Signal.Control

instance Derived Signal.Control where
    type Elem Signal.Control = Signal.Y
    from_cache_entry (CachedControl ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedControl

-- ** pitch

type PitchDeriver = LogsDeriver PitchSignal.Signal

instance Derived PitchSignal.Signal where
    type Elem PitchSignal.Signal = PitchSignal.Pitch
    from_cache_entry (CachedPitch ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedPitch


-- * state

data State = State {
    -- | This data is modified in a dynamically scoped way, for
    -- sub-derivations.
    state_dynamic :: Dynamic
    -- | This data is mappended.  It functions like an implicit return value.
    , state_collect :: !Collect
    -- | This data is constant throughout the derivation.
    , state_constant :: !Constant
    }

initial_state :: Scope -> TrackLang.Environ -> Constant -> State
initial_state scope environ constant = State
    { state_dynamic = initial_dynamic scope environ
    , state_collect = mempty
    , state_constant = constant
    }

-- | This is a dynamically scoped environment that applies to generated events
-- inside its scope.
data Dynamic = Dynamic {
    -- | Derivers can modify it for sub-derivers, or look at it, whether to
    -- attach to an Event or to handle internally.
    state_controls :: !Score.ControlMap
    -- | Named pitch signals.
    , state_pitches :: !Score.PitchMap
    -- | The unnamed pitch signal currently in scope.  This is the pitch signal
    -- that's applied to notes by default.  It's split off from 'state_pitches'
    -- because it's convenient to guarentee that the main pitch signal is
    -- always present.
    , state_pitch :: !PitchSignal.Signal
    , state_environ :: !TrackLang.Environ
    , state_warp :: !Score.Warp
    -- | Stack of calls currently in scope.
    , state_scope :: !Scope
    , state_control_damage :: !ControlDamage

    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: !Stack.Stack
    -- | This is a free-form stack which can be used to prefix log msgs with
    -- a certain string.
    , state_log_context :: ![String]
    } deriving (Show)

initial_dynamic :: Scope -> TrackLang.Environ -> Dynamic
initial_dynamic scope environ = Dynamic
    { state_controls = initial_controls
    , state_pitches = Map.empty
    , state_pitch = mempty
    , state_environ = environ
    , state_warp = Score.id_warp
    , state_scope = scope
    , state_control_damage = mempty
    , state_stack = Stack.empty
    , state_log_context = []
    }

-- | Initial control environment.
initial_controls :: Score.ControlMap
initial_controls = Map.fromList
    [ (Score.c_dynamic, Score.untyped (Signal.constant default_dynamic))
    ]

-- | See 'Perform.Midi.Perform.default_velocity' for 0.79.
default_dynamic :: Signal.Y
default_dynamic = 0.79

-- ** scope

-- | This represents all calls in scope.  Different types of calls are in scope
-- depending on the track type, except ValCalls, which are in scope everywhere.
-- This is dynamic scope, not lexical scope.
data Scope = Scope {
    scope_note :: !(ScopeType NoteCall)
    , scope_control :: !(ScopeType ControlCall)
    , scope_pitch :: !(ScopeType PitchCall)
    , scope_val :: !(ScopeType ValCall)
    } deriving (Show)

empty_scope :: Scope
empty_scope = Scope empty_scope_type empty_scope_type empty_scope_type
    empty_scope_type

-- | An instrument or scale may put calls into scope.  If that instrument
-- or scale is replaced with another, the old calls must be replaced with
-- the new ones.
--
-- This is hard-coded for the types of scopes I currently use.  It would be
-- more general to make this a Map from symbol to scopes, but since I only
-- have three types at the moment, this is simpler.
data ScopeType call = ScopeType {
    stype_instrument :: ![LookupCall call]
    , stype_scale :: ![LookupCall call]
    , stype_builtin :: ![LookupCall call]
    }

empty_scope_type :: ScopeType call
empty_scope_type = ScopeType [] [] []

instance Show (ScopeType call) where
    show (ScopeType inst scale builtin) =
        "((ScopeType" ++ n inst ++ n scale ++ n builtin ++ "))"
        where n = (" "++) . show . length

-- | For flexibility, a scope is represented not by a map from symbols to
-- derivers, but a function.  That way, it can inspect the CallId and return
-- an appropriate call, as is the case for some scales.
--
-- It's in Deriver because this same type is used for at the top-level track
-- derivation lookup, so it needs to look in the environment for the Scope.
-- The lookup itself presumably won't use Deriver and will just be a return, as
-- in 'make_lookup' or 'Derive.Call.lookup_scale_val'.
type LookupCall call = TrackLang.CallId -> Deriver (Maybe call)

-- | In the common case, a scope is simply a static map.
make_lookup :: Map.Map TrackLang.CallId call -> LookupCall call
make_lookup cmap call_id = return $ Map.lookup call_id cmap

-- ** constant

data Constant = Constant {
    state_ui :: State.State
    , state_control_op_map :: Map.Map TrackLang.CallId ControlOp
    , state_lookup_scale :: LookupScale
    -- | Get the calls and environ that should be in scope with a certain
    -- instrument.  The environ is merged with the environ in effect.
    , state_lookup_instrument :: Score.Instrument -> Maybe Instrument
    -- | Cache from the last derivation.
    , state_cache :: !Cache
    , state_score_damage :: !ScoreDamage
    }

initial_constant :: State.State -> LookupScale
    -> (Score.Instrument -> Maybe Instrument) -> Cache -> ScoreDamage
    -> Constant
initial_constant ui_state lookup_scale lookup_inst cache score_damage = Constant
    { state_ui = ui_state
    , state_control_op_map = default_control_op_map
    , state_lookup_scale = lookup_scale
    , state_lookup_instrument = lookup_inst
    , state_cache = invalidate_damaged score_damage cache
    , state_score_damage = score_damage
    }

-- ** instrument

-- | Mostly the deriver just deals with instruments as strings, and doesn't
-- understand anything else about them.  However, it does need a few other
-- things, which are expressed here to avoid excessive dependencies between the
-- systems.
data Instrument = Instrument {
    inst_calls :: InstrumentCalls
    , inst_environ :: TrackLang.Environ
    } deriving (Show)

-- | Some ornaments only apply to a particular instrument, so each instrument
-- can bring a set of note calls and val calls into scope, via the 'Scope'
-- type.
data InstrumentCalls =
    InstrumentCalls [LookupCall NoteCall] [LookupCall ValCall]

instance Show InstrumentCalls where
    show (InstrumentCalls nlookups vlookups) = "((InstrumentCalls note "
        ++ show (length nlookups) ++ " val " ++ show (length vlookups) ++ "))"


-- ** control

-- | This is a monoid used for combining two signals.  The identity value
-- is only used when a relative signal is applied when no signal is in scope.
-- This is useful for e.g. a transposition signal which shouldn't care if
-- there is or isn't a transposition signal already in scope.
type ControlOp = (Signal.Control -> Signal.Control -> Signal.Control, Signal.Y)

-- *** control ops

-- | Default set of control operators.  Merged at runtime with the static
-- config.  TODO but not yet
default_control_op_map :: Map.Map TrackLang.CallId ControlOp
default_control_op_map = Map.fromList $ map (first TrackLang.Symbol)
    [ ("add", op_add)
    , ("sub", op_sub)
    , ("mul", op_mul)
    -- These values should never be seen since any reasonable combining signal
    -- will be within this range.
    , ("max", (Signal.sig_max, -2^32))
    , ("min", (Signal.sig_min, 2^32))
    ]

op_add, op_sub, op_mul :: ControlOp
op_add = (Signal.sig_add, 0)
op_sub = (Signal.sig_subtract, 0)
op_mul = (Signal.sig_multiply, 1)


-- ** collect

-- | These are things that collect throughout derivation, and are cached in
-- addition to the derived values.  Effectively they are return values
-- alongside the values.
data Collect = Collect {
    -- | Remember the warp signal for each track.  A warp usually applies to
    -- a set of tracks, so remembering them together will make the updater more
    -- efficient when it inverts them to get playback position.
    collect_warp_map :: !TrackWarp.WarpMap
    , collect_track_signals :: !Track.TrackSignals
    , collect_track_dynamic :: !TrackDynamic
    -- | This is how a call records its dependencies.  After evaluation of
    -- a deriver, this will contain the dependencies of the most recent call.
    , collect_local_dep :: !GeneratorDep

    -- | New caches accumulating over the course of the derivation.
    , collect_cache :: !Cache
    , collect_integrated :: ![Integrated]
    } deriving (Show)

data Integrated = Integrated {
    -- BlockId for a block integration, TrackId for a track integration.
    integrated_source :: !(Either BlockId TrackId)
    , integrated_events :: !Events
    -- | Needed to convert the event pitches back to symbolic form.
    -- TODO but to work with changing keys I'd need to collect key on each
    -- event.  Of course, the integrator isn't going to be able to reproduce
    -- the changing keys.  I don't think integrate nedes to get that fancy
    -- anyway.
    , integrated_key :: !(Maybe Pitch.Key)
    } deriving (Show)

instance Monoid.Monoid Collect where
    mempty = Collect mempty mempty mempty mempty mempty mempty
    mappend (Collect warps1 signals1 env1 deps1 cache1 integrated1)
            (Collect warps2 signals2 env2 deps2 cache2 integrated2) =
        Collect (warps1 <> warps2) (signals1 <> signals2) (env1 <> env2)
            (deps1 <> deps2) (cache1 <> cache2) (integrated1 <> integrated2)

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
type TrackDynamic = Map.Map (BlockId, TrackId) Dynamic


-- ** calls

type NoteCallMap = Map.Map TrackLang.CallId NoteCall
type ControlCallMap = Map.Map TrackLang.CallId ControlCall
type PitchCallMap = Map.Map TrackLang.CallId PitchCall
type ValCallMap = Map.Map TrackLang.CallId ValCall

-- | Data passed to a 'Call'.
--
-- PassedArgs and 'CallInfo' could take the Elem type directly instead of
-- derived, but it's more convenient for the callers to pass the derived.
data PassedArgs derived = PassedArgs {
    passed_vals :: ![TrackLang.Val]
    , passed_call :: !TrackLang.CallId
    , passed_info :: !(CallInfo derived)
    }

-- | Additional data for a call.  This part is invariant for all calls on
-- an event.
--
-- Not used at all for val calls.  The events not used for transform calls.
-- TODO make separate types so the irrelevent data need not be passed
data CallInfo derived = CallInfo {
    -- | The expression currently being evaluated.  Why I need this is
    -- documented in 'Derive.Call.Note.inverting'.
    info_expr :: !TrackLang.Expr
    -- The below is not used at all for val calls, and the events are not
    -- used for transform calls.  It might be cleaner to split those out, but
    -- too much bother.

    -- | Hack so control calls have access to the previous sample, since
    -- they tend to want to interpolate from that value.
    , info_prev_val :: !(Maybe (RealTime, Elem derived))

    , info_event :: !Event.Event
    , info_prev_events :: ![Event.Event]
    , info_next_events :: ![Event.Event]

    -- | The extent of the note past its duration.  Since notes have decay,
    -- its important to capture control for that.  Normally this is the next
    -- event's start.  If there's no next event because it's the last event of
    -- the block, this is the block end, otherwise if there's no next event
    -- because it was sliced off, this is where that event would have started.
    --
    -- This is the same as the first element of 'info_next_events' except of
    -- course it has a value even when there is no next event.
    , info_event_end :: !ScoreTime

    -- | This is the track range from 'State.tevents_range'.  For sliced
    -- tracks, it will tell where in the track the slice lies.  This is needed
    -- for 'Stack.Region' entries.
    , info_track_range :: !(ScoreTime, ScoreTime)

    -- | The track tree below note tracks.  Not given for control tracks.
    , info_sub_tracks :: !TrackTree.EventsTree
    }

-- | Transformer calls don't necessarily apply to any particular event, and
-- neither to generators for that matter.
dummy_call_info :: ScoreTime -> ScoreTime -> String -> CallInfo derived
dummy_call_info start dur text = CallInfo
    { info_expr = []
    , info_prev_val = Nothing
    , info_event = Event.event start dur s
    , info_prev_events = []
    , info_next_events = []
    , info_event_end = start + dur
    , info_track_range = (start, start + dur)
    , info_sub_tracks = []
    } where s = if null text then "<no-event>" else "<" ++ text ++ ">"

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
type PitchCall = Call PitchSignal.Signal

data ValCall = ValCall {
    vcall_name :: !String
    , vcall_call :: PassedArgs TrackLang.Val -> Deriver TrackLang.Val
    }

instance Show ValCall where
    show (ValCall name _) = "((ValCall " ++ show name ++ "))"

-- ** generator

-- | args -> deriver
type GeneratorCall derived = PassedArgs derived -> LogsDeriver derived

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
    String -> GeneratorCall derived -> Call derived
stream_generator name func = Call name (Just func) Nothing

-- ** transformer

-- | args -> deriver -> deriver
type TransformerCall derived =
    PassedArgs derived -> LogsDeriver derived -> LogsDeriver derived

transformer :: (Derived derived) =>
    String -> TransformerCall derived -> Call derived
transformer name func = Call name Nothing (Just func)


-- ** cache types

-- $cache_doc
-- The cache types are nominally exported from "Derive.Cache", but must be
-- defined here to avoid circular dependencies.

-- instead of a stack, this could be a tree of frames
newtype Cache = Cache (Map.Map Stack.Stack Cached)
    deriving (Monoid.Monoid, Show)
    -- The monoid instance winds up being a left-biased union.  This is ok
    -- because merged caches shouldn't overlap anyway.

-- | When cache entries are invalidated by ScoreDamage, a marker is left in
-- their place.  This is just for a nicer log msg that can tell the difference
-- between never evaluated and damaged.
data Cached = Cached CacheEntry | Invalid
    deriving (Show)

cache_size :: Cache -> Int
cache_size (Cache c) = Map.size c

-- | Since an entire track is one type but will have many different calls of
-- different types, the deriver type division goes above the call type
-- division.
data CacheEntry =
    CachedEvents !(CallType Score.Event)
    | CachedControl !(CallType Signal.Control)
    | CachedPitch !(CallType PitchSignal.Signal)
    deriving (Show)

-- | The type here should match the type of the stack it's associated with,
-- but I'm not quite up to those type gymnastics yet.
type CallType derived = (Collect, LEvent.LEvents derived)

-- ** deps

newtype GeneratorDep = GeneratorDep (Set.Set BlockId)
    deriving (Monoid.Monoid, Show, Eq)

-- ** damage

-- | Modified ranges in the score.
data ScoreDamage = ScoreDamage {
    -- | Damaged ranges in tracks.
    sdamage_tracks :: !(Map.Map TrackId (Ranges.Ranges ScoreTime))
    -- | The blocks with damaged tracks.  Calls depend on blocks
    -- ('GeneratorDep') rather than tracks, so it's convenient to keep the
    -- blocks here.  This is different than block damage because a damaged
    -- block will invalidate all caches below it, but a block with damaged
    -- tracks must be called but may still have valid caches within.
    , sdamage_track_blocks :: !(Set.Set BlockId)
    -- | Blocks which are entirely damaged.
    , sdamage_blocks :: !(Set.Set BlockId)
    } deriving (Eq, Show)

instance Monoid.Monoid ScoreDamage where
    mempty = ScoreDamage Map.empty Set.empty Set.empty
    mappend (ScoreDamage tracks1 tblocks1 blocks1)
            (ScoreDamage tracks2 tblocks2 blocks2) =
        ScoreDamage (Map.mappend tracks1 tracks2)
            (tblocks1 <> tblocks2) (blocks1 <> blocks2)

instance Pretty.Pretty ScoreDamage where
    format (ScoreDamage tracks track_blocks blocks) =
        Pretty.record_title "ScoreDamage"
            [ ("tracks", Pretty.format tracks)
            , ("track_blocks", Pretty.format track_blocks)
            , ("blocks", Pretty.format blocks)
            ]

-- | Clear the damaged portions out of the cache so they will rederive.
invalidate_damaged :: ScoreDamage -> Cache -> Cache
invalidate_damaged (ScoreDamage tracks _ blocks) (Cache cache) =
    Cache $ Map.mapWithKey invalidate $ Map.filter is_valid cache
    where
    is_valid Invalid = False
    is_valid _ = True
    invalidate stack cached
        | has_damage stack = Invalid
        | otherwise = cached
    has_damage stack =
        any (`Stack.member` stack) (map Stack.Block (Set.elems blocks))
        || any (overlapping stack) (Map.assocs tracks)
    overlapping stack (track_id, ranges) =
        any (Ranges.overlapping ranges) (Stack.track_regions stack track_id)

-- | Control damage indicates that a section of control signal has been
-- modified.  It's dynamically scoped over the same range as the control
-- itself, so that events that depend on it can be rederived.
newtype ControlDamage = ControlDamage (Ranges.Ranges ScoreTime)
    deriving (Pretty.Pretty, Monoid.Monoid, Eq, Show)


-- * scale

-- $scale_doc
-- Like the cache types, this is supposed to be defined in "Derive.Scale", but
-- must be here due to circular dependencies.

data Scale = Scale {
    scale_id :: !Pitch.ScaleId
    -- | A pattern describing what the scale notes look like.  Used only for
    -- error msgs (i.e. parse errors) so it should be human readable and
    -- doesn't have to follow any particular syntax.  A regex is recommended
    -- though.
    , scale_pattern :: !String
    -- | This is passed to the UI so it knows what to call scale degrees when
    -- rendering a pitch signal with this scale.
    , scale_map :: !Track.ScaleMap

    -- | If a scale uses 'Symbol.Symbol's, it can include the definitions here
    -- so they are close to their use.  This symbol list should be loaded as
    -- soon as possible, which means program startup for hardcoded scales.
    , scale_symbols :: ![Symbol.Symbol]

    -- | The controls that will casue a pitch from this scale to change.
    -- This is used by 'PitchSignal.apply_controls' to know when to reevaluate
    -- a given pitch.  Other controls can affect the pitch, but if they aren't
    -- in this set, the pitch won't be reevaluated when they change.
    , scale_transposers :: !(Set.Set Score.Control)

    -- | Transpose a Note by a given number of octaves and integral degrees.
    -- Will be nothing if the pitch is out of range, or the scale doesn't have
    -- octaves.
    , scale_transpose :: !Transpose
    , scale_enharmonics :: !Enharmonics

    -- | Used by derivation.
    , scale_note_to_call :: !(Pitch.Note -> Maybe ValCall)

    -- | Used by note input.
    , scale_input_to_note :: !(Maybe Pitch.Key -> Pitch.InputKey
        -> Maybe Pitch.Note)
    -- | Used by MIDI thru.  This is a shortcut for
    -- @eval . note_to_call . input_to_note@ but can often be implemented more
    -- efficiently by the scale.
    --
    -- TODO if controls had (shift, stretch) I could shift the controls
    -- efficiently and not have to worry about passing the pos separately.
    , scale_input_to_nn ::
        !(ScoreTime -> Pitch.InputKey -> Deriver (Maybe Pitch.NoteNumber))
    }

type LookupScale = Pitch.ScaleId -> Maybe Scale
type Transpose = Maybe Pitch.Key -> Pitch.Octave -> Pitch.Transpose
    -> Pitch.Note -> Either ScaleError Pitch.Note
type Enharmonics = Maybe Pitch.Key -> Pitch.Note
    -> Either ScaleError [Pitch.Note]

-- | Things that can go wrong during scale operations.
data ScaleError = InvalidTransposition | UnparseableKey | KeyNeeded
    | UnparseableNote
    deriving (Eq, Show)
