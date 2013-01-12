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
    , LogsDeriver

    , EventDeriver, Events
    , ControlDeriver, PitchDeriver

    -- * state
    , State(..), initial_state
    , Dynamic(..), initial_dynamic
    , initial_controls, default_dynamic

    -- ** scope
    , Scope(..), empty_scope, ScopeType(..), empty_scope_type
    , DocumentedCall(..), prepend_doc
    , LookupDocs(..)
    , LookupCall(lookup_call, lookup_docs)
    , map_lookup, map_val_lookup, pattern_lookup
    , extract_doc, extract_val_doc
    , lookup_val_call

    -- ** constant
    , Constant(..), initial_constant
    , op_add, op_sub, op_mul

    -- ** instrument
    , Instrument(..), InstrumentCalls(..)

    -- ** control
    , ControlOp(..)

    -- ** collect
    , Collect(..), ControlModification(..), Integrated(..)
    , TrackDynamic

    -- * calls
    , NoteCallMap, ControlCallMap, PitchCallMap, ValCallMap
    , CallInfo(..), dummy_call_info
    , Call(..)
    , CallDoc(..), ArgDoc(..), ArgParser(..), ArgDocs(..)
    , NoteCall, ControlCall, PitchCall
    , WithArgDoc
    , PassedArgs(..)

    -- ** generator
    , GeneratorCall(..), GeneratorFunc
    , generator, generator1, stream_generator, generator_call

    -- ** transformer
    , TransformerCall(..), TransformerFunc
    , transformer, transformer_call

    -- ** val
    , ValCall(..), val_call

    -- ** cache types
    -- $cache_doc
    , Cache(..), Cached(..), cache_size
    , CacheEntry(..), CallType(..)
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
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)

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
import qualified Derive.TrackInfo as TrackInfo
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
    lookup_callable :: TrackLang.CallId -> Deriver (Maybe (Call derived))
    callable_name :: derived -> String

type LogsDeriver d = Deriver (LEvent.LEvents d)

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
    lookup_callable = lookup_with scope_note
    callable_name _ = "note"

-- ** control

type ControlDeriver = LogsDeriver Signal.Control

instance Derived Signal.Control where
    type Elem Signal.Control = Signal.Y
    from_cache_entry (CachedControl ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedControl
    lookup_callable = lookup_with scope_control
    callable_name _ = "control"

-- ** pitch

type PitchDeriver = LogsDeriver PitchSignal.Signal

instance Derived PitchSignal.Signal where
    type Elem PitchSignal.Signal = PitchSignal.Pitch
    from_cache_entry (CachedPitch ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = CachedPitch
    lookup_callable = lookup_with scope_pitch
    callable_name _ = "pitch"


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
    -- | Calls currently in scope.
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

-- | A default dynamic that's not 0 is useful because otherwise you have to add
-- dyn to everything.  Since control tracks multiply by default, 1 is the most
-- convenient value.
default_dynamic :: Signal.Y
default_dynamic = 1

instance Pretty.Pretty Dynamic where
    format (Dynamic controls pitches pitch environ warp scope damage stack _) =
        Pretty.record_title "Dynamic"
            [ ("controls", Pretty.format controls)
            , ("pitches", Pretty.format pitches)
            , ("pitch", Pretty.format pitch)
            , ("environ", Pretty.format environ)
            , ("warp", Pretty.format warp)
            , ("scope", Pretty.format scope)
            , ("damage", Pretty.format damage)
            , ("stack", Pretty.format stack)
            ]

instance DeepSeq.NFData Dynamic where
    rnf (Dynamic controls pitches pitch environ warp scope damage stack _) =
        rnf controls `seq` rnf pitches `seq` rnf pitch `seq` rnf environ
        `seq` rnf warp `seq` rnf scope `seq` rnf damage `seq` rnf stack

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

instance Pretty.Pretty Scope where
    format (Scope note control pitch val) = Pretty.record_title "Scope"
        [ ("note", Pretty.format note)
        , ("control", Pretty.format control)
        , ("pitch", Pretty.format pitch)
        , ("val", Pretty.format val)
        ]

instance DeepSeq.NFData Scope where
    rnf (Scope _ _ _ _) = ()

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

instance Pretty.Pretty (ScopeType call) where pretty = show

-- | For flexibility, a scope is represented not by a map from symbols to
-- derivers, but a function.  That way, it can inspect the CallId and return
-- an appropriate call, as is the case for some scales.
--
-- It's in Deriver because this same type is used for at the top-level track
-- derivation lookup, so it needs to look in the environment for the Scope.
-- The lookup itself presumably won't use Deriver and will just be a return, as
-- in 'map_lookup' or 'Derive.Call.lookup_scale_val'.
data LookupCall call = LookupCall {
    lookup_call :: TrackLang.CallId -> Deriver (Maybe call)
    , lookup_docs :: LookupDocs
    }

data LookupDocs =
    -- | Documentation for a table of calls.
    LookupMap (Map.Map TrackLang.CallId DocumentedCall)
    -- | Documentation for a programmatic lookup.  This is for calls that need
    -- to inspect the CallId.  The String should document what kinds of CallIds
    -- this lookup will match.
    | LookupPattern String DocumentedCall

-- | This is like 'Call', but with only documentation.
data DocumentedCall =
    -- Name (Maybe GeneratorDoc) (Maybe TransformerDoc)
    DocumentedCall String (Maybe CallDoc) (Maybe CallDoc)
    | DocumentedValCall String CallDoc

-- | Prepend a bit of text to the documentation.
prepend_doc :: String -> DocumentedCall -> DocumentedCall
prepend_doc text = modify_doc ((text ++ "\n") ++)

modify_doc :: (String -> String) -> DocumentedCall -> DocumentedCall
modify_doc modify doc = case doc of
    DocumentedCall name generator transformer ->
        DocumentedCall name (annotate <$> generator) (annotate <$> transformer)
    DocumentedValCall name cdoc -> DocumentedValCall name (annotate cdoc)
    where
    annotate (CallDoc cdoc args) = CallDoc (modify cdoc) args

-- | In the common case, a lookup is simply a static map.
map_lookup :: Map.Map TrackLang.CallId (Call d) -> LookupCall (Call d)
map_lookup cmap = LookupCall
    { lookup_call = \call_id -> return $ Map.lookup call_id cmap
    , lookup_docs = LookupMap $ Map.map extract_doc cmap
    }

-- | Like 'map_lookup', but for val calls.  It's too bad this is no longer
-- the same as 'map_lookup', but val calls don't have the weird generator
-- transformer namespace split, so their doc format is different.
map_val_lookup :: Map.Map TrackLang.CallId ValCall -> LookupCall ValCall
map_val_lookup cmap = LookupCall
    { lookup_call = \call_id -> return $ Map.lookup call_id cmap
    , lookup_docs = LookupMap $ Map.map extract_val_doc cmap
    }

-- | Create a lookup that uses a function instead of a Map.
pattern_lookup :: String -> DocumentedCall
    -> (TrackLang.CallId -> Deriver (Maybe call))
    -> LookupCall call
pattern_lookup name doc lookup = LookupCall lookup (LookupPattern name doc)

extract_doc :: Call d -> DocumentedCall
extract_doc (Call name generator transformer) = DocumentedCall name
    (generator_doc <$> generator) (transformer_doc <$> transformer)

extract_val_doc :: ValCall -> DocumentedCall
extract_val_doc vcall = DocumentedValCall (vcall_name vcall) (vcall_doc vcall)

-- ** lookup

lookup_val_call :: TrackLang.CallId -> Deriver (Maybe ValCall)
lookup_val_call = lookup_with scope_val

lookup_with :: (Scope -> ScopeType call)
    -> (TrackLang.CallId -> Deriver (Maybe call))
lookup_with get call_id = do
    lookups <- get_scopes get
    lookup_scopes lookups call_id

get_scopes :: (Scope -> ScopeType call) -> Deriver [LookupCall call]
get_scopes get = do
    ScopeType inst scale builtin <- get <$> gets (state_scope . state_dynamic)
    return $ inst ++ scale ++ builtin

-- | Convert a list of lookups into a single lookup by returning the first
-- one to yield a Just.
lookup_scopes :: [LookupCall call] -> (TrackLang.CallId -> Deriver (Maybe call))
lookup_scopes [] _ = return Nothing
lookup_scopes (lookup:rest) call_id =
    maybe (lookup_scopes rest call_id) (return . Just)
        =<< lookup_call lookup call_id

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
data ControlOp = ControlOp
    !String !(Signal.Control -> Signal.Control -> Signal.Control) !Signal.Y

instance Show ControlOp where
    show (ControlOp name _ _) = "((ControlOp " ++ name ++ "))"

-- *** control ops

-- | Default set of control operators.  Merged at runtime with the static
-- config.  TODO but not yet
default_control_op_map :: Map.Map TrackLang.CallId ControlOp
default_control_op_map = Map.fromList $ map (first TrackLang.Symbol)
    [ ("add", op_add)
    , ("sub", op_sub)
    , ("mul", op_mul)
    , ("max", op_max)
    , ("min", op_min)
    ]

op_add, op_sub, op_mul :: ControlOp
op_add = ControlOp "add" Signal.sig_add 0
op_sub = ControlOp "subtract" Signal.sig_subtract 0
op_mul = ControlOp "multiply" Signal.sig_multiply 1

-- These values should never be seen since any reasonable combining signal
-- will be within this range.
op_max, op_min :: ControlOp
op_max = ControlOp "max" Signal.sig_max (-2^32)
op_min = ControlOp "min" Signal.sig_min (2^32)


-- ** collect

-- | These are things that collect throughout derivation, and are cached in
-- addition to the derived values.  Effectively they are return values
-- alongside the values.
data Collect = Collect {
    -- | Remember the warp signal for each track.  A warp usually applies to
    -- a set of tracks, so remembering them together will make the play monitor
    -- more efficient when it inverts them to get playback position.
    collect_warp_map :: !TrackWarp.WarpMap
    , collect_track_signals :: !Track.TrackSignals
    , collect_track_dynamic :: !TrackDynamic
    -- | This is how a call records its dependencies.  After evaluation of
    -- a deriver, this will contain the dependencies of the most recent call.
    , collect_local_dep :: !GeneratorDep

    -- | New caches accumulating over the course of the derivation.
    , collect_cache :: !Cache
    , collect_integrated :: ![Integrated]
    , collect_control_modifications :: ![ControlModification]
    } deriving (Show)

-- | This is a hack so a call on a control track can modify other controls.
-- The motivating case is pitch ornaments that also want to affect the
-- dynamics.  The modifications are a secondary return value from control
-- and pitch calls.  The track deriver will extract them and merge them into
-- the dynamic environment.  [note control-modification]
data ControlModification =
    ControlModification !Score.Control !Signal.Control !ControlOp
    deriving (Show)

instance Monoid.Monoid Collect where
    mempty = Collect mempty mempty mempty mempty mempty mempty mempty
    mappend (Collect warps1 signals1 env1 deps1 cache1 integrated1 cmods1)
            (Collect warps2 signals2 env2 deps2 cache2 integrated2 cmods2) =
        Collect (warps1 <> warps2) (signals1 <> signals2) (env1 <> env2)
            (deps1 <> deps2) (cache1 <> cache2) (integrated1 <> integrated2)
            (cmods1 <> cmods2)

instance DeepSeq.NFData Collect where
    rnf (Collect warp_map tsigs track_dyn local_dep cache integrated _cmods) =
        rnf warp_map `seq` rnf tsigs `seq` rnf track_dyn
        `seq` rnf local_dep `seq` rnf cache `seq` rnf integrated

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

instance Pretty.Pretty Integrated where
    format (Integrated source events key) = Pretty.record_title "Integrated"
        [ ("source", Pretty.format source)
        , ("events", Pretty.format events)
        , ("key", Pretty.format key)
        ]

instance DeepSeq.NFData Integrated where
    rnf (Integrated source events _key) = rnf source `seq` rnf events

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
    -- | Used by CallSig to look for default arg values in the environment.
    -- This is technically redundant since a call should know its own name,
    -- but it turns out to be inconvenient to pass the name to all of those
    -- functions.
    , passed_call_name :: !String
    , passed_info :: !(CallInfo derived)
    }

-- | Additional data for a call.  This part is invariant for all calls on
-- an event.
--
-- The events are not used for transform calls.
--
-- TODO make separate types so the irrelevent data need not be passed?
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
    , info_track_type :: !(Maybe TrackInfo.Type)
    }

-- | Transformer calls don't necessarily apply to any particular event, and
-- neither to generators for that matter.
dummy_call_info :: ScoreTime -> ScoreTime -> String -> CallInfo derived
dummy_call_info start dur text = CallInfo
    { info_expr = TrackLang.Call (TrackLang.Symbol "") [] :| []
    , info_prev_val = Nothing
    , info_event = Event.event start dur s
    , info_prev_events = []
    , info_next_events = []
    , info_event_end = start + dur
    , info_track_range = (start, start + dur)
    , info_sub_tracks = []
    , info_track_type = Nothing
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

-- | Documentation for a call.  The documentation is in markdown format, except
-- that a single newline will be replaced with two, so a single \n is enough
-- to start a new paragraph.
data CallDoc = CallDoc {
    cdoc_doc :: String
    , cdoc_args :: ArgDocs
    } deriving (Eq, Ord, Show)

data ArgDocs = ArgDocs [ArgDoc]
    -- | This means the call parses the args itself in some special way.
    | ArgsParsedSpecially String
    deriving (Eq, Ord, Show)

data ArgDoc = ArgDoc {
    arg_name :: String
    , arg_type :: TrackLang.Type
    , arg_parser :: ArgParser
    , arg_doc :: String
    } deriving (Eq, Ord, Show)

-- | These enumerate the different ways an argumnt can be parsed, and
-- correspond to parsers in "Derive.CallSig2".
data ArgParser = Required | Defaulted !String | Optional | Many | Many1
    deriving (Eq, Ord, Show)

type NoteCall = Call Score.Event
type ControlCall = Call Signal.Control
type PitchCall = Call PitchSignal.Signal

-- | A value annotated with argument docs.  This is returned by the functions
-- in "Derive.CallSig", and accepted by the Call constructors here.
type WithArgDoc f = (f, ArgDocs)

-- ** generator

data GeneratorCall d = GeneratorCall
    { generator_func :: GeneratorFunc d
    , generator_doc :: CallDoc
    }
type GeneratorFunc d = PassedArgs d -> LogsDeriver d

-- | Create a generator that expects a list of derived values (e.g. Score.Event
-- or Signal.Control), with no logs mixed in.  The result is wrapped in
-- LEvent.Event.
generator :: (Derived d) =>
    String -> String -> WithArgDoc (PassedArgs d -> Deriver (LEvent.Stream d))
    -> Call d
generator name doc (func, arg_docs) =
    stream_generator name doc ((map LEvent.Event <$>) . func, arg_docs)

-- | Since Signals themselves are collections, there's little reason for a
-- signal generator to return a Stream of events.  So wrap the generator result
-- in a Stream singleton.
generator1 :: (Derived d) =>
    String -> String -> WithArgDoc (PassedArgs d -> Deriver d) -> Call d
generator1 name doc (func, arg_docs) =
    generator name doc ((LEvent.one <$>) . func, arg_docs)

-- | Like 'generator', but the deriver returns 'LEvent.LEvents' which already
-- have logs mixed in.  Useful if the generator calls a sub-deriver which will
-- already have merged the logs into the output.
stream_generator :: (Derived d) =>
    String -> String -> WithArgDoc (PassedArgs d -> LogsDeriver d) -> Call d
stream_generator name doc with_docs = Call
    { call_name = name
    , call_generator = Just $ generator_call doc with_docs
    , call_transformer = Nothing
    }

generator_call :: String -> (GeneratorFunc d, ArgDocs) -> GeneratorCall d
generator_call doc (func, arg_docs) = GeneratorCall func (CallDoc doc arg_docs)

-- ** transformer

-- | args -> deriver -> deriver
data TransformerCall d = TransformerCall
    { transformer_func :: TransformerFunc d
    , transformer_doc :: CallDoc
    }
type TransformerFunc d = PassedArgs d -> LogsDeriver d -> LogsDeriver d

transformer :: (Derived d) =>
    String -> String
    -> WithArgDoc (PassedArgs d -> LogsDeriver d -> LogsDeriver d) -> Call d
transformer name doc with_docs = Call
    { call_name = name
    , call_generator = Nothing
    , call_transformer = Just $ transformer_call doc with_docs
    }

transformer_call :: String -> (TransformerFunc d, ArgDocs) -> TransformerCall d
transformer_call doc (func, arg_docs) =
    TransformerCall func (CallDoc doc arg_docs)

-- ** val

data ValCall = ValCall {
    vcall_name :: !String
    , vcall_call :: PassedArgs () -> Deriver TrackLang.Val
    , vcall_doc :: !CallDoc
    }

instance Show ValCall where
    show (ValCall name _ _) = "((ValCall " ++ show name ++ "))"

val_call :: String -> String
    -> WithArgDoc (PassedArgs () -> Deriver TrackLang.Val) -> ValCall
val_call name doc (call, arg_docs) = ValCall
    { vcall_name = name
    , vcall_call = call
    , vcall_doc = CallDoc doc arg_docs
    }


-- ** cache types

-- $cache_doc
-- The cache types are nominally exported from "Derive.Cache", but must be
-- defined here to avoid circular dependencies.

-- instead of a stack, this could be a tree of frames
newtype Cache = Cache (Map.Map Stack.Stack Cached)
    deriving (Monoid.Monoid, Show, Pretty.Pretty, DeepSeq.NFData)
    -- The monoid instance winds up being a left-biased union.  This is ok
    -- because merged caches shouldn't overlap anyway.

cache_size :: Cache -> Int
cache_size (Cache c) = Map.size c

-- | When cache entries are invalidated by ScoreDamage, a marker is left in
-- their place.  This is just for a nicer log msg that can tell the difference
-- between never evaluated and damaged.
data Cached = Cached !CacheEntry | Invalid
    deriving (Show)

instance Pretty.Pretty Cached where
    format Invalid = Pretty.text "Invalid"
    format (Cached entry) = Pretty.format entry

instance DeepSeq.NFData Cached where
    rnf Invalid = ()
    rnf (Cached entry) = rnf entry

-- | Since an entire track is one type but will have many different calls of
-- different types, the deriver type division goes above the call type
-- division.
data CacheEntry =
    CachedEvents !(CallType Score.Event)
    | CachedControl !(CallType Signal.Control)
    | CachedPitch !(CallType PitchSignal.Signal)
    deriving (Show)

instance Pretty.Pretty CacheEntry where
    format (CachedEvents (CallType _ events)) = Pretty.format events
    format (CachedControl (CallType _ events)) = Pretty.format events
    format (CachedPitch (CallType _ events)) = Pretty.format events

instance DeepSeq.NFData CacheEntry where
    rnf (CachedEvents c) = rnf c
    rnf (CachedControl c) = rnf c
    rnf (CachedPitch c) = rnf c

-- | The type here should match the type of the stack it's associated with,
-- but I'm not quite up to those type gymnastics yet.
data CallType derived = CallType !Collect !(LEvent.LEvents derived)
    deriving (Show)

instance (DeepSeq.NFData d) => DeepSeq.NFData (CallType d) where
    rnf (CallType collect events) = rnf collect `seq` rnf events

-- ** deps

newtype GeneratorDep = GeneratorDep (Set.Set BlockId)
    deriving (Monoid.Monoid, Show, Eq, DeepSeq.NFData)

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

instance DeepSeq.NFData ScoreDamage where
    rnf (ScoreDamage tracks track_blocks blocks) =
        rnf tracks `seq` rnf track_blocks `seq` rnf blocks

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
    deriving (Pretty.Pretty, Monoid.Monoid, Eq, Show, DeepSeq.NFData)


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

    -- | Documentation for all of the ValCalls that 'scale_note_to_call' can
    -- return.
    , scale_call_doc :: !DocumentedCall
    }

type LookupScale = Pitch.ScaleId -> Maybe Scale
type Transpose = Maybe Pitch.Key -> Pitch.Octave -> Pitch.Transpose
    -> Pitch.Note -> Either ScaleError Pitch.Note
-- | Get the enharmonics of the note.  The given note is omitted, and the
-- enharmonics are in ascending order until they wrap around, so if you always
-- take the head of the list you will cycle through all of the enharmonics.
type Enharmonics = Maybe Pitch.Key -> Pitch.Note
    -> Either ScaleError [Pitch.Note]

-- | Things that can go wrong during scale operations.
data ScaleError =
    InvalidTransposition | KeyNeeded | UnparseableNote
    -- | An environ value was unparseable.
    | UnparseableEnviron !TrackLang.ValName !String
        -- String should be TrackLang.Val except that makes Eq not work.
    deriving (Eq, Show)

{- note control-modification
    . Control tracks return a single control, and how that merges into the
      environ is up to the track.
    . It would be convenient to do it in an existing track, e.g. the pitch
      track, and not have to add a whole new track just for a few fancy
      ornaments.  The pitch track is a natural choice, but if it's going to
      modify other controls it should be beneath them, right?  The pitch track
      is on top, not for a good reason, but just because it's next to the note
      track.  I could solve this by reversing the order of the skeleton, at
      the cost of it looking a little weird.
    . However, once I'm doing this, why is the pitch track special?  Why
      wouldn't I want to have calls that affect multiple non-pitch controls?
    . The note track seems like a natural place for this stuff, but that would
      require some notion of a non-note event within a note event.  But that
      means either sub-event text, which seems like a giant increase in
      complexity, or some awful hack like e.g. events starting with ';' are
      merged into the non-; note event, and somehow evaluated with it.
      Besides, this doesn't compose nicely, what if I want multiple high-level
      control tracks at once?
    / Generalize control tracks to return
      'Either (Name, PitchSignal) (Name, ControlSignal, ControlOp).
      The track call splits them apart, and merges into the environ.  This
      would make pitch tracks and control tracks the same, just a different
      default.  But that's no good because they already have separate
      namespaces, which are very useful.
    * Control calls and pitch calls can emit "control modifications", which
      are (control, signal) pairs that will be multiplied with the environ.
      Each one is an entire signal.  Since the signals are combined with
      multiplication, it doesn't matter what order they go in.  The
      modifications can go in Collect.
      Pitch modification has to transposition, but it would have to use a
      transpose signal.  However, transpose signals are additive, not
      multiplicative.  So I need some way to indicate the combining operator
      after all.
-}
