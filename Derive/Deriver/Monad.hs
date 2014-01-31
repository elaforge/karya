-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-} -- for super-classes of Callable
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types, BangPatterns #-}
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
    , Error(..), ErrorVal(..), CallError(..), ErrorPlace(..), EvalSource(..)
    , throw, throw_srcpos, throw_arg_error, throw_arg_error_srcpos
    , throw_error, throw_error_srcpos

    -- * derived types
    , Callable(..), Elem, Tagged(..), ToTagged(..)
    , LogsDeriver

    , Note, NoteDeriver, NoteArgs, Events
    , Control, ControlDeriver, ControlArgs
    , Pitch, PitchDeriver, PitchArgs

    -- * state
    , State(..), initial_state
    , Dynamic(..), initial_dynamic
    , initial_controls, default_dynamic

    -- ** scope
    , Scopes(..), empty_scopes, s_generator, s_transformer, s_val
    , Scope(..), s_note, s_control, s_pitch
    , empty_scope
    , ScopeType(..), s_override, s_instrument, s_scale, s_builtin
    , DocumentedCall(..), prepend_doc
    , LookupDocs(..)
    , LookupCall(lookup_call, lookup_docs)
    , map_lookup, map_val_lookup, pattern_lookup
    , extract_doc, extract_val_doc
    , lookup_val_call, lookup_with

    -- ** constant
    , Constant(..), initial_constant
    , op_add, op_sub, op_mul

    -- ** instrument
    , Instrument(..), InstrumentCalls(..)

    -- ** control
    , Merge(..), ControlOp(..)

    -- ** collect
    , Collect(..), ControlMod(..), Integrated(..)
    , TrackDynamic(..)

    -- * calls
    , CallMap, ValCallMap
    , CallMaps, make_calls, call_maps, generator_call_map, transformer_call_map
    , CallInfo(..), coerce_call_info, dummy_call_info, tag_call_info
    , Call(..), make_call
    , CallDoc(..), ArgDoc(..), ArgParser(..), EnvironDefault(..), ArgDocs(..)
    , WithArgDoc
    , PassedArgs(..)

    -- ** generator
    , Generator, GeneratorFunc
    , generator, generator1

    -- ** transformer
    , Transformer, TransformerFunc
    , transformer

    -- ** val
    , ValCall(..), val_call

    -- ** cache types
    -- $cache_doc
    , Cache(..), Cached(..), cache_size
    , CacheEntry(..), CallType(..)
    , BlockDeps(..)

    -- ** damage
    , ScoreDamage(..)
    , ControlDamage(..)

    -- * scale
    -- $scale_doc
    , Scale(..)
    , LookupScale, Transpose, Transposition(..), Enharmonics, Layout
    , ScaleError(..)

    -- * testing
    , invalidate_damaged
) where
import qualified Control.Applicative as Applicative
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)

import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector.Unboxed

import Util.Control
import qualified Util.Lens as Lens
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

import qualified Derive.Call.Tags as Tags
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Lilypond.Types as Lilypond.Types
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
modify f = Deriver $ \st logs _ win -> let !x = f st in win x logs ()

{-# INLINE get #-}
get :: Deriver State
get = Deriver $ \st logs _ win -> win st logs st

{-# INLINE gets #-}
gets :: (State -> a) -> Deriver a
gets f = do
    state <- get
    return $! f state

{-# INLINE put #-}
put :: State -> Deriver ()
put !st = Deriver $ \_ logs _ win -> win st logs ()

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
        -- If the msg was created explicitly, it may already have a stack.
        stack <- maybe (gets (Stack.to_strings . state_stack . state_dynamic))
            return (Log.msg_stack msg)
        return $ msg { Log.msg_stack = Just stack }

run :: State -> Deriver a -> RunResult a
run state m = _runD m state []
    (\st logs err -> (Left err, st, reverse logs))
    (\st logs a -> (Right a, st, reverse logs))


-- * error

data Error = Error SrcPos.SrcPos Stack.Stack ErrorVal
    deriving (Show)

instance Pretty.Pretty Error where
    pretty (Error srcpos stack val) =
        SrcPos.show_srcpos srcpos ++ " " ++ pretty stack ++ ": " ++ pretty val

data ErrorVal = GenericError String | CallError CallError
    deriving (Show)

instance Pretty.Pretty ErrorVal where
    pretty (GenericError s) = s
    pretty (CallError err) = pretty err

data CallError =
    -- | ErrorPlace, EvalSource, arg name, expected type, received val
    TypeError !ErrorPlace !EvalSource !Text !TrackLang.Type
        !(Maybe TrackLang.Val)
    -- | Error evaluating a 'TrackLang.VQuoted' while processing a particular
    -- argument.
    | EvalError !ErrorPlace !TrackLang.Quoted !Text !Error
    -- | Couldn't even call the thing because the name was not found.
    | CallNotFound !TrackLang.CallId
    -- | Calling error that doesn't fit into the above categories.
    | ArgError !Text
    deriving (Show)

-- | Where a type error came from.  The arg number starts at 0.
data ErrorPlace = TypeErrorArg !Int | TypeErrorEnviron !TrackLang.Symbol
    deriving (Eq, Show)

data EvalSource =
    -- | The value in error came from a literal expression.
    Literal
    -- | The value in error came from a 'TrackLang.VQuoted' bit of code.
    | Quoted !TrackLang.Quoted
    deriving (Show)

instance Pretty.Pretty CallError where
    pretty err = case err of
        TypeError place source name expected received ->
            "TypeError: arg " <> pretty place <> "/" <> untxt name
            <> source_desc <> ": expected " <> pretty expected
            <> " but got " <> pretty (TrackLang.type_of <$> received)
            <> ": " <> pretty received
            where
            source_desc = case source of
                Literal -> ""
                Quoted call -> " from " <> untxt (ShowVal.show_val call)
        EvalError place call name (Error _ _ error_val) ->
            -- The srcpos and stack of the derive error is probably not
            -- interesting, so I strip those out.
            "EvalError: arg " <> pretty place <> "/" <> untxt name
            <> " from " <> untxt (ShowVal.show_val call)
            <> ": " <> pretty error_val
        ArgError err -> "ArgError: " <> untxt err
        CallNotFound call_id -> "CallNotFound: " <> pretty call_id

instance Pretty.Pretty ErrorPlace where
    pretty (TypeErrorArg num) = show (num + 1)
    pretty (TypeErrorEnviron key) = "environ:" <> pretty key

throw :: String -> Deriver a
throw msg = throw_error (GenericError msg)

throw_srcpos :: SrcPos.SrcPos -> String -> Deriver a
throw_srcpos srcpos msg = throw_error_srcpos srcpos (GenericError msg)

throw_arg_error :: String -> Deriver a
throw_arg_error = throw_arg_error_srcpos Nothing

throw_arg_error_srcpos :: SrcPos.SrcPos -> String -> Deriver a
throw_arg_error_srcpos srcpos =
    throw_error_srcpos srcpos . CallError . ArgError . txt

throw_error :: ErrorVal -> Deriver a
throw_error = throw_error_srcpos Nothing

throw_error_srcpos :: SrcPos.SrcPos -> ErrorVal -> Deriver a
throw_error_srcpos srcpos err = do
    stack <- gets (state_stack . state_dynamic)
    _throw (Error srcpos stack err)


-- * derived types

-- | Each kind of deriver looks a different scope for its calls.  By making
-- this a class method, I can figure out which scope to look in just from
-- the type.
class (Show d, ToTagged (Elem d)) => Callable d where
    lookup_generator :: TrackLang.CallId -> Deriver (Maybe (Generator d))
    lookup_transformer :: TrackLang.CallId -> Deriver (Maybe (Transformer d))
    callable_name :: d -> Text

-- | This converts the deriver return type to the scalar value used by
-- 'info_prev_val'.
type family Elem d :: *
type LogsDeriver d = Deriver (LEvent.LEvents d)

-- | This is for 'info_prev_val'.  Normally the previous value is available
-- in all its untagged glory based on the type of the call, but ValCalls can
-- occur with all the different types, so they need a tagged 'info_prev_val'.
data Tagged =
    TagEvent Score.Event | TagControl Signal.Y | TagPitch PitchSignal.Pitch

class ToTagged a where to_tagged :: a -> Tagged
instance ToTagged Tagged where to_tagged = id

-- ** event

type Note = Score.Event
type NoteDeriver = LogsDeriver Score.Event
type NoteArgs = PassedArgs Score.Event

type instance Elem Score.Event = Score.Event
instance ToTagged Score.Event where to_tagged = TagEvent

-- | This might seem like an inefficient way to represent the Event stream,
-- but I can't think of how to make it better.
--
-- Each call generates a chunk [Event], and the chunks are then joined with
-- 'd_merge_asc'.  This means every cons is copied once, but I think this is
-- hard to avoid if I want to merge streams.
type Events = LEvent.LEvents Score.Event

instance Callable Score.Event where
    lookup_generator = lookup_with (scope_note . scopes_generator)
    lookup_transformer = lookup_with (scope_note . scopes_transformer)
    callable_name _ = "note"

-- ** control

type Control = Signal.Control
type ControlDeriver = LogsDeriver Signal.Control
type ControlArgs = PassedArgs Signal.Y

type instance Elem Signal.Control = Signal.Y
instance ToTagged Signal.Y where to_tagged = TagControl

instance Callable Signal.Control where
    lookup_generator = lookup_with (scope_control . scopes_generator)
    lookup_transformer = lookup_with (scope_control . scopes_transformer)
    callable_name _ = "control"

-- ** pitch

type Pitch = PitchSignal.Signal
type PitchDeriver = LogsDeriver PitchSignal.Signal
type PitchArgs = PassedArgs PitchSignal.Pitch

type instance Elem PitchSignal.Signal = PitchSignal.Pitch
instance ToTagged PitchSignal.Pitch where to_tagged = TagPitch

instance Callable PitchSignal.Signal where
    lookup_generator = lookup_with (scope_pitch . scopes_generator)
    lookup_transformer = lookup_with (scope_pitch . scopes_transformer)
    callable_name _ = "pitch"

-- * state

-- | All the state available during derivation.  It has three parts: Dynamic is
-- scoped to sub-computations like Reader, Collect is written to with
-- 'mappend', and Constant is constant.  This means that in principle
-- derivation of siblings could be parallelized.  However, events on a track
-- (except a note track) still must be serialized, thanks to 'info_prev_val'.
data State = State {
    -- | This data is modified in a dynamically scoped way, for
    -- sub-derivations.
    state_dynamic :: !Dynamic
    -- | This data is mappended.  It functions like an implicit return value.
    , state_collect :: !Collect
    -- | This data is constant throughout the derivation.
    , state_constant :: !Constant
    }

initial_state :: Scopes -> TrackLang.Environ -> Constant -> State
initial_state scopes environ constant = State
    { state_dynamic = initial_dynamic scopes environ
    , state_collect = mempty
    , state_constant = constant
    }

-- | This is a dynamically scoped environment that applies to generated events
-- inside its scope.
data Dynamic = Dynamic {
    -- | Derivers can modify it for sub-derivers, or look at it, whether to
    -- attach to an Event or to handle internally.
    state_controls :: !Score.ControlMap
    -- | Function variant of controls.  Normally they modify a backing
    -- 'Signal.Control', but could be synthesized as well.  See
    -- 'TrackLang.ControlFunction' for details.
    , state_control_functions :: !Score.ControlFunctionMap
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
    , state_scopes :: !Scopes
    , state_control_damage :: !ControlDamage

    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: !Stack.Stack
    } deriving (Show)

initial_dynamic :: Scopes -> TrackLang.Environ -> Dynamic
initial_dynamic scopes environ = Dynamic
    { state_controls = initial_controls
    , state_control_functions = mempty
    , state_pitches = Map.empty
    , state_pitch = mempty
    , state_environ = environ
    , state_warp = Score.id_warp
    , state_scopes = scopes
    , state_control_damage = mempty
    , state_stack = Stack.empty
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
    format (Dynamic controls cfuncs pitches pitch environ warp scopes damage
            stack) =
        Pretty.record_title "Dynamic"
            [ ("controls", Pretty.format controls)
            , ("control functions", Pretty.format cfuncs)
            , ("pitches", Pretty.format pitches)
            , ("pitch", Pretty.format pitch)
            , ("environ", Pretty.format environ)
            , ("warp", Pretty.format warp)
            , ("scopes", Pretty.format scopes)
            , ("damage", Pretty.format damage)
            , ("stack", Pretty.format stack)
            ]

instance DeepSeq.NFData Dynamic where
    rnf (Dynamic controls cfuncs pitches pitch environ warp _scopes damage
            stack) =
        rnf controls `seq` rnf cfuncs `seq` rnf pitches `seq` rnf pitch
        `seq` rnf environ `seq` rnf warp `seq` rnf damage `seq` rnf stack

-- ** scope

-- | This represents all calls in scope.  Different types of calls are in scope
-- depending on the track type, except ValCalls, which are in scope everywhere.
-- This is dynamic scope, not lexical scope.
--
-- Perhaps this should be called Namespaces, but Id.Namespace is already taken
-- and Scopes is shorter.
data Scopes = Scopes {
    scopes_generator :: !GeneratorScope
    , scopes_transformer :: !TransformerScope
    , scopes_val :: !(ScopeType ValCall)
    } deriving (Show)

empty_scopes :: Scopes
empty_scopes = Scopes empty_scope empty_scope mempty

s_generator = Lens.lens scopes_generator (\v r -> r { scopes_generator = v })
s_transformer =
    Lens.lens scopes_transformer (\v r -> r { scopes_transformer = v })
s_val = Lens.lens scopes_val (\v r -> r { scopes_val = v })

instance Pretty.Pretty Scopes where
    format (Scopes g t v) = Pretty.record_title "Scopes"
        [ ("generator", Pretty.format g)
        , ("transformer", Pretty.format t)
        , ("val", Pretty.format v)
        ]

type GeneratorScope =
    Scope (Generator Note) (Generator Control) (Generator Pitch)
type TransformerScope =
    Scope (Transformer Note) (Transformer Control) (Transformer Pitch)

data Scope note control pitch = Scope {
    scope_note :: !(ScopeType note)
    , scope_control :: !(ScopeType control)
    , scope_pitch :: !(ScopeType pitch)
    } deriving (Show)

s_note = Lens.lens scope_note (\v r -> r { scope_note = v })
s_control = Lens.lens scope_control (\v r -> r { scope_control = v })
s_pitch = Lens.lens scope_pitch (\v r -> r { scope_pitch = v })

empty_scope :: Scope a b c
empty_scope = Scope mempty mempty mempty

instance (Pretty.Pretty note, Pretty.Pretty control, Pretty.Pretty pitch) =>
        Pretty.Pretty (Scope note control pitch) where
    format (Scope note control pitch) = Pretty.record_title "Scope"
        [ ("note", Pretty.format note)
        , ("control", Pretty.format control)
        , ("pitch", Pretty.format pitch)
        ]

instance DeepSeq.NFData (Scope a b c) where
    rnf (Scope _ _ _) = ()

-- | An instrument or scale may put calls into scope.  If that instrument
-- or scale is replaced with another, the old calls must be replaced with
-- the new ones.
--
-- This is hard-coded for the types of scopes I currently use.  It would be
-- more general to make this a Map from symbol to scopes, but since I only
-- have three types at the moment, this is simpler.
--
-- Priority is determined by 'get_scopes', which returns them in the
-- declaration order.  So override calls take priority over instrument calls,
-- which take priority over scale and builtin calls.
data ScopeType call = ScopeType {
    -- | Override calls shadow all others.  They're useful when you want to
    -- prevent instruments from overriding calls, which the lilypond deriver
    -- needs to do.
    stype_override :: ![LookupCall call]
    , stype_instrument :: ![LookupCall call]
    , stype_scale :: ![LookupCall call]
    , stype_builtin :: ![LookupCall call]
    }

s_override = Lens.lens stype_override (\v r -> r { stype_override = v })
s_instrument = Lens.lens stype_instrument (\v r -> r { stype_instrument = v })
s_scale = Lens.lens stype_scale (\v r -> r { stype_scale = v })
s_builtin = Lens.lens stype_builtin (\v r -> r { stype_builtin = v })

instance Monoid.Monoid (ScopeType call) where
    mempty = ScopeType [] [] [] []
    mappend (ScopeType a1 b1 c1 d1) (ScopeType a2 b2 c2 d2) =
        ScopeType (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2)

instance Show (ScopeType call) where show = pretty
instance Pretty.Pretty (ScopeType call) where
    format (ScopeType override inst scale builtin) =
        Pretty.record_title "ScopeType"
            [ ("override", Pretty.format override)
            , ("inst", Pretty.format inst)
            , ("scale", Pretty.format scale)
            , ("builtin", Pretty.format builtin)
            ]

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
    | LookupPattern Text DocumentedCall

instance Pretty.Pretty (LookupCall call) where
    format look = "Lookup: " <> case lookup_docs look of
        LookupMap calls -> Pretty.format (Map.keys calls)
        LookupPattern doc _ -> Pretty.text (untxt doc)

-- | This is like 'Call', but with only documentation.
data DocumentedCall = DocumentedCall !Text !CallDoc

-- | Prepend a bit of text to the documentation.
prepend_doc :: Text -> DocumentedCall -> DocumentedCall
prepend_doc text = modify_doc ((text <> "\n") <>)

modify_doc :: (Text -> Text) -> DocumentedCall -> DocumentedCall
modify_doc modify (DocumentedCall name doc) = DocumentedCall name (annotate doc)
    where annotate (CallDoc tags cdoc args) = CallDoc tags (modify cdoc) args

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
pattern_lookup :: Text -> DocumentedCall
    -> (TrackLang.CallId -> Deriver (Maybe call))
    -> LookupCall call
pattern_lookup name doc lookup = LookupCall lookup (LookupPattern name doc)

extract_doc :: Call d -> DocumentedCall
extract_doc call = DocumentedCall (call_name call) (call_doc call)

extract_val_doc :: ValCall -> DocumentedCall
extract_val_doc vcall = DocumentedCall (vcall_name vcall) (vcall_doc vcall)

-- ** lookup

lookup_val_call :: TrackLang.CallId -> Deriver (Maybe ValCall)
lookup_val_call = lookup_with scopes_val

lookup_with :: (Scopes -> ScopeType call)
    -> (TrackLang.CallId -> Deriver (Maybe call))
lookup_with get call_id = do
    lookups <- get_scopes get
    lookup_scopes lookups call_id

get_scopes :: (Scopes -> ScopeType call) -> Deriver [LookupCall call]
get_scopes get = do
    ScopeType override inst scale builtin <-
        gets $ get . state_scopes . state_dynamic
    return $ override ++ inst ++ scale ++ builtin

-- | Convert a list of lookups into a single lookup by returning the first
-- one to yield a Just.
lookup_scopes :: [LookupCall call] -> (TrackLang.CallId -> Deriver (Maybe call))
lookup_scopes [] _ = return Nothing
lookup_scopes (lookup:rest) call_id =
    maybe (lookup_scopes rest call_id) (return . Just)
        =<< lookup_call lookup call_id

-- ** constant

data Constant = Constant {
    state_ui :: !State.State
    , state_control_op_map :: !(Map.Map TrackLang.CallId ControlOp)
    , state_lookup_scale :: !LookupScale
    -- | Get the calls and environ that should be in scope with a certain
    -- instrument.  The environ is merged with the environ in effect.
    , state_lookup_instrument :: !(Score.Instrument -> Maybe Instrument)
    -- | Cache from the last derivation.
    , state_cache :: !Cache
    , state_score_damage :: !ScoreDamage
    -- | Config for lilypond derivation.  Set only when deriving for the
    -- lilypond backend.  Various calls can check for its presence and derive
    -- differently (e.g. trill should emit trill ly code instead of notes).
    , state_lilypond :: !(Maybe Lilypond.Types.Config)
    }

initial_constant :: State.State -> LookupScale
    -> (Score.Instrument -> Maybe Instrument) -> Cache -> ScoreDamage
    -> Constant
initial_constant ui_state lookup_scale lookup_inst cache
        score_damage = Constant
    { state_ui = ui_state
    , state_control_op_map = default_control_op_map
    , state_lookup_scale = lookup_scale
    , state_lookup_instrument = lookup_inst
    , state_cache = invalidate_damaged score_damage cache
    , state_score_damage = score_damage
    , state_lilypond = Nothing
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
data InstrumentCalls = InstrumentCalls
    [LookupCall (Generator Note)]
    [LookupCall (Transformer Note)]
    [LookupCall ValCall]

instance Show InstrumentCalls where
    show (InstrumentCalls gen trans val) =
        "((InstrumentCalls " <> show (length gen, length trans, length val)
            <> "))"

instance Pretty.Pretty InstrumentCalls where
    format (InstrumentCalls gen trans val) =
        Pretty.record_title "InstrumentCalls"
            [ ("generator", Pretty.format gen)
            , ("transformer", Pretty.format trans)
            , ("val", Pretty.format val)
            ]

instance Monoid.Monoid InstrumentCalls where
    mempty = InstrumentCalls mempty mempty mempty
    mappend (InstrumentCalls a1 b1 c1) (InstrumentCalls a2 b2 c2) =
        InstrumentCalls (a1<>a2) (b1<>b2) (c1<>c2)


-- ** control

-- | How to merge a control into 'Dynamic'.
data Merge =
    -- | Replace the existing signal.
    Set
    -- | Merge according to the signal's default.
    | Default
    -- | Merge with a specific operator.
    | Merge !ControlOp
    deriving (Show)

instance Pretty.Pretty Merge where pretty = show

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
-- addition to the derived values.  Effectively they are extra return values,
-- which are combined with mappend.
data Collect = Collect {
    -- | Remember the warp signal for each track.  A warp usually applies to
    -- a set of tracks, so remembering them together will make the play monitor
    -- more efficient when it inverts them to get playback position.
    collect_warp_map :: !TrackWarp.WarpMap
    , collect_track_signals :: !Track.TrackSignals
    , collect_track_dynamic :: !TrackDynamic
    -- | This is how a call records its dependencies.  After evaluation of
    -- a deriver, this will contain the dependencies of the most recent call.
    , collect_block_deps :: !BlockDeps

    -- | New caches accumulating over the course of the derivation.
    , collect_cache :: !Cache
    , collect_integrated :: ![Integrated]
    , collect_control_mods :: ![ControlMod]
    } deriving (Show)

-- | This is a hack so a call on a control track can modify other controls.
-- The motivating case is pitch ornaments that also want to affect the
-- dynamics.  The modifications are a secondary return value from control
-- and pitch calls.  The track deriver will extract them and merge them into
-- the dynamic environment.  [NOTE control-modification]
data ControlMod = ControlMod !Score.Control !Signal.Control !Merge
    deriving (Show)

instance Pretty.Pretty ControlMod where
    format (ControlMod control signal merge) =
        Pretty.constructor "ControlMod"
            [Pretty.format control, Pretty.format signal, Pretty.format merge]

instance Monoid.Monoid Collect where
    mempty = Collect mempty mempty mempty mempty mempty mempty mempty
    mappend (Collect warps1 tsigs1 trackdyn1 deps1 cache1 integrated1 cmods1)
            (Collect warps2 tsigs2 trackdyn2 deps2 cache2 integrated2 cmods2) =
        Collect (warps1 <> warps2)
            (Map.unionWith Track.merge_signals tsigs1 tsigs2)
            (trackdyn1 <> trackdyn2) (deps1 <> deps2) (cache1 <> cache2)
            (integrated1 <> integrated2) (cmods1 <> cmods2)

instance DeepSeq.NFData Collect where
    rnf (Collect warp_map tsigs track_dyn local_dep cache integrated _cmods) =
        rnf warp_map `seq` rnf tsigs `seq` rnf track_dyn
        `seq` rnf local_dep `seq` rnf cache `seq` rnf integrated

data Integrated = Integrated {
    -- BlockId for a block integration, TrackId for a track integration.
    integrated_source :: !(Either BlockId TrackId)
    , integrated_events :: !Events
    } deriving (Show)

instance Pretty.Pretty Integrated where
    format (Integrated source events) = Pretty.record_title "Integrated"
        [ ("source", Pretty.format source)
        , ("events", Pretty.format events)
        ]

instance DeepSeq.NFData Integrated where
    rnf (Integrated source events) = rnf source `seq` rnf events

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
--
-- NOTE [record-track-dynamics] So it hasn't quite worked well enough in
-- practice.  The problem is that if I get controls from sliced tracks, the
-- controls are also sliced, which means they have zeroes where they shouldn't.
-- However, I can't just not record TrackDynamic from sliced or inverted tracks
-- because then in the very common case of [">i", "*scale"], >i won't get the
-- *scale because that only happens post-inversion.  In addition, an orphan
-- track is only seen in sliced form.
--
-- I really want to record the dynamic of each track as if there were no
-- slicing, but that would require a whole new derive pass.  I think I could
-- fix this by storing TrackDynamics by (BlockId, TrackId, Maybe TrackTime),
-- and have the lookup function find the most specific one.
--
-- TODO That seems like an annoying bit of work that will probably bring its
-- own complications, so I'm settling for another hack for the moment (those
-- never go wrong, right?).  I generally want the controls from the
-- pre-inversion track, since those aren't sliced.  But I want the environ from
-- post-inversion, because of the *scale thing, and besides the environ can't
-- be sliced anyway.  So TrackDynamics's mappend takes the environ from the
-- first arg and the rest (including controls) from the second.  Since inverted
-- tracks wind up being mappended on the right, this gets controls from
-- non-inverted and environ from inverted.  Of course there are probably more
-- ways that could go wrong, and inverted tracks mappending on the right is
-- brittle and subtle, so I won't be too shocked if I have to come revisit this
-- someday, and maybe implement the TrackTime thing.
--
-- One more hack: the controls on the second nesting of a slice are also sliced
-- fragments.  I can't just merge them, since they have (0, 0) samples which
-- would cause a later fragment to wipe out the earlier ones.  But I can merge
-- them after I drop the initial 0, as in 'merge_controls'.  This only works
-- because the slices are evaluated and mappended in order.
newtype TrackDynamic = TrackDynamic (Map.Map (BlockId, TrackId) Dynamic)
    deriving (Show, DeepSeq.NFData, Pretty.Pretty)

instance Monoid.Monoid TrackDynamic where
    mempty = TrackDynamic mempty
    mappend (TrackDynamic d1) (TrackDynamic d2) =
        TrackDynamic $ Map.unionWith merge d1 d2
        where
        merge d1 d2 = d2
            { state_environ = state_environ d1
            , state_controls = Map.unionWith merge_controls
                (state_controls d1) (state_controls d2)
            }

merge_controls :: Score.TypedControl -> Score.TypedControl -> Score.TypedControl
merge_controls (Score.Typed typ c1) (Score.Typed _ c2) =
    -- c1 is the new one being merged in, c2 is the old one.
    Score.Typed typ (c2 <> drop0 c1)
    where
    drop0 sig = case Signal.head sig of
        Just (0, 0) -> Signal.drop 1 sig
        _ -> sig


-- ** calls

type CallMap call = Map.Map TrackLang.CallId call
type ValCallMap = Map.Map TrackLang.CallId ValCall

-- | Previously, a single Call contained both generator and transformer.
-- This turned out to not be flexible enough, because an instrument that
-- wanted to override a generator meant you couldn't use a transformer that
-- happened to have the same name.  However, there are a number of calls that
-- want both generator and transformer versions, and it's convenient to be
-- able to deal with those together.
type CallMaps d = (CallMap (Generator d), CallMap (Transformer d))

-- | Make a call map.
make_calls :: [(TrackLang.CallId, call)] -> Map.Map TrackLang.CallId call
make_calls = Map.fromList

-- | Bundle generators and transformers up together for convenience.
call_maps :: [(TrackLang.CallId, Generator d)]
    -> [(TrackLang.CallId, Transformer d)] -> CallMaps d
call_maps generators transformers =
    (make_calls generators, make_calls transformers)

generator_call_map :: [(TrackLang.CallId, Generator d)] -> CallMaps d
generator_call_map = flip call_maps []

transformer_call_map :: [(TrackLang.CallId, Transformer d)] -> CallMaps d
transformer_call_map = call_maps []

-- | Data passed to a 'Call'.
data PassedArgs val = PassedArgs {
    passed_vals :: ![TrackLang.Val]
    -- | Used by "Derive.Sig" to look for default arg values in the
    -- environment.  This is technically redundant since a call should know its
    -- own name, but it turns out to be inconvenient to pass the name to all of
    -- those functions.
    , passed_call_name :: !Text
    , passed_info :: !(CallInfo val)
    }

instance (Pretty.Pretty val) => Pretty.Pretty (PassedArgs val) where
    format (PassedArgs vals call_name info) = Pretty.record_title "PassedArgs"
        [ ("vals", Pretty.format vals)
        , ("call_name", Pretty.format call_name)
        , ("info", Pretty.format info)
        ]

-- | Additional data for a call.  This part is invariant for all calls on
-- an event.
--
-- The events are not used for transform calls.
--
-- TODO make separate types so the irrelevent data need not be passed?
data CallInfo val = CallInfo {
    -- | The expression currently being evaluated.  Why I need this is
    -- documented in 'Derive.Call.Sub.invert_call'.
    info_expr :: !Event.Text

    -- The below is not used at all for val calls, and the events are not
    -- used for transform calls.  It might be cleaner to split those out, but
    -- too much bother.

    -- | Hack so control calls have access to the previous sample, since
    -- they tend to want to interpolate from that value.
    --
    -- This used to be the only way a call could get the previous value, but
    -- now if the prev val is unset, then "Derive.Args.prev_val" will evaluate
    -- 'info_prev_events'.  But checking info_prev_val is cheaper, so I'll keep
    -- it around.  The evaluation fallback has to exist because track slicing
    -- may snip off the previous event.
    --
    -- See NOTE [prev-val] in "Derive.Args" for details.
    , info_prev_val :: !(Maybe (RealTime, val))

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

    -- | If true, this call is being run under inversion.  This means that
    -- it's the second time it's been seen, though not necessarily executed.
    , info_inverted :: !Bool

    -- | The track tree below note tracks.  Not given for control tracks.
    , info_sub_tracks :: !TrackTree.EventsTree
    -- | If present, 'Derive.Sub.sub_events' will directly return these sub
    -- events instead of slicing sub-tracks.  Track evaluation will never set
    -- this, but calls can set this to reapply a note transformer.  It should
    -- be 'Derive.Sub.Event's, but isn't to avoid circular imports.
    , info_sub_events :: !(Maybe [[(ScoreTime, ScoreTime, NoteDeriver)]])
    -- | This is needed by val calls that want to evaluate events around them.
    -- Since val calls are the same on all track types, they need to know
    -- explicitly what the track type is to evaluate events on it.
    , info_track_type :: !(Maybe TrackInfo.Type)
    }

instance (Pretty.Pretty val) => Pretty.Pretty (CallInfo val) where
    format (CallInfo expr prev_val event prev_events next_events event_end
            track_range inverted sub_tracks sub_events track_type) =
        Pretty.record_title "CallInfo"
            [ ("expr", Pretty.format expr)
            , ("prev_val", Pretty.format prev_val)
            , ("event", Pretty.format event)
            , ("prev_events", Pretty.format prev_events)
            , ("next_events", Pretty.format next_events)
            , ("event_end", Pretty.format event_end)
            , ("track_range", Pretty.format track_range)
            , ("inverted", Pretty.format inverted)
            , ("sub_tracks", Pretty.format sub_tracks)
            , ("sub_events", Pretty.format $
                map (map (\(s, d, _) -> (s, d))) <$> sub_events)
            , ("track_type", Pretty.format track_type)
            ]

coerce_call_info :: CallInfo a -> CallInfo b
coerce_call_info cinfo = cinfo { info_prev_val = Nothing }

-- | Transformer calls don't necessarily apply to any particular event, and
-- neither to generators for that matter.
dummy_call_info :: ScoreTime -> ScoreTime -> String -> CallInfo a
dummy_call_info start dur text = CallInfo
    { info_expr = ""
    , info_inverted = False
    , info_prev_val = Nothing
    , info_event = Event.event start dur s
    , info_prev_events = []
    , info_next_events = []
    , info_event_end = start + dur
    , info_track_range = (start, start + dur)
    , info_sub_tracks = []
    , info_sub_events = Nothing
    , info_track_type = Nothing
    } where s = if null text then "<no-event>" else "<" ++ text ++ ">"

-- | Tag the polymorphic part of the CallInfo so it can be given to
-- a 'ValCall'.  Otherwise, ValCall would have to be polymorphic too,
-- which means it would hard to write generic ones.
tag_call_info :: (ToTagged a) => CallInfo a -> CallInfo Tagged
tag_call_info cinfo = cinfo
    { info_prev_val = second to_tagged <$> info_prev_val cinfo }

-- | A Call will be called as either a generator or a transformer, depending on
-- its position.  A call at the end of a compose pipeline will be called as
-- a generator while ones composed with it will be called as transformers, so
-- in @a | b@, @a@ is a transformer and @b@ is a generator.
--
-- More details on this strange setup are in the "Derive.Call" haddock.
data Call func = Call {
    -- | Since call IDs may be rebound dynamically, each call has its own name
    -- so that error msgs are unambiguous.
    call_name :: !Text
    , call_doc :: !CallDoc
    , call_func :: func
    }
type Generator d = Call (GeneratorFunc d)
type Transformer d = Call (TransformerFunc d)

instance Show (Call derived) where
    show (Call name _ _) = "((Call " <> show name <> "))"
instance Pretty.Pretty (Call derived) where
    pretty (Call name _ _) = untxt name

-- | Documentation for a call.  The documentation is in markdown format, except
-- that a single newline will be replaced with two, so a single \n is enough
-- to start a new paragraph.  Also, single quotes are turned into links as per
-- "Util.TextUtil".haddockUrl.
data CallDoc = CallDoc {
    cdoc_tags :: Tags.Tags
    , cdoc_doc :: Text
    , cdoc_args :: ArgDocs
    } deriving (Eq, Ord, Show)

data ArgDocs = ArgDocs [ArgDoc]
    -- | This means the call parses the args itself in some special way.
    | ArgsParsedSpecially Text
    deriving (Eq, Ord, Show)

data ArgDoc = ArgDoc {
    arg_name :: Text
    , arg_type :: TrackLang.Type
    , arg_parser :: ArgParser
    , arg_environ_default :: !EnvironDefault
    , arg_doc :: Text
    } deriving (Eq, Ord, Show)

-- | These enumerate the different ways an argumnt can be parsed, and
-- correspond to parsers in "Derive.Sig".
data ArgParser = Required | Defaulted !Text | Optional !Text | Many | Many1
    | Environ !(Maybe Text)
    deriving (Eq, Ord, Show)

-- | This configures how an argument looks for a default in the environ.
data EnvironDefault =
    -- | Don't default from environ at all.
    None
    -- | Look for @callname-argname@.
    | Prefixed
    -- | Look for @argname@.  This is useful for generic parameters that
    -- should configure many calls simultaneously.
    | Unprefixed
    -- | First look for a prefixed key, then for an unprefixed one.
    | Both
    deriving (Eq, Ord, Show)

-- | A value annotated with argument docs.  This is returned by the functions
-- in "Derive.Sig", and accepted by the Call constructors here.
type WithArgDoc f = (f, ArgDocs)

-- ** make calls

type GeneratorFunc d = PassedArgs (Elem d) -> LogsDeriver d
-- | args -> deriver -> deriver
type TransformerFunc d = PassedArgs (Elem d) -> LogsDeriver d -> LogsDeriver d

make_call :: Text -> Tags.Tags -> Text -> WithArgDoc func -> Call func
make_call name tags doc (func, arg_docs) = Call
    { call_name = name
    , call_doc = CallDoc tags doc arg_docs
    , call_func = func
    }

-- | Create a generator that expects a list of derived values (e.g. Score.Event
-- or Signal.Control), with no logs mixed in.  The result is wrapped in
-- LEvent.Event.
generator :: (Functor m) => Text -> Tags.Tags -> Text
    -> WithArgDoc (a -> m [d]) -> Call (a -> m [LEvent.LEvent d])
generator name tags doc (func, arg_docs) =
    make_call name tags doc ((map LEvent.Event <$>) . func, arg_docs)

-- | Since Signals themselves are collections, there's little reason for a
-- signal generator to return a Stream of events.  So wrap the generator result
-- in a Stream singleton.
generator1 :: (Functor m) => Text -> Tags.Tags -> Text -> WithArgDoc (a -> m d)
    -> Call (a -> m [LEvent.LEvent d])
generator1 name tags doc (func, arg_docs) =
    generator name tags doc ((LEvent.one <$>) . func, arg_docs)

-- ** transformer

-- | Just 'make_call' with a more specific signature.
transformer :: Text -> Tags.Tags -> Text -> WithArgDoc (TransformerFunc d)
    -> Call (TransformerFunc d)
transformer = make_call

-- ** val

data ValCall = ValCall {
    vcall_name :: !Text
    , vcall_call :: PassedArgs Tagged -> Deriver TrackLang.Val
    , vcall_doc :: !CallDoc
    }

instance Show ValCall where
    show (ValCall name _ _) = "((ValCall " ++ show name ++ "))"

val_call :: TrackLang.Typecheck a => Text -> Tags.Tags -> Text
    -> WithArgDoc (PassedArgs Tagged -> Deriver a) -> ValCall
val_call name tags doc (call, arg_docs) = ValCall
    { vcall_name = name
    , vcall_call = fmap TrackLang.to_val . call
    , vcall_doc = CallDoc tags doc arg_docs
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

newtype BlockDeps = BlockDeps (Set.Set BlockId)
    deriving (Monoid.Monoid, Show, Eq, DeepSeq.NFData)

-- ** damage

-- | Modified ranges in the score.
data ScoreDamage = ScoreDamage {
    -- | Damaged ranges in tracks.
    sdamage_tracks :: !(Map.Map TrackId (Ranges.Ranges ScoreTime))
    -- | The blocks with damaged tracks.  Calls depend on blocks
    -- ('BlockDeps') rather than tracks, so it's convenient to keep the
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
--
-- Block damage also clears track caches that are on that block.
invalidate_damaged :: ScoreDamage -> Cache -> Cache
invalidate_damaged (ScoreDamage tracks _ blocks) (Cache cache) =
    Cache $ Map.mapWithKey invalidate $ Map.filter is_valid cache
    where
    is_valid Invalid = False
    is_valid _ = True
    invalidate stack cached
        | has_damage stack = Invalid
        | otherwise = cached
    -- Block damage clears caches for that block, but not parents because the
    -- normal cache machinery will take care of that.
    has_damage stack
        | Just (Stack.Block block_id) <- Seq.head (Stack.innermost stack) =
            Set.member block_id blocks
        | Just (block_id, track_id) <- Stack.block_track_of stack =
            Map.member track_id tracks || Set.member block_id blocks
        | otherwise = False

-- | Control damage indicates that a section of control signal has been
-- modified.  It's dynamically scoped over the same range as the control
-- itself, so that events that depend on it can be rederived.
newtype ControlDamage = ControlDamage (Ranges.Ranges ScoreTime)
    deriving (Pretty.Pretty, Monoid.Monoid, Eq, Show, DeepSeq.NFData)


-- * scale

-- Like the cache types, this is supposed to be defined in "Derive.Scale", but
-- must be here due to circular dependencies.

data Scale = Scale {
    scale_id :: !Pitch.ScaleId
    -- | A pattern describing what the scale notes look like.  Used only for
    -- error msgs (i.e. parse errors) so it should be human readable and
    -- doesn't have to follow any particular syntax.  A regex is recommended
    -- though.
    , scale_pattern :: !Text

    -- | If a scale uses 'Symbol.Symbol's, it can include the definitions here
    -- so they are close to their use.  This symbol list should be loaded as
    -- soon as possible, which means program startup for hardcoded scales.
    , scale_symbols :: ![Symbol.Symbol]

    -- | The controls that will casue a pitch from this scale to change.
    -- This is used by 'PitchSignal.apply_controls' to know when to reevaluate
    -- a given pitch.  Other controls can affect the pitch, but if they aren't
    -- in this set, the pitch won't be reevaluated when they change.
    , scale_transposers :: !(Set.Set Score.Control)
    -- | Parse a Note into a Pitch.Pitch with scale degree and accidentals.
    , scale_read :: Maybe Pitch.Key -> Pitch.Note
        -> Either ScaleError Pitch.Pitch
    , scale_show :: Maybe Pitch.Key -> Pitch.Pitch
        -> Either ScaleError Pitch.Note
    , scale_layout :: !Layout
    , scale_transpose :: !Transpose
    , scale_enharmonics :: !Enharmonics

    -- | Used by derivation.
    , scale_note_to_call :: !(Pitch.Note -> Maybe ValCall)

    -- | Used by note input.
    , scale_input_to_note :: !(Maybe Pitch.Key -> Pitch.Input
        -> Maybe Pitch.Note)
    -- | Used by MIDI thru.  This is a shortcut for
    -- @eval . note_to_call . input_to_note@ but can often be implemented more
    -- efficiently by the scale.
    --
    -- The ScoreTime is the time at which this pitch is evaluated.  If it
    -- depends on a control, it can get the control from 'Dynamic', but it
    -- needs to know at what point in time to look at the signal.
    --
    -- This is because pitch val calls aren't evaluated in normalized time.
    -- If controls had (shift, stretch) I could normalize them efficiently
    -- and the pitch would just always look at time 0.  But they don't.
    , scale_input_to_nn ::
        !(ScoreTime -> Pitch.Input -> Deriver (Maybe Pitch.NoteNumber))

    -- | Documentation for all of the ValCalls that 'scale_note_to_call' can
    -- return.
    , scale_call_doc :: !DocumentedCall
    }

instance Pretty.Pretty Scale where
    pretty = pretty . scale_id

type LookupScale = Pitch.ScaleId -> Maybe Scale

-- | Scales may ignore Transposition if they don't support it.
--
-- Transposition could almost always succeed, and leaving the error reporting
-- to 'scale_show'.  But for some scales it has to parse the 'Pitch.Key', which
-- can fail.  Parsing the key is pretty unfortunate, since it winds up getting
-- repeated for 'scale_read' and 'scale_show', but I don't want to make the Key
-- type concrete, since each scale has a different one.
--
-- TODO could make the key an existential type and export scale_parse_key?
type Transpose = Transposition -> Maybe Pitch.Key -> Pitch.Step -> Pitch.Pitch
    -> Either ScaleError Pitch.Pitch

data Transposition = Chromatic | Diatonic deriving (Show)

-- | Get the enharmonics of the note.  The given note is omitted, and the
-- enharmonics are in ascending order until they wrap around, so if you always
-- take the head of the list you will cycle through all of the enharmonics.
type Enharmonics = Maybe Pitch.Key -> Pitch.Note
    -> Either ScaleError [Pitch.Note]

-- | The number of chromatic intervals between each 'Pitch.PitchClass',
-- starting from 0, as returned by 'scale_read'.  The length is the number of
-- degree per octave.  A diatonic-only scale will have all 1s, and a scale
-- without octaves has an empty layout.
--
-- This is analogous to 'Theory.Layout', but is intended to be a minimal
-- implementation that all scales can export, without having to support the
-- full complexity of a chromatic scale.
type Layout = Vector.Unboxed.Vector Pitch.Semi

-- | Things that can go wrong during scale operations.
data ScaleError =
    InvalidTransposition | KeyNeeded | UnparseableNote
    -- | An environ value was unparseable.  Has the environ key and a text
    -- description of the error.
    | UnparseableEnviron !TrackLang.ValName !Text
        -- The Text should be TrackLang.Val except that makes Eq not work.
    deriving (Eq, Show)

instance Pretty.Pretty ScaleError where
    pretty err = case err of
        InvalidTransposition -> "invalid transposition"
        KeyNeeded -> "key needed"
        UnparseableNote -> "unparseable note"
        UnparseableEnviron key val -> "unparseable environ "
            <> pretty key <> ": " <> untxt val

{- NOTE [control-modification]
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
