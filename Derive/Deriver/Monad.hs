-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-} -- for super-classes of Callable
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
    , Callable(..), Tagged(..), Taggable(..)
    , LogsDeriver

    , Note, NoteDeriver, NoteArgs, Events
    , Control, ControlDeriver, ControlArgs
    , Pitch, PitchDeriver, PitchArgs

    -- * state
    , State(..), initial_state
    , Threaded(..), initial_threaded
    , Dynamic(..), Inversion(..), initial_dynamic
    , initial_controls, default_dynamic

    -- ** scope
    , Library(..)
    , Scopes(..), empty_scopes, s_generator, s_transformer, s_val
    , Scope(..), s_note, s_control, s_pitch
    , empty_scope
    , ScopeType(..), s_override, s_instrument, s_scale, s_imported
    , DocumentedCall(..)
    , LookupCall(..)
    , extract_doc, extract_val_doc
    , lookup_val_call, lookup_with

    -- ** constant
    , Constant(..), initial_constant
    , op_add, op_sub, op_mul, op_scale

    -- ** instrument
    , Instrument(..), InstrumentCalls(..)

    -- ** control
    , Merge(..), ControlOp(..)

    -- ** collect
    , Collect(..), SignalFragments
    , ControlMod(..), Integrated(..)
    , TrackDynamic

    -- * calls
    , CallMaps(..), call_map
    , call_maps, generator_call_map, transformer_call_map
    , CallInfo(..), info_track_range, coerce_call_info
    , dummy_call_info, tag_call_info
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
    , LookupScale(..)
    , Transpose, Transposition(..), Enharmonics, Layout
    , ScaleError(..)

    -- * merge
    , error_to_warn, merge_events, merge_event_lists, merge_asc_events
    , levent_key
    , merge_logs

    -- * testing
    , invalidate_damaged
) where
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector.Unboxed

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

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Deriver.DeriveM as DeriveM
import Derive.Deriver.DeriveM (get, gets, modify, put, run)
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Lilypond.Types as Lilypond.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


type Deriver = DeriveM.Deriver State Error
type RunResult a = (Either Error a, State, [Log.Msg])

instance Log.LogMonad Deriver where
    write = DeriveM.write
    initialize_msg msg = do
        -- If the msg was created explicitly, it may already have a stack.
        stack <- maybe (gets (state_stack . state_dynamic)) return
            (Log.msg_stack msg)
        return $ msg { Log.msg_stack = Just stack }


-- * error

data Error = Error SrcPos.SrcPos Stack.Stack ErrorVal
    deriving (Show)

instance Pretty.Pretty Error where
    pretty (Error srcpos stack val) =
        txt (SrcPos.show_srcpos srcpos) <> " " <> pretty stack
            <> ": " <> pretty val

data ErrorVal = GenericError !Text | CallError !CallError
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
            "TypeError: arg " <> pretty place <> "/" <> name
            <> source_desc <> ": expected " <> pretty expected
            <> " but got " <> pretty (TrackLang.type_of <$> received)
            <> ": " <> pretty received
            where
            source_desc = case source of
                Literal -> ""
                Quoted call -> " from " <> ShowVal.show_val call
        EvalError place call name (Error _ _ error_val) ->
            -- The srcpos and stack of the derive error is probably not
            -- interesting, so I strip those out.
            "EvalError: arg " <> pretty place <> "/" <> name
            <> " from " <> ShowVal.show_val call
            <> ": " <> pretty error_val
        ArgError err -> "ArgError: " <> err
        CallNotFound call_id -> "CallNotFound: " <> pretty call_id

instance Pretty.Pretty ErrorPlace where
    pretty (TypeErrorArg num) = showt (num + 1)
    pretty (TypeErrorEnviron key) = "environ:" <> pretty key

throw :: Text -> Deriver a
throw = throw_error . GenericError

throw_srcpos :: SrcPos.SrcPos -> Text -> Deriver a
throw_srcpos srcpos = throw_error_srcpos srcpos . GenericError

throw_arg_error :: Text -> Deriver a
throw_arg_error = throw_arg_error_srcpos Nothing

throw_arg_error_srcpos :: SrcPos.SrcPos -> Text -> Deriver a
throw_arg_error_srcpos srcpos =
    throw_error_srcpos srcpos . CallError . ArgError

throw_error :: ErrorVal -> Deriver a
throw_error = throw_error_srcpos Nothing

throw_error_srcpos :: SrcPos.SrcPos -> ErrorVal -> Deriver a
throw_error_srcpos srcpos err = do
    stack <- gets (state_stack . state_dynamic)
    DeriveM.throw (Error srcpos stack err)


-- * derived types

-- | Each kind of deriver looks a different scope for its calls.  By making
-- this a class method, I can figure out which scope to look in just from
-- the type.
class (Show d, Taggable d) => Callable d where
    lookup_generator :: TrackLang.CallId -> Deriver (Maybe (Generator d))
    lookup_transformer :: TrackLang.CallId -> Deriver (Maybe (Transformer d))
    callable_name :: Proxy d -> Text

type LogsDeriver d = Deriver [LEvent.LEvent d]

-- | This is for 'info_prev_val'.  Normally the previous value is available
-- in all its untagged glory based on the type of the call, but ValCalls can
-- occur with all the different types, so they need a tagged 'info_prev_val'.
data Tagged = TagEvent Score.Event | TagControl Signal.Control
    | TagPitch PitchSignal.Signal
    deriving (Show)

instance Pretty.Pretty Tagged where
    format (TagEvent a) = Pretty.format a
    format (TagControl a) = Pretty.format a
    format (TagPitch a) = Pretty.format a

class (Show a, Pretty.Pretty a) => Taggable a where
    to_tagged :: a -> Tagged
    from_tagged :: Tagged -> Maybe a

instance Taggable Tagged where
    to_tagged = id
    from_tagged = Just

-- ** event

type Note = Score.Event
type NoteDeriver = LogsDeriver Score.Event
type NoteArgs = PassedArgs Score.Event

instance Taggable Score.Event where
    to_tagged = TagEvent
    from_tagged (TagEvent a) = Just a
    from_tagged _ = Nothing

-- | This might seem like an inefficient way to represent the Event stream,
-- but I can't think of how to make it better.
--
-- Each call generates a chunk [Event], and the chunks are then joined with
-- 'd_merge_asc'.  This means every cons is copied once, but I think this is
-- hard to avoid if I want to merge streams.
type Events = [LEvent.LEvent Score.Event]

instance Callable Score.Event where
    lookup_generator = lookup_with (scope_note . scopes_generator)
    lookup_transformer = lookup_with (scope_note . scopes_transformer)
    callable_name _ = "note"

instance Monoid.Monoid NoteDeriver where
    mempty = return []
    mappend d1 d2 = d_merge [d1, d2]
    mconcat = d_merge

-- ** control

type Control = Signal.Control
type ControlDeriver = LogsDeriver Signal.Control
type ControlArgs = PassedArgs Control

instance Taggable Control where
    to_tagged = TagControl
    from_tagged (TagControl a) = Just a
    from_tagged _ = Nothing

instance Callable Signal.Control where
    lookup_generator = lookup_with (scope_control . scopes_generator)
    lookup_transformer = lookup_with (scope_control . scopes_transformer)
    callable_name _ = "control"

-- ** pitch

type Pitch = PitchSignal.Signal
type PitchDeriver = LogsDeriver PitchSignal.Signal
type PitchArgs = PassedArgs Pitch

instance Taggable Pitch where
    to_tagged = TagPitch
    from_tagged (TagPitch a) = Just a
    from_tagged _ = Nothing

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
    state_threaded :: !Threaded
    -- | This data is modified in a dynamically scoped way, for
    -- sub-derivations.  This is used like a Reader.
    , state_dynamic :: !Dynamic
    -- | This data is mappended.  It functions like an implicit return value.
    -- This is used like a Writer.
    , state_collect :: !Collect
    -- | This data is constant throughout the derivation.
    , state_constant :: !Constant
    }

initial_state :: TrackLang.Environ -> Constant -> State
initial_state environ constant = State
    { state_threaded = initial_threaded
    , state_dynamic = initial_dynamic environ
    , state_collect = mempty
    , state_constant = constant
    }

-- * Threaded

-- | State which is threaded linearly.  This destroys the ability to
-- parallelize derivation, so it's not so great, but since it only tracks
-- previous value, it can be broken at block boundaries.
newtype Threaded = Threaded {
    -- | Keep track of the previous value for each track currently being
    -- evaluated.  See NOTE [prev-val].
    state_prev_val :: Map.Map (BlockId, TrackId) Tagged
    } deriving (Show)

initial_threaded :: Threaded
initial_threaded = Threaded mempty

-- * Dynamic

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
    , state_control_merge_defaults :: Map.Map Score.Control Merge
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
    -- | Instrument aliases as (alias, destination) pairs.  Map through this
    -- before looking in 'state_lookup_instrument'.  This is analogous to the
    -- instrument db level aliasing, except this only present in a dynamically
    -- scoped part of derivation.  An alias can map to another alias later in
    -- the list.
    , state_instrument_aliases :: ![(Score.Instrument, Score.Instrument)]
    , state_control_damage :: !ControlDamage
    -- | This is a delayed transform.  If a call wants to evaluate under
    -- inversion, it composes itself on to this, which is then applied as
    -- a transformation to the eventual synthesized event at the bottom of the
    -- inversion.
    , state_under_invert :: !(NoteDeriver -> NoteDeriver)
    , state_inversion :: !Inversion

    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: !Stack.Stack
    }

data Inversion =
    -- | Pre-inversion.
    NotInverted
    -- | After inversion, but not yet at the bottom.  The inverted generator
    -- is captured here.
    | InversionInProgress !NoteDeriver

instance Pretty.Pretty Inversion where
    pretty NotInverted = "NotInverted"
    pretty (InversionInProgress {}) = "InversionInProgress"

initial_dynamic :: TrackLang.Environ -> Dynamic
initial_dynamic environ = Dynamic
    { state_controls = initial_controls
    , state_control_functions = mempty
    , state_control_merge_defaults = initial_control_merge_defaults
    , state_pitches = Map.empty
    , state_pitch = mempty
    , state_environ = environ
    , state_warp = Score.id_warp
    , state_scopes = empty_scopes
    , state_instrument_aliases = []
    , state_control_damage = mempty
    , state_under_invert = id
    , state_inversion = NotInverted
    , state_stack = Stack.empty
    }

-- | Initial control environment.
initial_controls :: Score.ControlMap
initial_controls = Map.fromList
    [ (Controls.dynamic, Score.untyped (Signal.constant default_dynamic))
    ]

initial_control_merge_defaults :: Map.Map Score.Control Merge
initial_control_merge_defaults =
    Map.fromList [(c, Merge op_add) | c <- Controls.additive_controls]

-- | A default dynamic that's not 0 is useful because otherwise you have to add
-- dyn to everything.  Since control tracks multiply by default, 1 is the most
-- convenient value.
default_dynamic :: Signal.Y
default_dynamic = 1

instance Pretty.Pretty Dynamic where
    format (Dynamic controls cfuncs cmerge pitches pitch environ warp scopes
            aliases damage _under_invert inversion stack) =
        Pretty.record "Dynamic"
            [ ("controls", Pretty.format controls)
            , ("control functions", Pretty.format cfuncs)
            , ("control merge defaults", Pretty.format cmerge)
            , ("pitches", Pretty.format pitches)
            , ("pitch", Pretty.format pitch)
            , ("environ", Pretty.format environ)
            , ("warp", Pretty.format warp)
            , ("scopes", Pretty.format scopes)
            , ("instrument aliases", Pretty.format aliases)
            , ("damage", Pretty.format damage)
            , ("inversion", Pretty.format inversion)
            , ("stack", Pretty.format stack)
            ]

instance DeepSeq.NFData Dynamic where
    rnf (Dynamic controls cfuncs cmerge pitches pitch environ warp _scopes
            aliases damage _under_invert _inverted_gen stack) =
        rnf controls `seq` rnf cfuncs `seq` rnf cmerge `seq` rnf pitches
        `seq` rnf pitch `seq` rnf environ `seq` rnf warp `seq` rnf aliases
        `seq` rnf damage `seq` rnf stack

-- ** scope

-- | This is the library of built-in calls.  The 'stype_imported' Scope fields
-- are imported from this.
data Library = Library {
    lib_note :: !(CallMaps Note)
    , lib_control :: !(CallMaps Control)
    , lib_pitch :: !(CallMaps Pitch)
    , lib_val :: ![LookupCall ValCall]
    }

instance Monoid.Monoid Library where
    mempty = Library mempty mempty mempty mempty
    mappend (Library note1 control1 pitch1 val1)
            (Library note2 control2 pitch2 val2) =
        Library (note1<>note2) (control1<>control2) (pitch1<>pitch2)
            (val1<>val2)

instance Show Library where show _ = "((Library))"
instance Pretty.Pretty Library where
    format (Library note control pitch val) = Pretty.record "Library"
        [ ("note", Pretty.format note)
        , ("control", Pretty.format control)
        , ("pitch", Pretty.format pitch)
        , ("val", Pretty.format val)
        ]

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

s_generator = Lens.lens scopes_generator
    (\f r -> r { scopes_generator = f (scopes_generator r) })
s_transformer = Lens.lens scopes_transformer
    (\f r -> r { scopes_transformer = f (scopes_transformer r) })
s_val = Lens.lens scopes_val
    (\f r -> r { scopes_val = f (scopes_val r) })

instance Pretty.Pretty Scopes where
    format (Scopes g t v) = Pretty.record "Scopes"
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

s_note = Lens.lens scope_note
    (\f r -> r { scope_note = f (scope_note r) })
s_control = Lens.lens scope_control
    (\f r -> r { scope_control = f (scope_control r) })
s_pitch = Lens.lens scope_pitch
    (\f r -> r { scope_pitch = f (scope_pitch r) })

empty_scope :: Scope a b c
empty_scope = Scope mempty mempty mempty

instance (Pretty.Pretty note, Pretty.Pretty control, Pretty.Pretty pitch) =>
        Pretty.Pretty (Scope note control pitch) where
    format (Scope note control pitch) = Pretty.record "Scope"
        [ ("note", Pretty.format note)
        , ("control", Pretty.format control)
        , ("pitch", Pretty.format pitch)
        ]

instance DeepSeq.NFData (Scope a b c) where rnf _ = ()

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
-- which take priority over scale and imported calls.
data ScopeType call = ScopeType {
    -- | Override calls shadow all others.  They're useful when you want to
    -- prevent instruments from overriding calls, which the lilypond deriver
    -- needs to do.
    stype_override :: ![LookupCall call]
    , stype_instrument :: ![LookupCall call]
    , stype_scale :: ![LookupCall call]
    -- | Imported from the 'Library'.
    , stype_imported :: ![LookupCall call]
    }

s_override = Lens.lens stype_override
    (\f r -> r { stype_override = f (stype_override r) })
s_instrument = Lens.lens stype_instrument
    (\f r -> r { stype_instrument = f (stype_instrument r) })
s_scale = Lens.lens stype_scale
    (\f r -> r { stype_scale = f (stype_scale r) })
s_imported = Lens.lens stype_imported
    (\f r -> r { stype_imported = f (stype_imported r) })

instance Monoid.Monoid (ScopeType call) where
    mempty = ScopeType [] [] [] []
    mappend (ScopeType a1 b1 c1 d1) (ScopeType a2 b2 c2 d2) =
        ScopeType (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2)

instance Show (ScopeType call) where show = prettys
instance Pretty.Pretty (ScopeType call) where
    format (ScopeType override inst scale imported) =
        Pretty.record "ScopeType"
            [ ("override", Pretty.format override)
            , ("inst", Pretty.format inst)
            , ("scale", Pretty.format scale)
            , ("imported", Pretty.format imported)
            ]

-- | This is like 'Call', but with only documentation.  (name, CallDoc)
data DocumentedCall = DocumentedCall !Text !CallDoc

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
    ScopeType override inst scale imported <-
        gets $ get . state_scopes . state_dynamic
    return $ override ++ inst ++ scale ++ imported

-- | Convert a list of lookups into a single lookup by returning the first
-- one to yield a Just.
lookup_scopes :: [LookupCall call] -> (TrackLang.CallId -> Deriver (Maybe call))
lookup_scopes [] _ = return Nothing
lookup_scopes (lookup:rest) call_id =
    maybe (lookup_scopes rest call_id) (return . Just)
        =<< lookup_call lookup call_id

lookup_call :: LookupCall call -> TrackLang.CallId -> Deriver (Maybe call)
lookup_call (LookupMap calls) call_id = return $ Map.lookup call_id calls
lookup_call (LookupPattern _ _ lookup) call_id = lookup call_id

-- ** constant

data Constant = Constant {
    state_ui :: !State.State
    , state_library :: !Library
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

initial_constant :: State.State -> Library -> LookupScale
    -> (Score.Instrument -> Maybe Instrument) -> Cache -> ScoreDamage
    -> Constant
initial_constant ui_state library lookup_scale lookup_inst cache
        score_damage = Constant
    { state_ui = ui_state
    , state_library = library
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
    format (InstrumentCalls gen trans val) = Pretty.record "InstrumentCalls"
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
data Merge = Set -- ^ Replace the existing signal.
    | Merge !ControlOp -- ^ Merge with a specific operator.
    deriving (Show)

instance Pretty.Pretty Merge where pretty = showt
instance DeepSeq.NFData Merge where rnf _ = ()

-- | This is a monoid used for combining two signals.  The identity value
-- is only used when a relative signal is applied when no signal is in scope.
-- This is useful for e.g. a transposition signal which shouldn't care if
-- there is or isn't a transposition signal already in scope.
data ControlOp = ControlOp
    !Text !(Signal.Control -> Signal.Control -> Signal.Control)

instance Show ControlOp where
    show (ControlOp name _) = "((ControlOp " ++ untxt name ++ "))"

-- *** control ops

-- | Default set of control operators.  Merged at runtime with the static
-- config.  TODO but not yet
default_control_op_map :: Map.Map TrackLang.CallId ControlOp
default_control_op_map = Map.fromList $ map (first TrackLang.Symbol)
    [ ("add", op_add)
    , ("sub", op_sub)
    , ("mul", op_mul)
    , ("scale", op_scale)
    , ("max", op_max)
    , ("min", op_min)
    ]

op_add, op_sub, op_mul, op_scale :: ControlOp
op_add = ControlOp "add" Signal.sig_add
op_sub = ControlOp "subtract" Signal.sig_subtract
op_mul = ControlOp "multiply" Signal.sig_multiply
op_scale = ControlOp "scale" Signal.sig_scale

-- These values should never be seen since any reasonable combining signal
-- will be within this range.
op_max, op_min :: ControlOp
op_max = ControlOp "max" Signal.sig_max
op_min = ControlOp "min" Signal.sig_min


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
    , collect_signal_fragments :: !SignalFragments
    , collect_track_dynamic :: !TrackDynamic
    -- | I prefer the Dynamic from the inverted version of a track, if it
    -- exists.  But I want the controls to come from the non-inverted version,
    -- since they are sliced to a particular event in the inverted version.
    -- So I record both, and merge them together at the end.
    , collect_track_dynamic_inverted :: !TrackDynamic
    -- | This is how a call records its dependencies.  After evaluation of
    -- a deriver, this will contain the dependencies of the most recent call.
    , collect_block_deps :: !BlockDeps

    -- | New caches accumulating over the course of the derivation.
    , collect_cache :: !Cache
    , collect_integrated :: ![Integrated]
    , collect_control_mods :: ![ControlMod]
    }

-- | These are fragments of a signal, which will be later collected into
-- 'collect_track_signals'.  This is part of a complicated mechanism to
-- evaluate TrackSignals only once.  When the sliced fragments of a track are
-- evaluated, they collect signal fragments.  When the track is fully
-- evaluated, they are sorted and merged into 'collect_track_signals'.
-- If the track is then evaluated again, the monoid instance will discard the
-- duplicate.
--
-- The signal fragments are kept sorted by the slice order.  Since
-- 'Signal.merge' makes the earlier signals win in case of overlaps, this
-- ensures a trimmed earlier fragment won't replace a more complete later one.
type SignalFragments =
    Map.Map (BlockId, TrackId) (Map.Map TrackTime Signal.Control)

instance Pretty.Pretty Collect where
    format (Collect warp_map tsigs frags trackdyn trackdyn_inv deps cache
            integrated cmods) =
        Pretty.record "Collect"
            [ ("warp_map", Pretty.format warp_map)
            , ("track_signals", Pretty.format tsigs)
            , ("signal_fragments", Pretty.format frags)
            , ("track_dynamic", Pretty.format trackdyn)
            , ("track_dynamic_inverted", Pretty.format trackdyn_inv)
            , ("block_deps", Pretty.format deps)
            , ("cache", Pretty.format cache)
            , ("integrated", Pretty.format integrated)
            , ("control_mods", Pretty.format cmods)
            ]

instance Monoid.Monoid Collect where
    mempty = Collect mempty mempty mempty mempty mempty mempty mempty mempty
        mempty
    mappend (Collect warps1 tsigs1 frags1 trackdyn1 trackdyn_inv1 deps1 cache1
                integrated1 cmods1)
            (Collect warps2 tsigs2 frags2 trackdyn2 trackdyn_inv2 deps2 cache2
                integrated2 cmods2) =
        Collect (warps1 <> warps2)
            (tsigs1 <> tsigs2) (Map.unionWith (<>) frags1 frags2)
            (trackdyn1 <> trackdyn2) (trackdyn_inv1 <> trackdyn_inv2)
            (deps1 <> deps2) (cache1 <> cache2) (integrated1 <> integrated2)
            (cmods1 <> cmods2)

instance DeepSeq.NFData Collect where
    rnf (Collect warp_map frags tsigs track_dyn track_dyn_inv local_dep cache
            integrated _cmods) =
        rnf warp_map `seq` rnf frags `seq` rnf tsigs `seq` rnf track_dyn
        `seq` rnf track_dyn_inv `seq` rnf local_dep `seq` rnf cache
        `seq` rnf integrated

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

data Integrated = Integrated {
    -- BlockId for a block integration, TrackId for a track integration.
    integrated_source :: !(Either BlockId TrackId)
    , integrated_events :: !Events
    } deriving (Show)

instance Pretty.Pretty Integrated where
    format (Integrated source events) = Pretty.record "Integrated"
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
-- NOTE [record-track-dynamics] One complication is that when I get controls
-- from sliced tracks, the controls are also sliced.  But I need the environ
-- from the inverted version of the track so the common case of [>i, *scale]
-- gets the correct scale.  So I record TrackDynamic for both inverted and non
-- inverted tracks and prefer the inverted tracks, but take controls from
-- the non-inverted versions.
type TrackDynamic = Map.Map (BlockId, TrackId) Dynamic


-- ** calls

data LookupCall call =
    LookupMap !(Map.Map TrackLang.CallId call)
    -- | Text description of the CallIds accepted.  The function is in Deriver
    -- because some calls want to look at the state to know if the CallId is
    -- valid, e.g. block calls.
    | LookupPattern !Text !DocumentedCall
        !(TrackLang.CallId -> Deriver (Maybe call))

instance Pretty.Pretty (LookupCall call) where
    format c = case c of
        LookupMap calls -> "Map: " <> Pretty.format (Map.keys calls)
        LookupPattern name _ _ -> "Pattern: " <> Pretty.text name

-- | Previously, a single Call contained both generator and transformer.
-- This turned out to not be flexible enough, because an instrument that
-- wanted to override a generator meant you couldn't use a transformer that
-- happened to have the same name.  However, there are a number of calls that
-- want both generator and transformer versions, and it's convenient to be
-- able to deal with those together.
data CallMaps d = CallMaps ![LookupCall (Generator d)]
    ![LookupCall (Transformer d)]

instance Monoid.Monoid (CallMaps d) where
    mempty = CallMaps [] []
    mappend (CallMaps gs1 ts1) (CallMaps gs2 ts2) =
        CallMaps (gs1 <> gs2) (ts1 <> ts2)

instance Pretty.Pretty (CallMaps d) where
    format (CallMaps gs ts) = Pretty.record "CallMaps"
        [ ("generators", Pretty.format gs)
        , ("transformers", Pretty.format ts)
        ]

-- | Make LookupCalls whose the calls are all 'LookupMap's.  The LookupMaps
-- are all singletons since names are allowed to overlap when declaring calls.
-- It is only when they are imported into a scope that the maps are combined.
call_map :: [(TrackLang.CallId, call)] -> [LookupCall call]
call_map = map (LookupMap . uncurry Map.singleton)

-- | Bundle generators and transformers up together for convenience.
call_maps :: [(TrackLang.CallId, Generator d)]
    -> [(TrackLang.CallId, Transformer d)] -> CallMaps d
call_maps generators transformers =
    CallMaps (call_map generators) (call_map transformers)

generator_call_map :: [(TrackLang.CallId, Generator d)] -> CallMaps d
generator_call_map generators = call_maps generators []

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

instance Pretty.Pretty val => Pretty.Pretty (PassedArgs val) where
    format (PassedArgs vals call_name info) = Pretty.record "PassedArgs"
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
    info_prev_val :: !(Maybe val)

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
    -- | From 'TrackTree.track_shifted'.
    , info_track_shifted :: !TrackTime

    -- | The track tree below note tracks.  Not given for control tracks.
    -- TODO should this be Either with info_sub_events?  I don't think I ever
    -- need both set.
    , info_sub_tracks :: !TrackTree.EventsTree
    -- | If present, 'Derive.Sub.sub_events' will directly return these sub
    -- events instead of slicing sub-tracks.  Track evaluation will never set
    -- this, but calls can set this to reapply a note transformer.  It should
    -- be 'Derive.Sub.Event's, but isn't to avoid circular imports.
    , info_sub_events :: !(Maybe [[(ScoreTime, ScoreTime, NoteDeriver)]])
    -- | This is needed by val calls that want to evaluate events around them.
    -- Since val calls are the same on all track types, they need to know
    -- explicitly what the track type is to evaluate events on it.
    , info_track_type :: !(Maybe ParseTitle.Type)
    }

-- | Range of the event in TrackTime.
info_track_range :: CallInfo a -> (TrackTime, TrackTime)
info_track_range info = (shifted, shifted + info_event_end info)
    where shifted = info_track_shifted info

instance Pretty.Pretty val => Pretty.Pretty (CallInfo val) where
    format (CallInfo prev_val event prev_events next_events event_end
            track_range sub_tracks sub_events track_type) =
        Pretty.record "CallInfo"
            [ ("prev_val", Pretty.format prev_val)
            , ("event", Pretty.format event)
            , ("prev_events", Pretty.format prev_events)
            , ("next_events", Pretty.format next_events)
            , ("event_end", Pretty.format event_end)
            , ("track_range", Pretty.format track_range)
            , ("sub_tracks", Pretty.format sub_tracks)
            , ("sub_events", Pretty.format $
                map (map (\(s, d, _) -> (s, d))) <$> sub_events)
            , ("track_type", Pretty.format track_type)
            ]

coerce_call_info :: CallInfo a -> CallInfo b
coerce_call_info cinfo = cinfo { info_prev_val = Nothing }

-- | Transformer calls don't necessarily apply to any particular event, and
-- neither to generators for that matter.
dummy_call_info :: ScoreTime -> ScoreTime -> Text -> CallInfo a
dummy_call_info start dur text = CallInfo
    { info_prev_val = Nothing
    , info_event = Event.event start dur s
    , info_prev_events = []
    , info_next_events = []
    , info_event_end = start + dur
    , info_track_shifted = 0
    , info_sub_tracks = []
    , info_sub_events = Nothing
    , info_track_type = Nothing
    } where s = if Text.null text then "<no-event>" else "<" <> text <> ">"

-- | Taggable the polymorphic part of the CallInfo so it can be given to
-- a 'ValCall'.  Otherwise, ValCall would have to be polymorphic too,
-- which means it would hard to write generic ones.
tag_call_info :: Taggable a => CallInfo a -> CallInfo Tagged
tag_call_info cinfo = cinfo
    { info_prev_val = to_tagged <$> info_prev_val cinfo }

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
    pretty (Call name _ _) = name

-- | Documentation for a call.  The documentation is in markdown format, except
-- that a single newline will be replaced with two, so a single \n is enough
-- to start a new paragraph.  Also, single quotes are turned into links as per
-- "Util.TextUtil".haddockUrl.
data CallDoc = CallDoc {
    cdoc_module :: !Module.Module
    , cdoc_tags :: !Tags.Tags
    , cdoc_doc :: !Text
    , cdoc_args :: !ArgDocs
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

type GeneratorFunc d = PassedArgs d -> LogsDeriver d
-- | args -> deriver -> deriver
type TransformerFunc d = PassedArgs d -> LogsDeriver d -> LogsDeriver d

make_call :: Module.Module -> Text -> Tags.Tags -> Text -> WithArgDoc func
    -> Call func
make_call module_ name tags doc (func, arg_docs) = Call
    { call_name = name
    , call_doc = CallDoc
        { cdoc_module = module_
        , cdoc_tags = tags
        , cdoc_doc = doc
        , cdoc_args = arg_docs
        }
    , call_func = func
    }

-- | Create a generator that expects a list of derived values (e.g. Score.Event
-- or Signal.Control), with no logs mixed in.  The result is wrapped in
-- LEvent.Event.
generator :: Module.Module -> Text -> Tags.Tags -> Text
    -> WithArgDoc (PassedArgs d -> Deriver [d]) -> Call (GeneratorFunc d)
generator module_ name tags doc (func, arg_docs) =
    make_call module_ name tags doc ((map LEvent.Event <$>) . func, arg_docs)

-- | Since Signals themselves are collections, there's little reason for a
-- signal generator to return a Stream of events.  So wrap the generator result
-- in a Stream singleton.
--
-- TODO call this signal_generator?
generator1 :: Module.Module -> Text -> Tags.Tags -> Text
    -> WithArgDoc (PassedArgs d -> Deriver d) -> Call (GeneratorFunc d)
generator1 module_ name tags doc (func, arg_docs) =
    generator module_ name tags doc (((:[]) <$>) . func, arg_docs)

-- ** transformer

-- | Just 'make_call' with a more specific signature.
transformer :: Module.Module -> Text -> Tags.Tags -> Text
    -> WithArgDoc (TransformerFunc d) -> Call (TransformerFunc d)
transformer = make_call

-- ** val

data ValCall = ValCall {
    vcall_name :: !Text
    , vcall_doc :: !CallDoc
    , vcall_call :: PassedArgs Tagged -> Deriver TrackLang.Val
    }

instance Show ValCall where
    show (ValCall name _ _) = "((ValCall " ++ show name ++ "))"

val_call :: TrackLang.Typecheck a => Module.Module -> Text -> Tags.Tags -> Text
    -> WithArgDoc (PassedArgs Tagged -> Deriver a) -> ValCall
val_call module_ name tags doc (call, arg_docs) = ValCall
    { vcall_name = name
    , vcall_doc = CallDoc
        { cdoc_module = module_
        , cdoc_tags = tags
        , cdoc_doc = doc
        , cdoc_args = arg_docs
        }
    , vcall_call = fmap TrackLang.to_val . call
    }


-- ** cache types

-- $cache_doc
-- The cache types are nominally exported from "Derive.Cache", but must be
-- defined here to avoid circular dependencies.

-- instead of a stack, this could be a tree of frames
newtype Cache = Cache (Map.Map Stack.Stack Cached)
    deriving (Monoid.Monoid, Pretty.Pretty, DeepSeq.NFData)
    -- The monoid instance winds up being a left-biased union.  This is ok
    -- because merged caches shouldn't overlap anyway.

cache_size :: Cache -> Int
cache_size (Cache c) = Map.size c

-- | When cache entries are invalidated by ScoreDamage, a marker is left in
-- their place.  This is just for a nicer log msg that can tell the difference
-- between never evaluated and damaged.
data Cached = Cached !CacheEntry | Invalid

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
data CallType d = CallType !Collect ![LEvent.LEvent d]

instance (DeepSeq.NFData d) => DeepSeq.NFData (CallType d) where
    rnf (CallType collect events) = rnf collect `seq` rnf events

-- ** deps

newtype BlockDeps = BlockDeps (Set.Set BlockId)
    deriving (Pretty.Pretty, Monoid.Monoid, Show, Eq, DeepSeq.NFData)

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
        Pretty.record "ScoreDamage"
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
    has_damage stack = any overlaps (Stack.to_ui_innermost stack)
    overlaps (block, track, range) = maybe False (`Set.member` blocks) block
        || case (track, range) of
            (Just track_id, Nothing) -> Map.member track_id tracks
            (Just track_id, Just (s, e))
                | Just ranges <- Map.lookup track_id tracks ->
                    Ranges.overlapping ranges (Ranges.range s e)
            _ -> False

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
    , scale_read :: TrackLang.Environ -> Pitch.Note
        -> Either ScaleError Pitch.Pitch
    , scale_show :: TrackLang.Environ -> Pitch.Pitch
        -> Either ScaleError Pitch.Note
    , scale_layout :: !Layout
    , scale_transpose :: !Transpose
    , scale_enharmonics :: !Enharmonics

    -- | Used by derivation.
    , scale_note_to_call :: !(Pitch.Note -> Maybe ValCall)

    -- | Used by note input.
    , scale_input_to_note :: !(TrackLang.Environ -> Pitch.Input
        -> Either ScaleError Pitch.Note)
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
    , scale_input_to_nn :: !(ScoreTime -> Pitch.Input
        -> Deriver (Either ScaleError Pitch.NoteNumber))

    -- | Documentation for all of the ValCalls that 'scale_note_to_call' can
    -- return.
    , scale_call_doc :: !DocumentedCall
    }

instance Pretty.Pretty Scale where
    pretty = pretty . scale_id

-- | A scale can configure itself by looking in the environment and by looking
-- up other scales.
newtype LookupScale = LookupScale (TrackLang.Environ -> LookupScale
    -> Pitch.ScaleId -> Maybe (Either ScaleError Scale))
instance Show LookupScale where show _ = "((LookupScale))"

-- | Scales may ignore Transposition if they don't support it.
--
-- Transposition could almost always succeed, and leaving the error reporting
-- to 'scale_show'.  But for some scales it has to parse the 'Pitch.Key' from
-- the environ, which can fail.  Parsing the key is pretty unfortunate, since
-- it winds up getting repeated for 'scale_read' and 'scale_show', but I don't
-- want to make the Key type concrete, since each scale has a different one.
--
-- TODO could make the key an existential type and export scale_parse_key?
type Transpose = Transposition -> TrackLang.Environ -> Pitch.Step -> Pitch.Pitch
    -> Either ScaleError Pitch.Pitch

data Transposition = Chromatic | Diatonic deriving (Show)

-- | Get the enharmonics of the note.  The given note is omitted, and the
-- enharmonics are in ascending order until they wrap around, so if you always
-- take the head of the list you will cycle through all of the enharmonics.
type Enharmonics = TrackLang.Environ -> Pitch.Note
    -> Either ScaleError [Pitch.Note]

-- | The number of chromatic intervals between each 'Pitch.PitchClass',
-- starting from 0, as returned by 'scale_read'.  The length is the number of
-- degrees per octave.  A diatonic-only scale will have all 1s, and a scale
-- without octaves has an empty layout.
--
-- This is analogous to 'Theory.Layout', but is intended to be a minimal
-- implementation that all scales can export, without having to support the
-- full complexity of a chromatic scale.
--
-- Combined with 'scale_read' and 'scale_show', I can use this to do math on
-- scale degrees.
type Layout = Vector.Unboxed.Vector Pitch.Semi

-- | Things that can go wrong during scale operations.
data ScaleError =
    InvalidTransposition | UnparseableNote
    -- | Note out of the scale's range.
    | OutOfRange
    -- | Input note doesn't map to a scale note.
    | InvalidInput
    -- | A required environ value was missing.
    | EnvironMissing !TrackLang.ValName
    -- | An environ value was unparseable.  Has the environ key and a text
    -- description of the error.
    | UnparseableEnviron !TrackLang.ValName !Text
        -- The Text should be TrackLang.Val except that makes Eq not work.
    -- | Other kind of error.
    | ScaleError !Text
    deriving (Eq, Show)

instance Pretty.Pretty ScaleError where
    pretty err = case err of
        InvalidTransposition -> "invalid transposition"
        UnparseableNote -> "unparseable note"
        OutOfRange -> "out of range"
        InvalidInput -> "invalid input"
        EnvironMissing key -> "missing environ value: " <> pretty key
        UnparseableEnviron key val -> "unparseable environ: "
            <> pretty key <> "=" <> val
        ScaleError msg -> msg

-- * merge

-- | The EventDerivers run as sub-derivers and the results are mappended, which
-- lets them to interleave their work or run in parallel.
d_merge :: [NoteDeriver] -> NoteDeriver
d_merge [] = mempty
d_merge [d] = d
d_merge derivers = merge_event_lists <$> sequence derivers
    -- Previously, each deriver was run independently, and their Collects
    -- merged.  The theory was to allow their derivation to be interleaved
    -- on demand as the events themselves are interleaved.  However, profiling
    -- doesn't show a significant difference, and this way is simpler.

merge_logs :: Either Error [LEvent.LEvent d] -> [Log.Msg] -> [LEvent.LEvent d]
merge_logs result logs = case result of
    Left err -> map LEvent.Log (logs ++ [error_to_warn err])
    Right events -> events ++ map LEvent.Log logs

error_to_warn :: Error -> Log.Msg
error_to_warn (Error srcpos stack val) = Log.msg_srcpos srcpos Log.Warn
    (Just stack) ("Error: " <> pretty val)

merge_events :: Events -> Events -> Events
merge_events = Seq.merge_on levent_key

merge_event_lists :: [Events] -> Events
merge_event_lists = Seq.merge_lists levent_key

-- | Merge sorted lists of events.  If the lists themselves are also sorted,
-- I can produce output without scanning the entire input list, so this should
-- be more efficient for a large input list than 'merge_events'.
merge_asc_events :: [Events] -> Events
merge_asc_events = Seq.merge_asc_lists levent_key

-- | This will make logs always merge ahead of score events, but that should
-- be ok.
levent_key :: LEvent.LEvent Score.Event -> RealTime
    -- Yeah it's a hack and I could use a pair, but RealTime should never go
    -- far negative.
levent_key (LEvent.Log _) = -RealTime.large
levent_key (LEvent.Event event) = Score.event_start event

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
