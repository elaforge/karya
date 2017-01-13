-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-} -- for super-classes of Callable
{-# LANGUAGE ImplicitParams #-}
{- | Implementation for the Deriver monad.

    This module should contain only 'Deriver' and the definitions needed to
    implement it.  Unfortunately that winds up being quite a lot.  Deriver
    carries a 'State', which is depended upon directly by all derivation, so
    it's just as complicated as derivation itself, which is to say, very.

    Since State only really requires data types, the majority of this module
    is data declarations, with the exception of a few constructors which
    are intimately concerned with the type they are constructing.  The library
    of functions to manipulate these types are split into "Derive.Deriver.Lib".

    This module is way too big.  Unfortunately it's hard to split up because
    of circular imports.  Anyone who directly or indirectly needs Deriver
    (e.g. calls) needs to import Derive.  However, anything directly or
    indirectly used by State must be imported by Derive.  Since State is the
    central type that must hold anything that persists beyond the evaluation
    of a single note, that winds up being a lot.  At one point I tried to
    reign in the madness with hs-boot files, but I decided that hs-boot was
    worse.
-}
module Derive.Deriver.Monad (
    -- * Deriver
    Deriver, RunResult
    , modify, get, gets, put, run
    , initialize_log_msg

    -- * error
    , Error(..), ErrorVal(..), CallError(..), ErrorPlace(..), EvalSource(..)
    , throw, throw_arg_error, throw_error

    -- * derived types
    , Callable(..), Tagged(..), Taggable(..)

    , Note, NoteDeriver, NoteArgs
    , Control, ControlDeriver, ControlArgs
    , Pitch, PitchDeriver, PitchArgs

    -- * state
    , State(..), initial_state
    , Threaded(..), initial_threaded
    , Dynamic(..), Inversion(..), initial_dynamic, strip_dynamic
    , initial_controls, default_dynamic

    -- ** scope
    , Library(..)
    , Scopes(..), empty_scopes, s_generator, s_transformer, s_val
    , Scope(..), s_note, s_control, s_pitch
    , empty_scope
    , ScopePriority(..), CallPriority(..)
    , scope_priority, lookup_priority, add_priority, replace_priority
    , DocumentedCall(..)
    , LookupCall(..)
    , extract_doc, extract_val_doc
    , lookup_val_call, lookup_with

    -- ** constant
    , Constant(..), initial_constant
    , Mode(..)
    , mergers, merge_add, merge_sub, merge_mul, merge_scale

    -- ** instrument
    , Instrument(..), InstrumentCalls(..)

    -- ** control
    , Merge(..), Merger(..)

    -- ** collect
    , Collect(..), SignalFragments
    , ControlMod(..), Integrated(..)
    , TrackDynamic, CallDuration(..)

    -- * calls
    , CallMaps(..), call_map
    , call_maps, generator_call_map, transformer_call_map
    , Context(..), ctx_track_range, coerce_context
    , dummy_context, tag_context
    , Call(..), make_call
    , CallName(..), ArgName(..), sym_to_call_name, sym_to_arg_name
    , CallDoc(..), ArgDoc(..), ArgParser(..), EnvironDefault(..)
    , WithArgDoc
    , PassedArgs(..)

    -- ** generator
    , Generator, GeneratorFunc(..), GeneratorF, generator_func
    , generator, generator_events, generator1
    , with_score_duration, with_real_duration

    -- ** transformer
    , Transformer, TransformerF
    , transformer

    -- ** val
    , ValCall(..), make_val_call

    -- ** cache types
    -- $cache_doc
    , Cache(..), CacheKey(..), Cached(..), cache_size
    , CacheEntry(..), CallType(..)
    , BlockDeps(..)

    -- ** damage
    , ScoreDamage(..)
    , ControlDamage(..)

    -- * util
    , score_to_real, real_to_score

    -- * scale
    -- $scale_doc
    , Scale(..)
    , LookupScale(..)
    , Transpose, Transposition(..), Enharmonics, Layout

    -- * merge
    , error_to_warn, merge_logs

    -- * testing
    , invalidate_damaged
) where
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Vector.Unboxed as Vector.Unboxed

import qualified GHC.Stack

import qualified Util.CallStack as CallStack
import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.Map
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges

import qualified Ui.Event as Event
import qualified Ui.Ui as Ui
import qualified Ui.Symbol as Symbol
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Deriver.DeriveM as DeriveM
import Derive.Deriver.DeriveM (get, gets, modify, put, run)
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.ValType as ValType

import qualified Perform.Lilypond.Types as Lilypond.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Global
import Types


type Deriver = DeriveM.Deriver State Error
type RunResult a = DeriveM.RunResult State Error a

instance Log.LogMonad Deriver where
    write = DeriveM.write <=< initialize_log_msg

initialize_log_msg :: Log.Msg -> Deriver Log.Msg
initialize_log_msg msg = case Log.msg_stack msg of
    -- If the msg was created explicitly, it may already have a stack.
    Just _ -> return msg
    Nothing -> do
        stack <- gets (state_stack . state_dynamic)
        return $! msg { Log.msg_stack = Just stack }

-- * error

data Error = Error !GHC.Stack.CallStack !Stack.Stack !ErrorVal
    deriving (Show)

instance Pretty.Pretty Error where
    pretty (Error call_stack stack val) =
        CallStack.showCaller (CallStack.caller call_stack)
            <> " " <> pretty stack <> ": " <> pretty val

data ErrorVal = GenericError !Text | CallError !CallError
    deriving (Show)

instance Pretty.Pretty ErrorVal where
    pretty (GenericError s) = s
    pretty (CallError err) = pretty err

data CallError =
    -- | ErrorPlace, EvalSource, arg name, expected type, received val, derive
    -- error.  If the derive error is present, the type check required running
    -- a Deriver and the Deriver crashed.
    TypeError !ErrorPlace !EvalSource
        !ArgName
        !ValType.Type -- expected type
        !(Maybe BaseTypes.Val) -- received val
        !(Maybe Error) -- derive error
    -- | Couldn't even call the thing because the name was not found.
    | CallNotFound !BaseTypes.CallId
    -- | Calling error that doesn't fit into the above categories.
    | ArgError !Text
    deriving (Show)

-- | Where a type error came from.  The arg number starts at 0.
data ErrorPlace = TypeErrorArg !Int | TypeErrorEnviron !BaseTypes.Symbol
    deriving (Eq, Show)

data EvalSource =
    -- | The value in error came from a literal expression.
    Literal
    -- | The value in error came from a 'BaseTypes.VQuoted' bit of code.
    | Quoted !BaseTypes.Quoted
    deriving (Show)

instance Pretty.Pretty CallError where
    pretty err = case err of
        TypeError place source (ArgName arg_name) expected received
                derive_error ->
            "TypeError: arg " <> pretty place <> "/" <> arg_name
            <> source_desc <> ": expected " <> pretty expected
            <> " but got " <> pretty (ValType.type_of <$> received)
            <> ": " <> pretty received
            <> maybe "" show_derive_error derive_error
            where
            source_desc = case source of
                Literal -> ""
                Quoted call -> " from " <> ShowVal.show_val call
        ArgError err -> "ArgError: " <> err
        CallNotFound call_id -> "CallNotFound: " <> pretty call_id
        where
        -- The srcpos and stack of the derive error is probably not
        -- interesting, so I strip those out.
        show_derive_error (Error _ _ error_val) =
            " (the type conversion required derivation, which crashed: "
            <> pretty error_val <> ")"

instance Pretty.Pretty ErrorPlace where
    pretty (TypeErrorArg num) = showt (num + 1)
    pretty (TypeErrorEnviron key) = "environ:" <> pretty key

throw :: CallStack.Stack => Text -> Deriver a
throw = throw_error . GenericError

throw_arg_error :: CallStack.Stack => Text -> Deriver a
throw_arg_error = throw_error . CallError . ArgError

throw_error :: CallStack.Stack => ErrorVal -> Deriver a
throw_error err = do
    stack <- gets (state_stack . state_dynamic)
    DeriveM.throw (Error ?stack stack err)


-- * derived types

-- | Each kind of deriver looks a different scope for its calls.  By making
-- this a class method, I can figure out which scope to look in just from
-- the type.
class (Show d, Taggable d) => Callable d where
    lookup_generator :: BaseTypes.CallId -> Deriver (Maybe (Generator d))
    lookup_transformer :: BaseTypes.CallId -> Deriver (Maybe (Transformer d))
    callable_name :: Proxy d -> Text

-- | This is for 'ctx_prev_val'.  Normally the previous value is available
-- in all its untagged glory based on the type of the call, but ValCalls can
-- occur with all the different types, so they need a tagged 'ctx_prev_val'.
data Tagged = TagEvent Score.Event | TagControl Signal.Control
    | TagPitch PSignal.PSignal
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
type NoteDeriver = Deriver (Stream.Stream Score.Event)
type NoteArgs = PassedArgs Score.Event

instance Taggable Score.Event where
    to_tagged = TagEvent
    from_tagged (TagEvent a) = Just a
    from_tagged _ = Nothing

instance Callable Score.Event where
    lookup_generator = lookup_with (scope_note . scopes_generator)
    lookup_transformer = lookup_with (scope_note . scopes_transformer)
    callable_name _ = "note"

instance Monoid NoteDeriver where
    mempty = return mempty
    mappend d1 d2 = d_merge [d1, d2]
    mconcat = d_merge

-- ** control

type Control = Signal.Control
type ControlDeriver = Deriver (Stream.Stream Signal.Control)
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

type Pitch = PSignal.PSignal
type PitchDeriver = Deriver (Stream.Stream PSignal.PSignal)
type PitchArgs = PassedArgs Pitch

instance Taggable Pitch where
    to_tagged = TagPitch
    from_tagged (TagPitch a) = Just a
    from_tagged _ = Nothing

instance Callable PSignal.PSignal where
    lookup_generator = lookup_with (scope_pitch . scopes_generator)
    lookup_transformer = lookup_with (scope_pitch . scopes_transformer)
    callable_name _ = "pitch"

-- * state

-- | All the state available during derivation.
data State = State {
    -- | Threaded state means deriving one event depends on the results of the
    -- previous event.  This corresponds to StateT.
    state_threaded :: !Threaded
    -- | This data is modified in a dynamically scoped way, for
    -- sub-derivations.  This corresponds to ReaderT.
    , state_dynamic :: !Dynamic
    -- | This data is mappended.  It functions like an implicit return value.
    -- This corresponds to WriterT.
    , state_collect :: !Collect
    -- | This data is constant throughout the derivation.
    , state_constant :: !Constant
    }

initial_state :: Constant -> Dynamic -> State
initial_state constant dynamic = State
    { state_threaded = initial_threaded
    , state_dynamic = dynamic
    , state_collect = mempty
    , state_constant = constant
    }

-- * Threaded

-- | State which is threaded linearly.  This destroys the ability to
-- parallelize derivation, so it's not so great.  However, the only threaded
-- state is state_prev_val, which is only needed within a track, so sibling
-- tracks can still be parallelized.
data Threaded = Threaded {
    -- | Keep track of the previous value for each track currently being
    -- evaluated.  See NOTE [prev-val].
    state_prev_val :: !(Map (BlockId, TrackId) Tagged)
    -- | This is used for 'Stack.Serial' to ensure a unique stack for multiple
    -- generator calls within a single track event.  It's reset on the
    -- evaluation of each uninverted track event, and incremented after
    -- every Score.Event is emitted.  See NOTE [event-serial] for history.
    , state_event_serial :: !Stack.Serial
    } deriving (Show)

initial_threaded :: Threaded
initial_threaded = Threaded mempty 0


-- * Dynamic

-- | This is a dynamically scoped environment that applies to generated events
-- inside its scope.
data Dynamic = Dynamic {
    -- | Derivers can modify it for sub-derivers, or look at it, whether to
    -- attach to an Event or to handle internally.
    state_controls :: !Score.ControlMap
    -- | Function variant of controls.  Normally they modify a backing
    -- 'Signal.Control', but could be synthesized as well.  See
    -- 'BaseTypes.ControlFunction' for details.
    , state_control_functions :: !Score.ControlFunctionMap
    , state_control_merge_defaults ::
        Map Score.Control (Merger Signal.Control)
    -- | Named pitch signals.
    , state_pitches :: !Score.PitchMap
    -- | The unnamed pitch signal currently in scope.  This is the pitch signal
    -- that's applied to notes by default.  It's split off from 'state_pitches'
    -- because it's convenient to guarentee that the main pitch signal is
    -- always present.
    , state_pitch :: !PSignal.PSignal
    , state_environ :: !BaseTypes.Environ
    , state_warp :: !Score.Warp
    -- | Calls currently in scope.
    , state_scopes :: !Scopes
    -- | Instrument aliases as (alias, destination) pairs.  Map through this
    -- before looking in 'state_lookup_instrument'.  The alias destination is
    -- always the final instrument, not another alias, so you never have to
    -- look up multiple times.
    , state_instrument_aliases :: !(Map Score.Instrument Score.Instrument)
    , state_control_damage :: !ControlDamage
    -- | This is a delayed transform.  If a call wants to evaluate under
    -- inversion, it composes itself on to this, which is then applied as
    -- a transformation to the eventual synthesized event at the bottom of the
    -- inversion.
    , state_under_invert :: !(NoteDeriver -> NoteDeriver)
    , state_inversion :: !Inversion
    {- | Each note track sets this to either an unsliced evaluation of the
        closest pitch track below it, or its surrounding 'state_pitch' if there
        is no pitch track below.  Calls can then use it to get neighboring
        pitches.  It's lazily evaluated so there's no extra derivation if you
        don't need it.

        This is cleared when evaluating for itself, so there's no recursion.
        This means given two "next pitch"es in a row, they will both get
        Nothing.  Then on real evaluation, the 2nd will get the next pitch, but
        the 1st will get whatever the 2nd does when it can't get a next pitch.

        TODO if they both emit no pitch, then the 1st will actually get the
        previous pitch, which seems error-prone.  But I think for it to be an
        error, I'd have to have it return an error, e.g. Map TrackTime Pitch
    -}
    , state_pitch_map :: !(Maybe (Maybe PSignal.PSignal, [Log.Msg]))

    -- | This is set to the current note track being evaluated.  It's useful
    -- to look up 'state_prev_val' when evaluating other tracks in an
    -- inversion.  It's set when entering a note track, and unset when entering
    -- a block.
    , state_note_track :: !(Maybe (BlockId, TrackId))
    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: !Stack.Stack
    , state_mode :: !Mode
    }

{- | When a note call inverts, it stashes its actual note-generating code so
    it can re-invoke track evaluation on the control tracks below it.  It's
    kind of like saving a continuation.

    Previously I did it by copying the text of the inverting call to the
    generated track.  The problem was that I therefore had to keep the
    evaluated expression around in the call 'Context', and if I forgot to clear
    it in the right places things would be very confusing when a later
    inversion executed unexpected code.  'Derive.Call.Sub.under_invert'
    transforms are now also stored as code rather than data, in
    'state_under_invert'.
-}
data Inversion =
    -- | Pre-inversion.
    NotInverted
    -- | After inversion, but not yet at the bottom.  The inverted generator
    -- is captured here.
    | InversionInProgress !NoteDeriver

instance Pretty.Pretty Inversion where
    pretty NotInverted = "NotInverted"
    pretty (InversionInProgress {}) = "InversionInProgress"

initial_dynamic :: BaseTypes.Environ -> Dynamic
initial_dynamic environ = Dynamic
    { state_controls = initial_controls
    , state_control_functions = mempty
    , state_control_merge_defaults = initial_control_merge_defaults
    , state_pitches = Map.empty
    , state_pitch = mempty
    , state_environ = environ
    , state_warp = Score.id_warp
    , state_scopes = empty_scopes
    , state_instrument_aliases = mempty
    , state_control_damage = mempty
    , state_under_invert = id
    , state_inversion = NotInverted
    , state_pitch_map = Nothing
    , state_note_track = Nothing
    , state_stack = Stack.empty
    , state_mode = Normal
    }

-- | Strip out fields that I don't need to remember in a TrackDynamic.
--
-- If I don't do this, I get a memory leak.  Presumably the cause is that
-- 'state_pitch_map' has an unevaluated pitch derivation, which in turn
-- somehow retains the previous derivation, and then the previous, and so on.
-- This makes each derivation leak more space.
strip_dynamic :: Dynamic -> Dynamic
strip_dynamic dyn = dyn
    { state_pitch_map = Nothing
    }

-- | Initial control environment.
initial_controls :: Score.ControlMap
initial_controls = Map.fromList
    [ (Controls.dynamic, Score.untyped (Signal.constant default_dynamic))
    ]

initial_control_merge_defaults :: Map Score.Control (Merger Signal.Control)
initial_control_merge_defaults =
    Map.fromList [(c, merge_add) | c <- Controls.additive_controls]

-- | A default dynamic that's not 0 is useful because otherwise you have to add
-- dyn to everything.  Since control tracks multiply by default, 1 is the most
-- convenient value.
default_dynamic :: Signal.Y
default_dynamic = 1

instance Pretty.Pretty Dynamic where
    format (Dynamic controls cfuncs cmerge pitches pitch environ warp scopes
            aliases damage _under_invert inversion pitch_map
            note_track stack mode) =
        Pretty.record "Dynamic"
            [ ("controls", Pretty.format controls)
            , ("control_functions", Pretty.format cfuncs)
            , ("control_merge_defaults", Pretty.format cmerge)
            , ("pitches", Pretty.format pitches)
            , ("pitch", Pretty.format pitch)
            , ("environ", Pretty.format environ)
            , ("warp", Pretty.format warp)
            , ("scopes", Pretty.format scopes)
            , ("instrument_aliases", Pretty.format aliases)
            , ("damage", Pretty.format damage)
            , ("inversion", Pretty.format inversion)
            , ("pitch_map", Pretty.format pitch_map)
            , ("note_track", Pretty.format note_track)
            , ("stack", Pretty.format stack)
            , ("mode", Pretty.format mode)
            ]

instance DeepSeq.NFData Dynamic where
    rnf (Dynamic controls cfuncs cmerge pitches pitch environ warp _scopes
            aliases damage _under_invert _inversion pitch_map
            note_track stack _mode) =
        rnf controls `seq` rnf cfuncs `seq` rnf cmerge `seq` rnf pitches
        `seq` rnf pitch `seq` rnf environ `seq` rnf warp `seq` rnf aliases
        `seq` rnf damage `seq` rnf pitch_map `seq` rnf note_track
        `seq` rnf stack

-- ** scope

-- | This is the library of built-in calls.  The 'prio_library' Scope fields
-- are imported from this.
data Library = Library {
    lib_note :: !(CallMaps Note)
    , lib_control :: !(CallMaps Control)
    , lib_pitch :: !(CallMaps Pitch)
    , lib_val :: ![LookupCall ValCall]
    -- | A set of instrument aliases to apply to 'state_instrument_aliases'
    -- at the beginning of derivation.  This doesn't really fit here since
    -- it's not a CallMap, but it's convenient to return from the ky loading
    -- function, and other users can leave it empty.
    , lib_instrument_aliases :: !(Map Score.Instrument Score.Instrument)
    }

instance Monoid Library where
    mempty = Library mempty mempty mempty mempty mempty
    mappend (Library note1 control1 pitch1 val1 aliases1)
            (Library note2 control2 pitch2 val2 aliases2) =
        Library (note1<>note2) (control1<>control2) (pitch1<>pitch2)
            (val1<>val2) (aliases2<>aliases1)
            -- Put aliases2 first so it takes priority.

instance Show Library where show _ = "((Library))"
instance Pretty.Pretty Library where
    format (Library note control pitch val aliases) = Pretty.record "Library"
        [ ("note", Pretty.format note)
        , ("control", Pretty.format control)
        , ("pitch", Pretty.format pitch)
        , ("val", Pretty.format val)
        , ("aliases", Pretty.format aliases)
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
    , scopes_val :: !(ScopePriority ValCall)
    }

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
    scope_note :: !(ScopePriority note)
    , scope_control :: !(ScopePriority control)
    , scope_pitch :: !(ScopePriority pitch)
    }

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

{- | An instrument or scale may put calls into scope.  If that instrument
    or scale is replaced with another, the old calls must be replaced with the
    new ones.

    Priority is determined by 'get_scopes', which returns them in the fields'
    declaration order.

    The reason this can't be accomplished just by arranging imports in the
    right order is that when an instrument or scale comes into scope, it needs
    to replace existing instrument or scale calls.  To do that, I need to keep
    each category separate.  Also, this way I can import the ky file once at
    the toplevel, and it will still override library imported calls.
-}
newtype ScopePriority call =
    ScopePriority (Map CallPriority [LookupCall call])
    deriving (Pretty.Pretty)

instance Monoid (ScopePriority call) where
    mempty = ScopePriority mempty
    mappend (ScopePriority a) (ScopePriority b) =
        ScopePriority (Util.Map.mappend a b)

data CallPriority =
    -- | Override calls shadow all others.  They're useful when you want to
    -- prevent instruments from overriding calls, which the lilypond deriver
    -- needs to do.
    PrioOverride
    -- | These are instrument-specific calls implicitly imported by note
    -- tracks.
    | PrioInstrument
    -- | Block calls are local definitions, so they should override library
    -- calls, but are still below instrument calls.  Otherwise, it's easy to
    -- define a block that shadows a drum stroke and get confused.
    -- TODO there could be a mechanism to set PrioOverride in case I actually
    -- do want to shadow an instrument call.
    | PrioBlock
    -- | This is for value calls introduced by a scale.  They are implicitly
    -- imported by pitch tracks.
    | PrioScale
    -- | Calls imported from the 'Library'.
    | PrioLibrary
    deriving (Show, Eq, Ord)

instance Pretty.Pretty CallPriority where pretty = showt

scope_priority :: [(CallPriority, [LookupCall call])] -> ScopePriority call
scope_priority = ScopePriority . Map.fromList

lookup_priority :: CallPriority -> ScopePriority call -> [LookupCall call]
lookup_priority prio (ScopePriority scopes) = Map.findWithDefault [] prio scopes

add_priority :: CallPriority -> LookupCall call -> ScopePriority call
    -> ScopePriority call
add_priority prio call (ScopePriority scopes) =
    ScopePriority $ Map.alter (Just . maybe [call] (call:)) prio scopes

replace_priority :: CallPriority -> [LookupCall call] -> ScopePriority call
    -> ScopePriority call
replace_priority prio calls (ScopePriority scopes) =
    ScopePriority $ Map.insert prio calls scopes

-- | This is like 'Call', but with only documentation.  (name, CallDoc)
data DocumentedCall = DocumentedCall !CallName !CallDoc

extract_doc :: Call d -> DocumentedCall
extract_doc call = DocumentedCall (call_name call) (call_doc call)

extract_val_doc :: ValCall -> DocumentedCall
extract_val_doc vcall = DocumentedCall (vcall_name vcall) (vcall_doc vcall)

-- ** lookup

lookup_val_call :: BaseTypes.CallId -> Deriver (Maybe ValCall)
lookup_val_call = lookup_with scopes_val

lookup_with :: (Scopes -> ScopePriority call)
    -> (BaseTypes.CallId -> Deriver (Maybe call))
lookup_with get call_id = do
    lookups <- get_scopes get
    lookup_scopes lookups call_id

get_scopes :: (Scopes -> ScopePriority call) -> Deriver [LookupCall call]
get_scopes get = do
    ScopePriority scopes <- gets $ get . state_scopes . state_dynamic
    return $ concat $ Map.elems scopes

-- | Convert a list of lookups into a single lookup by returning the first
-- one to yield a Just.
lookup_scopes :: [LookupCall call] -> (BaseTypes.CallId -> Deriver (Maybe call))
lookup_scopes lookups call_id =
    firstJusts $ map (flip lookup_call call_id) lookups

lookup_call :: LookupCall call -> BaseTypes.CallId -> Deriver (Maybe call)
lookup_call (LookupMap calls) call_id = return $ Map.lookup call_id calls
lookup_call (LookupPattern _ _ lookup) call_id = lookup call_id

-- ** mode

-- | Derivation can run in a few distinct modes.
data Mode =
    -- | Standard derivation.
    Normal
    -- | This indicates that I'm running the deriver just to find out its
    -- duration.  There's a hack in "Derive.Eval" that will fill in
    -- 'collect_score_duration' when it sees this mode.  More detail in
    -- 'CallDuration'.
    | ScoreDurationQuery | RealDurationQuery
    -- | Emit events intended for the lilypond backend.  Calls that have
    -- corresponding staff notation (e.g. trills) emit special events with
    -- attached lilypond code in this mode.
    | Lilypond !Lilypond.Types.Config
    deriving (Show)

instance Pretty.Pretty Mode where
    format (Lilypond config) = "Lilypond" Pretty.<+> Pretty.format config
    format mode = Pretty.text (showt mode)

-- * Constant

-- | Values that don't change during one derive run.
data Constant = Constant {
    state_ui :: !Ui.State
    , state_library :: !Library
    -- | Global map of signal mergers.  Unlike calls, this is static.
    , state_mergers :: !(Map BaseTypes.CallId (Merger Signal.Control))
    , state_pitch_mergers :: !(Map BaseTypes.CallId (Merger PSignal.PSignal))
    , state_lookup_scale :: !LookupScale
    -- | Get the calls and environ that should be in scope with a certain
    -- instrument.  The environ is merged with the environ in effect.
    , state_lookup_instrument :: !(Score.Instrument -> Maybe Instrument)
    -- | Cache from the last derivation.
    , state_cache :: !Cache
    , state_score_damage :: !ScoreDamage
    }

initial_constant :: Ui.State -> Library -> LookupScale
    -> (Score.Instrument -> Maybe Instrument) -> Cache -> ScoreDamage
    -> Constant
initial_constant ui_state library lookup_scale lookup_inst cache score_damage =
    Constant
        { state_ui = ui_state
        , state_library = library
        , state_mergers = mergers
        , state_pitch_mergers = pitch_mergers
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
    inst_calls :: !InstrumentCalls
    -- | Merge this with the 'state_environ' when the instrument comes into
    -- scope.
    , inst_environ :: !BaseTypes.Environ
    -- | Like 'inst_environ', merge these controls.
    , inst_controls :: !Score.ControlValMap
    -- | This is a list of the attributes that the instrument understands, in
    -- order of priority.  It corresponds to 'Perform.Midi.Patch.AttributeMap'.
    , inst_attributes :: ![Attrs.Attributes]
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

instance Monoid InstrumentCalls where
    mempty = InstrumentCalls mempty mempty mempty
    mappend (InstrumentCalls a1 b1 c1) (InstrumentCalls a2 b2 c2) =
        InstrumentCalls (a1<>a2) (b1<>b2) (c1<>c2)


-- ** control

-- | How to merge a control into 'Dynamic'.
data Merge sig = DefaultMerge -- ^ Apply the default merge for this control.
    | Merge !(Merger sig) -- ^ Merge with a specific operator.
    deriving (Show)

instance Pretty.Pretty (Merge a) where pretty = showt
instance DeepSeq.NFData (Merge a) where rnf _ = ()

-- | Combine two signals.  The element should be an identity, like mempty.
-- ControlMod uses it to avoid affecting signal outside of the modified range.
-- The merge function is not obliged to be associative, so this isn't actually
-- a monoid.  TODO it's all the fault of 'merge_scale'... do I lose something
-- important with associativity?
data Merger sig =
    Merger !Text !(sig -> sig -> sig) !sig -- ^ name merge identity
    | Set -- ^ Replace the existing signal.

-- It's not really a 'BaseTypes.Val', so this is a bit wrong for ShowVal.  But
-- I want to express that this is meant to be valid syntax for the track title.
instance ShowVal.ShowVal (Merger a) where
    show_val Set = "set"
    show_val (Merger name _ _) = name
instance Pretty.Pretty (Merger a) where pretty = ShowVal.show_val
instance Show (Merger a) where
    show merger = "((Merger " ++ untxt (ShowVal.show_val merger) ++ "))"
instance DeepSeq.NFData (Merger a) where
    rnf _ = ()

-- *** control ops

-- | The built-in set of control Mergers.
mergers :: Map BaseTypes.CallId (Merger Signal.Control)
mergers = Map.fromList $ map to_pair
    [ Set, merge_add, merge_sub, merge_mul, merge_scale
    , Merger "max" Signal.sig_max (Signal.constant (-2^32))
    , Merger "min" Signal.sig_min (Signal.constant (2^32))
    -- flip ensures that when samples collide, the later track wins.
    , Merger "interleave" (flip Signal.interleave) mempty
    ]
    where to_pair merger = (BaseTypes.Symbol (ShowVal.show_val merger), merger)

merge_add, merge_sub, merge_mul  :: Merger Signal.Control
merge_add = Merger "add" Signal.sig_add (Signal.constant 0)
merge_sub = Merger "sub" Signal.sig_subtract (Signal.constant 0)
merge_mul = Merger "mul" Signal.sig_multiply (Signal.constant 1)

-- | Unlike the rest, this one is not associative.
merge_scale :: Merger Signal.Control
merge_scale = Merger "scale" Signal.sig_scale (Signal.constant 0)

pitch_mergers :: Map BaseTypes.CallId (Merger PSignal.PSignal)
pitch_mergers = Map.fromList $ map to_pair
    [ Set, Merger "interleave" (flip PSignal.interleave) mempty
    ]
    where to_pair merger = (BaseTypes.Symbol (ShowVal.show_val merger), merger)


-- * Collect

-- | These are things that collect throughout derivation, and are cached in
-- addition to the derived values.  Effectively they are extra return values,
-- which are combined with mappend.  So this is the WriterT part of 'State'.
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
    , collect_score_duration :: !(CallDuration ScoreTime)
    , collect_real_duration :: !(CallDuration RealTime)
    }

-- | These are fragments of a signal, which will be later collected into
-- 'collect_track_signals'.  This is part of a complicated mechanism to
-- evaluate TrackSignals only once.  When the sliced fragments of a track are
-- evaluated, they collect signal fragments.  When the track is fully
-- evaluated, they are sorted and merged into 'collect_track_signals'.
-- If the track is then evaluated again, the monoid instance will discard the
-- duplicate.
--
-- The signal fragments are indexed by the slice position.  Since
-- 'Signal.merge' makes the earlier signals win in case of overlaps, this
-- ensures a trimmed earlier fragment won't replace a more complete later one.
type SignalFragments = Map (BlockId, TrackId) (Map TrackTime Signal.Control)

instance Pretty.Pretty Collect where
    format (Collect warp_map tsigs frags trackdyn trackdyn_inv deps cache
            integrated cmods call_dur call_end) =
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
            , ("call duration", Pretty.format call_dur)
            , ("call end", Pretty.format call_end)
            ]

instance Monoid Collect where
    mempty = Collect mempty mempty mempty mempty mempty mempty mempty mempty
        mempty mempty mempty
    mappend (Collect warps1 tsigs1 frags1 trackdyn1 trackdyn_inv1 deps1 cache1
                integrated1 cmods1 cdur1 cend1)
            (Collect warps2 tsigs2 frags2 trackdyn2 trackdyn_inv2 deps2 cache2
                integrated2 cmods2 cdur2 cend2) =
        Collect (warps1 <> warps2)
            (tsigs1 <> tsigs2) (Map.unionWith (<>) frags1 frags2)
            (trackdyn1 <> trackdyn2) (trackdyn_inv1 <> trackdyn_inv2)
            (deps1 <> deps2) (cache1 <> cache2) (integrated1 <> integrated2)
            (cmods1 <> cmods2) (cdur1 <> cdur2) (cend1 <> cend2)

instance DeepSeq.NFData Collect where
    rnf (Collect warp_map frags tsigs track_dyn track_dyn_inv local_dep cache
            integrated _cmods _cdur _cend) =
        rnf warp_map `seq` rnf frags `seq` rnf tsigs `seq` rnf track_dyn
        `seq` rnf track_dyn_inv `seq` rnf local_dep `seq` rnf cache
        `seq` rnf integrated

-- | This is a hack so a call on a control track can modify other controls.
-- The motivating case is pitch ornaments that also want to affect the
-- dynamics.  The modifications are a secondary return value from control
-- and pitch calls.  The track deriver will extract them and merge them into
-- the dynamic environment.  [NOTE control-modification]
data ControlMod =
    ControlMod !Score.Control !Signal.Control !(Merger Signal.Control)
    deriving (Show)

instance Pretty.Pretty ControlMod where
    format (ControlMod control signal merge) =
        Pretty.constructor "ControlMod"
            [Pretty.format control, Pretty.format signal, Pretty.format merge]

data Integrated = Integrated {
    -- BlockId for a block integration, TrackId for a track integration.
    integrated_source :: !(Either BlockId TrackId)
    , integrated_events :: !(Stream.Stream Score.Event)
    } deriving (Show)

instance Pretty.Pretty Integrated where
    format (Integrated source events) = Pretty.record "Integrated"
        [ ("source", Pretty.format source)
        , ("events", Pretty.format events)
        ]

instance DeepSeq.NFData Integrated where
    rnf (Integrated source events) = rnf source `seq` rnf events

{- | Snapshots of the environ at each track.  This is used by the Cmd layer to
    figure out what the scale and instrument are for a given track.

    Originally this was a map from Stacks to Environ (and only the changed
    parts).  The idea was that I could walk up the stack to find the Environ
    value in scope at a given point, and given Stack.Region, could even get
    e.g. per event instruments.  Unfortunately, while it's easy to do that on
    the Derive side, it seems really complicated and somewhat expensive to try
    to retrace a complete stack on every cmd.  Since this implementation
    doesn't store the entire stack, a track with a different instrument at
    different times will wind up with the last one.

    This is a much simpler solution which will hopefully work well enough in
    practice.

    NOTE [record-track-dynamics] One complication is that when I get controls
    from sliced tracks, the controls are also sliced.  But I need the environ
    from the inverted version of the track so the common case of [>i, *scale]
    gets the correct scale.  So I record TrackDynamic for both inverted and non
    inverted tracks and prefer the inverted tracks, but take controls from the
    non-inverted versions.
-}
type TrackDynamic = Map (BlockId, TrackId) Dynamic

{- | This is the logical duration of a call.  This may be different from its
    actual duration (which is to say, the end time of the last event it emits).
    Also, while most calls adjust their duration to the duration of the event
    they are called from, some of them have their own intrinsic duration.  For
    example, a block call may stretch to its calling event's duration, but it
    also has its own duration that is used to align the block's end, or to
    sequence blocks.

    Since the call duration is sometimes used to place the call in the first
    place (e.g. to align its end), I want to evaluate the minimum amount
    necessary to find the duration.  The implementation is that each generator
    call has a 'gfunc_score_duration' field.  When "Derive.Eval" is evaluating
    a generator call, if it sees that 'state_mode' is 'ScoreDurationQuery',
    instead of calling 'gfunc_f', it will call gfunc_score_duration and return
    the result via 'collect_score_duration'.  You shouldn't stick your fingers
    into this machinery, but instead use @Derive.get_call_duration@ to do the
    gefingerpoken for you.

    I'm not very happy with this implementation, but I tried several approaches
    and this is the only one that worked.  Historical details are in
    NOTE [call-duration].
-}
data CallDuration a = Unknown | CallDuration !a
    deriving (Eq, Show)

instance Show a => Pretty.Pretty (CallDuration a) where pretty = showt

-- I think it would be more correct to take the stack depth, and pick the one
-- with the shallower stack, and then the max.  But it's more expensive and
-- picking the second one seems to work.
instance Monoid (CallDuration a) where
    mempty = Unknown
    mappend Unknown a = a
    mappend a Unknown = a
    mappend _ a = a

-- ** calls

data LookupCall call =
    LookupMap !(Map BaseTypes.CallId call)
    -- | Text description of the CallIds accepted.  The function is in Deriver
    -- because some calls want to look at the state to know if the CallId is
    -- valid, e.g. block calls.
    | LookupPattern !Text !DocumentedCall
        !(BaseTypes.CallId -> Deriver (Maybe call))

instance Pretty.Pretty (LookupCall call) where
    format c = case c of
        LookupMap calls -> "Map: " <> Pretty.format (Map.keys calls)
        LookupPattern doc _ _ -> "Pattern: " <> Pretty.text doc

{- | Previously, a single Call contained both generator and transformer.
    This turned out to not be flexible enough, because an instrument that
    wanted to override a generator meant you couldn't use a transformer that
    happened to have the same name.  However, there are a number of calls that
    want both generator and transformer versions, and it's convenient to be
    able to deal with those together.
-}
data CallMaps d = CallMaps ![LookupCall (Generator d)]
    ![LookupCall (Transformer d)]

instance Monoid (CallMaps d) where
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
call_map :: [(BaseTypes.CallId, call)] -> [LookupCall call]
call_map = map (LookupMap . uncurry Map.singleton)

-- | Bundle generators and transformers up together for convenience.
call_maps :: [(BaseTypes.CallId, Generator d)]
    -> [(BaseTypes.CallId, Transformer d)] -> CallMaps d
call_maps generators transformers =
    CallMaps (call_map generators) (call_map transformers)

generator_call_map :: [(BaseTypes.CallId, Generator d)] -> CallMaps d
generator_call_map generators = call_maps generators []

transformer_call_map :: [(BaseTypes.CallId, Transformer d)] -> CallMaps d
transformer_call_map = call_maps []

-- | Data passed to a 'Call'.
data PassedArgs val = PassedArgs {
    passed_vals :: ![BaseTypes.Val]
    -- | Used by "Derive.Sig" to look for default arg values in the
    -- environment.  This is technically redundant since a call should know its
    -- own name, but it turns out to be inconvenient to pass the name to all of
    -- those functions.
    , passed_call_name :: !CallName
    , passed_ctx :: !(Context val)
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
data Context val = Context {
    -- The below is not used at all for val calls, and the events are not
    -- used for transform calls.  It might be cleaner to split those out, but
    -- too much bother.

    -- | Hack so control calls have access to the previous sample, since
    -- they tend to want to interpolate from that value.
    --
    -- This used to be the only way a call could get the previous value, but
    -- now if the prev val is unset, then "Derive.Args.prev_val" will evaluate
    -- 'ctx_prev_events'.  But checking ctx_prev_val is cheaper, so I'll keep
    -- it around.  The evaluation fallback has to exist because track slicing
    -- may snip off the previous event.
    --
    -- See NOTE [prev-val] in "Derive.Args" for details.
    ctx_prev_val :: !(Maybe val)

    , ctx_event :: !Event.Event
    , ctx_prev_events :: ![Event.Event]
    , ctx_next_events :: ![Event.Event]

    -- | The extent of the note past its duration.  Since notes have decay,
    -- its important to capture control for that.  Normally this is the next
    -- event's start.  If there's no next event because it's the last event of
    -- the block, this is the block end, otherwise if there's no next event
    -- because it was sliced off, this is where that event would have started.
    --
    -- This is the same as the first element of 'ctx_next_events' except of
    -- course it has a value even when there is no next event.
    , ctx_event_end :: !ScoreTime
    -- | From 'TrackTree.track_shifted'.
    , ctx_track_shifted :: !TrackTime

    -- | The track tree below note tracks.  Not given for control tracks.
    -- TODO should this be Either with ctx_sub_events?  I don't think I ever
    -- need both set.
    , ctx_sub_tracks :: !TrackTree.EventsTree
    -- | If present, 'Derive.Sub.sub_events' will directly return these sub
    -- events instead of slicing sub-tracks.  Track evaluation will never set
    -- this, but calls can set this to reapply a note parent.  It should
    -- be 'Derive.Sub.Event's, but isn't to avoid circular imports.
    , ctx_sub_events :: !(Maybe [[(ScoreTime, ScoreTime, NoteDeriver)]])
    -- | This is needed by val calls that want to evaluate events around them.
    -- Since val calls are the same on all track types, they need to know
    -- explicitly what the track type is to evaluate events on it.
    , ctx_track_type :: !(Maybe ParseTitle.Type)
    }

-- | Range of the event in TrackTime.
ctx_track_range :: Context a -> (TrackTime, TrackTime)
ctx_track_range info = (shifted, shifted + ctx_event_end info)
    where shifted = ctx_track_shifted info

instance Pretty.Pretty val => Pretty.Pretty (Context val) where
    format (Context prev_val event prev_events next_events event_end
            track_range sub_tracks sub_events track_type) =
        Pretty.record "Context"
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

coerce_context :: Context a -> Context b
coerce_context ctx = ctx { ctx_prev_val = Nothing }

-- | Transformer calls don't necessarily apply to any particular event, and
-- neither do generators for that matter.
dummy_context :: ScoreTime -> ScoreTime -> Text -> Context a
dummy_context start dur text = Context
    { ctx_prev_val = Nothing
    , ctx_event = Event.event start dur text
    , ctx_prev_events = []
    , ctx_next_events = []
    , ctx_event_end = start + dur
    , ctx_track_shifted = 0
    , ctx_sub_tracks = []
    , ctx_sub_events = Nothing
    , ctx_track_type = Nothing
    }

-- | Taggable the polymorphic part of the Context so it can be given to
-- a 'ValCall'.  Otherwise, ValCall would have to be polymorphic too,
-- which means it would hard to write generic ones.
tag_context :: Taggable a => Context a -> Context Tagged
tag_context ctx = ctx { ctx_prev_val = to_tagged <$> ctx_prev_val ctx }

-- | A Call will be called as either a generator or a transformer, depending on
-- its position.  A call at the end of a compose pipeline will be called as
-- a generator while ones composed with it will be called as transformers, so
-- in @a | b@, @a@ is a transformer and @b@ is a generator.
--
-- More details on this strange setup are in the "Derive.Call" haddock.
data Call func = Call {
    call_name :: !CallName
    , call_doc :: !CallDoc
    , call_func :: !func
    }
type Generator d = Call (GeneratorFunc d)
type Transformer d = Call (TransformerF d)

instance Show (Call derived) where
    show (Call name _ _) = "((Call " <> show name <> "))"
instance Pretty.Pretty (Call derived) where
    pretty (Call (CallName name) _ _) = name

{- | Each call has an intrinsic name.  Since call IDs may be rebound
    dynamically, each call has its own name so that error msgs are unambiguous.
    It's also used along with 'ArgName' for argument defaulting, so if you want
    that to work it should be short and parseable by 'Derive.Parse.p_symbol'.
    The name is not necessarily unique, and in fact may be intentionally
    non-unique to share defaults with another.

    The documentation for all calls that differ only in name are grouped
    together, so it's easier to read if small modifications are reflected in
    the name only.  If you put invalid identifiers in the name, it can't be
    used to set default arguments.
-}
newtype CallName = CallName Text
    deriving (Eq, Ord, Show, Pretty.Pretty, String.IsString)

-- | Each call argument has its own name, which is used for documentation as
-- well as argument defaulting, as documented in "Derive.Sig".
newtype ArgName = ArgName Text
    deriving (Eq, Ord, Show, Pretty.Pretty, String.IsString)

sym_to_call_name :: BaseTypes.Symbol -> CallName
sym_to_call_name (BaseTypes.Symbol sym) = CallName sym

sym_to_arg_name :: BaseTypes.Symbol -> ArgName
sym_to_arg_name (BaseTypes.Symbol sym) = ArgName sym

-- | Documentation for a call.  The documentation is in markdown format, except
-- that a single newline will be replaced with two, so a single \n is enough
-- to start a new paragraph.  Also, single quotes are turned into links as per
-- "Util.TextUtil".haddockUrl.
data CallDoc = CallDoc {
    cdoc_module :: !Module.Module
    , cdoc_tags :: !Tags.Tags
    , cdoc_doc :: !Doc.Doc
    , cdoc_args :: ![ArgDoc]
    } deriving (Eq, Ord, Show)

data ArgDoc = ArgDoc {
    arg_name :: !ArgName
    , arg_type :: !ValType.Type
    , arg_parser :: !ArgParser
    , arg_environ_default :: !EnvironDefault
    , arg_doc :: !Doc.Doc
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
type WithArgDoc f = (f, [ArgDoc])

-- ** make calls

data GeneratorFunc d = GeneratorFunc {
    gfunc_f :: !(GeneratorF d)
    -- | This gets the logical duration of this call.  'CallDuration' has
    -- details.
    , gfunc_score_duration
        :: !(PassedArgs d -> Deriver (CallDuration ScoreTime))
    , gfunc_real_duration :: !(PassedArgs d -> Deriver (CallDuration RealTime))
    }

type GeneratorF d = PassedArgs d -> Deriver (Stream.Stream d)

generator_func :: (PassedArgs d -> Deriver (Stream.Stream d)) -> GeneratorFunc d
generator_func f = GeneratorFunc {
    gfunc_f = f
    , gfunc_score_duration = default_score_duration
    , gfunc_real_duration = default_real_duration
    }

-- | Most calls have the same logical duration as their event.
default_score_duration :: PassedArgs d -> Deriver (CallDuration ScoreTime)
default_score_duration =
    return . CallDuration . Event.duration . ctx_event . passed_ctx

default_real_duration :: PassedArgs d -> Deriver (CallDuration RealTime)
default_real_duration args = CallDuration <$>
    score_to_real (Event.duration $ ctx_event $ passed_ctx args)

-- | args -> deriver -> deriver
type TransformerF d = PassedArgs d -> Deriver (Stream.Stream d)
    -> Deriver (Stream.Stream d)

make_call :: Module.Module -> CallName -> Tags.Tags -> Doc.Doc
    -> WithArgDoc func -> Call func
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
generator :: Module.Module -> CallName -> Tags.Tags -> Doc.Doc
    -> WithArgDoc (GeneratorF d) -> Generator d
generator module_ name tags doc (func, arg_docs) =
    make_call module_ name tags doc (generator_func func, arg_docs)

-- | Make a generator from a function which returns events in sorted order.
-- TODO this just trusts that the events will be sorted.  Is there a safer way?
generator_events :: Module.Module -> CallName -> Tags.Tags -> Doc.Doc
    -> WithArgDoc (PassedArgs d -> Deriver [d]) -> Generator d
generator_events module_ name tags doc (func, arg_docs) =
    generator module_ name tags doc
        ((Stream.from_sorted_events <$>) . func, arg_docs)

-- | Since Signals themselves are collections, there's little reason for a
-- signal generator to return a Stream of events.  So wrap the generator result
-- in a Stream singleton.
--
-- TODO call this signal_generator?
generator1 :: Module.Module -> CallName -> Tags.Tags -> Doc.Doc
    -> WithArgDoc (PassedArgs d -> Deriver d) -> Generator d
generator1 module_ name tags doc (func, arg_docs) =
    generator module_ name tags doc
        ((Stream.from_event <$>) . func, arg_docs)

-- | Set the 'gfunc_score_duration' field to get ScoreTime CallDuration.
with_score_duration :: (PassedArgs d -> Deriver (CallDuration ScoreTime))
    -> Generator d -> Generator d
with_score_duration get call = call
    { call_func = (call_func call) { gfunc_score_duration = get } }

with_real_duration :: (PassedArgs d -> Deriver (CallDuration RealTime))
    -> Generator d -> Generator d
with_real_duration get call = call
    { call_func = (call_func call) { gfunc_real_duration = get } }

-- ** transformer

-- | Just 'make_call' with a more specific signature.
transformer :: Module.Module -> CallName -> Tags.Tags -> Doc.Doc
    -> WithArgDoc (TransformerF d) -> Transformer d
transformer = make_call

-- ** val

data ValCall = ValCall {
    vcall_name :: !CallName
    , vcall_doc :: !CallDoc
    , vcall_call :: PassedArgs Tagged -> Deriver BaseTypes.Val
    }

instance Show ValCall where
    show (ValCall name _ _) = "((ValCall " ++ show name ++ "))"

make_val_call :: Module.Module -> CallName -> Tags.Tags -> Doc.Doc
    -> WithArgDoc (PassedArgs Tagged -> Deriver BaseTypes.Val) -> ValCall
make_val_call module_ name tags doc (call, arg_docs) = ValCall
    { vcall_name = name
    , vcall_doc = CallDoc
        { cdoc_module = module_
        , cdoc_tags = tags
        , cdoc_doc = doc
        , cdoc_args = arg_docs
        }
    , vcall_call = call
    }


-- ** cache types

-- $cache_doc
-- The cache types are nominally exported from "Derive.Cache", but must be
-- defined here to avoid circular dependencies.

-- instead of a stack, this could be a tree of frames
newtype Cache = Cache (Map CacheKey Cached)
    deriving (Monoid, Pretty.Pretty, DeepSeq.NFData)
    -- The monoid instance winds up being a left-biased union.  This is ok
    -- because merged caches shouldn't overlap anyway.

cache_size :: Cache -> Int
cache_size (Cache c) = Map.size c

{- | Ideally, the cache would be keyed by all data that can affect derivation,
    which would mean all of 'Dynamic' and 'Threaded'.  Effectively a deriver is
    a function that takes 'State' as its input, and this would be memoizing
    that function.  But in practice, there's too much junk in there, so I have
    to do an approximation.

    The first approximation is the stack, which is a proxy for the things that
    are likely to affect derivation.  Different calls in the stack are likely
    to result in a different environment, or a different 'Stack.Region' likely
    means a different warp.  'Stack.Serial' attempts to ensure that multiple
    generators within a single event also have unique stacks.
-}
newtype CacheKey = CacheKey { key_stack :: Stack.Stack }
    deriving (Eq, Ord, Show, DeepSeq.NFData, Pretty.Pretty)

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
    | CachedPitch !(CallType PSignal.PSignal)

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
data CallType d = CallType !Collect !(Stream.Stream d)

instance DeepSeq.NFData d => DeepSeq.NFData (CallType d) where
    rnf (CallType collect events) = rnf collect `seq` rnf events

-- ** deps

newtype BlockDeps = BlockDeps (Set BlockId)
    deriving (Pretty.Pretty, Monoid, Show, Eq, DeepSeq.NFData)

-- ** damage

-- | Modified ranges in the score.
data ScoreDamage = ScoreDamage {
    -- | Damaged ranges in tracks.
    sdamage_tracks :: !(Map TrackId (Ranges.Ranges ScoreTime))
    -- | The blocks with damaged tracks.  Calls depend on blocks
    -- ('BlockDeps') rather than tracks, so it's convenient to keep the
    -- blocks here.  This is different than block damage because a damaged
    -- block will invalidate all caches below it, but a block with damaged
    -- tracks must be called but may still have valid caches within.
    , sdamage_track_blocks :: !(Set BlockId)
    -- | Blocks which are entirely damaged.
    , sdamage_blocks :: !(Set BlockId)
    } deriving (Eq, Show)

instance Monoid ScoreDamage where
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
    invalidate key cached
        | has_damage (key_stack key) = Invalid
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
    deriving (Pretty.Pretty, Monoid, Eq, Show, DeepSeq.NFData)

-- * util

score_to_real :: ScoreTime -> Deriver RealTime
score_to_real pos = do
    warp <- gets (state_warp . state_dynamic)
    return $ Score.warp_pos warp pos

real_to_score :: RealTime -> Deriver ScoreTime
real_to_score pos = do
    warp <- gets (state_warp . state_dynamic)
    return $ Score.unwarp_pos warp pos


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
    -- This is used by 'PSignal.apply_controls' to know when to reevaluate
    -- a given pitch.  Other controls can affect the pitch, but if they aren't
    -- in this set, the pitch won't be reevaluated when they change.
    , scale_transposers :: !(Set Score.Control)
    -- | Parse a Note into a Pitch.Pitch with scale degree and accidentals.
    , scale_read :: BaseTypes.Environ -> Pitch.Note
        -> Either BaseTypes.PitchError Pitch.Pitch
    , scale_show :: BaseTypes.Environ -> Pitch.Pitch
        -> Either BaseTypes.PitchError Pitch.Note
    -- | Bottom pitch of the scale, if there is one.  You can find the top
    -- pitch by transposing until you get OutOfRange.  TODO that's a dumb way,
    -- if I explicitly need the top I should just add it.
    , scale_bottom :: !Pitch.Pitch
    , scale_layout :: !Layout
    , scale_transpose :: !Transpose
    , scale_enharmonics :: !Enharmonics

    -- | Used by derivation.
    , scale_note_to_call :: !(Pitch.Note -> Maybe ValCall)

    -- | Used by note input.
    , scale_input_to_note :: !(BaseTypes.Environ -> Pitch.Input
        -> Either BaseTypes.PitchError Pitch.Note)
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
        -> Deriver (Either BaseTypes.PitchError Pitch.NoteNumber))

    -- | Documentation for all of the ValCalls that 'scale_note_to_call' can
    -- return.
    , scale_call_doc :: !DocumentedCall
    }

instance Pretty.Pretty Scale where
    pretty = pretty . scale_id

-- | A scale can configure itself by looking in the environment and by looking
-- up other scales.
newtype LookupScale = LookupScale (BaseTypes.Environ -> LookupScale
    -> Pitch.ScaleId -> Maybe (Either BaseTypes.PitchError Scale))
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
type Transpose = Transposition -> BaseTypes.Environ -> Pitch.Step -> Pitch.Pitch
    -> Either BaseTypes.PitchError Pitch.Pitch

data Transposition = Chromatic | Diatonic deriving (Show)

-- | Get the enharmonics of the note.  The given note is omitted, and the
-- enharmonics are in ascending order until they wrap around, so if you always
-- take the head of the list you will cycle through all of the enharmonics.
type Enharmonics = BaseTypes.Environ -> Pitch.Note
    -> Either BaseTypes.PitchError [Pitch.Note]

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

-- * merge

-- | Merge the events of the given derivers by their starting times.
d_merge :: [NoteDeriver] -> NoteDeriver
d_merge [] = mempty
d_merge [d] = d
d_merge derivers = mconcat <$> sequence derivers
    -- Previously, each deriver was run independently, and their Collects
    -- merged.  The theory was to allow their derivation to be interleaved
    -- on demand as the events themselves are interleaved.  However, profiling
    -- doesn't show a significant difference, and this way is simpler.

merge_logs :: Either Error (Stream.Stream a) -> [Log.Msg] -> Stream.Stream a
merge_logs result logs = case result of
    Right stream -> Stream.merge_logs logs stream
    Left err -> Stream.from_logs $ error_to_warn err : logs

error_to_warn :: Error -> Log.Msg
error_to_warn (Error call_stack stack val) =
    Log.msg_call_stack call_stack Log.Warn (Just stack)
        ("Error: " <> pretty val)

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
      'Either (Name, PSignal) (Name, ControlSignal, Merger).
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

{- NOTE [call-duration]
    This is containued from 'CallDuration'.

    Initially I used just Collect, and each deriver could put a CallDuration
    into Collect.  Unfortunately this means I have to run the whole deriver and
    evaluate all the notes.  I tried to exploit laziness a bit, but it's
    probably impossible.  In addition, merging durations is not correct, because
    the duration set by the block call should override the durations set by
    the events inside.

    Then I tried splitting Deriver into (DeriveM, CallDuration) and merging
    CallDurations in the Applicative instance.  This prevents evaluation, but
    stops propagating the CallDurations as soon as it hits a (>>=).  This turns
    out to be pretty much immediately, since an event call needs to parse text
    and look up a call, which can throw.  The underlying reason is that
    I actually do need to evaluate a certain amount of the deriver to figure out
    what kind of deriver it is.

    Since the things that have CallDuration are not so much any old deriver,
    but specifically calls, I tried putting a special field in 'Generator'
    calls, which turned out to work.
-}

{- NOTE [event-serial]
    Here are the notes leading up to the current implementation of
    'state_event_serial':

    Kilitan notes all have the same random seed, so randomizing start
    offsets moves them all the same amount.  Multiple notes generated by
    the same call should have unique stacks so they have have different
    seeds.
    . I could do it manually by having a ID number argument to the various
      create note calls, e.g. Call.note and callers.  This would rely on
      manually keeping the ID number.
    . Or automatically with a per-call serial number in Derive.Threaded.
      It would be ok to reset it as soon as the call changed, because then
      the stack has changed.  But anyone who makes a note, e.g. Call.note,
      will have to get and increment it.
    . Each note should have a unique stack, which means that multiple
      Score.Events created by a single call need something on the end to make
      them unique.  But how do I define a "single note"?  Even the "" call
      could create multiple Score.Events.  I could put the increment in
      the note call itself, but I need to make sure I reset the ID.
    . But where to reset?  All of Threaded is reset per-track, though perhaps
      it actually isn't.  But the reason to reset is to distinguish events
      with the same stack.  So it should be reset when the stack is popped,
      but I don't think that's an explicit thing, just using the old state.
      I could make Derive.local reset the serial on every old state, but that
      sounds slow as well as being in the wrong place.
    . If I reset on every call, then I will reset on block calls that make
      lots of notes, but it's ok because I'll reset again on each note call.
      So I can reset whenever I add a Stack.Call.
    . Actually that's no good, because when I call note multiple times it adds
      the same Stack.Call "note" each time.  Maybe I only reset when setting
      Region?  Of course any evaluation may be inverting, so it may add any
      number of new stack entries.  It's just that they will happen to be
      identical.
    . Really I want to say if I've seen this stack before, then append
      something to make it unique.  But that sounds really expensive.  Or
      really?  What if I had a set of seeds, and I check it for duplicates
      on each note?  Every event in the track eval could reset it, so it
      wouldn't get too large.  But how again is that different from just
      incrementing the serial in the default note call, and resetting
      in track eval?
    * Add serial field to Derive.Threaded.
    * Reset the seed in track eval, for each new UI event.
      . I put it in EvalTrack.derive_event_stream, but why not in
        derive_event_ctx?
      . One thing is that this only resets the serial for the whole
        expression, but I don't actually need that since the stack will have
        different Calls.  I'd want to reset per call.  But does per-expression
        hurt?  For per-call, I'd have to put it in Eval.eval_toplevel, and
        then it would need a way to tell if it was called from a track.
        . Well, since LPerf.control_vals has to pass 0 for the serial, using
          0 more often would make it more likely to be accurate.
      . What tests can I do to verify?
    * Include the serial in the seed calculation.
      . I need the seed when I look up cf-rnd-a, so it's in the note deriver?
        Yeah, Note.get_start_offset
      . So this means if I bump the serial at the end of
        Note.make_event_control_vals then it will be reflected in subsequent
        notes.
    * Tests to verify serial is reset for each event.
      . Or rather, that I get different randomization for each note in
        a single event, but randomization is not affected if a previous event
        changes.
-}
