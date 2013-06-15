-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | Main module for the deriver monad.

    TODO update Derive\/README, move it to doc\/, and link from here

    The convention is to prepend deriver names with @d_@, so if the deriver is
    normally implemented purely, a d_ version can be made simply by composing
    'return'.

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
module Derive.Deriver.Lib where
import Prelude hiding (error)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Track as Track
import qualified Derive.Deriver.Internal as Internal
import Derive.Deriver.Monad
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Lilypond.Types as Lilypond.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Types


-- * derive

-- This should probably be in Internal, but can't due to a circular dependency
-- with 'real'.

-- | Package up the results of a derivation.
--
-- NOTE TO SELF: Don't put bangs on this and then be surprised when the
-- laziness tests fail, you doofus.
data Result = Result {
    r_events :: Events
    , r_cache :: Cache
    , r_track_warps :: [TrackWarp.Collection]
    , r_track_signals :: Track.TrackSignals
    , r_track_dynamic :: TrackDynamic
    , r_integrated :: [Integrated]

    -- | The relevant parts of the final state should be extracted into the
    -- above fields, but returning the whole state can be useful for testing.
    , r_state :: State
    }

-- | Kick off a derivation.
--
-- The derivation state is quite involved, so there are a lot of arguments
-- here.
derive :: Constant -> Scope -> TrackLang.Environ -> Deriver a -> RunResult a
derive constant scope environ deriver =
    run state (with_initial_scope environ deriver)
    where state = initial_state scope environ constant

extract_result :: RunResult Events -> Result
extract_result (result, state, logs) = Result
    { r_events = merge_logs result logs
    , r_cache = collect_cache collect <> state_cache (state_constant state)
    , r_track_warps = TrackWarp.collections (collect_warp_map collect)
    , r_track_signals = collect_track_signals collect
    , r_track_dynamic = collect_track_dynamic collect
    , r_integrated = collect_integrated collect
    , r_state = state
    }
    where collect = state_collect state

-- | Given an environ, bring instrument and scale calls into scope.
with_initial_scope :: TrackLang.Environ -> Deriver d -> Deriver d
with_initial_scope env deriver = set_inst (set_scale deriver)
    where
    set_inst = case TrackLang.get_val Environ.instrument env of
        Right inst -> with_instrument inst
        _ -> id
    set_scale = case TrackLang.get_val Environ.scale env of
        Right sym -> \deriver -> do
            scale <- get_scale (TrackLang.sym_to_scale_id sym)
            with_scale scale deriver
        _ -> id


-- * errors

require :: String -> Maybe a -> Deriver a
require msg = maybe (throw $ "required: " ++ msg) return

require_right :: (err -> String) -> Either err a -> Deriver a
require_right fmt_err = either (throw . fmt_err) return

with_msg :: Text -> Deriver a -> Deriver a
with_msg msg = Internal.local $ \st ->
    st { state_log_context = msg : state_log_context st }

error_to_warn :: Error -> Log.Msg
error_to_warn (Error srcpos stack val) = Log.msg_srcpos srcpos Log.Warn
    (Just (Stack.to_strings stack)) ("Error: " ++ Pretty.pretty val)


-- * state access

get_stack :: Deriver Stack.Stack
get_stack = gets (state_stack . state_dynamic)

-- ** scale

-- | Lookup a scale_id or throw.
get_scale :: Pitch.ScaleId -> Deriver Scale
get_scale scale_id = maybe
    (throw $ "get_scale: unknown " ++ untxt (ShowVal.show_val scale_id))
    return =<< lookup_scale scale_id

lookup_scale :: Pitch.ScaleId -> Deriver (Maybe Scale)
lookup_scale scale_id = do
    lookup_scale <- gets (state_lookup_scale . state_constant)
    return $ lookup_scale scale_id


-- ** environment

lookup_val :: (TrackLang.Typecheck a) => TrackLang.ValName -> Deriver (Maybe a)
lookup_val name = do
    environ <- Internal.get_dynamic state_environ
    either throw return (TrackLang.checked_val name environ)

-- | Like 'lookup_val', but throw if the value isn't present.
get_val :: (TrackLang.Typecheck a) => TrackLang.ValName -> Deriver a
get_val name = do
    val <- lookup_val name
    maybe (throw $ "environ val not found: " ++ Pretty.pretty name) return val

is_lilypond_derive :: Deriver Bool
is_lilypond_derive = Maybe.isJust <$> lookup_lilypond_config

lookup_lilypond_config :: Deriver (Maybe Lilypond.Types.Config)
lookup_lilypond_config = gets (state_lilypond . state_constant)

-- | Set the given val dynamically within the given computation.  This is
-- analogous to a dynamic let.
--
-- There is intentionally no way to modify the environment via assignment.
-- It would introduce an order of execution dependency that would complicate
-- caching as well as have a confusing non-local effect.
--
-- This dispatches to 'with_scale' or 'with_instrument' if it's setting the
-- scale or instrument, so scale or instrument scopes are always set when scale
-- and instrument are.
with_val :: (TrackLang.Typecheck val) => TrackLang.ValName -> val
    -> Deriver a -> Deriver a
with_val name val deriver
    | name == Environ.scale, Just scale_id <- TrackLang.to_scale_id v = do
        scale <- get_scale scale_id
        with_scale scale deriver
    | name == Environ.instrument, Just inst <- TrackLang.from_val v =
        with_instrument inst deriver
    | otherwise = with_val_raw name val deriver
    where v = TrackLang.to_val val

-- | Like 'with_val', but don't set scopes for instrument and scale.
with_val_raw :: (TrackLang.Typecheck val) => TrackLang.ValName -> val
    -> Deriver a -> Deriver a
with_val_raw name val = Internal.localm $ \st -> do
    environ <- Internal.insert_environ name val (state_environ st)
    return $! st { state_environ = environ }

modify_val :: (TrackLang.Typecheck val) => TrackLang.ValName
    -> (Maybe val -> val) -> Deriver a -> Deriver a
modify_val name modify = Internal.localm $ \st -> do
    let env = state_environ st
    val <- modify <$> either throw return (TrackLang.checked_val name env)
    return $! st { state_environ =
        TrackLang.insert_val name (TrackLang.to_val val) env }

with_scale :: Scale -> Deriver d -> Deriver d
with_scale scale =
    with_val_raw Environ.scale (TrackLang.scale_id_to_sym (scale_id scale))
    . with_scope (\scope -> scope { scope_val = set (scope_val scope) })
    where
    set stype = stype { stype_scale = [scale_to_lookup scale] }

scale_to_lookup :: Scale -> LookupCall ValCall
scale_to_lookup scale =
    pattern_lookup name (scale_call_doc scale)
        (\call_id -> return $ scale_note_to_call scale (to_note call_id))
    where
    name = ShowVal.show_val (scale_id scale) <> ": " <> scale_pattern scale
    to_note (TrackLang.Symbol sym) = Pitch.Note sym

with_instrument :: Score.Instrument -> Deriver d -> Deriver d
with_instrument inst deriver = do
    lookup_inst <- gets (state_lookup_instrument . state_constant)
    let maybe_inst = lookup_inst inst
        calls = maybe (InstrumentCalls [] []) inst_calls maybe_inst
        environ = maybe mempty inst_environ maybe_inst
    with_val_raw Environ.instrument inst $
        with_scope (set_scope calls) $ with_environ environ deriver
    where
    -- Replace the calls in the instrument scope type.
    set_scope (InstrumentCalls notes vals) scope = scope
        { scope_val = set_inst vals (scope_val scope)
        , scope_note = set_inst notes (scope_note scope)
        }
    set_inst notes stype = stype { stype_instrument = notes }

-- | Merge the given environ into the environ in effect.
with_environ :: TrackLang.Environ -> Deriver a -> Deriver a
with_environ environ
    | TrackLang.null_environ environ = id
    | otherwise = Internal.local $ \st -> st
        { state_environ = environ <> state_environ st }


-- ** control

-- | Return an entire signal.  Remember, signals are in RealTime, so if you
-- want to index them in ScoreTime you will have to call 'real'.
get_control :: Score.Control -> Deriver (Maybe Score.TypedSignal)
get_control cont = Map.lookup cont <$> get_controls

get_controls :: Deriver Score.ControlMap
get_controls = Internal.get_dynamic state_controls

control_at :: Score.Control -> RealTime -> Deriver (Maybe Score.TypedVal)
control_at cont pos = do
    controls <- get_controls
    return $ fmap (Score.control_at pos) (Map.lookup cont controls)

untyped_control_at :: Score.Control -> RealTime -> Deriver (Maybe Signal.Y)
untyped_control_at cont = fmap (fmap Score.typed_val) . control_at cont

controls_at :: RealTime -> Deriver PitchSignal.Controls
controls_at pos = Score.controls_at pos <$> get_controls

with_control :: Score.Control -> Score.TypedSignal -> Deriver a -> Deriver a
with_control cont signal = Internal.local $ \st ->
    st { state_controls = Map.insert cont signal (state_controls st) }

-- | Modify an existing control.
--
-- If both signals are typed, the existing type wins over the relative
-- signal's type.  If one is untyped, the typed one wins.
with_relative_control :: Score.Control -> ControlOp -> Score.TypedSignal
    -> Deriver a -> Deriver a
with_relative_control cont (ControlOp _ op ident) signal deriver = do
    controls <- get_controls
    let old = Map.findWithDefault ident_sig cont controls
    with_control cont (apply old signal) deriver
    where
    apply old new = Score.Typed (Score.type_of old <> Score.type_of new)
        (op (Score.typed_val old) (Score.typed_val new))
    ident_sig = Score.Typed (Score.type_of signal) (Signal.constant ident)

with_added_control :: Score.Control -> Score.TypedSignal -> Deriver a
    -> Deriver a
with_added_control cont = with_relative_control cont op_add

with_multiplied_control :: Score.Control -> Score.TypedSignal -> Deriver a
    -> Deriver a
with_multiplied_control cont = with_relative_control cont op_mul

multiply_control :: Score.Control -> Signal.Y -> Deriver a -> Deriver a
multiply_control cont val
    | val == 1 = id
    | otherwise = with_multiplied_control cont
        (Score.untyped (Signal.constant val))

get_control_op :: TrackLang.CallId -> Deriver ControlOp
get_control_op c_op = do
    op_map <- gets (state_control_op_map . state_constant)
    maybe (throw ("unknown control op: " ++ show c_op)) return
        (Map.lookup c_op op_map)

modify_control :: ControlOp -> Score.Control -> Signal.Control -> Deriver ()
modify_control op control signal = Internal.modify_collect $ \collect ->
    collect { collect_control_modifications =
        ControlModification control signal op
            : collect_control_modifications collect }

-- | Apply the collected control modifications to the given deriver and clear
-- them out.
apply_control_modifications :: Deriver a -> Deriver a
apply_control_modifications deriver = do
    mods <- gets (collect_control_modifications . state_collect)
    Internal.modify_collect $ \collect ->
        collect { collect_control_modifications = [] }
    foldr ($) deriver (map apply mods)
    where
    apply (ControlModification control signal op) =
        with_relative_control control op (Score.untyped signal)

-- ** pitch

-- | The pitch at the given time.  The transposition controls have not been
-- applied since that is supposed to be done once only when the event is
-- generated.
--
-- The scenario is a call that generates a note based on the current pitch.
-- If 'pitch_at' applied the transposition, the new note would have to remove
-- the transposition signals so they don't get applied again at performance
-- conversion.
pitch_at :: RealTime -> Deriver (Maybe PitchSignal.Pitch)
pitch_at pos = PitchSignal.at pos <$> Internal.get_dynamic state_pitch

named_pitch_at :: Score.Control -> RealTime
    -> Deriver (Maybe PitchSignal.Pitch)
named_pitch_at name pos = do
    psig <- get_named_pitch name
    return $ maybe Nothing (PitchSignal.at pos) psig

-- | Unlike 'pitch_at', the transposition has already been applied, because you
-- can't transpose any further once you have a NoteNumber.
nn_at :: RealTime -> Deriver (Maybe Pitch.NoteNumber)
nn_at pos = do
    controls <- controls_at pos
    justm (pitch_at pos) $ \pitch -> do
    logged_pitch_nn ("nn " ++ Pretty.pretty pos) $
        PitchSignal.apply controls pitch

get_named_pitch :: Score.Control -> Deriver (Maybe PitchSignal.Signal)
get_named_pitch name = Map.lookup name <$> Internal.get_dynamic state_pitches

named_nn_at :: Score.Control -> RealTime -> Deriver (Maybe Pitch.NoteNumber)
named_nn_at name pos = do
    controls <- controls_at pos
    justm (named_pitch_at name pos) $ \pitch -> do
    logged_pitch_nn ("named_nn " ++ Pretty.pretty (name, pos)) $
        PitchSignal.apply controls pitch

-- | Version of 'PitchSignal.pitch_nn' that logs errors.
logged_pitch_nn :: String -> PitchSignal.Pitch
    -> Deriver (Maybe Pitch.NoteNumber)
logged_pitch_nn msg pitch = case PitchSignal.pitch_nn pitch of
    Left (PitchSignal.PitchError err) -> do
        Log.warn $ "pitch_nn " <> msg <> ": " <> untxt err
        return Nothing
    Right nn -> return $ Just nn

-- | Run the deriver in a context with the given pitch signal.  If a Control
-- is given, the pitch has that name, otherwise it's the unnamed default
-- pitch.
with_pitch :: Maybe Score.Control -> PitchSignal.Signal
    -> Deriver a -> Deriver a
with_pitch cont = modify_pitch cont . const

with_constant_pitch :: Maybe Score.Control -> Scale -> PitchSignal.Pitch
    -> Deriver a -> Deriver a
with_constant_pitch maybe_name scale = with_pitch maybe_name
    . PitchSignal.constant (pitch_signal_scale scale)

with_no_pitch :: Deriver a -> Deriver a
with_no_pitch = modify_pitch Nothing (const mempty)

pitch_signal_scale :: Scale -> PitchSignal.Scale
pitch_signal_scale scale =
    PitchSignal.Scale (scale_id scale) (scale_transposers scale)

modify_pitch :: Maybe Score.Control
    -> (Maybe PitchSignal.Signal -> PitchSignal.Signal)
    -> Deriver a -> Deriver a
modify_pitch Nothing f = Internal.local $ \st ->
    st { state_pitch = f (Just (state_pitch st)) }
modify_pitch (Just name) f = Internal.local $ \st ->
    st { state_pitches = Map.alter (Just . f) name (state_pitches st) }

-- ** with_scope

-- | Run the derivation with a modified scope.
with_scope :: (Scope -> Scope) -> Deriver a -> Deriver a
with_scope modify_scope = Internal.local $ \st ->
    st { state_scope = modify_scope (state_scope st) }

-- * calls

-- | Make a call map.
make_calls :: [(Text, call)] -> Map.Map TrackLang.CallId call
make_calls = Map.fromList . map (first TrackLang.Symbol)

-- * postproc

-- | Shift the controls of a deriver.  You're supposed to apply the warp
-- before deriving the controls, but I don't have a good solution for how to
-- do this yet, so I can leave these here for the moment.
shift_control :: ScoreTime -> Deriver a -> Deriver a
shift_control shift deriver = do
    real <- Internal.real shift
    Internal.local
        (\st -> st
            { state_controls = nudge real (state_controls st)
            , state_pitch = nudge_pitch real (state_pitch st) })
        deriver
    where
    nudge delay = Map.map (fmap (Signal.shift delay))
    nudge_pitch = PitchSignal.shift


-- * utils

-- | Run a computation in a logging context of a certain event.  Catch and
-- log any exception thrown.
--
-- When events are created, they are assigned their stack based on the current
-- event_stack, which is set by the with_stack_* functions.  Then, when they
-- are processed, the stack is used to *set* event_stack, which is what
-- 'Log.warn' and 'throw' will look at.
with_event :: Score.Event -> Deriver a -> Deriver (Maybe a)
with_event event deriver = do
    result <- Internal.detached_local
        (\st -> st { state_stack = Score.event_stack event }) deriver
    case result of
        Left err -> do
            Log.write $ error_to_warn err
            return Nothing
        Right val -> return (Just val)


-- ** merge

-- | The EventDerivers run as sub-derivers and the results are mappended, which
-- lets them to interleave their work or run in parallel.
d_merge :: [EventDeriver] -> EventDeriver
d_merge [] = mempty
d_merge [d] = d
d_merge derivers = do
    state <- get
    -- Clear collect so they can be merged back without worrying about dups.
    let cleared = state { state_collect = mempty }
    let (streams, collects) = unzip (map (run_sub cleared) derivers)
    modify $ \st -> st
        { state_collect = Monoid.mconcat (state_collect state : collects) }
    return (Seq.merge_lists _event_start streams)

-- | Like 'd_merge', but the derivers are assumed to return events that are
-- non-decreasing in time, so the merge can be more efficient.  It also assumes
-- each deriver is small, so it threads collect instead of making them
-- independent.
d_merge_asc :: [EventDeriver] -> EventDeriver
d_merge_asc = fmap merge_asc_events . sequence

type PureResult d = (LEvent.LEvents d, Collect)

-- | Run the given deriver and return the relevant data.
run_sub :: State -> LogsDeriver derived -> PureResult derived
run_sub state deriver = (merge_logs result logs, state_collect state2)
    where (result, state2, logs) = run state deriver

merge_logs :: Either Error (LEvent.LEvents d) -> [Log.Msg]
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

instance Monoid.Monoid EventDeriver where
    mempty = return []
    mappend d1 d2 = d_merge [d1, d2]
    mconcat = d_merge
