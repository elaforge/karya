{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
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
import qualified Prelude
import Prelude hiding (error)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Derive.Deriver.Internal as Internal
import Derive.Deriver.Internal (score_to_real)
import Derive.Deriver.Monad
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


-- * derive

-- This should probably be in Internal, but can't due to a circular dependency
-- with score_to_real.

-- | Package up the results of a derivation.
--
-- NOTE TO SELF: Don't put bangs on this and then be surprised when the
-- laziness tests fail, you doofus.
data Result = Result {
    r_events :: Events
    , r_cache :: Cache
    , r_track_warps :: TrackWarp.Collections
    , r_track_signals :: Track.TrackSignals
    , r_track_environ :: TrackEnviron

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
extract_result (result, state, logs) =
    Result (merge_logs result logs)
        (collect_cache collect <> state_cache (state_constant state))
        warps (collect_track_signals collect) (collect_track_environ collect)
        state
    where
    collect = state_collect state
    warps = TrackWarp.collections (collect_warp_map collect)

-- | Given an environ, bring instrument and scale calls into scope.
with_initial_scope :: TrackLang.Environ -> Deriver d -> Deriver d
with_initial_scope env deriver = set_inst (set_scale deriver)
    where
    set_inst = case TrackLang.lookup_val TrackLang.v_instrument env of
        Right inst -> with_instrument inst
        _ -> id
    set_scale = case TrackLang.lookup_val TrackLang.v_scale env of
        Right scale_id -> \deriver -> do
            scale <- get_scale scale_id
            with_scale scale deriver
        _ -> id


-- * errors

require :: String -> Maybe a -> Deriver a
require msg = maybe (throw msg) return

with_msg :: String -> Deriver a -> Deriver a
with_msg msg = Internal.local state_log_context
    (\old st -> st { state_log_context = old })
    (\st -> return $ st { state_log_context = msg : state_log_context st })

error_to_warn :: Error -> Log.Msg
error_to_warn (Error srcpos stack val) = Log.msg_srcpos srcpos Log.Warn
    (Just stack) ("Error: " ++ Pretty.pretty val)


-- * state access

-- ** scale

-- | Lookup a scale_id or throw.
get_scale :: Pitch.ScaleId -> Deriver Scale
get_scale scale_id = maybe (throw $ "unknown " ++ show scale_id) return
    =<< lookup_scale scale_id

lookup_scale :: Pitch.ScaleId -> Deriver (Maybe Scale)
lookup_scale scale_id = do
    -- Defaulting the scale here means that relative pitch tracks don't need
    -- to mention their scale.
    scale_id <- if scale_id == Pitch.default_scale_id
        then Internal.get_dynamic (PitchSignal.sig_scale . state_pitch)
        else return scale_id
    lookup_scale <- gets (state_lookup_scale . state_constant)
    return $ lookup_scale scale_id


-- ** environment

lookup_val :: forall a. (TrackLang.Typecheck a) =>
    TrackLang.ValName -> Deriver (Maybe a)
lookup_val name = do
    environ <- Internal.get_dynamic state_environ
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
    Internal.local state_environ (\old st -> st { state_environ = old }) $
    \st -> do
        environ <- Internal.insert_environ name val (state_environ st)
        return $ st { state_environ = environ }

with_scale :: Scale -> Deriver d -> Deriver d
with_scale scale = with_val TrackLang.v_scale (scale_id scale)
    . with_scope (\scope -> scope { scope_val = set (scope_val scope) })
    where
    set stype = stype { stype_scale = [lookup_scale_val scale] }
    lookup_scale_val :: Scale -> LookupCall ValCall
    lookup_scale_val scale call_id =
        return $ scale_note_to_call scale (to_note call_id)
        where to_note (TrackLang.Symbol sym) = Pitch.Note sym

with_instrument :: Score.Instrument -> Deriver d -> Deriver d
with_instrument inst deriver = do
    lookup_inst_calls <- gets (state_instrument_calls . state_constant)
    let inst_calls = maybe (InstrumentCalls [] []) id (lookup_inst_calls inst)
    with_val TrackLang.v_instrument inst
        (with_scope (set_scope inst_calls) deriver)
    where
    -- Replace the calls in the instrument scope type.
    set_scope (InstrumentCalls notes vals) scope = scope
        { scope_val = set_val vals (scope_val scope)
        , scope_note = set_note notes (scope_note scope)
        }
    set_val vals stype = stype { stype_instrument = vals }
    set_note notes stype = stype { stype_instrument = notes }


-- ** control

-- | Return an entire signal.  Remember, signals are in RealTime, so if you
-- want to index them in ScoreTime you will have to call 'score_to_real'.
-- 'control_at_score' does that for you.
get_control :: Score.Control -> Deriver (Maybe Signal.Control)
get_control cont = Map.lookup cont <$> Internal.get_dynamic state_controls

control_at_score :: Score.Control -> ScoreTime -> Deriver (Maybe Signal.Y)
control_at_score cont pos = control_at cont =<< score_to_real pos

control_at :: Score.Control -> RealTime -> Deriver (Maybe Signal.Y)
control_at cont pos = do
    controls <- Internal.get_dynamic state_controls
    return $ fmap (Signal.at pos) (Map.lookup cont controls)

pitch_at_score :: ScoreTime -> Deriver PitchSignal.Y
pitch_at_score pos = pitch_at =<< score_to_real pos

pitch_at :: RealTime -> Deriver PitchSignal.Y
pitch_at pos = do
    psig <- Internal.get_dynamic state_pitch
    return (PitchSignal.at pos psig)

degree_at :: RealTime -> Deriver Pitch.Degree
degree_at pos = PitchSignal.y_to_degree <$> pitch_at pos

get_named_pitch :: Score.Control -> Deriver (Maybe PitchSignal.PitchSignal)
get_named_pitch name = Map.lookup name <$> Internal.get_dynamic state_pitches

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
    Internal.local (Map.lookup cont . state_controls) insert alter
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
    controls <- Internal.get_dynamic state_controls
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
    pitch <- Internal.get_dynamic state_pitch
    with_pitch maybe_name
        (PitchSignal.constant (PitchSignal.sig_scale pitch) degree) deriver

with_relative_pitch :: Maybe Score.Control
    -> PitchOp -> PitchSignal.Relative -> Deriver a -> Deriver a
with_relative_pitch maybe_name sig_op signal deriver = do
    old <- Internal.get_dynamic state_pitch
    if old == PitchSignal.empty
        then do
            -- This shouldn't happen normally because of the default pitch.
            Log.warn
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
modify_pitch f Nothing signal = Internal.local
    state_pitch (\old st -> st { state_pitch = old })
    (\st -> return $ st { state_pitch = f (state_pitch st) signal })
modify_pitch f (Just name) signal = Internal.local
    (Map.lookup name . ps)
    (\old st -> st { state_pitches = Map.alter (const old) name (ps st) })
    (\st -> return $ st { state_pitches = Map.alter alter name (ps st) })
    where
    ps = state_pitches
    alter Nothing = Just signal
    alter (Just old) = Just (f old signal)

-- *** control ops

lookup_control_op :: TrackLang.CallId -> Deriver ControlOp
lookup_control_op c_op = do
    op_map <- gets (state_control_op_map . state_constant)
    maybe (throw ("unknown control op: " ++ show c_op)) return
        (Map.lookup c_op op_map)

lookup_pitch_control_op :: TrackLang.CallId -> Deriver PitchOp
lookup_pitch_control_op c_op = do
    op_map <- gets (state_pitch_op_map . state_constant)
    maybe (throw ("unknown pitch op: " ++ show c_op)) return
        (Map.lookup c_op op_map)


-- ** with_scope

-- | Run the derivation with a modified scope.
with_scope :: (Scope -> Scope) -> Deriver a -> Deriver a
with_scope modify_scope =
    Internal.local state_scope (\old st -> st { state_scope = old })
    (\st -> return $ st { state_scope = modify_scope (state_scope st) })



-- * calls

-- | Functions for writing calls.

make_calls :: [(String, call)] -> Map.Map TrackLang.CallId call
make_calls = Map.fromList . map (first TrackLang.Symbol)

-- ** passed args

passed_event :: PassedArgs derived -> Events.PosEvent
passed_event = info_event . passed_info

-- | Get the previous derived val.  This is used by control derivers so they
-- can interpolate from the previous sample.
passed_prev_val :: PassedArgs derived -> Maybe (RealTime, Elem derived)
passed_prev_val args = info_prev_val (passed_info args)

-- | Get the start of the next event, if there is one.  Used by calls to
-- determine their extent, especially control calls, which have no explicit
-- duration.
passed_next_begin :: PassedArgs d -> Maybe ScoreTime
passed_next_begin = fmap fst . Seq.head . info_next_events . passed_info

passed_next :: PassedArgs d -> ScoreTime
passed_next args = case info_next_events info of
        [] -> info_block_end info
        (pos, _) : _ -> pos
    where info = passed_info args

passed_prev_begin :: PassedArgs d -> Maybe ScoreTime
passed_prev_begin = fmap fst . Seq.head . info_prev_events . passed_info

-- | Range of the called event.  Note that range is the minimum to maximum,
-- which is not the same as the start and end if the event has negative
-- duration.
passed_range :: PassedArgs d -> (ScoreTime, ScoreTime)
passed_range = Events.range . passed_event

passed_real_range :: PassedArgs d -> Deriver (RealTime, RealTime)
passed_real_range args = (,) <$> score_to_real start <*> score_to_real end
    where (start, end) = passed_range args

passed_extent :: PassedArgs d -> (ScoreTime, ScoreTime)
passed_extent = (\(p, e) -> (p, Event.event_duration e)) . passed_event

-- TODO crummy name, come up with a better one
passed_score :: PassedArgs d -> ScoreTime
passed_score = fst . passed_event

passed_real :: PassedArgs d -> Deriver RealTime
passed_real = score_to_real . passed_score


-- * postproc

-- | Shift the controls of a deriver.  You're supposed to apply the warp
-- before deriving the controls, but I don't have a good solution for how to
-- do this yet, so I can leave these here for the moment.
shift_control :: ScoreTime -> Deriver a -> Deriver a
shift_control shift deriver = do
    real <- score_to_real shift
    Internal.local (\st -> (state_controls st, state_pitch st))
        (\(controls, pitch) st -> st { state_controls = controls,
            state_pitch = pitch })
        (\st -> return $ st
            { state_controls = nudge real (state_controls st)
            , state_pitch = nudge_pitch real (state_pitch st )})
        deriver
    where
    nudge delay = Map.map (Signal.shift delay)
    nudge_pitch = PitchSignal.shift


-- * utils

get_ui_state :: Deriver State.State
get_ui_state = gets (state_ui . state_constant)

-- | Because Deriver is not a UiStateMonad.
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
with_event event = Internal.local state_stack
    (\old st -> st { state_stack = old })
    (\st -> return $ st { state_stack = Score.event_stack event })


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

type PureResult d = (Stream (LEvent.LEvent d), Collect)

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
