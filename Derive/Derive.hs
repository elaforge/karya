{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
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
    the screen, they should be in local unwarped time, not global time.

    The current stack is stored in 'state_stack' and will be added to by
    'with_stack_block', 'with_stack_track', and 'with_stack_pos' as the deriver
    processes a block, a track, and individual events respectively.
    'warn' and 'throw' will pick the current stack out of 'state_stack'.

    When 'Derive.Score.Event's are emitted they are also given the stack at the
    time of their derivation.  If there is a problem in performance, log msgs
    still have access to the stack.
-}
module Derive.Derive where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as Monad.State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import Data.Function
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.SrcPos as SrcPos

import Ui
import qualified Ui.State as State

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Transport as Transport
import qualified Perform.Warning as Warning

import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang


-- * DeriveT

type TrackDeriver m e = TrackId -> DeriveT m [e]

type EventDeriver = DeriveT Identity.Identity [Score.Event]
type SignalDeriver sig = DeriveT Identity.Identity [(TrackId, sig)]

newtype DeriveT m a = DeriveT (DeriveStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError DeriveError)
run_derive_t (DeriveT m) = m

type DeriveStack m = Error.ErrorT DeriveError
    (Monad.State.StateT State
        (Log.LogT m))

data State = State {
    -- Environment.  These form a dynamically scoped environment that applies
    -- to generated events inside its scope.

    -- | Derivers can modify it for sub-derivers, or look at it, whether to
    -- attach to an Event or to handle internally.
    state_controls :: Score.ControlMap
    -- | Absolute pitch signal currently in scope.
    , state_pitch :: PitchSignal.PitchSignal
    , state_instrument :: Maybe Score.Instrument
    , state_attributes :: Score.Attributes
    , state_warp :: Warp
    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: [Warning.StackPos]
    , state_log_context :: [String]

    -- | Remember the warp signal for each track.  A warp usually a applies to
    -- a set of tracks, so remembering them together will make the updater more
    -- efficient when it inverts them to get playback position.
    , state_track_warps :: [TrackWarp]

    -- | Constant throughout the derivation.  Used to look up tracks and
    -- blocks.
    , state_ui :: State.State
    , state_lookup_deriver :: LookupDeriver
    , state_control_op_map :: Map.Map Operator ControlOp
    , state_pitch_op_map :: Map.Map Operator PitchOp
    , state_call_map :: CallEnv
    -- | This is set if the derivation is for a signal deriver.  Signal
    -- derivers skip all special tempo treatment.  Ultimately, this is needed
    -- because of the 'add_track_warp' hack.  It might be 'add_track_warp' is
    -- too error-prone to allow to live...
    , state_ignore_tempo :: Bool
    }

initial_state ui_state lookup_deriver calls ignore_tempo = State {
    state_controls = initial_controls
    , state_pitch = PitchSignal.empty
    , state_instrument = Nothing
    , state_attributes = Score.no_attrs
    , state_warp = initial_warp
    , state_stack = []
    , state_log_context = []

    , state_track_warps = []
    , state_ui = ui_state
    , state_lookup_deriver = lookup_deriver
    , state_control_op_map = default_control_op_map
    , state_pitch_op_map = default_pitch_op_map
    , state_call_map = calls
    , state_ignore_tempo = ignore_tempo
    }

-- | Initial control environment.
--
-- See 'Perform.Midi.Perform.default_velocity' for 0.79.
initial_controls :: Score.ControlMap
initial_controls = Map.fromList
    [(Score.c_velocity, Signal.constant default_velocity)]

default_velocity :: Signal.Y
default_velocity = 0.79

initial_warp :: Warp
initial_warp = make_warp (Signal.signal [(0, 0), (Signal.max_x, Signal.max_y)])

data CallEnv = CallEnv {
    calls_note :: CallMap
    , calls_control :: CallMap
    , calls_sub :: SubMap
    }
empty_call_map = CallEnv Map.empty Map.empty Map.empty

type CallMap = Map.Map TrackLang.CallId Call
type SubMap = Map.Map TrackLang.CallId SubDerive

type Call = [TrackLang.Val] -> [Score.Event]
    -> Either TrackLang.TypeError EventDeriver
type SubDerive = [TrackLang.Val]
    -> Either TrackLang.TypeError EventDeriver

make_calls :: [(String, Call)] -> CallMap
make_calls = Map.fromList . map (Util.Control.first TrackLang.CallId)

make_subs :: [(String, SubDerive)] -> SubMap
make_subs = Map.fromList . map (Util.Control.first TrackLang.CallId)

instance Show CallEnv where
    show (CallEnv note control sub) = "(CallEnv "
            ++ keys note ++ " " ++ keys control ++ " " ++ keys sub ++ ")"
        where
        keys m = "<" ++ Seq.join ", " [c | TrackLang.CallId c <- Map.keys m]
            ++ ">"


-- | Since the deriver may vary based on the block, this is needed to find
-- the appropriate deriver.  It's created by 'Schema.lookup_deriver'.
type LookupDeriver = BlockId -> Either State.StateError EventDeriver

-- | LookupDeriver that suppresses all sub-derivations.  Used by the signal
-- deriver since it only derives one block at a time.
empty_lookup_deriver :: LookupDeriver
empty_lookup_deriver = const (Right (return []))

-- | Each track warp is a warp indexed by the block and tracks it covers.
-- The start and end pos are in global time.
data TrackWarp = TrackWarp {
    tw_start :: TrackPos
    , tw_end :: TrackPos
    , tw_block :: BlockId
    , tw_tracks :: [TrackId]
    , tw_warp :: Warp
    } deriving (Show)

-- | A tempo warp signal.  The shift and stretch are an optimization hack
-- stolen from nyquist.  The idea is to make composed shifts and stretches more
-- efficient since only the shift and stretch are changed.  They have to be
-- flattened out when the warp is composed though (in 'd_warp').
--
-- TODO Is this really that much of a win?
data Warp = Warp {
    warp_signal :: Signal.Warp
    , warp_shift :: TrackPos
    , warp_stretch :: TrackPos
    } deriving (Eq, Show)

-- | Convert a Signal to a Warp.
make_warp :: Signal.Warp -> Warp
make_warp sig = Warp sig (TrackPos 0) (TrackPos 1)

data DeriveError = DeriveError SrcPos.SrcPos [Warning.StackPos] String
    deriving (Eq)
instance Error.Error DeriveError where
    strMsg = DeriveError Nothing []
instance Show DeriveError where
    show (DeriveError srcpos stack msg) =
        "<DeriveError " ++ SrcPos.show_srcpos srcpos ++ " "
        ++ Log.show_stack stack ++ ": " ++ msg ++ ">"

error_message (DeriveError _ _ s) = s

instance Monad m => Log.LogMonad (DeriveT m) where
    write = DeriveT . lift . lift . Log.write


-- * monadic ops

derive :: LookupDeriver -> State.State -> CallEnv -> Bool
    -> DeriveT Identity.Identity a
    -> (Either DeriveError a,
        Transport.TempoFunction, Transport.InverseTempoFunction, [Log.Msg],
        State) -- ^ State is not actually needed, but is handy for testing.
derive lookup_deriver ui_state calls ignore_tempo deriver =
    (result, tempo_func, inv_tempo_func, logs, state)
    where
    (result, state, logs) = Identity.runIdentity $
        run (initial_state ui_state lookup_deriver calls ignore_tempo) deriver
    track_warps = state_track_warps state
    tempo_func = make_tempo_func track_warps
    inv_tempo_func = make_inverse_tempo_func track_warps

d_block :: BlockId -> EventDeriver
d_block block_id = do
    state <- get
    let rethrow exc = throw $ "lookup deriver for " ++ show block_id
            ++ ": " ++ show exc
    deriver <- either rethrow return (state_lookup_deriver state block_id)
    with_stack_block block_id deriver

d_sub_derive :: (Monad m) => a -> DeriveT Identity.Identity a -> DeriveT m a
d_sub_derive fail_val deriver = do
    state <- get
    let (res, state2, logs) = Identity.runIdentity $ run state deriver
    mapM_ Log.write logs
    case res of
        Left err -> do
            warn $ "error sub-deriving: " ++ show err
            return fail_val
        Right val -> do
            modify (const state2)
            return val

run :: (Monad m) =>
    State -> DeriveT m a -> m (Either DeriveError a, State, [Log.Msg])
run derive_state m = do
    ((err, state2), logs) <- (Log.run . flip Monad.State.runStateT derive_state
        . Error.runErrorT . run_derive_t) m
    return (err, state2, logs)

make_tempo_func :: [TrackWarp] -> Transport.TempoFunction
make_tempo_func track_warps block_id track_id pos = do
    warp <- lookup_track_warp block_id track_id track_warps
    -- TODO what about shift and stretch?
    let warped = Signal.y_to_x (Signal.at_linear pos (warp_signal warp))
    return $ Timestamp.from_track_pos warped

lookup_track_warp :: BlockId -> TrackId -> [TrackWarp] -> Maybe Warp
lookup_track_warp block_id track_id track_warps = case matches of
        [] -> Nothing
        (w:_) -> Just w
    where
    matches =
        [ tw_warp tw | tw <- track_warps, tw_block tw == block_id
        , any (==track_id) (tw_tracks tw)
        ]

make_inverse_tempo_func :: [TrackWarp] -> Transport.InverseTempoFunction
make_inverse_tempo_func track_warps ts = do
    (block_id, track_ids, Just pos) <- track_pos
    return (block_id, [(track_id, pos) | track_id <- track_ids])
    where
    pos = Timestamp.to_track_pos ts
    track_pos = [(tw_block tw, tw_tracks tw, dewarp (tw_warp tw) ts) |
            tw <- track_warps, tw_start tw <= pos && pos < tw_end tw]
    dewarp (Warp sig shift stretch) ts = do
        p <- Signal.inverse_at sig ts
        return $ (p - shift)  / stretch

modify :: (Monad m) => (State -> State) -> DeriveT m ()
modify f = (DeriveT . lift) (Monad.State.modify f)

get :: (Monad m) => DeriveT m State
get = (DeriveT . lift) Monad.State.get

gets :: (Monad m) => (State -> a) -> DeriveT m a
gets f = fmap f get

-- | This is a little different from Reader.local because only a portion of
-- the state is used Reader-style, i.e. 'state_track_warps' always collects.
-- TODO split State into dynamically scoped portion and use Reader for that.
-- this should also properly restore state after an exception
local :: (Monad m) => (State -> b) -> (State -> State) -> (b -> State -> State)
    -> DeriveT m a -> DeriveT m a
local from_state modify_state to_state m = do
    old <- gets from_state
    modify modify_state
    result <- m
    modify (to_state old)
    return result

-- | So this is kind of confusing.  When events are created, they are assigned
-- their stack based on the current event_stack, which is set by the
-- with_stack_* functions.  Then, when they are processed, the stack is used
-- to *set* event_stack, which is what 'warn' and 'throw' will look at.
with_event :: (Monad m, Score.Eventlike e) => e -> DeriveT m a -> DeriveT m a
with_event event = local state_stack
    (\st -> st { state_stack = Score.stack event })
    (\old st -> st { state_stack = old })

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

-- ** errors

throw :: (Monad m) => String -> DeriveT m a
throw = throw_srcpos Nothing

throw_srcpos :: (Monad m) => SrcPos.SrcPos -> String -> DeriveT m a
throw_srcpos srcpos msg = do
    stack <- gets state_stack
    context <- gets state_log_context
    Error.throwError (DeriveError srcpos stack (_add_context context msg))

with_msg :: (Monad m) => String -> DeriveT m a -> DeriveT m a
with_msg msg = local state_log_context
    (\st -> st { state_log_context = msg : state_log_context st })
    (\old st -> st { state_log_context = old })

-- | Catch DeriveErrors and convert them into warnings.  If an error is caught,
-- return Nothing, otherwise return Just op's value.
catch_warn :: (Monad m) => DeriveT m a -> DeriveT m (Maybe a)
catch_warn op = Error.catchError (fmap Just op) $
    \(DeriveError srcpos stack msg) ->
        Log.warn_stack_srcpos srcpos stack msg >> return Nothing

warn :: (Monad m) => String -> DeriveT m ()
warn = warn_srcpos Nothing

warn_srcpos :: (Monad m) => SrcPos.SrcPos -> String -> DeriveT m ()
warn_srcpos srcpos msg = do
    stack <- gets state_stack
    context <- gets state_log_context
    Log.warn_stack_srcpos srcpos stack (_add_context context msg)

_add_context [] s = s
_add_context context s = Seq.join " / " (reverse context) ++ ": " ++ s

-- ** environment

with_instrument :: (Monad m) => Maybe Score.Instrument -> Score.Attributes
    -> DeriveT m t -> DeriveT m t
with_instrument inst attrs op = do
    old_inst <- gets state_instrument
    old_attrs <- gets state_attributes
    modify $ \st -> st { state_instrument = inst, state_attributes = attrs }
    result <- op
    modify $ \st -> st
        { state_instrument = old_inst, state_attributes = old_attrs }
    return result

-- *** control

type Operator = String
type ControlOp = Signal.Control -> Signal.Control -> Signal.Control
type PitchOp = PitchSignal.PitchSignal -> PitchSignal.Relative
    -> PitchSignal.PitchSignal

-- | This gets the control from the environment, not the event.  The difference
-- is that the environment is the current state at this point of evaluation, so
-- it would represent the context of the computation, while the event is the
-- environment at the time the event was created.
--
-- Control events don't have their own signal, but there is a signal in the
-- environment at the time of their evaluation.
control_at :: (Monad m) => TrackPos -> Score.Control -> Maybe Signal.Y
    -> DeriveT m Signal.Y
control_at pos cont deflt = do
    controls <- gets state_controls
    case Map.lookup cont controls of
        Nothing -> maybe
            (throw $ "control_at: not in environment and no default given: "
                ++ show cont) return deflt
        Just sig -> return (Signal.at pos sig)

with_control :: (Monad m) =>
    Score.Control -> Signal.Control -> DeriveT m t -> DeriveT m t
with_control cont signal op = do
    controls <- gets state_controls
    modify $ \st -> st { state_controls = Map.insert cont signal controls }
    result <- op
    modify $ \st -> st { state_controls = controls }
    return result

with_relative_control :: (Monad m) =>
    Score.Control -> Operator -> Signal.Control -> DeriveT m t -> DeriveT m t
with_relative_control cont c_op signal op = do
    sig_op <- lookup_control_op c_op
    controls <- gets state_controls
    let msg = "relative control applied when no absolute control is in scope: "
    case Map.lookup cont controls of
        Nothing -> do
            warn (msg ++ show cont)
            op
        Just old_sig -> do
            modify $ \st -> st { state_controls =
                Map.insert cont (sig_op old_sig signal) controls }
            result <- op
            modify $ \st -> st { state_controls = controls }
            return result

with_pitch :: (Monad m) => PitchSignal.PitchSignal -> DeriveT m t -> DeriveT m t
with_pitch signal op = do
    old <- gets state_pitch
    modify $ \st -> st { state_pitch = signal }
    result <- op
    modify $ \st -> st { state_pitch = old }
    return result

with_relative_pitch :: (Monad m) =>
    Operator -> PitchSignal.Relative -> DeriveT m t -> DeriveT m t
with_relative_pitch c_op signal op = do
    sig_op <- lookup_pitch_control_op c_op
    old <- gets state_pitch
    if old == PitchSignal.empty
        then do
            warn $ "relative pitch applied when no absolute pitch is in scope"
            op
        else do
            modify $ \st -> st { state_pitch = sig_op old signal }
            result <- op
            modify $ \st -> st { state_pitch = old }
            return result

lookup_control_op :: (Monad m) => Operator -> DeriveT m ControlOp
lookup_control_op c_op = do
    op_map <- gets state_control_op_map
    maybe (throw ("unknown control op: " ++ show c_op)) return
        (Map.lookup c_op op_map)

-- | Default set of control operators.  Merged at runtime with the static
-- config.  TODO but not yet
default_control_op_map :: Map.Map Operator ControlOp
default_control_op_map = Map.fromList
    [ ("+", Signal.sig_add)
    , ("-", Signal.sig_subtract)
    , ("*", Signal.sig_multiply)
    , ("max", Signal.sig_max)
    , ("min", Signal.sig_min)
    ]

lookup_pitch_control_op :: (Monad m) => Operator -> DeriveT m PitchOp
lookup_pitch_control_op c_op = do
    op_map <- gets state_pitch_op_map
    maybe (throw ("unknown pitch op: " ++ show c_op)) return
        (Map.lookup c_op op_map)

-- | As with 'default_control_op_map', but pitch ops have a different type.
default_pitch_op_map :: Map.Map Operator PitchOp
default_pitch_op_map = Map.fromList
    [ ("+", PitchSignal.sig_add)
    , ("max", PitchSignal.sig_max)
    , ("min", PitchSignal.sig_min)
    ]

lookup_note_call :: (Monad m) => TrackLang.CallId -> DeriveT m Call
lookup_note_call call_id = do
    cmap <- gets state_call_map
    maybe (throw $ "lookup_note_call: unknown " ++ show call_id) return
        (Map.lookup call_id (calls_note cmap))

lookup_control_call :: (Monad m) => TrackLang.CallId -> DeriveT m Call
lookup_control_call call_id = do
    cmap <- gets state_call_map
    maybe (throw $ "lookup_control_call: unknown " ++ show call_id) return
        (Map.lookup call_id (calls_control cmap))

lookup_sub_call :: (Monad m) => TrackLang.CallId -> DeriveT m SubDerive
lookup_sub_call call_id = do
    cmap <- gets state_call_map
    maybe (throw $ "lookup_sub_call: unknown " ++ show call_id) return
        (Map.lookup call_id (calls_sub cmap))


-- ** stack

get_current_block_id :: (Monad m) => DeriveT m BlockId
get_current_block_id = do
    stack <- gets state_stack
    case stack of
        [] -> throw "empty state_stack"
        ((block_id, _, _):_) -> return block_id

-- | Make a quick trick block stack.
with_stack_block :: (Monad m) => BlockId -> DeriveT m a -> DeriveT m a
with_stack_block block_id op = do
    modify $ \st ->
        st { state_stack = (block_id, Nothing, Nothing) : state_stack st }
    v <- op
    modify $ \st -> st { state_stack = drop 1 (state_stack st) }
    return v

-- | Make a quick trick track stack.
with_stack_track :: (Monad m) => TrackId -> DeriveT m a -> DeriveT m a
with_stack_track track_id = modify_stack "with_stack_track" $
    \(block_id, _, _) -> (block_id, Just track_id, Nothing)

with_stack_pos :: (Monad m) => TrackPos -> TrackPos -> DeriveT m a
    -> DeriveT m a
with_stack_pos pos dur = modify_stack "with_stack_pos" $
    \(block_id, track_id, _) -> (block_id, track_id, Just (start, end))
    where (start, end) = (min pos (pos+dur), max pos (pos+dur))

modify_stack :: (Monad m) => String -> (Warning.StackPos -> Warning.StackPos)
    -> DeriveT m a -> DeriveT m a
modify_stack caller f op = do
    old_stack <- gets state_stack
    new_stack <- case old_stack of
        [] -> throw $ caller ++ ": can't modify empty stack"
        (x:xs) -> return (f x : xs)
    modify $ \st -> st { state_stack = new_stack }
    v <- op
    modify $ \st -> st { state_stack = old_stack }
    return v

-- ** track warps

-- | This doesn't actually take the Warp because it adds this track to the
-- existing \"open\" warp.  This is a hack to assign the same warp to many
-- tracks without having to compare warps to each other, which may be expensive
-- since they can be complicated signals.  Since many tracks should share the
-- same warp, I think grouping them should be a performance win for the
-- playback updater, but I don't know if it's necessary.
--
-- The hack should work as long as 'start_new_warp' is called when the warp is
-- set (d_warp does this).  Otherwise, tracks will be grouped with the wrong
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
    start <- local_to_global (TrackPos 0)
    ui_state <- gets state_ui
    let time_end = either (const (TrackPos 0)) id $
            State.eval ui_state (State.event_end block_id)
    end <- local_to_global time_end
    modify $ \st ->
        let tw = TrackWarp start end block_id [] (state_warp st)
        in st { state_track_warps = tw : state_track_warps st }

-- * basic derivers

-- ** tempo

-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo implies,
-- which is integral (1/tempo).

local_to_global :: (Monad m) => TrackPos -> DeriveT m TrackPos
local_to_global pos = do
    (Warp sig shift stretch) <- gets state_warp
    return $ Signal.y_to_x (Signal.at_linear (pos * stretch + shift) sig)

tempo_srate = Signal.default_srate
min_tempo :: Signal.Y
min_tempo = 0.001

d_stretch :: (Monad m) => TrackPos -> DeriveT m a -> DeriveT m a
d_stretch factor d
    | factor <= 0 = throw $ "stretch <= 0: " ++ show factor
    | otherwise = with_warp
        (\w -> w { warp_stretch = warp_stretch w * factor }) d

d_at :: (Monad m) => TrackPos -> DeriveT m a -> DeriveT m a
d_at shift = with_warp $ \w ->
    w { warp_shift = warp_shift w + warp_stretch w * shift }

with_warp :: (Monad m) => (Warp -> Warp) -> DeriveT m a -> DeriveT m a
with_warp f d = do
    old <- gets state_warp
    modify $ \st -> st { state_warp = f (state_warp st) }
    v <- d
    modify $ \st -> st { state_warp = old }
    return v

-- | Warp the given deriver with the given signal.
--
-- The track_id passed so that the track that emitted the signal can be marked
-- as having the tempo that it emits, so the tempo track's play position will
-- move at the tempo track's tempo.
d_tempo :: (Monad m) => TrackId -> DeriveT m Signal.Tempo
    -> DeriveT m a -> DeriveT m a
d_tempo track_id signalm deriver = do
    signal <- signalm
    d_warp (tempo_to_warp signal) $ do
        add_track_warp track_id
        deriver

tempo_to_warp :: Signal.Tempo -> Signal.Warp
tempo_to_warp = Signal.integrate tempo_srate . Signal.map_y (1/)
    . Signal.clip_min min_tempo

d_warp :: (Monad m) => Signal.Warp -> DeriveT m a -> DeriveT m a
d_warp sig deriver = do
    old_warp <- gets state_warp
    modify $ \st -> st { state_warp = compose old_warp sig }
    start_new_warp
    result <- deriver
    modify $ \st -> st { state_warp = old_warp }
    return result
    where
    compose warp sig
        -- If the top level block has no tempo, don't bother composing.
        | warp == initial_warp = make_warp sig
        | otherwise = compose_warp warp sig

-- | Warp a Warp with a warp signal.
--
-- From the nyquist warp function:
--
-- > f(stretch * g(t) + shift)
-- > f(scale(stretch, g) + offset)
-- > (shift f -offset)(scale(stretch, g))
-- > (compose (shift-time f (- offset)) (scale stretch g))
compose_warp :: Warp -> Signal.Warp -> Warp
compose_warp (Warp warpsig shift stretch) sig = make_warp
    (Signal.shift (-shift) warpsig
        `Signal.compose` Signal.scale (Signal.x_to_y stretch) sig)


-- ** track

-- | This does setup common to all track derivation, namely recording the tempo
-- warp and putting the track in the stack, and then calls the specific track
-- deriver.
with_track_warp :: (Monad m) => TrackDeriver m e -> TrackDeriver m e
with_track_warp track_deriver track_id = do
    ignore_tempo <- gets state_ignore_tempo
    unless ignore_tempo (add_track_warp track_id)
    track_deriver track_id

-- | This is a special version of 'with_track_warp' just for the tempo track.
-- It doesn't record the track warp, see 'd_tempo' for why.
without_track_warp :: (Monad m) =>
    (TrackId -> DeriveT m [e]) -> TrackDeriver m e
without_track_warp track_deriver track_id = do
    with_warp (const initial_warp) (track_deriver track_id)

-- * utils

get_track track_id = get >>= lookup_id track_id . State.state_tracks . state_ui
get_block block_id = get >>= lookup_id block_id . State.state_blocks . state_ui

-- | Lookup @map!key@, throwing if it doesn't exist.
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
map_events :: (Monad m, Score.Eventlike e) =>
    (state -> x -> DeriveT m (a, state))
    -> state -> (x -> e) -> [x] -> DeriveT m [a]
map_events f state event_of xs =
    fmap Maybe.catMaybes (Util.Control.map_accuml_m apply state xs)
    where
    apply cur_state x = with_event (event_of x) $ do
        val <- catch_warn (f cur_state x)
        return $ case val of
            Nothing -> (cur_state, Nothing)
            Just (val, next_state) -> (next_state, Just val)

-- | A little more descriptive than ().
data NoState = NoState deriving (Show)

-- ** merge

d_merge :: (Monad m) => [[Score.Event]] -> m [Score.Event]
d_merge = return . merge_events

d_signal_merge :: (Monad m) => [[(TrackId, Signal.Signal y)]]
    -> m [(TrackId, Signal.Signal y)]
d_signal_merge = return . concat

merge_events :: [[Score.Event]] -> [Score.Event]
merge_events = foldr (Seq.merge_by (compare `on` Score.event_start)) []
