{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XPatternGuards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- Control.Monad
{- |

    Derivers are always in DeriveT, even if they don't need its facilities.
    This makes them more regular to compose.  The convention is to prepend
    deriver names with 'd_', so if the deriver is normally implement purely,
    a d_ version can be made simply by composing 'return'.

    tempo:

    Signals are described on the track as [TrackSegment], and are transformed
    to Signal, which can be sampled at any srate.  The tempo signal is sampled
    at tempo_srate.

    This is a special controller given in tempo_controller.  d_track realizes
    it by dividing each event's start and end positions by it.

    'derive' needs to produce a forward tempo map that maps (BlockId, TrackPos)
    -> Timestamp, so that playing from a certain trackpos on a certain block
    knows where to seek in the midi stream, and

    BlockId -> Timestamp -> Maybe TrackPos

    or maybe

    Timestamp -> Maybe [(BlockId, TrackPos)] -- Nothing indicates no blocks
    left

    so the play updater knows where to put the play selections

    so it seems like I need to convert the whole nested set of tempos into
    a single tempo map and invert it.  It needs to keep track of block_ids in
    there somehow too.

    Blocks get translated and stretched according to the calling event pos and
    dur.


    Each block has a tempo signal associated.  When you derive the sub-block,
    you get its tempo track, and then integrate it into your own: translate,
    stretch it, and insert it into the block list.

    I think this means tempo can't be a plain controller anymore.  I need to
    enforce anyway that there is only one tempo signal per block.  Actually...
    everything in the environment is global.  But if I do with_env it will get
    reset afterwards.  Just have it be a special set-once variable?

    tempo_map = integral (Signal.div tempo_sig 1)

    I have a similar sort of setup to nyquist, with a "transformation
    environment" that functions can look at to implement behavioral
    abstraction.  The main differences are that I don't actually generate audio
    signal, but my "ugens" eventually render down to MIDI or OSC (or nyquist
    source!), and that my Signals (the equivalent to nyquist's "sounds") are
    functions Time->Val rather than being sample streams.  Of course the
    function could be *implemented* as a sample stream, but the idea was that
    I don't have to worry about a sampling rates:. I just compose the signal
    with the tempo warp (which is a Time->Time signal).

    The problem is the representation of the tempo warp.  In addition to MIDI
    or OSC or whatever rendered with certain timestamps, I want to get from the
    process a global tempo function ScoreTime->Time and then an inverse one
    Time->ScoreTime.

    I don't think I can use arbitrary functions any more at this point, because
    if I take the time function as the integral of the reciprical of the tempo
    function, I don't think I can integrate any function without knowing
    anything about it.  And after that, I can't find the inverse of just any
    old function, even if I do know that it's a proper injection (is that the
    term?), which I think should be guaranteed if I say the tempo signal >0.

    So I can see two ways around that: either require that I do know the
    structure of the function, or take what I believe is nyquist's approach and
    pick a sampling rate, sample the function.  Then the integral is easy: just
    sum the samples, and for the inverse I just scan the samples for the
    correct value.

    Taking the first approach for the moment, I know that the signals are built
    up of a few envelope breakpoint type primitives: a discontinuity that jumps
    to some value, a linear interpolation between two points, and an
    exponential t^n type interpolation between two points.  All I have to do is
    find the integral for each segment and then sum them.  If I introduce new
    functions (e.g. sine), I can introduce them as a pair (f, integral of f).
    I can represent a typical sampled signal by just using a lot of regularly
    spaced segments.

    The complexity of this approach and the ability of the sampling approach to
    simulate it are making me favor the

    do integrals for set and linear

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

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Transport as Transport

import qualified Derive.Score as Score


-- * DeriveT

type TrackDeriver m = Track.TrackId -> DeriveT m [Score.Event]

type EventDeriver = DeriveT Identity.Identity [Score.Event]
type SignalDeriver = DeriveT Identity.Identity [(Track.TrackId, Signal.Signal)]

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
    state_controllers :: Score.ControllerMap
    , state_instrument :: Maybe Score.Instrument
    , state_warp :: Warp
    -- | This is the call stack for events.  It's used for error reporting,
    -- and attached to events in case they want to emit errors later (say
    -- during performance).
    , state_stack :: [Warning.StackPos]

    -- | Remember the warp signal for each track.  A warp usually a applies to
    -- a set of tracks, so remembering them together will make the updater more
    -- efficient when it inverts them to get playback position.
    , state_track_warps :: TrackWarps

    -- | Constant throughout the derivation.  Used to look up tracks and
    -- blocks.
    , state_ui :: State.State
    } deriving (Show)

initial_state = State {
    state_controllers = Map.empty
    , state_instrument = Nothing
    , state_warp = initial_warp
    , state_stack = []

    , state_track_warps = []
    , state_ui = State.empty
    }

-- | This maps a trackpos range (in global time) to the TrackWarps that are
-- active within that range.
newtype WarpMap = WarpMap (Map.Map (TrackPos, TrackPos) [TrackWarps])

-- | Each track warp is a warp indexed by the block and tracks in covers.
type TrackWarps = [(Block.BlockId, [Track.TrackId], Warp)]

-- | A tempo warp signal.  The shift and stretch are an optimization hack
-- stolen from nyquist.  The idea is to make composed shifts and stretches more
-- efficient since only the shift and stretch are changed.  They have to be
-- flattened out when the warp is composed though (in 'd_warp').
--
-- TODO Is this really that much of a win?
data Warp = Warp {
    warp_signal :: Signal.Signal
    , warp_shift :: TrackPos
    , warp_stretch :: TrackPos
    } deriving (Show)

initial_warp = make_warp (Signal.signal [(0, 0),
    (Signal.max_track_pos, Signal.pos_to_val Signal.max_track_pos)])

-- | Convert a Signal to a Warp.
make_warp :: Signal.Signal -> Warp
make_warp sig = Warp sig (TrackPos 0) (TrackPos 1)

data DeriveError =
    -- | A general deriver error.
    DeriveError String
    -- | An error deriving a particular event, with its position included.
    | EventError [Warning.StackPos] String
    deriving (Eq, Show)
instance Error.Error DeriveError where
    strMsg = DeriveError

error_message (DeriveError s) = s
error_message (EventError _ s) = s

instance Monad m => Log.LogMonad (DeriveT m) where
    write = DeriveT . lift . lift . Log.write


-- * monadic ops

derive :: State.State -> Block.BlockId -> DeriveT Identity.Identity a
    -> (Either DeriveError a,
        Transport.TempoFunction, Transport.InverseTempoFunction, [Log.Msg])
derive ui_state block_id deriver = (result, tempo_func, inv_tempo_func, logs)
    where
    initial_derive_state = initial_state
        { state_ui = ui_state, state_stack = [(block_id, Nothing, Nothing)] }
    (result, derive_state, logs) = Identity.runIdentity $
        run initial_derive_state deriver

    time_end = either (const []) id $ State.eval ui_state $ do
        end <- State.event_end block_id
        return [(block_id, end)]
    track_warps = state_track_warps derive_state
    tempo_func = make_tempo_func track_warps
    inv_tempo_func = make_inverse_tempo_func (TrackPos 0) time_end track_warps

run :: (Monad m) =>
    State -> DeriveT m a -> m (Either DeriveError a, State, [Log.Msg])
run derive_state m = do
    ((err, state2), logs) <- (Log.run . flip Monad.State.runStateT derive_state
        . Error.runErrorT . run_derive_t) m
    return (err, state2, logs)

lookup_track_warp :: Block.BlockId -> Track.TrackId -> TrackWarps -> Maybe Warp
lookup_track_warp block_id track_id track_warps = case matches of
        [] -> Nothing
        (w:_) -> Just w
    where
    matches = [warp
        | (w_block, w_tracks, warp) <- track_warps, w_block == block_id
        , w_track <- w_tracks, w_track == track_id]

make_tempo_func :: TrackWarps -> Transport.TempoFunction
make_tempo_func track_warps block_id track_id pos = do
    warp <- lookup_track_warp block_id track_id track_warps
    -- TODO shift/stretch?
    let warped = Signal.val_to_pos (Signal.at pos (warp_signal warp))
    return $ Timestamp.from_track_pos warped

make_inverse_tempo_func :: TrackPos -> [(Block.BlockId, TrackPos)]
    -> TrackWarps -> Transport.InverseTempoFunction
make_inverse_tempo_func _start block_ends track_warps ts = do
    (block_id, track_ids, Just pos) <- track_pos
    guard $ case lookup block_id block_ends of
        Nothing -> False
        Just end -> pos <= end
    return (block_id, map (flip (,) pos) track_ids)
    where
    -- TODO take shift/stretch into account?
    track_pos = [(block, tracks, Signal.inverse_at (warp_signal warp) ts)
            | (block, tracks, warp) <- track_warps]

modify :: (Monad m) => (State -> State) -> DeriveT m ()
modify f = (DeriveT . lift) (Monad.State.modify f)
get :: (Monad m) => DeriveT m State
get = (DeriveT . lift) Monad.State.get

with_event :: (Monad m) => Score.Event -> DeriveT m a -> DeriveT m a
with_event event op = do
    old <- fmap state_stack get
    -- TODO this is broken, but will go away when I work controllers into the
    -- new subderive thing
    modify $ \st -> st { state_stack = Score.event_stack event }
    v <- op
    modify $ \st -> st { state_stack = old }
    return v


-- ** errors

throw :: (Monad m) => String -> DeriveT m a
throw msg = Error.throwError (DeriveError msg)

throw_event :: (Monad m) => String -> DeriveT m a
throw_event msg = do
    stack <- fmap state_stack get
    Error.throwError (EventError stack msg)

-- | Catch EventErrors and convert them into warnings.  If an error is caught,
-- return Nothing, otherwise return Just op's value.
catch_event :: (Monad m) => DeriveT m a -> DeriveT m (Maybe a)
catch_event op = Error.catchError (fmap Just op) $ \exc -> case exc of
    DeriveError _ -> Error.throwError exc
    EventError stack msg -> do
        Log.warn_stack stack msg
        return Nothing

warn :: (Monad m) => String -> DeriveT m ()
warn msg = do
    event_stack <- fmap state_stack get
    Log.warn_stack event_stack msg

-- ** environment

with_controller :: (Monad m) =>
    Score.Controller -> Signal.Signal -> DeriveT m t -> DeriveT m t
with_controller cont signal op = do
    old_env <- fmap state_controllers get
    modify $ \st -> st { state_controllers = Map.insert cont signal old_env }
    result <- op
    modify $ \st -> st { state_controllers = old_env }
    return result

with_instrument :: (Monad m) => Score.Instrument -> DeriveT m t -> DeriveT m t
with_instrument inst op = do
    old <- fmap state_instrument get
    modify $ \st -> st { state_instrument = Just inst}
    result <- op
    modify $ \st -> st { state_instrument = old }
    return result

-- with_state getf set new op = do
--     old <- fmap getf get
--     modify (set new)
--     result <- op
--     modify (set old)
--     return result

-- with_pitch :: (Monad m) => Pitch.Pitch -> DeriveT m t -> DeriveT m t
-- with_pitch (Pitch.Pitch _ (Pitch.NoteNumber nn)) op =
--     with_controller pitch_controller (Signal.constant nn) op

-- |
-- Is this supplanting the deriver mechanism?  Well, not so much because it
-- returns derivers, but it does mean there are two ways to do things, e.g.
-- instrument assigning.
-- lookup_deriver :: (Monad m) => Track.TrackId -> Track.PosEvent
--     -> DeriveT m (Maybe EventDeriver)
-- lookup_deriver track_id (pos, event) = do
--     st <- get
--     return $ state_lookup_deriver st track_id pos event


-- ** stack

get_current_block_id :: (Monad m) => DeriveT m Block.BlockId
get_current_block_id = do
    stack <- fmap state_stack get
    case stack of
        [] -> throw "empty state_stack"
        ((block_id, _, _):_) -> return block_id

with_stack_block :: (Monad m) => Block.BlockId -> DeriveT m a -> DeriveT m a
with_stack_block block_id op = do
    modify $ \st ->
        st { state_stack = (block_id, Nothing, Nothing) : state_stack st }
    v <- op
    modify $ \st -> st { state_stack = drop 1 (state_stack st) }
    return v

with_stack_track :: (Monad m) => Track.TrackId -> DeriveT m a -> DeriveT m a
with_stack_track track_id = modify_stack $ \(block_id, _, _) ->
    (block_id, Just track_id, Nothing)

with_stack_pos :: (Monad m) => TrackPos -> DeriveT m a -> DeriveT m a
with_stack_pos pos = modify_stack $ \(block_id, track_id, _) ->
    (block_id, track_id, Just pos)

modify_stack f op = do
    old_stack <- fmap state_stack get
    new_stack <- case old_stack of
        [] -> throw "can't modify empty state stack"
        (x:xs) -> return (f x : xs)
    modify $ \st -> st { state_stack = new_stack }
    v <- op
    modify $ \st -> st { state_stack = old_stack }
    return v

-- ** track warps

add_track_warp :: (Monad m) => Track.TrackId -> Warp -> DeriveT m ()
add_track_warp track_id warp = do
    block_id <- get_current_block_id
    modify $ \st ->
        st { state_track_warps = update_warps block_id (state_track_warps st) }
    where
    -- This is a hack to avoid having to compare tempo warps to see group
    -- tracks under the same warp.  This should work as long as
    -- 'start_new_warp' is called when the tempo is changed as d_warp does.
    -- Otherwise, tracks will be grouped with the wrong tempo, which will cause
    -- the playback cursor to not track properly.
    update_warps block_id track_warps
        | ((wblock, track_ids, warp):rest) <- track_warps, wblock == block_id =
            (block_id, track_id : track_ids, warp) : rest
        | otherwise = (block_id, [track_id], warp) : track_warps

replace_track_warp :: (Monad m) => Track.TrackId -> Warp -> DeriveT m ()
replace_track_warp track_id warp = do
    modify $ \st ->
        st { state_track_warps = filter_track (state_track_warps st) }
    add_track_warp track_id warp
    where
    filter_track track_warps =
        filter (\(_, track_ids, _) -> not (null track_ids)) $
        map (\(block_id, track_ids, warp) ->
            (block_id, filter (/=track_id) track_ids, warp)) track_warps

start_new_warp :: (Monad m) => DeriveT m ()
start_new_warp = do
    block_id <- get_current_block_id
    modify $ \st -> st { state_track_warps =
        (block_id, [], state_warp st) : state_track_warps st }

-- * basic derivers

-- ** tempo

-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo implies,
-- which is integral (1/tempo).

local_to_global :: (Monad m) => TrackPos -> DeriveT m TrackPos
local_to_global pos = do
    (Warp sig shift stretch) <- fmap state_warp get
    return $ Signal.val_to_pos (Signal.at (pos * stretch + shift) sig)

default_warp = Signal.signal
    [(0, 0), (Signal.max_track_pos, Signal.pos_to_val Signal.max_track_pos)]
tempo_srate = Signal.default_srate
min_tempo :: Signal.Val
min_tempo = 0.001

-- | Warp the given deriver with the given signal.
--
-- The track_id passed is a hack so that the track that emitted the signal can
-- be marked as having the tempo that it emits, so the tempo track's play
-- position will move at the tempo track's tempo.
d_tempo :: (Monad m) => Track.TrackId -> DeriveT m Signal.Signal
    -> DeriveT m a -> DeriveT m a
d_tempo track_id signalm deriver = do
    signal <- signalm
    d_warp (tempo_to_warp signal) $ do
        warp <- fmap state_warp get
        replace_track_warp track_id warp
        deriver

tempo_to_warp = Signal.integrate tempo_srate . Signal.map_val (1/)
    . Signal.clip_min min_tempo

d_warp :: (Monad m) => Signal.Signal -> DeriveT m a -> DeriveT m a
d_warp sig deriver = do
    old_warp <- fmap state_warp get
    -- let new_warp = compose_warp (state_warp st) sig
    modify $ \st -> st { state_warp = compose_warp (state_warp st) sig }
    start_new_warp
    result <- deriver
    modify $ \st -> st { state_warp = old_warp }
    return result

-- | Warp a Warp with a warp signal.
--
-- From the nyquist warp function:
-- f(stretch * g(t) + shift)
-- f(scale(stretch, g) + offset)
-- (shift f -offset)(scale(stretch, g))
-- (compose (shift-time f (- offset)) (scale stretch g))
compose_warp :: Warp -> Signal.Signal -> Warp
compose_warp (Warp warpsig shift stretch) sig = make_warp
    (Signal.shift (-shift) warpsig `Signal.compose` Signal.stretch stretch sig)


-- ** track

-- | This does setup common to all track derivation, namely recording the tempo
-- warp, and then calls the specific track deriver.
with_track_warp :: (Monad m) => (Track.TrackId -> DeriveT m [Score.Event])
    -> TrackDeriver m
with_track_warp track_deriver track_id = do
    add_track_warp track_id =<< fmap state_warp get
    with_stack_track track_id (track_deriver track_id)

-- * util

get_track track_id = get >>= lookup_id track_id . State.state_tracks . state_ui

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- ** merge

d_merge :: (Monad m) => [[Score.Event]] -> m [Score.Event]
d_merge = return . merge_events

d_signal_merge :: (Monad m) => [[(Track.TrackId, Signal.Signal)]]
    -> m [(Track.TrackId, Signal.Signal)]
d_signal_merge = return . concat

merge_events :: [[Score.Event]] -> [Score.Event]
merge_events = foldr (Seq.merge_by (compare `on` Score.event_start)) []

-- * utils

-- | General purpose iterator over events.
--
-- It's like 'map_accuml_m' but sets the current event stack before operating
-- on each event, so that Derive.warn can use it.  In addition, EventErrors are
-- caught and turned into warnings.  Events that threw aren't included in the
-- output.
map_events f state event_of xs =
    fmap Maybe.catMaybes (Util.Control.map_accuml_m apply state xs)
    where
    apply st x = with_event (event_of x) $ do
        val <- catch_event (f st x)
        return $ case val of
            Nothing -> (st, Nothing)
            Just val -> (st, Just val)
