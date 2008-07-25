{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
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
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Perform.Signal2 as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Transport as Transport

import qualified Derive.Score as Score


-- * DeriveT

type Deriver = DeriveT Identity.Identity [Score.Event]
type SignalDeriver = DeriveT Identity.Identity [(Track.TrackId, Signal.Signal)]

newtype DeriveT m a = DeriveT (DeriveStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError DeriveError)
run_derive_t (DeriveT m) = m

type DeriveStack m = Error.ErrorT DeriveError
    (Monad.State.StateT State
        (Log.LogT m))

data State = State {
    -- | Derivers can modify it for sub-derivers, or look at it, whether to
    -- attach to an Event or to handle internally.
    state_env :: Score.ControllerMap
    -- | The tempo signal governing the currently derived block.
    -- I don't yet see how composed tempos could be returned as a single
    -- tempo signal for the updater.
    , state_maybe_warp :: Maybe Signal.Signal
    , state_ui :: State.State
    -- | Modified to reflect current event's stack.
    , state_current_event_stack :: [Warning.CallPos]
    } deriving (Show)
initial_state = State Map.empty Nothing State.empty []

state_warp :: State -> Signal.Signal
state_warp = Maybe.fromMaybe default_warp . state_maybe_warp

data DeriveError =
    -- | A general deriver error.
    DeriveError String
    -- | An error deriving a particular event, with its position included.
    | EventError [Warning.CallPos] String
    deriving (Eq, Show)
instance Error.Error DeriveError where
    strMsg = DeriveError

error_message (DeriveError s) = s
error_message (EventError _ s) = s

instance Monad m => Log.LogMonad (DeriveT m) where
    write = DeriveT . lift . lift . Log.write


-- * monadic ops

run :: (Monad m) =>
    State.State -> DeriveT m a -> m (Either DeriveError a, State, [Log.Msg])
run ui_state m = do
    let state = initial_state { state_ui = ui_state }
    ((err, state2), logs) <- (Log.run . flip Monad.State.runStateT state
        . Error.runErrorT . run_derive_t) m
    return (err, state2, logs)

derive :: State.State -> Block.BlockId -> DeriveT Identity.Identity a
    -> (Either DeriveError a,
        Transport.TempoFunction, Transport.InverseTempoFunction, [Log.Msg])
derive ui_state block_id deriver = (result, tempo_func, inv_tempo_func, logs)
    where
    (result, derive_state, logs) = Identity.runIdentity $ run ui_state deriver

    time_end = either (const (TrackPos 0)) id $
        State.eval ui_state (block_time_end block_id)
    warp = state_warp derive_state
    tempo_func = make_tempo_func warp
    inv_tempo_func = make_inverse_tempo_func block_id (TrackPos 0) time_end warp

-- TODO duplicated in State?
block_time_end :: (State.UiStateMonad m) => Block.BlockId -> m TrackPos
block_time_end block_id = do
    block <- State.get_block block_id
    tracks <- mapM State.get_track
        (Block.track_ids_of (Block.block_tracks block))
    return $ maximum (TrackPos 0 : map Track.track_time_end tracks)


make_tempo_func :: Signal.Signal -> Transport.TempoFunction
make_tempo_func warp pos = Timestamp.from_track_pos warped
    where warped = Signal.val_to_pos (Signal.at pos warp)

make_inverse_tempo_func :: Block.BlockId -> TrackPos -> TrackPos
    -> Signal.Signal -> Transport.InverseTempoFunction
make_inverse_tempo_func block_id _start end warp ts =
    case Signal.inverse_at warp ts of
        Nothing -> []
        Just pos
            | pos >= end -> []
            | otherwise -> [(block_id, pos)]

modify :: (Monad m) => (State -> State) -> DeriveT m ()
modify f = (DeriveT . lift) (Monad.State.modify f)
get :: (Monad m) => DeriveT m State
get = (DeriveT . lift) Monad.State.get

with_event :: (Monad m) => Score.Event -> DeriveT m a -> DeriveT m a
with_event event op = do
    old <- fmap state_current_event_stack get
    modify $ \st -> st { state_current_event_stack = (Score.event_stack event) }
    v <- op
    modify $ \st -> st { state_current_event_stack = old }
    return v


-- ** errors

throw :: (Monad m) => String -> DeriveT m a
throw msg = Error.throwError (DeriveError msg)

throw_event :: (Monad m) => String -> DeriveT m a
throw_event msg = do
    stack <- fmap state_current_event_stack get
    Error.throwError (EventError stack msg)

-- | Catch EventErrors and convert them into warnings.  If an error is caught,
-- return Nothing, otherwise return Just op's value.
catch_event op = Error.catchError (fmap Just op) $ \exc -> case exc of
    DeriveError _ -> Error.throwError exc
    EventError stack msg -> do
        Log.warn_stack stack msg
        return Nothing

warn :: (Monad m) => String -> DeriveT m ()
warn msg = do
    event_stack <- fmap state_current_event_stack get
    Log.warn_stack event_stack msg

-- ** environment

with_env :: (Monad m) =>
    Score.Controller -> Signal.Signal -> DeriveT m t -> DeriveT m t
with_env cont signal op = do
    old_env <- fmap state_env get
    modify $ \st -> st { state_env = Map.insert cont signal old_env }
    result <- op
    modify $ \st -> st { state_env = old_env }
    return result

-- * basic derivers

-- ** tempo

-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo implies,
-- which is integral (1/tempo).

default_warp = Signal.signal
    [(0, 0), (Signal.max_track_pos, Signal.pos_to_val Signal.max_track_pos)]
tempo_srate = Signal.default_srate
min_tempo :: Signal.Val
min_tempo = 0.001

-- | Associate a tempo signal with the current derivation.  It will govern the
-- current block because the sub-block deriver clears it from the State for
-- the sub-block.
--
-- The extra @deriver@ argument is technically unnecessary, but it makes
-- this look like a d_controller, which it is, except the restriction that
-- you can only set it once.
--
-- TODO this should probably go back to being like a plain controller so they
-- can be nested.  I haven't thought about how to merge that into the overall
-- block tempo, but I'll wait until I do sub-block derivation for that.
d_tempo :: (Monad m) => DeriveT m Signal.Signal -> DeriveT m a -> DeriveT m a
d_tempo signalm deriver = do
    old_warp <- fmap state_maybe_warp get

    -- Special hack so that the tempo track itself isn't warped by the
    -- (default) warp.
    modify $ \st -> st { state_maybe_warp = Just default_warp }
    signal <- signalm
    case old_warp of
        Nothing -> modify $ \st ->
            st { state_maybe_warp = Just (tempo_to_warp signal) }
        Just sig -> throw $
            "tried to add a tempo to a block that already has one: " ++ show sig
    deriver

tempo_to_warp = Signal.integrate tempo_srate . Signal.map_samples (1/)
    . Signal.clip_min min_tempo

-- ** track

-- | Get events from a track, convert them to Score events, and applying the
-- enviroment's controllers to them.
d_track :: (Monad m) => Track.TrackId -> DeriveT m [Score.Event]
d_track track_id = do
    track <- get_track track_id
    cmap <- fmap state_env get
    warp <- fmap state_warp get
    -- TODO how could I get effects like "overlap with next note by 16 pos"?
    -- If I handle the tempo higher up, the event doesn't get to handle tempo
    -- itself.  I think I could do this by making each event a deriver instead
    -- of getting them all with d_track.
    let events = map (Score.from_track_event cmap track_id) $
            Track.event_list (Track.track_events track)
        pos_list = concatMap extract_pos events
        pos_map = zip pos_list
            (map (Signal.val_to_pos . flip Signal.at warp) pos_list)
    -- merge_pos_map pos_map
    return (inject_pos pos_map events)

{-
merge_pos_map :: (Monad m) => [(TrackPos, TrackPos)] -> DeriveT m ()
merge_pos_map pos_map =
    modify $ \st -> st { state_pos_map = merge (state_pos_map st) pos_map }
    where
    -- Merge will result in dups if events from different tracks occur
    -- simultaneously.
    merge xs ys = Seq.drop_dups ((==) `on` fst) $
        Seq.merge_by (compare `on` fst) xs ys
-}

extract_pos event = [Score.event_start event, Score.event_end event]

inject_pos :: [(TrackPos, TrackPos)] -> [Score.Event] -> [Score.Event]
inject_pos = go Map.empty
    where
    -- TODO warn about things left in overlap after the pos map has run out
    -- this expects an entry in pmap
    go _overlap [] _events = []
    go overlap pos_map@((from, to):rest_pos) events =
        case Map.lookup from overlap of
            Just (event, start) -> inject start to event
                : go (Map.delete from overlap) pos_map events
            Nothing
                | not (null events) && Score.event_start (head events)==from ->
                    go (insert (head events)) pos_map (tail events)
                | otherwise -> go overlap rest_pos events
        where
        insert event = Map.insert (Score.event_end event) (event, to) overlap
        inject start end event = event
            { Score.event_start = start, Score.event_duration = end - start }

get_block block_id = get >>= lookup_id block_id . State.state_blocks . state_ui
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

-- | Set instrument on the given events.
d_instrument :: (Monad m) =>
    Score.Instrument -> [Score.Event] -> m [Score.Event]
d_instrument inst events = return $
    map (\evt -> evt { Score.event_instrument = Just inst }) events

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
