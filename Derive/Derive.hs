{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- |

Derivers are always in DeriveT, even if they don't need its facilities.  This
makes them more regular to compose.  The convention is to prepend deriver names
with 'd_', so if the deriver is normally implement purely, a d_ version can be
made simply by composing 'return'.
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

import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Transport as Transport

import qualified Derive.Score as Score


-- * DeriveT

type Deriver = DeriveT Identity.Identity [Score.Event]

newtype DeriveT m a = DeriveT (DeriveStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError DeriveError)
run_derive_t (DeriveT m) = m

type DeriveStack m = Error.ErrorT DeriveError
    (Monad.State.StateT State
        (Log.LogT m))

data State = State {
    -- | Derivers can modify it for sub-derivers, or look at it, whether to
    -- attach to an Event or to handle internally.  Since it's attached to
    -- the stack it automatically gets popped when the stack does.
    state_env :: Score.ControllerMap
    , state_ui :: State.State
    -- | Modified to reflect current event's stack.
    , state_current_event_stack :: [Warning.CallPos]
    } deriving (Show)
initial_state = State Map.empty State.empty []

data DeriveError =
    -- | A general deriver error.
    DeriveError String
    -- | An error deriving a particular event, with its position included.
    | EventError [Warning.CallPos] String
    deriving (Eq, Show)
instance Error.Error DeriveError where
    strMsg = DeriveError

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

derive :: State.State -> Deriver
    -> (Either DeriveError [Score.Event], Transport.TempoMap, [Log.Msg])
derive ui_state m = (result, tempo_map, logs)
    where
    (result, _derive_state, logs) = Identity.runIdentity $ run ui_state m
    tempo_map = simple_tempo_map ui_state
    -- TODO implement with tempo map
    -- tempo_map = Map.lookup Score.c_tempo (state_env derive_state)

simple_tempo_map ui_state block_id ts = do
    block <- Map.lookup block_id (State.state_blocks ui_state)
    let tids = [tid | (Block.TId tid _, _) <- Block.block_tracks block]
    tracks <- mapM (flip Map.lookup (State.state_tracks ui_state)) tids
    let end = maximum (map Track.time_end tracks)
        pos = Timestamp.to_track_pos ts `div` 20
    if pos < end then Just pos else Nothing
    -- TODO 20 also hardcoded in Perform.Midi.Play

modify :: (Monad m) => (State -> State) -> DeriveT m ()
modify f = (DeriveT . lift) (Monad.State.modify f)
get :: (Monad m) => DeriveT m State
get = (DeriveT . lift) Monad.State.get

set_event_stack stack =
    modify $ \st -> st { state_current_event_stack = stack }

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

-- ** track

-- | Get events from a track, applying the enviroment's controllers to it.
d_track :: (Monad m) => Track.TrackId -> DeriveT m [Score.Event]
d_track track_id = do
    track <- get_track track_id
    cmap <- fmap state_env get
    return $ (map (Score.from_track_event cmap track_id) . Track.event_list
        . Track.track_events) track

get_block block_id = get >>= lookup_id block_id . State.state_blocks . state_ui
get_track track_id = get >>= lookup_id track_id . State.state_tracks . state_ui

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- ** merge

d_merge :: (Monad m) => [[Score.Event]] -> m [Score.Event]
d_merge = return . merge_events

merge_events :: [[Score.Event]] -> [Score.Event]
merge_events = foldr (Seq.merge_by (compare `on` Score.event_start)) []

-- | Set instrument on the given events.
d_instrument inst events = return $
    map (\evt -> evt { Score.event_instrument = Just inst }) events

-- * utils

-- | General purpose iterator over events.
--
-- It's like 'map_state_m' but sets the current event stack before operating on
-- each event, so that Derive.warn can use it.  In addition, EventErrors are
-- caught and turned into warnings.  Events that threw aren't included in the
-- output.
map_events state f event_of xs =
    fmap Maybe.catMaybes (Util.Control.map_state_m state apply xs)
    where
    apply st x = do
        set_event_stack (Score.event_stack (event_of x))
        val <- catch_event (f st x)
        return $ case val of
            Nothing -> (st, Nothing)
            Just val -> (st, Just val)
