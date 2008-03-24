{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- | Provide 'HandlerM', which msg handling functions run in.  It gives access
to some general 'State', and output lists for midi thru, actions, and log msgs,
and provides a simple "abort" exception.
-}
module Msg.Handler where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer as Writer
import qualified Data.Set as Set

import qualified Ui.Key as Key
import qualified Util.Log as Log
import qualified Midi.Midi as Midi

import qualified Ui.Block as Block
import qualified Action.Action as Action

import qualified Msg.Msg as Msg

-- An exception with no information.
data Abort = Abort deriving (Show)
abort = HandlerM $ Error.throwError Abort
instance Error.Error Abort where
    noMsg = Abort


data Modifier = KeyMod Key.Key
    | MouseMod Int
    -- | Only chan and key are stored.  While it may be useful to map according
    -- to the dev, this code doesn't know which devs are available.  Block or
    -- track level handlers can query the dev themselves.
    | MidiMod Midi.Channel Midi.Key
    deriving (Eq, Ord, Show)
data State = State {
    -- | Map of keys held down.  Maintained by Responder.cmd_record_keys.
    state_keys_down :: Set.Set Modifier
    -- | Block that has focus.  Maintained by Responder.cmd_block_focus,
    -- so non-ui msgs can know what block is active.
    , state_current_block :: Maybe Block.View
    } deriving (Show)
initial_state = State Set.empty Nothing

newtype HandlerM a = HandlerM
    (State.StateT State
        (Writer.WriterT MidiThru
            (Writer.WriterT [Action.Action]
                (Writer.WriterT [Log.Msg]
                    (Error.ErrorT Abort IO)))) a)
    deriving (Functor, Monad, State.MonadIO)
    -- The MonadIO derivation means they can run any IO action in HandlerM.
    -- It might be a little nicer to restrict to UI actions, but I'm not
    -- bothering right now.
unwrap (HandlerM x) = x

data HandlerResult = Done | Continue | Quit deriving (Eq, Show)
type Handler = Msg.Msg -> HandlerM HandlerResult
type MidiThru = [(Midi.Device, Midi.Message)]

-- | Log a msg.
write_msg msg = HandlerM $ (lift.lift.lift) (Writer.tell [msg])
write_log prio msg = HandlerM $ do
    msg <- Trans.liftIO (Log.msg_io prio msg)
    unwrap (write_msg msg)

-- | Send midi data thru.
midi_thru :: Midi.Device -> Midi.Message -> HandlerM ()
midi_thru dev msg = HandlerM $ lift (Writer.tell [(dev, msg)])

-- | Emit an Action.
action :: Action.Action -> HandlerM ()
action act = HandlerM $ (lift.lift) (Writer.tell [act])

get :: HandlerM State
get = HandlerM State.get
put :: State -> HandlerM ()
put st = HandlerM (State.put st)
modify f = HandlerM (State.modify f)

-- | Get current block, if there isn't one, abort the handler.
current_block :: HandlerM Block.View
current_block = do
    st <- get
    case state_current_block st of
        Nothing -> abort
        Just view -> return view

-- | Keys currently held down.
keys_down :: HandlerM [Modifier]
keys_down = do
    st <- get
    return (Set.elems (state_keys_down st))

-- I allow any kind of IO in HandlerM, but even if I never have a UI monad
-- it's still handy to differentiate UI actions.
lift_ui :: Trans.MonadIO m => IO a -> m a
lift_ui = Trans.liftIO

run_st = flip State.runStateT
run_wr = Writer.runWriterT
-- run_handler :: State -> Handler
run_handler state =
    Error.runErrorT . run_wr . run_wr . run_wr . run_st state . unwrap

run :: Handler -> State -> Msg.Msg
    -> IO (HandlerResult, State, MidiThru, [Action.Action], [Log.Msg])
run cmd state msg = do
    val <- run_handler state (cmd msg)
    return $ case val of
        Left _ -> (Continue, state, [], [], [])
        Right ((((res, state), midi_msgs), actions), log_msgs) ->
            (res, state, midi_msgs, actions, log_msgs)
