{- | Manage the function that maps incoming Ui.Ui.Msgs to Actions.

-}
module Msg.Responder where

import qualified Control.Monad as Monad
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Data.Set as Set
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Log as Log
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Action.Action as Action

import qualified Midi.Midi as Midi
import qualified Msg.Msg as Msg
import qualified Msg.Handler as Handler

-- | Do some IO (such as querying the UI), and return stuff to do.
type Responder = Handler.State -> Msg.Msg
    -> IO (Bool, Handler.State, [(Midi.Device, Midi.Message)], [Action.Action],
        [Log.Msg])

cmd_quit msg = return $ case msg of
    Msg.Ui (UiMsg.UiMsg {UiMsg.msg_data
        = UiMsg.Kbd {UiMsg.kbd_key = Key.Escape}}) -> Handler.Quit
    _ -> Handler.Continue

cmd_record_keys msg = do
    case msg of
        Msg.Ui (UiMsg.UiMsg {UiMsg.msg_type = UiMsg.TypeEvent,
            UiMsg.msg_data = UiMsg.Kbd key_state key})
                -> case key_state of
                    UiMsg.KeyDown -> do
                        modify_keys (Set.insert key)
                        keys <- Handler.keys_down
                        Handler.write_log Log.Debug ("keydown " ++ show keys)
                    UiMsg.KeyUp -> do
                        modify_keys (Set.delete key)
                        keys <- Handler.keys_down
                        Handler.write_log Log.Debug ("keyup " ++ show keys)
        _ -> return ()
    return Handler.Continue
    where
    modify_keys f = Handler.modify $ \st ->
        st {Handler.state_keys_down = f (Handler.state_keys_down st)}

responder_mvar :: MVar.MVar Responder
responder_mvar = Unsafe.unsafePerformIO (MVar.newMVar default_responder)
default_responder :: Responder
default_responder state msg = do
    putStrLn $ "msg: " ++ case msg of
        Msg.Ui msg -> UiMsg.pretty_ui_msg msg
        _ -> show msg
    let
        quit = case msg of 
            Msg.Ui (UiMsg.UiMsg {UiMsg.msg_data
                = UiMsg.Kbd {UiMsg.kbd_key = Key.Escape}}) -> True
            _ -> False
    return (quit, state, [], [], [])

-- | Replace the responder with the given one.
set_responder :: Responder -> IO ()
set_responder responder
    = MVar.modifyMVar_ responder_mvar (return . const responder)

-- | Runs in a loop, reading and handling msgs.
responder_thread :: IO Msg.Msg -> (Midi.CompleteMessage -> IO ()) -> IO ()
responder_thread = responder_loop Handler.initial_state

responder_loop state get_msg write_midi = do
    responder <- MVar.readMVar responder_mvar
    msg <- get_msg
    -- later this is hardcoded list merged with Block and Track mappings
    let cmd_stack = [cmd_record_keys, cmd_quit]
    (res, state') <- do_cmds write_midi state msg cmd_stack
    case res of
        Handler.Quit -> return ()
        _ -> responder_loop state' get_msg write_midi

do_cmds write_midi state msg [] = return (Handler.Done, state)
do_cmds write_midi state msg (c:cmds) = do
    (res, state') <- do_cmd write_midi state msg c
    case res of
        Handler.Continue -> do_cmds write_midi state' msg cmds
        _ -> return (res, state')

do_cmd write_midi state msg cmd = do
    (res, state', midi_msgs, actions, log_msgs) <- Handler.run cmd state msg
    sequence_ [write_midi (dev, 0, msg) | (dev, msg) <- midi_msgs]
    mapM_ Action.record actions
    mapM_ Log.write log_msgs
    return (res, state')

-- | Pass to 'responder_thread'
read_msg_chans :: TChan.TChan UiMsg.UiMsg -> TChan.TChan Midi.CompleteMessage
    -> IO Msg.Msg
read_msg_chans ui_chan midi_chan = STM.atomically $
    fmap Msg.Ui (TChan.readTChan ui_chan)
    `STM.orElse` fmap Msg.Midi (TChan.readTChan midi_chan)
