{- | Manage the function that maps incoming Ui.Ui.Msgs to Actions.

-}
module Msg.Responder where

import Control.Monad
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Data.List as List
import qualified Data.Set as Set

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
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd _ Key.Escape)))
        -> Handler.Quit
    _ -> Handler.Continue

cmd_record_keys msg = do
    case msg of
        Msg.Ui (UiMsg.UiMsg ctx (UiMsg.MsgEvent evt)) -> case evt of
            UiMsg.Kbd UiMsg.KeyDown key ->
                insert_mod (Handler.KeyMod key)
            UiMsg.Kbd UiMsg.KeyUp key ->
                delete_mod (Handler.KeyMod key)
            UiMsg.Mouse {UiMsg.mouse_state = UiMsg.MouseDown btn} ->
                insert_mod (Handler.MouseMod btn (mouse_context ctx))
            UiMsg.Mouse {UiMsg.mouse_state = UiMsg.MouseUp btn} -> do
                mods <- Handler.keys_down
                delete_mod $ case List.find (has_button btn) mods of
                    Just mod -> mod
                    -- I don't think it will be found, but this will trigger
                    -- a warning at least.
                    Nothing -> Handler.MouseMod btn (mouse_context ctx)
            _ -> return ()
        Msg.Midi (_dev, _timestamp, msg) -> case msg of
            Midi.ChannelMessage chan (Midi.NoteOn key _vel) ->
                insert_mod (Handler.MidiMod chan key)
            Midi.ChannelMessage chan (Midi.NoteOff key _vel) ->
                delete_mod (Handler.MidiMod chan key)
            _ -> return ()
        _ -> return ()
    return Handler.Continue
    where
    mouse_context (UiMsg.Context
        {UiMsg.ctx_track = Just n, UiMsg.ctx_pos = Just pos}) = Just (n, pos)
    mouse_context _ = Nothing
    has_button btn (Handler.MouseMod btn2 _) = btn == btn2
    has_button btn _ = False
    insert_mod mod = do
        mods <- Handler.keys_down
        when (mod `elem` mods) $
            Handler.write_log Log.Warn
                ("keydown for " ++ show mod ++ " already in modifiers")
        modify_keys (Set.insert mod)
        Handler.write_log Log.Debug ("keydown " ++ show (mod:mods))
    delete_mod mod = do
        mods <- Handler.keys_down
        when (mod `notElem` mods) $
            Handler.write_log Log.Warn
                ("keyup for " ++ show mod ++ " not in modifiers")
        modify_keys (Set.delete mod)
        Handler.write_log Log.Debug ("keyup " ++ show (List.delete mod mods))
    modify_keys f = Handler.modify $ \st ->
        st {Handler.state_keys_down = f (Handler.state_keys_down st)}

cmd_print :: Handler.Handler
cmd_print msg = do
    -- Handler.write_log Log.Debug ("msg: " ++ show msg)
    return Handler.Continue

-- | Runs in a loop, reading and handling msgs.
responder_thread :: IO Msg.Msg -> (Midi.CompleteMessage -> IO ()) -> IO ()
responder_thread = responder_loop Handler.initial_state

responder_loop state get_msg write_midi = do
    msg <- get_msg
    -- later this is hardcoded list merged with Block and Track mappings
    let cmd_stack = [cmd_record_keys, cmd_print, cmd_quit]
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
