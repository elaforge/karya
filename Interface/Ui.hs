{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Interface.Ui where
import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import System.IO.Unsafe

import Interface.Types
import qualified Interface.Util as Util
import qualified Interface.UiMsg as UiMsg
import qualified Interface.MidiMsg as MidiMsg
import qualified Interface.OscMsg as OscMsg


global_send :: MVar.MVar a
global_send = unsafePerformIO MVar.newEmptyMVar
ui_thread_id :: MVar.MVar Concurrent.ThreadId
ui_thread_id = unsafePerformIO MVar.newEmptyMVar

-- | Start up the ui thread
initialize :: IO (TChan.TChan Msg)
initialize = do
    act_chan <- TChan.newTChanIO
    msg_chan <- TChan.newTChanIO
    th_id <- Util.start_os_thread "ui handler" (ui_thread act_chan msg_chan)
    MVar.putMVar ui_thread_id th_id
    MVar.putMVar global_send (do_send_action act_chan)
    return msg_chan

send_action :: UI a -> IO a
send_action act = do
    send <- MVar.readMVar global_send
    send act

do_send_action :: TChan.TChan (IO ()) -> IO a -> IO a
do_send_action act_chan act = do
    putStrLn "wrote act"
    retbox <- MVar.newEmptyMVar
    write act_chan (act >>= MVar.putMVar retbox)
    awake
    MVar.takeMVar retbox

-- The ui thread's polling cycle.
ui_thread act_chan msg_chan = do
    c_initialize
    Monad.forever $ do
        wait
        handle_actions act_chan
        handle_msgs msg_chan

kill_ui_thread = do
    -- close all open windows?
    th_id <- MVar.readMVar ui_thread_id
    Concurrent.killThread th_id
    awake -- get it out of wait

foreign import ccall unsafe "initialize" c_initialize :: IO ()
-- "wait" must be safe since it blocks.
foreign import ccall safe "ui_msg_wait" wait :: IO ()
foreign import ccall unsafe "ui_msg_awake" awake :: IO ()

handle_actions act_chan = STM.atomically (read_all act_chan) >>= sequence_

read_all chan = fmap Just (TChan.readTChan chan) `STM.orElse` return Nothing
    >>= maybe (return []) (\v -> fmap (v:) (read_all chan))


data Msg = MUi UiMsg.UiMsg | MMidi MidiMsg.MidiMsg | MOsc OscMsg.OscMsg
    deriving (Show)

handle_msgs msg_chan = do
    ui <- UiMsg.take_ui_msgs
    midi <- MidiMsg.take_midi_msgs
    osc <- OscMsg.take_osc_msgs
    mapM_ (write msg_chan) (map MUi ui ++ map MMidi midi ++ map MOsc osc)

write tchan = STM.atomically . TChan.writeTChan tchan
