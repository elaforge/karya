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


actions :: MVar.MVar [a]
actions = unsafePerformIO (MVar.newMVar [])
ui_thread_id :: MVar.MVar Concurrent.ThreadId
ui_thread_id = unsafePerformIO MVar.newEmptyMVar

-- | Start up the ui thread, return a channel that will produce msgs.
initialize :: IO (TChan.TChan Msg)
initialize = do
    msg_chan <- TChan.newTChanIO
    th_id <- Util.start_os_thread "ui handler" (ui_thread actions msg_chan)
    MVar.putMVar ui_thread_id th_id
    return msg_chan

add_act x = MVar.modifyMVar_ actions (return . (x:))

send_action :: UI a -> IO a
send_action act = do
    retbox <- MVar.newEmptyMVar
    add_act (act >>= MVar.putMVar retbox)
    -- putStrLn "wrote act"
    awake
    -- putStrLn "wait result"
    MVar.takeMVar retbox

-- The ui thread's polling cycle.
ui_thread actions msg_chan = do
    c_initialize
    Monad.forever $ do
        wait
        -- putStrLn "woke up"
        handle_actions actions
        handle_msgs msg_chan

kill_ui_thread = do
    th_id <- MVar.readMVar ui_thread_id
    -- Send a kill over to be handled so I know it'll be awake, but don't wait
    -- for a return val.
    add_act (Concurrent.killThread th_id)
    awake -- get it out of wait

foreign import ccall unsafe "initialize" c_initialize :: IO ()
-- "wait" must be safe since it blocks.
foreign import ccall safe "ui_msg_wait" wait :: IO ()
foreign import ccall unsafe "ui_msg_awake" awake :: IO ()

handle_actions actions = MVar.modifyMVar_ actions $ \acts ->
    sequence_ acts >> return []

data Msg = MUi UiMsg.UiMsg | MMidi MidiMsg.MidiMsg | MOsc OscMsg.OscMsg
    deriving (Show)

handle_msgs msg_chan = do
    ui <- UiMsg.take_ui_msgs
    midi <- MidiMsg.take_midi_msgs
    osc <- OscMsg.take_osc_msgs
    mapM_ (write msg_chan) (map MUi ui ++ map MMidi midi ++ map MOsc osc)

write tchan = STM.atomically . TChan.writeTChan tchan
