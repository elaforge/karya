{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Ui.Ui (initialize, send_action) where
import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import System.IO.Unsafe

import Ui.Types
import qualified Util.Thread as Thread
import qualified Util.Log as Log
import qualified Ui.UiMsg as UiMsg


actions :: MVar.MVar [a]
actions = unsafePerformIO (MVar.newMVar [])
ui_thread_id :: MVar.MVar Concurrent.ThreadId
ui_thread_id = unsafePerformIO MVar.newEmptyMVar

-- | Run the 'app' thread, passing it a channel that produces msgs, and go
-- into the UI polling loop.  This is intended to be run from the main thread,
-- since some UIs don't work properly unless run from the main thread.
-- When 'app' exits, the ui loop will be aborted.
initialize app = do
    msg_chan <- STM.newTChanIO
    Thread.start_os_thread "app" (app_wrapper (app msg_chan))
    th_id <- Concurrent.myThreadId
    MVar.putMVar ui_thread_id th_id
    poll_loop actions msg_chan
    `Exception.finally`
    (Log.notice "main thread quitting")

app_wrapper app = do
    Exception.catch app (\exc -> Log.error ("caught exception: " ++ show exc))
    kill_ui_thread

add_act x = MVar.modifyMVar_ actions (return . (x:))

-- | Send the UI to the ui thread and run it, returning its result.
send_action :: UI a -> IO a
send_action act = do
    retbox <- MVar.newEmptyMVar
    add_act (act >>= MVar.putMVar retbox)
    -- putStrLn "wrote act"
    awake
    -- putStrLn "wait result"
    MVar.takeMVar retbox

-- | The ui's polling cycle.
poll_loop actions msg_chan = do
    c_initialize
    Monad.forever $ do
        wait
        -- putStrLn "woke up"
        handle_actions actions
        ui_msgs <- UiMsg.take_ui_msgs
        STM.atomically (mapM_ (STM.writeTChan msg_chan) ui_msgs)

kill_ui_thread = do
    th_id <- MVar.readMVar ui_thread_id
    -- Send a kill over to be handled so I know it'll be awake, but don't wait
    -- for a return val.
    add_act (Concurrent.killThread th_id)
    awake -- get it out of wait

foreign import ccall unsafe "initialize" c_initialize :: IO ()
-- "wait" must be safe since it blocks.
foreign import ccall safe "ui_wait" wait :: IO ()
foreign import ccall unsafe "ui_awake" awake :: IO ()

handle_actions actions = MVar.modifyMVar_ actions $ \acts ->
    sequence_ acts >> return []
