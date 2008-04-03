{-# LANGUAGE ForeignFunctionInterface #-}
module Ui.Initialize (initialize, send_action) where
import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import System.IO.Unsafe

import qualified Util.Thread as Thread
import qualified Util.Log as Log
import qualified Ui.UiMsg as UiMsg


ui_thread_id :: MVar.MVar Concurrent.ThreadId
ui_thread_id = unsafePerformIO MVar.newEmptyMVar

-- Global channel to the ui thread.  I tried passing this as an argument,
-- but for some reason when I do that it loses its polymorphism, even with a
-- rank2 type.
acts_mvar :: MVar.MVar [a]
acts_mvar = unsafePerformIO (MVar.newMVar [])

-- | Run the 'app' thread, passing it a channel that produces msgs, and go
-- into the UI polling loop.  This is intended to be run from the main thread,
-- since some UIs don't work properly unless run from the main thread.
-- When 'app' exits, the ui loop will be aborted.
initialize :: (STM.TChan UiMsg.UiMsg -> IO ()) -> IO ()
initialize app = do
    msg_chan <- STM.newTChanIO
    Thread.start_os_thread "app" $ app_wrapper acts_mvar (app msg_chan)
    th_id <- Concurrent.myThreadId
    MVar.putMVar ui_thread_id th_id
    poll_loop acts_mvar msg_chan
    `Exception.finally`
    (Log.notice "main thread quitting")

app_wrapper acts_mvar app = do
    Exception.catch app (\exc -> Log.error ("caught exception: " ++ show exc))
    kill_ui_thread acts_mvar

-- | Send the UI to the ui thread and run it, returning its result.
-- send_action :: MVar.MVar [IO ()] -> Send
send_action :: IO a -> IO a
send_action act = do
    retbox <- MVar.newEmptyMVar
    add_act acts_mvar (act >>= MVar.putMVar retbox)
    awake
    MVar.takeMVar retbox
add_act acts_mvar x = MVar.modifyMVar_ acts_mvar (return . (x:))

-- | The ui's polling cycle.
poll_loop acts_mvar msg_chan = do
    c_initialize
    Monad.forever $ do
        wait
        -- I think that fltk will wake up once for every call to awake, so I
        -- shouldn't have to worry about another awake call coming in right
        -- here.
        handle_actions acts_mvar
        ui_msgs <- UiMsg.take_ui_msgs
        STM.atomically (mapM_ (STM.writeTChan msg_chan) ui_msgs)

kill_ui_thread acts_mvar = do
    th_id <- MVar.readMVar ui_thread_id
    -- Send a kill over to be handled so I know it'll be awake, but don't wait
    -- for a return val.
    add_act acts_mvar (Concurrent.killThread th_id)
    awake -- get it out of wait

foreign import ccall "initialize" c_initialize :: IO ()
-- "wait" must be safe since it blocks.  Unsafe FFI calls block all haskell
-- threads.
foreign import ccall "ui_wait" wait :: IO ()
foreign import ccall "ui_awake" awake :: IO ()

handle_actions acts_mvar = MVar.modifyMVar_ acts_mvar $ \acts ->
    sequence_ acts >> return []
