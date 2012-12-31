{-# LANGUAGE ScopedTypeVariables #-}
module Ui.Ui (event_loop, send_action, quit_ui_thread) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Foreign
import System.IO.Unsafe as Unsafe

import qualified Util.Control as Control
import qualified Util.Log as Log
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiMsgC as UiMsgC
import qualified Ui.Util as Util


-- | Global channel to the ui thread.  TODO pass this as an argument?
global_acts_mvar :: Actions
{-# NOINLINE global_acts_mvar #-}
global_acts_mvar = Unsafe.unsafePerformIO (MVar.newMVar [])

type Actions = MVar.MVar [IO ()]

-- | Putting something into this mvar signals the UI thread to quit.
type QuitRequest = MVar.MVar ()

-- | Run the 'app' thread, passing it a channel that produces msgs, and go
-- into the UI polling loop.  This is intended to be run from the main thread,
-- since some UIs don't work properly unless run from the main thread.
-- When 'app' exits, the ui loop will be aborted.
event_loop :: QuitRequest -> STM.TChan UiMsg.UiMsg -> IO ()
event_loop quit_request msg_chan = do
    finalizer <- c_make_free_fun_ptr Util.free_fun_ptr
    c_initialize finalizer
    Control.while_ (fmap not (MVar.isEmptyMVar quit_request)) $
        poll_loop global_acts_mvar msg_chan

-- | When I do anything that will destroy previous callbacks, I have to pass
-- yet another callback which will be used to mark the old callbacks as done,
-- so that the haskell GC knows it can collect the data those callbacks use.
type FunPtrFinalizer a = Foreign.FunPtr a -> IO ()
foreign import ccall "wrapper"
    c_make_free_fun_ptr :: FunPtrFinalizer a
        -> IO (Foreign.FunPtr (FunPtrFinalizer a))

-- | Send the UI to the ui thread and run it, returning its result.
send_action :: IO () -> IO ()
send_action act = add_act global_acts_mvar act >> awake

add_act :: Actions -> IO () -> IO ()
add_act acts_mvar x = MVar.modifyMVar_ acts_mvar (return . (x:))

-- | The ui's polling cycle.
poll_loop :: Actions -> STM.TChan UiMsg.UiMsg -> IO ()
poll_loop acts_mvar msg_chan = do
    wait
    -- I think that fltk will wake up once for every call to awake, so I
    -- shouldn't have to worry about another awake call coming in right
    -- here.
    handle_actions acts_mvar
    ui_msgs <- UiMsgC.get_ui_msgs
    STM.atomically (mapM_ (STM.writeTChan msg_chan) ui_msgs)

quit_ui_thread :: QuitRequest -> IO ()
quit_ui_thread quit_request = do
    MVar.tryTakeMVar quit_request
    awake -- get it out of wait

foreign import ccall "initialize"
    c_initialize :: Foreign.FunPtr (FunPtrFinalizer a) -> IO ()
foreign import ccall "ui_wait" wait :: IO ()
foreign import ccall "ui_awake" awake :: IO ()

handle_actions :: Actions -> IO ()
handle_actions acts_mvar = MVar.modifyMVar_ acts_mvar $ \acts -> do
    -- Since acts are added to the front, reverse them before executing.
    sequence_ (reverse acts)
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.error $ "exception in event_loop: " ++ show exc
    return []
