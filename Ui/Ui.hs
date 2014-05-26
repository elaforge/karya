-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
module Ui.Ui (Channel, event_loop, send_action, quit_ui_thread) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Foreign

import Util.Control
import qualified Util.Log as Log
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiMsgC as UiMsgC
import qualified Ui.Util as Util


-- | Channel to communicate with the FLTK event loop.  Yes it's not a real
-- channel, but I want to get all actions in one go, and an MVar is suitable
-- for that.
type Channel = MVar.MVar [IO ()]

-- | Putting something into this mvar signals the UI thread to quit.
type QuitRequest = MVar.MVar ()

-- | Run the 'app' thread, passing it a channel that produces msgs, and go
-- into the UI polling loop.  This is intended to be run from the main thread,
-- since some UIs don't work properly unless run from the main thread.
-- When 'app' exits, the ui loop will be aborted.
event_loop :: Channel -> QuitRequest -> STM.TChan UiMsg.UiMsg -> IO ()
event_loop ui_chan quit_request msg_chan = do
    finalizer <- c_make_free_fun_ptr Util.free_fun_ptr
    c_initialize finalizer
    while_ (fmap not (MVar.isEmptyMVar quit_request)) $
        fltk_event_loop ui_chan msg_chan

-- | When I do anything that will destroy previous callbacks, I have to pass
-- yet another callback which will be used to mark the old callbacks as done,
-- so that the haskell GC knows it can collect the data those callbacks use.
type FunPtrFinalizer a = Foreign.FunPtr a -> IO ()
foreign import ccall "wrapper"
    c_make_free_fun_ptr :: FunPtrFinalizer a
        -> IO (Foreign.FunPtr (FunPtrFinalizer a))

-- | Send the UI to the ui thread and run it, returning its result.
send_action :: Channel -> IO () -> IO ()
send_action ui_chan act = do
    MVar.modifyMVar_ ui_chan $ return . (act:)
    awake

-- | The FLTK event loop.
fltk_event_loop :: Channel -> STM.TChan UiMsg.UiMsg -> IO ()
fltk_event_loop acts_mvar msg_chan = do
    wait
    -- I think that fltk will wake up once for every call to awake, so I
    -- shouldn't have to worry about another awake call coming in right
    -- here.
    handle_actions acts_mvar
    ui_msgs <- UiMsgC.get_ui_msgs
    STM.atomically $ mapM_ (STM.writeTChan msg_chan) ui_msgs

-- | Synchronously take actions out of the 'Channel' and run them.  This could
-- be asynchronous, but this way if the FLTK event loop wedges up then the UI
-- will also wedge up.  That's not exactly good, but it lets me know something
-- has gone wrong quickly.
handle_actions :: Channel -> IO ()
handle_actions acts_mvar = MVar.modifyMVar_ acts_mvar $ \acts -> do
    -- Since acts are added to the front, reverse them before executing.
    sequence_ (reverse acts)
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.error $ "exception in event_loop: " ++ show exc
    return []

quit_ui_thread :: QuitRequest -> IO ()
quit_ui_thread quit_request = do
    MVar.tryTakeMVar quit_request
    awake -- get it out of wait

foreign import ccall "initialize"
    c_initialize :: Foreign.FunPtr (FunPtrFinalizer a) -> IO ()
foreign import ccall "ui_wait" wait :: IO ()
foreign import ccall "ui_awake" awake :: IO ()
