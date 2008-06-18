{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
module LogViewer.LogViewC where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import Foreign
import Foreign.C

import qualified Util.Control

#include "interface.h"


-- TODO This is mostly the same as the Ui.Ui and fltk interface stuff.
-- Some of it could probably be extracted into a library.

data LogView = LogView {
    view_p :: Ptr LogView
    , view_chan :: STM.TChan Msg
    }
type MsgCallback = CInt -> CString -> IO ()
type Msg = (MsgType, String)

read_msg view = STM.readTChan (view_chan view)

-- | Enter the fltk event loop.  For portability, this should only be called
-- from the main thread.
run :: IO ()
run = do
    c_initialize
    Util.Control.while_ (fmap toBool c_has_windows) $ do
        c_wait
        handle_actions acts_mvar

handle_actions acts_mvar = Concurrent.modifyMVar_ acts_mvar $ \acts ->
    sequence_ (reverse acts) >> return []

send_action :: IO a -> IO ()
send_action act = do
    Concurrent.modifyMVar_ acts_mvar (return . (act:))
    c_awake

acts_mvar :: Concurrent.MVar [a]
acts_mvar = unsafePerformIO (Concurrent.newMVar [])

foreign import ccall "initialize" c_initialize :: IO ()
foreign import ccall "ui_wait" c_wait :: IO ()
foreign import ccall "ui_awake" c_awake :: IO ()
foreign import ccall "has_windows" c_has_windows :: IO CInt

create_logview :: Int -> Int -> Int -> Int -> IO LogView
create_logview x y w h = do
    chan <- STM.newTChanIO
    cb <- c_make_msg_callback (cb_msg_callback chan)
    putStrLn "create"
    viewp <- c_create_logview (c x) (c y) (c w) (c h) cb
    return (LogView viewp chan)
    where c = fromIntegral

append_log :: LogView -> String -> String -> IO ()
append_log view msg style =
    withCString msg $ \msgp -> withCString style $ \stylep ->
        c_append_log (view_p view) msgp stylep

clear_logs :: LogView -> IO ()
clear_logs view = c_clear_logs (view_p view)

set_status :: LogView -> String -> IO ()
set_status view status = withCString status $ \statusp ->
    c_set_status (view_p view) statusp


-- * implementation

cb_msg_callback :: STM.TChan Msg -> MsgCallback
cb_msg_callback msg_chan msg_type msgp = do
    msg <- peekCString msgp
    STM.atomically $ STM.writeTChan msg_chan (decode_type msg_type, msg)

data MsgType = Click | Command | Unknown CInt

decode_type msg_type = case msg_type of
    (#const cb_click) -> Click
    (#const cb_command) -> Command
    _ -> Unknown msg_type

foreign import ccall "wrapper"
    c_make_msg_callback :: MsgCallback -> IO (FunPtr MsgCallback)

foreign import ccall "create_logview"
    c_create_logview :: CInt -> CInt -> CInt -> CInt -> FunPtr MsgCallback
        -> IO (Ptr LogView)
foreign import ccall "append_log"
    c_append_log :: Ptr LogView -> CString -> CString -> IO ()
foreign import ccall "clear_logs"
    c_clear_logs :: Ptr LogView -> IO ()

foreign import ccall "set_status"
    c_set_status :: Ptr LogView -> CString -> IO ()
