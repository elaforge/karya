module LogView.LogViewC where
import Foreign
import Foreign.C

import qualified Util.Fltk as Fltk

#include "LogView/interface.h"


type Window = Fltk.Window MsgType

create :: Int -> Int -> Int -> Int -> String -> Int -> IO Window
create x y w h title max_bytes =
    Fltk.create_window decode_type (c_create_logview (fromIntegral max_bytes))
        x y w h title

foreign import ccall "create_logview"
    c_create_logview :: CInt -> CInt -> CInt -> CInt -> CInt -> CString
        -> FunPtr Fltk.MsgCallback -> IO (Ptr Window)

append_log :: Window -> String -> String -> IO ()
append_log view msg style =
    withCString msg $ \msgp -> withCString style $ \stylep ->
        c_append_log (Fltk.win_p view) msgp stylep
foreign import ccall "append_log"
    c_append_log :: Ptr Window -> CString -> CString -> IO ()

clear_logs :: Window -> IO ()
clear_logs view = c_clear_logs (Fltk.win_p view)
foreign import ccall "clear_logs"
    c_clear_logs :: Ptr Window -> IO ()

set_status :: Window -> String -> String -> IO ()
set_status view status style = withCString status $ \statusp ->
    withCString style $ \stylep -> c_set_status (Fltk.win_p view) statusp stylep
foreign import ccall "set_status"
    c_set_status :: Ptr Window -> CString -> CString -> IO ()

set_filter :: Window -> String -> IO ()
set_filter view filt = withCString filt $ \filtp ->
    c_set_filter (Fltk.win_p view) filtp
foreign import ccall "set_filter"
    c_set_filter :: Ptr Window -> CString -> IO ()


-- * implementation

data MsgType = Click | Command | Unknown CInt

decode_type msg_type = case msg_type of
    (#const cb_click) -> Click
    (#const cb_command) -> Command
    _ -> Unknown msg_type
