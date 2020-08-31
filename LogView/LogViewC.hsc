-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module LogView.LogViewC where
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as Text

import ForeignC
import qualified Util.Fltk as Fltk

#include "LogView/interface.h"


type Window = Fltk.Window MsgType

create :: Int -> Int -> Int -> Int -> String -> Int -> Fltk.Fltk Window
create x y w h title max_bytes = Fltk.fltk $
    Fltk.create_window decode_type (c_create_logview (fromIntegral max_bytes))
        x y w h title

foreign import ccall "create_logview"
    c_create_logview :: CInt -> CInt -> CInt -> CInt -> CInt -> CString
        -> FunPtr Fltk.MsgCallback -> IO (Ptr Window)

append_log :: Window -> UTF8.ByteString -> UTF8.ByteString -> Fltk.Fltk ()
append_log view msg style = Fltk.fltk $
    B.useAsCString msg $ \msgp -> B.useAsCString style $ \stylep ->
        c_append_log (Fltk.win_ptr view) msgp stylep
foreign import ccall "append_log"
    c_append_log :: Ptr Window -> CString -> CString -> IO ()

clear_logs :: Window -> Fltk.Fltk ()
clear_logs view = Fltk.fltk $ c_clear_logs (Fltk.win_ptr view)
foreign import ccall "clear_logs" c_clear_logs :: Ptr Window -> IO ()

set_status :: Window -> UTF8.ByteString -> UTF8.ByteString -> Fltk.Fltk ()
set_status view status style = Fltk.fltk $
    B.useAsCString status $ \statusp -> B.useAsCString style $ \stylep ->
        c_set_status (Fltk.win_ptr view) statusp stylep
foreign import ccall "set_status"
    c_set_status :: Ptr Window -> CString -> CString -> IO ()

set_filter :: Window -> Text.Text -> Fltk.Fltk ()
set_filter view filt = Fltk.fltk $ withCString (Text.unpack filt) $ \filtp ->
    c_set_filter (Fltk.win_ptr view) filtp
foreign import ccall "set_filter"
    c_set_filter :: Ptr Window -> CString -> IO ()

bring_to_front :: Window -> Fltk.Fltk ()
bring_to_front view = Fltk.fltk $ c_bring_to_front (Fltk.win_ptr view)
foreign import ccall "bring_to_front"
    c_bring_to_front :: Ptr Window -> IO ()

-- * implementation

data MsgType = Click | Command | Unknown CInt

decode_type msg_type = case msg_type of
    (#const cb_click) -> Click
    (#const cb_command) -> Command
    _ -> Unknown msg_type
