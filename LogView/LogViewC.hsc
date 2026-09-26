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
create x y w h title maxBytes = Fltk.fltk $
    Fltk.createWindow decodeType (cCreateLogview (fromIntegral maxBytes))
        x y w h title

foreign import ccall "create_logview"
    cCreateLogview :: CInt -> CInt -> CInt -> CInt -> CInt -> CString
        -> FunPtr Fltk.MsgCallback -> IO (Ptr Window)

appendLog :: Window -> UTF8.ByteString -> UTF8.ByteString -> Fltk.Fltk ()
appendLog view msg style = Fltk.fltk $
    B.useAsCString msg $ \msgp -> B.useAsCString style $ \stylep ->
        cAppendLog (Fltk.winPtr view) msgp stylep
foreign import ccall "append_log"
    cAppendLog :: Ptr Window -> CString -> CString -> IO ()

clearLogs :: Window -> Fltk.Fltk ()
clearLogs view = Fltk.fltk $ cClearLogs (Fltk.winPtr view)
foreign import ccall "clear_logs" cClearLogs :: Ptr Window -> IO ()

setStatus :: Window -> UTF8.ByteString -> UTF8.ByteString -> Fltk.Fltk ()
setStatus view status style = Fltk.fltk $
    B.useAsCString status $ \statusp -> B.useAsCString style $ \stylep ->
        cSetStatus (Fltk.winPtr view) statusp stylep
foreign import ccall "set_status"
    cSetStatus :: Ptr Window -> CString -> CString -> IO ()

setFilter :: Window -> Text.Text -> Fltk.Fltk ()
setFilter view filt = Fltk.fltk $ withCString (Text.unpack filt) $ \filtp ->
    cSetFilter (Fltk.winPtr view) filtp
foreign import ccall "set_filter"
    cSetFilter :: Ptr Window -> CString -> IO ()

bringToFront :: Window -> Fltk.Fltk ()
bringToFront view = Fltk.fltk $ cBringToFront (Fltk.winPtr view)
foreign import ccall "bring_to_front"
    cBringToFront :: Ptr Window -> IO ()

-- * implementation

data MsgType = Click | Command | Unknown CInt

decodeType msgType = case msgType of
    (#const cb_click) -> Click
    (#const cb_command) -> Command
    _ -> Unknown msgType
