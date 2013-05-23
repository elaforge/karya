module Instrument.BrowserC where
import Data.Text (Text)
import Foreign
import Foreign.C

import qualified Util.Fltk as Fltk
import qualified Ui.Util

#include "Instrument/interface.h"


type Window = Fltk.Window MsgType

create :: Int -> Int -> Int -> Int -> IO (Fltk.Window MsgType)
create x y w h = Fltk.create_window decode_type c_create_browser x y w h
    "instrument browser"

foreign import ccall "create_browser"
    c_create_browser :: CInt -> CInt -> CInt -> CInt -> CString
        -> FunPtr Fltk.MsgCallback -> IO (Ptr Window)

insert_line :: Window -> Int -> Text -> IO ()
insert_line win n line = Ui.Util.withText line $ \linep ->
    c_insert_line (Fltk.win_p win) (fromIntegral n) linep
foreign import ccall "insert_line"
    c_insert_line :: Ptr Window -> CInt -> CString -> IO ()

remove_line :: Window -> Int -> IO ()
remove_line win n = c_remove_line (Fltk.win_p win) (fromIntegral n)
foreign import ccall "remove_line" c_remove_line :: Ptr Window -> CInt -> IO ()

set_info :: Window -> Text -> IO ()
set_info win info = Ui.Util.withText info $ \infop ->
    c_set_info (Fltk.win_p win) infop
foreign import ccall "set_info" c_set_info :: Ptr Window -> CString -> IO ()


-- * implementation

data MsgType = Select | Choose | Query | Unknown CInt deriving (Show)

decode_type :: CInt -> MsgType
decode_type msg_type = case msg_type of
    (#const msg_select) -> Select
    (#const msg_choose) -> Choose
    (#const msg_query) -> Query
    _ -> Unknown msg_type
