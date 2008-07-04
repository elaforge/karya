{-# LANGUAGE ForeignFunctionInterface #-}
module Instrument.BrowserC where
import Foreign
import Foreign.C

import qualified Util.Fltk as Fltk

#include "interface.h"

type Window = Fltk.Window MsgType


create :: Int -> Int -> Int -> Int -> IO (Fltk.Window MsgType)
create x y w h = do
    (Fltk.create_window decode_type c_create_browser x y w h)

foreign import ccall "create_browser"
    c_create_browser :: CInt -> CInt -> CInt -> CInt -> FunPtr Fltk.MsgCallback
        -> IO (Ptr Window)

insert_line win n line = withCString line $ \linep ->
    c_insert_line (Fltk.win_p win) (fromIntegral n) linep
remove_line win n = c_remove_line (Fltk.win_p win) (fromIntegral n)
set_info win info = withCString info $ \infop ->
    c_set_info (Fltk.win_p win) infop

foreign import ccall "insert_line"
    c_insert_line :: Ptr Window -> CInt -> CString -> IO ()
foreign import ccall "remove_line" c_remove_line :: Ptr Window -> CInt -> IO ()
foreign import ccall "set_info" c_set_info :: Ptr Window -> CString -> IO ()

data MsgType = Select | Choose | Query | Unknown CInt deriving (Show)

decode_type msg_type = case msg_type of
    (#const msg_select) -> Select
    (#const msg_choose) -> Choose
    (#const msg_query) -> Query
    _ -> Unknown msg_type
