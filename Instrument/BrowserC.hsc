-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.BrowserC where
import Data.Text (Text)

import Util.ForeignC
import qualified Util.CUtil as CUtil
import qualified Util.Fltk as Fltk

#include "Instrument/interface.h"


type Window = Fltk.Window MsgType

create :: Int -> Int -> Int -> Int -> Fltk.Fltk (Fltk.Window MsgType)
create x y w h = Fltk.action $
    Fltk.create_window decode_type c_create_browser x y w h "instrument browser"

foreign import ccall "create_browser"
    c_create_browser :: CInt -> CInt -> CInt -> CInt -> CString
        -> FunPtr Fltk.MsgCallback -> IO (Ptr Window)

insert_line :: Window -> Int -> Text -> Fltk.Fltk ()
insert_line win n line = Fltk.action $ CUtil.withText line $ \linep ->
    c_insert_line (Fltk.win_ptr win) (fromIntegral n) linep
foreign import ccall "insert_line"
    c_insert_line :: Ptr Window -> CInt -> CString -> IO ()

remove_line :: Window -> Int -> Fltk.Fltk ()
remove_line win n = Fltk.action $
    c_remove_line (Fltk.win_ptr win) (fromIntegral n)

foreign import ccall "remove_line" c_remove_line :: Ptr Window -> CInt -> IO ()

set_info :: Window -> Text -> Fltk.Fltk ()
set_info win info = Fltk.action $ CUtil.withText info $ \infop ->
    c_set_info (Fltk.win_ptr win) infop

foreign import ccall "set_info" c_set_info :: Ptr Window -> CString -> IO ()


-- * implementation

data MsgType = Select | Choose | Query | Unknown CInt deriving (Show)

decode_type :: CInt -> MsgType
decode_type msg_type = case msg_type of
    (#const msg_select) -> Select
    (#const msg_choose) -> Choose
    (#const msg_query) -> Query
    _ -> Unknown msg_type
