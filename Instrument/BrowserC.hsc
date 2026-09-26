-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.BrowserC where
import Data.Text (Text)

import ForeignC
import qualified Util.FFI as FFI
import qualified Util.Fltk as Fltk

#include "Instrument/interface.h"


type Window = Fltk.Window MsgType

create :: Int -> Int -> Int -> Int -> Fltk.Fltk (Fltk.Window MsgType)
create x y w h = Fltk.fltk $
    Fltk.createWindow decodeType cCreateBrowser x y w h "instrument browser"

foreign import ccall "create_browser"
    cCreateBrowser :: CInt -> CInt -> CInt -> CInt -> CString
        -> FunPtr Fltk.MsgCallback -> IO (Ptr Window)

insertLine :: Window -> Int -> Text -> Fltk.Fltk ()
insertLine win n line = Fltk.fltk $ FFI.withText line $ \linep ->
    cInsertLine (Fltk.winPtr win) (fromIntegral n) linep
foreign import ccall "insert_line"
    cInsertLine :: Ptr Window -> CInt -> CString -> IO ()

removeLine :: Window -> Int -> Fltk.Fltk ()
removeLine win n = Fltk.fltk $
    cRemoveLine (Fltk.winPtr win) (fromIntegral n)

foreign import ccall "remove_line" cRemoveLine :: Ptr Window -> CInt -> IO ()

setInfo :: Window -> Text -> Fltk.Fltk ()
setInfo win info = Fltk.fltk $ FFI.withText info $ \infop ->
    cSetInfo (Fltk.winPtr win) infop

foreign import ccall "set_info" cSetInfo :: Ptr Window -> CString -> IO ()


-- * implementation

data MsgType = Select | Choose | Query | Unknown CInt deriving (Show)

decodeType :: CInt -> MsgType
decodeType msgType = case msgType of
    (#const msg_select) -> Select
    (#const msg_choose) -> Choose
    (#const msg_query) -> Query
    _ -> Unknown msgType
