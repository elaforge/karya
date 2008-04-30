{-# LANGUAGE ForeignFunctionInterface #-}
module Ui.UiMsgC where
import Control.Monad
import Foreign
import Foreign.C
import qualified Data.Maybe as Maybe

import qualified Ui.BlockC as BlockC

import qualified Ui.Key as Key
import qualified Ui.Block as Block
import qualified Ui.UiMsg as UiMsg


get_ui_msgs :: IO [UiMsg.UiMsg]
get_ui_msgs = with nullPtr $ \msgspp -> do
    count <- c_get_ui_msgs msgspp
    msgsp <- peek msgspp
    msgs <- peekArray (fromIntegral count) msgsp
    c_clear_ui_msgs
    return msgs

foreign import ccall unsafe "get_ui_msgs"
    c_get_ui_msgs :: Ptr (Ptr UiMsg.UiMsg) -> IO CInt
foreign import ccall unsafe "clear_ui_msgs" c_clear_ui_msgs :: IO ()

-- * Storable

#include "c_interface.h"

instance Storable UiMsg.UiMsg where
    sizeOf _ = #size UiMsg
    alignment _ = undefined
    peek = peek_msg
    poke = error "no poke for UiMsg"

peek_msg msgp = do
    -- MsgEvent data
    type_num <- (#peek UiMsg, type) msgp :: IO CInt
    event <- (#peek UiMsg, event) msgp :: IO CInt
    button <- (#peek UiMsg, button) msgp :: IO CInt
    clicks <- (#peek UiMsg, clicks) msgp :: IO CInt
    is_click <- (#peek UiMsg, is_click) msgp :: IO CInt
    x <- (#peek UiMsg, x) msgp :: IO CInt
    y <- (#peek UiMsg, y) msgp :: IO CInt
    key <- (#peek UiMsg, key) msgp :: IO CInt
    let evt_args = (i event, i button, i clicks, is_click /= 0, i x, i y, i key)

    -- UiUpdate args
    ctext <- (#peek UiMsg, update_text) msgp :: IO CString
    text <- maybePeek peekCString ctext
    width <- (#peek UiMsg, update_width) msgp :: IO CInt
    czoom <- (#peek UiMsg, update_zoom) msgp :: IO (Ptr Block.Zoom)
    zoom <- maybePeek peek czoom
    crect <- (#peek UiMsg, update_rect) msgp :: IO (Ptr Block.Rect)
    rect <- maybePeek peek crect
    let update_args = (text, i width, zoom, rect)

    -- UiMsg Context
    viewp <- (#peek UiMsg, view) msgp :: IO (Ptr BlockC.CView)
    has_tracknum <- (#peek UiMsg, has_tracknum) msgp :: IO CChar
    tracknum <- (#peek UiMsg, tracknum) msgp :: IO CInt
    has_pos <- (#peek UiMsg, has_pos) msgp :: IO CChar
    pos <- (#peek UiMsg, pos) msgp

    context <- make_context viewp has_tracknum tracknum has_pos pos
    return $ make_msg type_num context evt_args update_args
    where i = fromIntegral

make_msg type_num context evt_args update_args =
    UiMsg.UiMsg context $ case type_num of
        (#const UiMsg::msg_event) -> UiMsg.MsgEvent (decode_msg_event evt_args)
        (#const UiMsg::msg_close) -> UiMsg.MsgClose
        _ -> UiMsg.UiUpdate (decode_update type_num update_args)

decode_update typ (text, width, zoom, rect) = case typ of
    (#const UiMsg::msg_input) -> UiMsg.UpdateInput (Maybe.fromMaybe "" text)
    (#const UiMsg::msg_track_scroll) -> UiMsg.UpdateTrackScroll width
    (#const UiMsg::msg_zoom) -> UiMsg.UpdateZoom
        (Maybe.fromMaybe (BlockC.throw "UpdateZoom with null zoom") zoom)
    (#const UiMsg::msg_view_resize) -> UiMsg.UpdateViewResize
        (Maybe.fromMaybe (BlockC.throw "UpdateViewResize with null rect") rect)
    (#const UiMsg::msg_track_width) -> UiMsg.UpdateTrackWidth width
    -- msg_event and msg_close handled above
    _ -> error $ "unknown UiMsg type: " ++ show typ

make_context viewp has_tracknum tracknum has_pos pos
    | viewp == nullPtr = return $ context Nothing
    | otherwise = do
        view_id <- BlockC.get_id viewp
        return $ context (Just view_id)
    where
    context view = UiMsg.Context view
        (to_maybe has_tracknum (fromIntegral tracknum)) (to_maybe has_pos pos)
    to_maybe b val = if toBool b then Just val else Nothing

decode_msg_event (event, button, clicks, is_click, x, y, key) = msg
    where
    mouse state = UiMsg.Mouse state (x, y) 0 False
    kbd state = UiMsg.Kbd state (Key.Unknown 0)
    aux = UiMsg.AuxMsg
    partial_msg = case event of
        (#const FL_PUSH) -> mouse (UiMsg.MouseDown button)
        (#const FL_DRAG) -> mouse (UiMsg.MouseDrag button)
        (#const FL_RELEASE) -> mouse (UiMsg.MouseUp button)
        (#const FL_MOVE) -> mouse UiMsg.MouseMove
        (#const FL_KEYDOWN) -> kbd UiMsg.KeyDown
        (#const FL_KEYUP) -> kbd UiMsg.KeyUp

        (#const FL_ENTER) -> aux UiMsg.Enter
        (#const FL_LEAVE) -> aux UiMsg.Leave
        (#const FL_FOCUS) -> aux UiMsg.Focus
        (#const FL_UNFOCUS) -> aux UiMsg.Unfocus
        (#const FL_SHORTCUT) -> aux UiMsg.Shortcut
        (#const FL_DEACTIVATE) -> aux UiMsg.Deactivate
        (#const FL_ACTIVATE) -> aux UiMsg.Activate
        (#const FL_HIDE) -> aux UiMsg.Hide
        (#const FL_SHOW) -> aux UiMsg.Show
        _ -> UiMsg.Unhandled event
    msg = case partial_msg of
        UiMsg.Mouse {} -> partial_msg
            { UiMsg.mouse_clicks = clicks, UiMsg.mouse_is_click = is_click }
        UiMsg.Kbd state _ -> UiMsg.Kbd state (Key.decode_key key)
        _ -> partial_msg
