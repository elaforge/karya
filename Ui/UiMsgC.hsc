{-# LANGUAGE ForeignFunctionInterface #-}
module Ui.UiMsgC (get_ui_msgs) where
import Foreign
import Foreign.C
import qualified Data.Maybe as Maybe
import Util.Control

import Ui
import qualified Ui.BlockC as BlockC
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Types as Types


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
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable UiMsg.UiMsg where
    sizeOf _ = #size UiMsg
    alignment _ = #{alignment UiMsg}
    peek = peek_msg
    poke = error "UiMsg poke unimplemented"

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
    -- Big grody union, described in MsgCollector.h.
    width <- (#peek UiMsg, width_scroll_visible_track) msgp :: IO CInt
    visible_time <- (#peek UiMsg, visible_time) msgp :: IO CInt
    czoom <- (#peek UiMsg, update_zoom) msgp :: IO (Ptr Types.Zoom)
    zoom <- maybePeek peek czoom
    crect <- (#peek UiMsg, update_rect) msgp :: IO (Ptr Types.Rect)
    rect <- maybePeek peek crect
    let update_args = (text, i width, i visible_time, zoom, rect)

    -- UiMsg Context
    viewp <- (#peek UiMsg, view) msgp :: IO (Ptr BlockC.CView)
    has_tracknum <- toBool <$> ((#peek UiMsg, has_tracknum) msgp :: IO CChar)
    tracknum <- (#peek UiMsg, tracknum) msgp :: IO CInt
    has_pos <- toBool <$> ((#peek UiMsg, has_pos) msgp :: IO CChar)
    pos <- (#peek UiMsg, pos) msgp

    context <- make_context viewp has_tracknum tracknum has_pos pos
    return $ make_msg type_num context evt_args update_args
    where i = fromIntegral

-- | Data for a 'UiMsg.UiUpdate'.
type UpdateArgs = (Maybe String, Int, Int, Maybe Types.Zoom, Maybe Types.Rect)

-- | Data for a 'UiMsg.Data'.
type EventArgs = (Int, Int, Int, Bool, Int, Int, Int)

make_msg :: CInt -> UiMsg.Context -> EventArgs -> UpdateArgs -> UiMsg.UiMsg
make_msg type_num context evt_args update_args =
    UiMsg.UiMsg context $ case type_num of
        (#const UiMsg::msg_event) -> UiMsg.MsgEvent (decode_msg_event evt_args)
        (#const UiMsg::msg_close) -> UiMsg.MsgClose
        _ -> UiMsg.UiUpdate (decode_update type_num update_args)

decode_update :: CInt -> UpdateArgs -> UiMsg.UiUpdate
decode_update typ (text, width, visible_time, zoom, rect) = case typ of
    (#const UiMsg::msg_input) -> UiMsg.UpdateInput (Maybe.fromMaybe "" text)
    (#const UiMsg::msg_track_scroll) -> UiMsg.UpdateTrackScroll width
    (#const UiMsg::msg_zoom) -> UiMsg.UpdateZoom
        (require "UpdateZoom with null zoom" zoom)
    (#const UiMsg::msg_view_resize) -> UiMsg.UpdateViewResize
        (require "UpdateViewResize with null rect" rect)
        (width, visible_time)
    (#const UiMsg::msg_track_width) -> UiMsg.UpdateTrackWidth width
    (#const UiMsg::msg_screen_size) -> UiMsg.UpdateScreenSize
        width visible_time (require "ScreenSize without rect" rect)
    -- msg_event and msg_close handled above
    _ -> error $ "unknown UiMsg type: " ++ show typ

require :: String -> Maybe a -> a
require msg = Maybe.fromMaybe (BlockC.throw msg)

make_context :: Ptr BlockC.CView -> Bool -> CInt -> Bool -> ScoreTime
    -> IO UiMsg.Context
make_context viewp has_tracknum tracknum has_pos pos
    | viewp == nullPtr = return $ context Nothing
    | otherwise = do
        view_id <- BlockC.get_id viewp
        return $ context (Just view_id)
    where
    context view = UiMsg.Context view
        (to_maybe has_tracknum (fromIntegral tracknum)) (to_maybe has_pos pos)
    to_maybe b val = if b then Just val else Nothing

decode_msg_event :: EventArgs -> UiMsg.Data
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
