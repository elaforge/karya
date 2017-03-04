-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.UiMsgC (get_ui_msgs) where
import Util.ForeignC

import qualified Ui.Block as Block
import qualified Ui.PtrMap as PtrMap
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Util as Util
import Types


get_ui_msgs :: IO [UiMsg.UiMsg]
get_ui_msgs = with nullPtr $ \msgspp -> do
    count <- c_get_ui_msgs msgspp
    msgsp <- peek msgspp
    msgs <- peekArray (int count) msgsp
    c_clear_ui_msgs
    return msgs

foreign import ccall unsafe "get_ui_msgs"
    c_get_ui_msgs :: Ptr (Ptr UiMsg.UiMsg) -> IO CInt
foreign import ccall unsafe "clear_ui_msgs" c_clear_ui_msgs :: IO ()

-- * Storable

#include "Ui/c_interface.h"

instance CStorable UiMsg.UiMsg where
    sizeOf _ = #size UiMsg
    alignment _ = alignment nullPtr
    poke = error "UiMsg poke unimplemented"
    peek = peek_msg

peek_msg :: Ptr UiMsg.UiMsg -> IO UiMsg.UiMsg
peek_msg msgp = do
    type_num <- (#peek UiMsg, type) msgp :: IO CInt
    (context, maybe_view_id) <- peek_context msgp
    UiMsg.UiMsg context <$> case type_num of
        (#const UiMsg::msg_event) -> UiMsg.MsgEvent <$> peek_event msgp
        (#const UiMsg::msg_screen_size) -> do
            rect <- peek =<< (#peek UiMsg, screen.rect) msgp
            screen <- int <$> (#peek UiMsg, screen.screen) msgp :: IO Int
            screens <- int <$> (#peek UiMsg, screen.screens) msgp :: IO Int
            return $ UiMsg.UpdateScreenSize screen screens rect
        _ -> do
            view_id <- maybe
                (error $ "got a null view_id from a ui update: "
                    ++ show type_num)
                return maybe_view_id
            UiMsg.UiUpdate view_id <$> peek_ui_update type_num msgp

peek_context :: Ptr UiMsg.UiMsg -> IO (UiMsg.Context, Maybe ViewId)
peek_context msgp = do
    focusp <- (#peek UiMsg, context.focus) msgp :: IO (Ptr PtrMap.CView)
    focus <- lookup_id focusp
    viewp <- (#peek UiMsg, context.view) msgp :: IO (Ptr PtrMap.CView)
    view <- lookup_id viewp

    track_type <- (#peek UiMsg, context.track_type) msgp :: IO CChar
    tracknum <- int <$> (#peek UiMsg, context.tracknum) msgp :: IO Int
    has_pos <- toBool <$> ((#peek UiMsg, context.has_pos) msgp :: IO CChar)
    cpos <- (#peek UiMsg, context.pos) msgp
    let track = decode_track track_type tracknum has_pos cpos
        is_floating_input = track_type == (#const UiMsg::track_floating_input)
    return (UiMsg.Context focus track is_floating_input, view)
    where
    lookup_id p
        | p == nullPtr = return Nothing
        | otherwise = PtrMap.lookup_id p

decode_track :: CChar -> Int -> Bool -> ScoreTime
    -> Maybe (TrackNum, UiMsg.Track)
decode_track track_type tracknum has_pos pos
    | track_type == (#const UiMsg::track_none) = Nothing
    | has_pos = if track_type == (#const UiMsg::track_divider)
        then Just (tracknum, UiMsg.Divider)
        else Just (tracknum, UiMsg.Track pos)
    | otherwise = Just (tracknum, UiMsg.SkeletonDisplay)

peek_event :: Ptr UiMsg.UiMsg -> IO UiMsg.MsgEvent
peek_event msgp = do
    event <- int <$> (#peek UiMsg, event.event) msgp :: IO Int
    button <- int <$> (#peek UiMsg, event.button) msgp :: IO Int
    clicks <- int <$> (#peek UiMsg, event.clicks) msgp :: IO Int
    is_click <- toBool <$> ((#peek UiMsg, event.is_click) msgp :: IO CInt)
    x <- int <$> (#peek UiMsg, event.x) msgp :: IO Int
    y <- int <$> (#peek UiMsg, event.y) msgp :: IO Int
    key_code <- (#peek UiMsg, event.key) msgp :: IO CInt
    text <- (#peek UiMsg, event.text) msgp :: IO CChar
    modifier_state <- (#peek UiMsg, event.modifier_state) msgp :: IO CInt
    is_repeat <- toBool <$> ((#peek UiMsg, event.is_repeat) msgp :: IO CChar)
    let mouse state = UiMsg.Mouse state mods (x, y) clicks is_click
        mods = Key.decode_modifiers modifier_state
        key = Key.decode_key key_code
        kbd state = UiMsg.Kbd state mods key $ if text == 0 then Nothing
            else Just (toEnum (fromIntegral text))
        aux = UiMsg.AuxMsg
    return $ case event of
        (#const FL_PUSH) -> mouse (UiMsg.MouseDown button)
        (#const FL_DRAG) -> mouse (UiMsg.MouseDrag button)
        (#const FL_RELEASE) -> mouse (UiMsg.MouseUp button)
        (#const FL_MOVE) -> mouse UiMsg.MouseMove
        (#const FL_KEYDOWN) -> kbd
            (if is_repeat then UiMsg.KeyRepeat else UiMsg.KeyDown)
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

peek_ui_update :: CInt -> Ptr UiMsg.UiMsg -> IO UiMsg.UiUpdate
peek_ui_update type_num msgp = case type_num of
    (#const UiMsg::msg_input) -> do
        ctext <- (#peek UiMsg, input.text) msgp :: IO CString
        text <- if ctext == nullPtr then return Nothing
            else Just <$> Util.peekCString ctext
        return $ UiMsg.UpdateInput text
    (#const UiMsg::msg_track_scroll) -> do
        scroll <- int <$> (#peek UiMsg, track_scroll.scroll) msgp :: IO Int
        return $ UiMsg.UpdateTrackScroll scroll
    (#const UiMsg::msg_time_scroll) -> do
        scroll <- (#peek UiMsg, time_scroll.scroll) msgp :: IO ScoreTime
        return $ UiMsg.UpdateTimeScroll scroll
    (#const UiMsg::msg_resize) -> do
        rect <- peek =<< (#peek UiMsg, resize.rect) msgp
        padding <- (#peek UiMsg, resize.padding) msgp :: IO Block.Padding
        return $ UiMsg.UpdateViewResize rect padding
    (#const UiMsg::msg_track_width) -> do
        width <- int <$> (#peek UiMsg, track_width.width) msgp :: IO Int
        return $ UiMsg.UpdateTrackWidth width
    (#const UiMsg::msg_close) -> return UiMsg.UpdateClose
    _ -> error $ "unknown UiMsg type: " ++ show type_num

int :: CInt -> Int
int = fromIntegral

instance CStorable Block.Padding where
    sizeOf _ = #size Padding
    alignment _ = alignment (0 :: CInt)
    poke = error "Block.Padding poke unimplemented"
    peek p = do
        left <- int <$> (#peek Padding, left) p
        top <- int <$> (#peek Padding, top) p
        bottom <- int <$> (#peek Padding, bottom) p
        return $ Block.Padding { left = left, top = top, bottom = bottom }
