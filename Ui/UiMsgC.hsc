-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.UiMsgC (getUiMsgs) where
import qualified Util.FFI as FFI
import qualified Ui.Block as Block
import qualified Ui.PtrMap as PtrMap
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import           Types
import           ForeignC


getUiMsgs :: IO [UiMsg.UiMsg]
getUiMsgs = with nullPtr $ \msgspp -> do
    count <- cGetUiMsgs msgspp
    msgsp <- peek msgspp
    msgs <- peekArray (int count) msgsp
    cClearUiMsgs
    return msgs

foreign import ccall unsafe "get_ui_msgs"
    cGetUiMsgs :: Ptr (Ptr UiMsg.UiMsg) -> IO CInt
foreign import ccall unsafe "clear_ui_msgs" cClearUiMsgs :: IO ()

-- * Storable

#include "Ui/c_interface.h"

instance CStorable UiMsg.UiMsg where
    sizeOf _ = #size UiMsg
    alignment _ = alignment nullPtr
    poke = error "UiMsg poke unimplemented"
    peek = peekMsg

peekMsg :: Ptr UiMsg.UiMsg -> IO UiMsg.UiMsg
peekMsg msgp = do
    typeNum <- (#peek UiMsg, type) msgp :: IO CInt
    (context, maybeViewId) <- peekContext msgp
    UiMsg.UiMsg context <$> case typeNum of
        (#const UiMsg::msg_event) -> UiMsg.MsgEvent <$> peekEvent msgp
        (#const UiMsg::msg_screen_size) -> do
            rect <- peek =<< (#peek UiMsg, screen.rect) msgp
            screen <- int <$> (#peek UiMsg, screen.screen) msgp :: IO Int
            screens <- int <$> (#peek UiMsg, screen.screens) msgp :: IO Int
            return $ UiMsg.UpdateScreenSize screen screens rect
        _ -> do
            viewId <- maybe
                (error $ "got a null view_id from a ui update: "
                    ++ show typeNum)
                return maybeViewId
            UiMsg.UiUpdate viewId <$> peekUiUpdate typeNum msgp

peekContext :: Ptr UiMsg.UiMsg -> IO (UiMsg.Context, Maybe ViewId)
peekContext msgp = do
    focusp <- (#peek UiMsg, context.focus) msgp :: IO (Ptr PtrMap.CView)
    ctxFocus <- lookupId focusp
    viewp <- (#peek UiMsg, context.view) msgp :: IO (Ptr PtrMap.CView)
    view <- lookupId viewp

    trackType <- (#peek UiMsg, context.track_type) msgp :: IO CChar
    tracknum <- int <$> (#peek UiMsg, context.tracknum) msgp :: IO Int
    hasPos <- toBool <$> ((#peek UiMsg, context.has_pos) msgp :: IO CChar)
    cpos <- (#peek UiMsg, context.pos) msgp
    pure
        ( UiMsg.Context
            { ctxFocus
            , ctxTrack = decodeTrack trackType tracknum hasPos cpos
            , ctxFloatingInput =
                trackType == (#const UiMsg::track_floating_input)
            }
        , view
        )
    where
    lookupId p
        | p == nullPtr = return Nothing
        | otherwise = PtrMap.lookupId p

decodeTrack :: CChar -> Int -> Bool -> ScoreTime
    -> Maybe (TrackNum, UiMsg.Track)
decodeTrack trackType tracknum hasPos pos
    | trackType == (#const UiMsg::track_none) = Nothing
    | hasPos = if trackType == (#const UiMsg::track_divider)
        then Just (tracknum, UiMsg.Divider)
        else Just (tracknum, UiMsg.Track pos)
    | otherwise = Just (tracknum, UiMsg.SkeletonDisplay)

peekEvent :: Ptr UiMsg.UiMsg -> IO UiMsg.MsgEvent
peekEvent msgp = do
    event <- int <$> (#peek UiMsg, event.event) msgp :: IO Int
    button <- int <$> (#peek UiMsg, event.button) msgp :: IO Int
    clicks <- int <$> (#peek UiMsg, event.clicks) msgp :: IO Int
    isClick <- toBool <$> ((#peek UiMsg, event.is_click) msgp :: IO CInt)
    x <- int <$> (#peek UiMsg, event.x) msgp :: IO Int
    y <- int <$> (#peek UiMsg, event.y) msgp :: IO Int
    keyCode <- (#peek UiMsg, event.key) msgp :: IO CInt
    text <- (#peek UiMsg, event.text) msgp :: IO CChar
    modifierState <- (#peek UiMsg, event.modifier_state) msgp :: IO CInt
    isRepeat <- toBool <$> ((#peek UiMsg, event.is_repeat) msgp :: IO CChar)
    let mouse state = UiMsg.Mouse $ UiMsg.MouseEvent
            { mouseState = state
            , mouseModifiers = mods
            , mouseCoords = (x, y)
            , mouseClicks = clicks
            , mouseIsClick = isClick
            }
        mods = Key.decodeModifiers modifierState
        key = Key.decodeKey keyCode
        kbd state = UiMsg.Kbd state mods key $ if text == 0 then Nothing
            else Just (toEnum (fromIntegral text))
        aux = UiMsg.AuxMsg
    return $ case event of
        (#const FL_PUSH) -> mouse (UiMsg.MouseDown button)
        (#const FL_DRAG) -> mouse (UiMsg.MouseDrag button)
        (#const FL_RELEASE) -> mouse (UiMsg.MouseUp button)
        (#const FL_MOVE) -> mouse UiMsg.MouseMove
        (#const FL_KEYDOWN) -> kbd
            (if isRepeat then UiMsg.KeyRepeat else UiMsg.KeyDown)
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

peekUiUpdate :: CInt -> Ptr UiMsg.UiMsg -> IO UiMsg.UiUpdate
peekUiUpdate typeNum msgp = case typeNum of
    (#const UiMsg::msg_input) -> do
        ctext <- (#peek UiMsg, input.text) msgp :: IO CString
        text <- if ctext == nullPtr then return Nothing
            else Just <$> FFI.peekCString ctext
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
        width <- int <$> (#peek UiMsg, track_width.width) msgp
        suggestedWidth <-
            int <$> (#peek UiMsg, track_width.suggested_width) msgp
        return $ UiMsg.UpdateTrackWidth width suggestedWidth
    (#const UiMsg::msg_close) -> return UiMsg.UpdateClose
    _ -> error $ "unknown UiMsg type: " ++ show typeNum

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
