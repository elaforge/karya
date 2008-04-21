{-# LANGUAGE ForeignFunctionInterface #-}
{- |
As much functionality as possible is implemented at the app level, not the C++
UI level.  Except some hardcoded actions like selections and zooming, keyboard
and mouse events are dropped into a block-specific event queue.  The app must
receive those events and make the appropriate API calls.  This is so I can
dynamically change mouse and keyboard mapping at the haskell level.
-}

module Ui.UiMsg where
import Control.Monad
import Foreign
import Foreign.C
import Text.Printf

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Key as Key
import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC

take_ui_msgs :: IO [UiMsg]
take_ui_msgs = with nullPtr $ \msgspp -> do
    count <- c_take_ui_msgs msgspp
    msgsp <- peek msgspp
    msgs <- peekArray (fromIntegral count) msgsp
    return msgs

foreign import ccall unsafe "take_ui_msgs"
    c_take_ui_msgs :: Ptr (Ptr UiMsg) -> IO CInt

-- | Technically MsgClose and whatnot don't have ctx_track and ctx_pos, but
-- it's easier to give everyone Context.
-- These all derive Ord so they can go in Sets and Maps.
data UiMsg = UiMsg Context Msg
    deriving (Show)

data Context = Context
    { ctx_block :: Maybe Block.ViewId
    -- | Index into block tracks.
    , ctx_track :: Maybe Int
    , ctx_pos :: Maybe TrackPos
    } deriving (Show)

-- | Corresponds to UiMsg::MsgType enum.
-- Many of these are just view updates, and are only sent so they can be
-- stored as Actions and go into the undo list.
data Msg = MsgEvent Data
    | MsgInput | MsgTrackScroll | MsgZoom | MsgViewResize
    | MsgTrackWidth | MsgClose
    deriving (Eq, Ord, Show)

decode_type typ = case typ of
    -- This one is handled in make_msg, since MsgEvent needs args
    -- (#const UiMsg::msg_event) -> MsgEvent
    (#const UiMsg::msg_input) -> MsgInput
    (#const UiMsg::msg_track_scroll) -> MsgTrackScroll
    (#const UiMsg::msg_zoom) -> MsgZoom
    (#const UiMsg::msg_view_resize) -> MsgViewResize
    (#const UiMsg::msg_track_width) -> MsgTrackWidth
    (#const UiMsg::msg_close) -> MsgClose
    _ -> error $ "unknown UiMsg type: " ++ show typ

data Data = Mouse
    { mouse_state :: MouseState
    , mouse_coords :: (Int, Int)
    , mouse_clicks :: Int
    , mouse_is_click :: Bool
    }
    | Kbd KbdState Key.Key
    | AuxMsg AuxMsg
    | Unhandled Int
    deriving (Eq, Ord, Show)

data AuxMsg = Enter | Leave | Focus | Unfocus | Shortcut | Deactivate
    | Activate | Hide | Show
    deriving (Eq, Ord, Show)

data MouseState = MouseMove | MouseDrag | MouseDown Int | MouseUp Int
    deriving (Eq, Ord, Show)
data KbdState = KeyDown | KeyUp deriving (Eq, Ord, Show)

pretty_ui_msg :: UiMsg -> String
pretty_ui_msg (UiMsg ctx (MsgEvent mdata)) = case mdata of
    Mouse mstate coords clicks is_click ->
        printf "Mouse: %s %s %s click: %s %d" (show mstate) (show coords)
            (pretty_context ctx) (show is_click) clicks
    Kbd kstate key -> printf "Kbd: %s %s" (show kstate) (show key)
    AuxMsg msg -> printf "Aux: %s %s" (show msg) (pretty_context ctx)
    Unhandled x -> printf "Unhandled: %d" x
pretty_ui_msg (UiMsg ctx msg)
    = printf "Other Event: %s %s" (show msg) (pretty_context ctx)

pretty_context (Context block track pos) = "{" ++ contents ++ "}"
    where
    contents = Seq.join " " (filter (not.null) [show_maybe "block" block,
        show_maybe "track" track, show_maybe "pos" pos])
    show_maybe _ Nothing = ""
    show_maybe desc (Just x) = desc ++ "=" ++ show x

-- * Storable

#include "c_interface.h"

instance Storable UiMsg where
    sizeOf _ = #size UiMsg
    alignment _ = undefined
    peek = peek_msg
    poke = error "no poke for UiMsg"

peek_msg msgp = do
    type_num <- (#peek UiMsg, type) msgp :: IO CInt
    event <- (#peek UiMsg, event) msgp :: IO CInt
    button <- (#peek UiMsg, button) msgp :: IO CInt
    clicks <- (#peek UiMsg, clicks) msgp :: IO CInt
    is_click <- (#peek UiMsg, is_click) msgp :: IO CInt
    x <- (#peek UiMsg, x) msgp :: IO CInt
    y <- (#peek UiMsg, y) msgp :: IO CInt
    key <- (#peek UiMsg, key) msgp :: IO CInt

    viewp <- (#peek UiMsg, view) msgp :: IO (Ptr BlockC.CView)
    has_track <- (#peek UiMsg, has_track) msgp :: IO CChar
    track <- (#peek UiMsg, track) msgp :: IO CInt
    has_pos <- (#peek UiMsg, has_pos) msgp :: IO CChar
    pos <- (#peek UiMsg, pos) msgp

    context <- make_context viewp has_track track has_pos pos
    return $ make_msg type_num context
        (i event) (i button) (i clicks) (i is_click /= 0)
        (i x) (i y) (i key)
    where i = fromIntegral

make_msg type_num context event button clicks is_click x y key
    = UiMsg context $ case type_num of
        (#const UiMsg::msg_event) ->
            MsgEvent (decode_msg_event event button x y clicks is_click key)
        _ -> decode_type type_num

make_context viewp has_track track has_pos pos
    | viewp == nullPtr = return $ context Nothing
    | otherwise = do
        view_id <- BlockC.get_id viewp
        return $ context (Just view_id)
    where
    context view = Context view (to_maybe has_track (fromIntegral track))
        (to_maybe has_pos pos)
    to_maybe b val = if toBool b then Just val else Nothing

decode_msg_event event button x y clicks is_click key = msg
    where
    mouse state = Mouse state (x, y) 0 False
    kbd state = Kbd state (Key.Unknown 0)
    aux = AuxMsg
    partial_msg = case event of
        (#const FL_PUSH) -> mouse (MouseDown button)
        (#const FL_DRAG) -> mouse MouseDrag
        (#const FL_RELEASE) -> mouse (MouseUp button)
        (#const FL_MOVE) -> mouse MouseMove
        (#const FL_KEYDOWN) -> kbd KeyDown
        (#const FL_KEYUP) -> kbd KeyUp

        (#const FL_ENTER) -> aux Enter
        (#const FL_LEAVE) -> aux Leave
        (#const FL_FOCUS) -> aux Focus
        (#const FL_UNFOCUS) -> aux Unfocus
        (#const FL_SHORTCUT) -> aux Shortcut
        (#const FL_DEACTIVATE) -> aux Deactivate
        (#const FL_ACTIVATE) -> aux Activate
        (#const FL_HIDE) -> aux Hide
        (#const FL_SHOW) -> aux Show
        _ -> Unhandled event
    msg = case partial_msg of
        Mouse {} -> partial_msg
            { mouse_clicks = clicks, mouse_is_click = is_click }
        Kbd state _ -> Kbd state (Key.decode_key key)
        _ -> partial_msg
