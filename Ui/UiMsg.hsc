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
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import Foreign
import Foreign.C
import Text.Printf

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Key as Key
import qualified Ui.BlockImpl as BlockImpl

take_ui_msgs = with nullPtr $ \msgspp -> do
    count <- c_take_ui_msgs msgspp
    msgsp <- peek msgspp
    msgs <- peekArray (fromIntegral count) msgsp
    return msgs

foreign import ccall unsafe "take_ui_msgs"
    c_take_ui_msgs :: Ptr (Ptr UiMsg) -> IO CInt

-- TODO normalize this
-- put Type in UiMsg, remove Context, put msg_x, msg_y in Mouse
data UiMsg = UiMsg
    { msg_type :: Type
    , msg_context :: Context
    , msg_state :: [State]
    , msg_x :: Int
    , msg_y :: Int
    , msg_data :: Data
    } deriving (Show)

-- | Corresponds to UiMsg::MsgType enum.
-- Many of these are just view updates, and are only sent so they can be
-- stored as Actions and go into the undo list.
data Type = TypeEvent | TypeInput | TypeTrackScroll | TypeZoom
    | TypeViewResize | TypeTrackWidth | TypeClose
    deriving (Show)

decode_type typ = case typ of
    (#const UiMsg::msg_event) -> TypeEvent
    (#const UiMsg::msg_input) -> TypeInput
    (#const UiMsg::msg_track_scroll) -> TypeTrackScroll
    (#const UiMsg::msg_zoom) -> TypeZoom
    (#const UiMsg::msg_view_resize) -> TypeViewResize
    (#const UiMsg::msg_track_width) -> TypeTrackWidth
    (#const UiMsg::msg_close) -> TypeClose
    _ -> error $ "unknown UiMsg type: " ++ show typ

data Context = Context
    { ctx_block :: Maybe BlockImpl.View
    -- | Index into block tracks.
    , ctx_track :: Maybe Int
    , ctx_pos :: Maybe TrackPos
    } deriving (Show)

data State = Shift | CapsLock | Control | Alt | NumLock | Meta
    | ScrollLock | Button Int
    deriving (Show)

data Data = Mouse
    { mouse_state :: MouseState
    , mouse_clicks :: Int
    , mouse_is_click :: Bool
    }
    | Kbd
    { kbd_state :: KbdState
    , kbd_key :: Key.Key
    }
    | AuxMsg AuxMsg
    | Unhandled Int
    deriving (Show)

data AuxMsg = Enter | Leave | Focus | Unfocus | Shortcut | Deactivate
    | Activate | Hide | Show
    deriving (Eq, Show)

data MouseState = MouseMove | MouseDrag | MouseDown Int | MouseUp Int
    deriving (Show)
data KbdState = KeyDown | KeyUp deriving (Show)

pretty_ui_msg (UiMsg TypeEvent context state x y mdata) = case mdata of
    Mouse mstate clicks is_click ->
        printf "Mouse: %s (%d, %d) %s" (show mstate) x y
            (pretty_context context)
    Kbd kstate char -> printf "Kbd: %s %s %s" (show kstate) (show char)
        (show state)
    AuxMsg msg -> printf "Aux: %s" (show msg)
    Unhandled x -> printf "Unhandled: %d" x
pretty_ui_msg (UiMsg typ context _ _ _ _)
    = printf "Other Event: %s %s" (show typ) (pretty_context context)

pretty_context (Context block track pos) = "{" ++ contents ++ "}"
    where
    contents = Seq.join " " (filter (not.null) [show_maybe "block" block,
        show_maybe "track" track, show_maybe "pos" pos])
    show_maybe desc Nothing = ""
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
    state <- (#peek UiMsg, state) msgp :: IO CInt
    key <- (#peek UiMsg, key) msgp :: IO CInt

    viewp <- (#peek UiMsg, view) msgp :: IO (Ptr BlockImpl.CBlockView)
    has_track <- (#peek UiMsg, has_track) msgp :: IO CChar
    track <- (#peek UiMsg, track) msgp :: IO CInt
    has_pos <- (#peek UiMsg, has_pos) msgp :: IO CChar
    pos <- (#peek UiMsg, pos) msgp

    cxt <- make_context viewp has_track track has_pos pos
    return $ make_msg (decode_type type_num) cxt
        (i event) (i button) (i clicks) (i is_click /= 0)
        (i x) (i y) state (i key)
    where i = fromIntegral

make_msg typ context event button clicks is_click x y state key
        = UiMsg typ context (decode_state state) x y edata
    where edata = decode_msg event button clicks is_click key

make_context viewp has_track track has_pos pos
    | viewp == nullPtr = return (context Nothing)
    | otherwise = do
        ptr_map <- MVar.readMVar BlockImpl.view_ptr_to_view
        let { view = case Map.lookup viewp ptr_map of
            Nothing -> error $ "ptr to view not in the map: " ++ show viewp
            Just view -> view
            }
        return (context (Just view))
    where
    context view = Context view (to_maybe has_track (fromIntegral track))
        (to_maybe has_pos pos)
    to_maybe b val = if toBool b then Just val else Nothing

decode_msg event button clicks is_click key = msg
    where
    mouse typ = Mouse typ 0 False
    kbd typ = Kbd typ (Key.Unknown 0)
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
        Kbd {} -> partial_msg { kbd_key = Key.decode_key key }
        _ -> partial_msg

decode_state :: CInt -> [State]
decode_state fltk_state = [ st | (bit, st) <- const_map, has fltk_state bit ]
    where
    has x y = (x .&. y) /= 0
    const_map =
        [ ((#const FL_SHIFT), Shift)
        , ((#const FL_CAPS_LOCK), CapsLock)
        , ((#const FL_CTRL), Control)
        , ((#const FL_ALT), Alt)
        , ((#const FL_NUM_LOCK), NumLock)
        , ((#const FL_META), Meta)
        , ((#const FL_SCROLL_LOCK), ScrollLock)
        , ((#const FL_BUTTON1), Button 1)
        , ((#const FL_BUTTON2), Button 2)
        , ((#const FL_BUTTON3), Button 3)
        ]
