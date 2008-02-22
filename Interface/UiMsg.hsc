{-# LANGUAGE ForeignFunctionInterface #-}
{-
As much functionality as possible is implemented at the app level, not the C++
UI level.  Except some hardcoded actions like selections and zooming, keyboard
and mouse events are dropped into a block-specific event queue.  The app must
receive those events and make the appropriate API calls.  This is so I can
dynamically change mouse and keyboard mapping at the haskell level.
-}

module Interface.UiMsg where
import Control.Monad
import Foreign
import Foreign.C

-- import qualified Interface.Util as Util
import Interface.Types

import qualified Interface.BlockImpl as BlockImpl
import qualified Interface.TrackImpl as TrackImpl
-- import qualified Interface.Event as Event

take_ui_msgs = with nullPtr $ \msgspp -> do
    count <- c_take_ui_msgs msgspp
    msgsp <- peek msgspp
    msgs <- peekArray (fromIntegral count) msgsp
    return msgs

foreign import ccall unsafe "take_ui_msgs"
    c_take_ui_msgs :: Ptr (Ptr UiMsg) -> IO CInt

--

data UiMsg = UiMsg
    { msg_context :: Context
    , msg_state :: [MsgState]
    , msg_x :: Int
    , msg_y :: Int
    , msg_data :: Data
    } deriving (Show)

data Context = Context
    { ctx_block :: Maybe BlockImpl.BlockView
    , ctx_track :: Maybe TrackImpl.TrackView
    , ctx_event :: Maybe TrackPos
    } -- You can get the selection by asking the Track or Event.
    deriving (Show)

data MsgState = Shift | CapsLock | Control | Alt | NumLock | Meta
    | ScrollLock | Button Int
    deriving (Show)

-- TODO: split it off so that midi and osc can go in their own modules and
-- get combined later
data Data = Mouse
    { mouse_state :: MouseState
    , mouse_clicks :: Int
    , mouse_is_click :: Bool
    }
    | Kbd
    { kbd_state :: KbdState
    , kbd_char :: Char
    }
    | Unhandled Int
    deriving (Show)

data MouseState = MouseMove | MouseDrag | MouseDown Int | MouseUp Int
    deriving (Show)
data KbdState = KeyDown | KeyUp deriving (Show)

-- * Storable

#include "c_interface.h"

instance Storable UiMsg where
    sizeOf _ = #size UiEvent
    alignment _ = 1
    peek = peek_msg
    poke = error "no poke for UiMsg"

peek_msg msgp = do
    msg <- (#peek UiEvent, event) msgp :: IO CInt
    button <- (#peek UiEvent, button) msgp :: IO CInt
    clicks <- (#peek UiEvent, clicks) msgp :: IO CInt
    is_click <- (#peek UiEvent, is_click) msgp :: IO CInt
    x <- (#peek UiEvent, x) msgp :: IO CInt
    y <- (#peek UiEvent, y) msgp :: IO CInt
    state <- (#peek UiEvent, state) msgp :: IO CInt
    key <- (#peek UiEvent, key) msgp :: IO CInt
    return $ make_msg (i msg) (i button) (i clicks) (i is_click /= 0)
        (i x) (i y) state (i key)
    where i = fromIntegral

make_msg msg button clicks is_click x y state key
        = UiMsg context (decode_state state) x y edata
    where
    context = Context Nothing Nothing Nothing
    edata = decode_msg msg button clicks is_click key

decode_msg fltk_msg button clicks is_click key = msg
    where
    mouse typ = Mouse typ 0 False
    kbd typ = Kbd typ '\0'
    partial_msg = case fltk_msg of
        (#const FL_PUSH) -> mouse (MouseDown button)
        (#const FL_DRAG) -> mouse MouseDrag
        (#const FL_RELEASE) -> mouse (MouseUp button)
        (#const FL_MOVE) -> mouse MouseMove
        (#const FL_KEYDOWN) -> kbd KeyDown
        (#const FL_KEYUP) -> kbd KeyUp
        _ -> Unhandled fltk_msg
    msg = case partial_msg of
        Mouse {} -> partial_msg
            { mouse_clicks = clicks, mouse_is_click = is_click }
        Kbd {} -> partial_msg { kbd_char = toEnum key }
        _ -> partial_msg

decode_state :: CInt -> [MsgState]
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

{-
struct UiEvent {
    int event;
    int button, clicks, is_click, x, y;
    int state;
    int key;

    BlockViewWindow *inside_block;
    TrackView *inside_track;
    TrackPos inside_event;
};
-}
