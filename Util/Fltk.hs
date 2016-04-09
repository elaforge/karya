-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Fltk (
    Fltk, action, quit, run_action, Channel, new_channel
    , event_loop, send_action
    -- * window
    , Window, win_ptr, MsgCallback, Msg(..)
    , create_window, read_msg
) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error

import qualified Foreign
import qualified Foreign.C as C


data Fltk a = Quit | Action (IO a)
newtype Channel = Channel (Concurrent.MVar [Fltk ()])

action :: IO a -> Fltk a
action = Action

quit :: Channel -> IO ()
quit chan = send_action chan Quit

-- | Run an action directly in the main thread.
run_action :: Fltk a -> IO a
run_action (Action act) = act
run_action Quit = error "run_action on a Quit"

new_channel :: IO Channel
new_channel = Channel <$> Concurrent.newMVar []

-- | Enter the fltk event loop.  For portability, this should only be called
-- from the main thread.
event_loop :: Channel -> IO ()
event_loop chan = do
    c_initialize
    loop chan
    where
    loop chan = do
        done <- not . Foreign.toBool <$> c_has_windows
        if done then return () else do
            c_wait
            quit <- handle_actions chan
            if quit then return () else loop chan

handle_actions :: Channel -> IO Bool
handle_actions (Channel chan) = Concurrent.modifyMVar chan $ \acts -> do
    let quit = not $ null [Quit | Quit <- acts]
    if quit then return ([], True) else do
        -- The events are consed to the start, so reverse to get them back in
        -- the right order.
        sequence_ $ reverse [act | Action act <- acts]
        return ([], False)

send_action :: Channel -> Fltk () -> IO ()
send_action (Channel chan) act = do
    Concurrent.modifyMVar_ chan (return . (act:))
    c_awake

foreign import ccall "initialize" c_initialize :: IO ()
foreign import ccall "ui_wait" c_wait :: IO ()
foreign import ccall "ui_awake" c_awake :: IO ()
foreign import ccall "has_windows" c_has_windows :: IO C.CInt

-- * window

data Window a = Window {
    win_ptr :: Foreign.Ptr (Window a)
    , win_chan :: STM.TChan (Msg a)
    }
type MsgCallback = C.CInt -> C.CString -> IO ()
data Msg a = Msg a Text.Text

type CreateWindow a = C.CInt -> C.CInt -> C.CInt -> C.CInt -> C.CString
    -> Foreign.FunPtr MsgCallback -> IO (Foreign.Ptr (Window a))

create_window :: (C.CInt -> a) -> CreateWindow a -> Int -> Int -> Int -> Int
    -> String -> IO (Window a)
create_window decode_type create_win x y w h title = do
    chan <- STM.newTChanIO
    cb <- c_make_msg_callback (cb_msg_callback decode_type chan)
    winp <- C.withCString title $ \titlep ->
        create_win (c x) (c y) (c w) (c h) titlep cb
    return (Window winp chan)
    where c = fromIntegral

read_msg :: Window a -> STM.STM (Msg a)
read_msg = STM.readTChan . win_chan


-- * implementation

cb_msg_callback :: (C.CInt -> a) -> STM.TChan (Msg a) -> MsgCallback
cb_msg_callback decode_type msg_chan msg_type msgp = do
    msg <- peekCString msgp
    STM.atomically $ STM.writeTChan msg_chan (Msg (decode_type msg_type) msg)

foreign import ccall "wrapper"
    c_make_msg_callback :: MsgCallback -> IO (Foreign.FunPtr MsgCallback)

peekCString :: C.CString -> IO Text.Text
peekCString cstr
    | cstr == Foreign.nullPtr = return Text.empty
    | otherwise = Text.Encoding.decodeUtf8With Encoding.Error.lenientDecode <$>
        ByteString.packCString cstr
