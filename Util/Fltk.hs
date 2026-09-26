-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Fltk (
    Fltk, Result(..), fltk, action, quit, runAction, Channel, newChannel
    , eventLoop
    -- * window
    , Window, winPtr, MsgCallback, Msg(..)
    , createWindow, readMsg
) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans as Trans

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error

import qualified Foreign
import qualified Foreign.C as C


newtype Fltk a = Fltk (IO a)
    deriving (Functor, Applicative, Monad, Trans.MonadIO)

data Result = Continue | Quit
    deriving (Show)

newtype Channel = Channel (Concurrent.MVar [Fltk Result])

fltk :: IO a -> Fltk a
fltk = Fltk

-- | Send a Fltk action that doesn't quit.
action :: Channel -> Fltk () -> IO ()
action chan act = sendAction chan (act >> pure Continue)

quit :: Channel -> IO ()
quit chan = sendAction chan (pure Quit)

-- | Run an action directly, rather than over the 'Channel'.  Only use this if
-- you're certain you're the main thread!
runAction :: Fltk a -> IO a
runAction (Fltk act) = act

newChannel :: IO Channel
newChannel = Channel <$> Concurrent.newMVar []

-- | Enter the fltk event loop.  For portability, this should only be called
-- from the main thread.
eventLoop :: Channel -> IO ()
eventLoop chan = do
    cInitialize
    loop chan
    where
    loop chan = do
        done <- not . Foreign.toBool <$> cHasWindows
        if done then pure () else do
            cWait
            handleActions chan >>= \case
                Quit -> pure ()
                Continue -> loop chan

handleActions :: Channel -> IO Result
handleActions (Channel chan) = Concurrent.modifyMVar chan $ go . reverse
    -- The events are consed to the start, so reverse to get them back in
    -- the right order.
    where
    go (Fltk act : acts) = act >>= \case
        Quit -> pure ([], Quit)
        Continue -> go acts
    go [] = pure ([], Continue)

sendAction :: Channel -> Fltk Result -> IO ()
sendAction (Channel chan) act = do
    Concurrent.modifyMVar_ chan (pure . (act:))
    cAwake

foreign import ccall "initialize" cInitialize :: IO ()
foreign import ccall "ui_wait" cWait :: IO ()
foreign import ccall "ui_awake" cAwake :: IO ()
foreign import ccall "has_windows" cHasWindows :: IO C.CInt

-- * window

data Window a = Window
    { winPtr :: Foreign.Ptr (Window a)
    , winChan :: STM.TChan (Msg a)
    }
type MsgCallback = C.CInt -> C.CString -> IO ()
data Msg a = Msg a Text.Text

type CreateWindow a = C.CInt -> C.CInt -> C.CInt -> C.CInt -> C.CString
    -> Foreign.FunPtr MsgCallback -> IO (Foreign.Ptr (Window a))

createWindow :: (C.CInt -> a) -> CreateWindow a -> Int -> Int -> Int -> Int
    -> String -> IO (Window a)
createWindow decodeType createWin x y w h title = do
    chan <- STM.newTChanIO
    cb <- cMakeMsgCallback (cbMsgCallback decodeType chan)
    winp <- C.withCString title $ \titlep ->
        createWin (c x) (c y) (c w) (c h) titlep cb
    pure (Window winp chan)
    where c = fromIntegral

readMsg :: Window a -> STM.STM (Msg a)
readMsg = STM.readTChan . winChan


-- * implementation

cbMsgCallback :: (C.CInt -> a) -> STM.TChan (Msg a) -> MsgCallback
cbMsgCallback decodeType msgChan msgType msgp = do
    msg <- peekCString msgp
    STM.atomically $ STM.writeTChan msgChan (Msg (decodeType msgType) msg)

foreign import ccall "wrapper"
    cMakeMsgCallback :: MsgCallback -> IO (Foreign.FunPtr MsgCallback)

peekCString :: C.CString -> IO Text.Text
peekCString cstr
    | cstr == Foreign.nullPtr = pure Text.empty
    | otherwise = Text.Encoding.decodeUtf8With Encoding.Error.lenientDecode <$>
        ByteString.packCString cstr
