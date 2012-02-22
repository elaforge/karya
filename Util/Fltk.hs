{-# LANGUAGE ForeignFunctionInterface #-}
module Util.Fltk where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import Foreign
import Foreign.C
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Control


data Window a = Window {
    win_p :: Ptr (Window a)
    , win_chan :: STM.TChan (Msg a)
    }
type MsgCallback = CInt -> CString -> IO ()
data Msg a = Msg a String

-- | Enter the fltk event loop.  For portability, this should only be called
-- from the main thread.
run :: IO ()
run = do
    c_initialize
    Util.Control.while_ (fmap toBool c_has_windows) $ do
        c_wait
        handle_actions acts_mvar

handle_actions :: Concurrent.MVar [IO a] -> IO ()
handle_actions acts_mvar = Concurrent.modifyMVar_ acts_mvar $ \acts ->
    sequence_ (reverse acts) >> return []

send_action :: IO a -> IO ()
send_action act = do
    Concurrent.modifyMVar_ acts_mvar (return . (act:))
    c_awake

acts_mvar :: Concurrent.MVar [a]
acts_mvar = Unsafe.unsafePerformIO (Concurrent.newMVar [])
{-# NOINLINE acts_mvar #-}

foreign import ccall "initialize" c_initialize :: IO ()
foreign import ccall "ui_wait" c_wait :: IO ()
foreign import ccall "ui_awake" c_awake :: IO ()
foreign import ccall "has_windows" c_has_windows :: IO CInt

type CreateWindow a = CInt -> CInt -> CInt -> CInt -> CString
    -> FunPtr MsgCallback -> IO (Ptr (Window a))
create_window :: (CInt -> a) -> CreateWindow a -> Int -> Int -> Int -> Int
    -> String -> IO (Window a)
create_window decode_type create_win x y w h title = do
    chan <- STM.newTChanIO
    cb <- c_make_msg_callback (cb_msg_callback decode_type chan)
    winp <- withCString title $ \titlep ->
        create_win (c x) (c y) (c w) (c h) titlep cb
    return (Window winp chan)
    where c = fromIntegral


-- * implementation

cb_msg_callback :: (CInt -> a) -> STM.TChan (Msg a) -> MsgCallback
cb_msg_callback decode_type msg_chan msg_type msgp = do
    msg <- peekCString msgp
    STM.atomically $ STM.writeTChan msg_chan (Msg (decode_type msg_type) msg)

foreign import ccall "wrapper"
    c_make_msg_callback :: MsgCallback -> IO (FunPtr MsgCallback)
