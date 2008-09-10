module Util.Thread where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Util.Log as Log

start_thread, start_os_thread :: String -> IO () -> IO Concurrent.ThreadId
start_thread = do_start_thread Concurrent.forkIO
start_os_thread = do_start_thread Concurrent.forkOS

do_start_thread fork name th = fork $ Exception.bracket_
    (Log.notice $ "thread start: " ++ name)
    (Log.notice $ "thread exit: " ++ name)
    th

-- | Isn't there a simpler way to do this?  All I really want to do is return
-- when a shared value has changed, or it's timed out.
take_tmvar_timeout :: Double -> STM.TMVar a -> IO (Maybe a)
take_tmvar_timeout seconds tmvar = do
    res <- STM.newEmptyTMVarIO
    th1 <- Concurrent.forkIO $ STM.atomically $
        STM.takeTMVar tmvar >>= STM.putTMVar res . Just
    th2 <- Concurrent.forkIO $ do
        Concurrent.threadDelay (floor (seconds * 1000000))
        STM.atomically (STM.putTMVar res Nothing)
    val <- STM.atomically (STM.takeTMVar res)
    Concurrent.killThread th1
    Concurrent.killThread th2
    return val
