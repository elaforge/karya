-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Thread (
    start, start_logged
    , Seconds, delay
    , timeout
    , take_tmvar_timeout
    -- * util
    , time_action
) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified System.CPUTime as CPUTime
import qualified System.Timeout as Timeout

import qualified Util.Log as Log


-- | Start a noisy thread that will log when it starts and stops, and warn if
-- it dies from an exception.
--
-- TODO when killed and it logs, the logger can emit mixed up msgs, somehow the
-- lock is not actually serializing it.
start_logged :: String -> IO () -> IO Concurrent.ThreadId
start_logged name op = Concurrent.forkIO (handle_thread name op)

handle_thread :: String -> IO a -> IO ()
handle_thread name op = do
    thread_id <- Concurrent.myThreadId
    let thread_name = show thread_id ++ " " ++ name ++ ": "
    Log.debug $ thread_name ++ "started"
    result <- Exception.try op
    case result of
        Right _ -> Log.debug $ thread_name ++ "completed"
        Left err -> Log.warn $ thread_name ++ "died: "
            ++ show (err :: Exception.SomeException)

start :: IO () -> IO Concurrent.ThreadId
start = Concurrent.forkIO

type Seconds = Double

-- | Delay in seconds.  I can never remember what units 'threadDelay' is in.
delay :: Seconds -> IO ()
delay secs = Concurrent.threadDelay (floor (1000000 * secs))

timeout :: Double -> IO a -> IO (Maybe a)
timeout secs = Timeout.timeout (round (secs * 1000000))

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

-- * util

-- | Time an IO action in seconds.  Technically not thread related, but I don't
-- have a better place at the moment.
time_action :: IO () -> IO Double
time_action action = do
    start <- CPUTime.getCPUTime
    action
    end <- CPUTime.getCPUTime
    return $ cpu_to_sec (end - start)

cpu_to_sec :: Integer -> Double
cpu_to_sec s = fromIntegral s / fromIntegral (10^12)
