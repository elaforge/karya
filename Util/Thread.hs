-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Thread (
    start, start_logged
    , Seconds, delay
    , timeout
    -- * Flag
    , Flag, flag, set, wait, poll
    -- * util
    , time_action
) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified GHC.Conc as Conc
import qualified System.CPUTime as CPUTime
import qualified System.Timeout as Timeout

import qualified Util.Log as Log


-- | Start a noisy thread that will log when it starts and stops, and warn if
-- it dies from an exception.
start_logged :: String -> IO () -> IO Concurrent.ThreadId
start_logged name thread = do
    thread_id <- Concurrent.myThreadId
    Conc.labelThread thread_id name
    let thread_name = Text.pack $ show thread_id ++ " " ++ name ++ ": "
    Log.debug $ thread_name <> "started"
    Concurrent.forkFinally thread $ \result -> case result of
        Right () -> Log.debug $ thread_name <> "completed"
        Left err -> Log.warn $ thread_name <> "died: "
            <> Text.pack (show (err :: Exception.SomeException))

start :: IO () -> IO Concurrent.ThreadId
start = Concurrent.forkIO

-- | This is just NominalDiffTime, but with a name I might remember.
type Seconds = Time.NominalDiffTime

-- | Delay in seconds.
delay :: Seconds -> IO ()
delay = Concurrent.threadDelay . to_usec

timeout :: Seconds -> IO a -> IO (Maybe a)
timeout = Timeout.timeout . to_usec

to_usec :: Seconds -> Int
to_usec = round . (*1000000)

-- * Flag

-- | A Flag starts False, and can eventually become True.  It never goes back
-- to False again.
newtype Flag = Flag (STM.TVar Bool)

instance Show Flag where show _ = "((Flag))"

flag :: IO Flag
flag = Flag <$> STM.newTVarIO False

set :: Flag -> IO ()
set (Flag var) = STM.atomically $ STM.writeTVar var True

-- | Wait a finite amount of time for the flag to become true.
poll :: Seconds -> Flag -> IO Bool
poll time (Flag var)
    | time <= 0 = STM.readTVarIO var
    | otherwise = maybe False (const True) <$> timeout time (wait (Flag var))

-- | Wait until the flag becomes true.
wait :: Flag -> IO ()
wait (Flag var) = STM.atomically $ do
    val <- STM.readTVar var
    if val then return () else STM.retry

-- * util

-- | Time an IO action in seconds.  Technically not thread related, but I don't
-- have a better place at the moment.
time_action :: IO () -> IO Seconds
time_action action = do
    start <- CPUTime.getCPUTime
    action
    end <- CPUTime.getCPUTime
    return $ cpu_to_sec (end - start)

cpu_to_sec :: Integer -> Seconds
cpu_to_sec s = fromIntegral s / fromIntegral (10^12)
