-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
-- | Utilities to deal with processes.
module Util.Process where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.ByteString as ByteString
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Posix as Posix
import qualified System.Process as Process
import qualified System.Process.Internals as Internals

import qualified Util.File as File
import qualified Util.Log as Log


-- | Similar to 'Process.readProcessWithExitCode' but return ByteStrings
-- instead of String.
readProcessWithExitCode :: Maybe [(String, String)] -> FilePath -> [String]
    -> ByteString.ByteString
    -> IO (Exit.ExitCode, ByteString.ByteString, ByteString.ByteString)
readProcessWithExitCode env cmd args stdin = do
    (Just inh, Just outh, Just errh, hdl) <-
        Process.createProcess (Process.proc cmd args)
            { Process.env = env
            , Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            , Process.close_fds = True
            }
    outMVar <- MVar.newEmptyMVar
    errMVar <- MVar.newEmptyMVar
    Concurrent.forkIO $ MVar.putMVar outMVar =<< ByteString.hGetContents outh
    Concurrent.forkIO $ MVar.putMVar errMVar =<< ByteString.hGetContents errh
    Concurrent.forkIO $ ByteString.hPut inh stdin >> IO.hClose inh
    out <- MVar.takeMVar outMVar
    err <- MVar.takeMVar errMVar
    IO.hClose outh
    IO.hClose errh
    ex <- Process.waitForProcess hdl
    return (ex, out, err)

-- | Start a subprocess, wait for it to complete, and kill it if this thread
-- is killed.  This is like 'Async.withAsync', except for a subprocess, and
-- it's hardcoded to wait for the subprocess.
supervised :: Process.CreateProcess -> IO ()
supervised proc = Exception.mask $ \restore -> do
    -- Hopefully this mask means that I can't get killed after starting the
    -- process but before installing the exception handler.
    (_, _, _, hdl) <- create proc
    Exception.onException (restore (waitAndLog proc hdl)) $ case hdl of
        Nothing -> return ()
        Just hdl -> do
            pid <- handleToPid hdl
            Log.warn $ "received exception, killing " <> showt (cmdOf proc)
                <> maybe "" ((<>")") . (" (pid "<>) . showt) pid
            Process.terminateProcess hdl

-- | Start multiple processes, and kill them all if this thread is killed.
multipleSupervised :: [Process.CreateProcess] -> IO ()
multipleSupervised = Async.mapConcurrently_ supervised

-- | Wait for the process (if it started) and log if it didn't exit
-- successfully.
waitAndLog :: Process.CreateProcess -> Maybe Process.ProcessHandle -> IO ()
waitAndLog proc hdl = maybe (return ()) Log.warn =<< waitError proc hdl

waitError :: Process.CreateProcess -> Maybe Process.ProcessHandle
    -> IO (Maybe Text)
waitError proc maybeHdl = fmap annotate <$> case maybeHdl of
    -- If the binary doesn't exist, sometimes 'create' notices, and sometimes
    -- waitForProcess notices.  I think it's dependent on GHC version.
    Nothing -> return noBinary
    Just hdl -> do
        result <- File.ignoreEnoent (Process.waitForProcess hdl)
        return $ case result of
            Nothing -> Nothing
            Just code -> case code of
                Exit.ExitSuccess -> Nothing
                Exit.ExitFailure c
                    | c == 127 -> noBinary
                    | otherwise -> Just $ "process failed: " <> showt c
    where
    noBinary = Just "binary not found"
    annotate msg = msg <> ": " <> Text.pack (cmdOf proc)

-- | Like 'Process.createProcess', but log if the binary wasn't found or
-- failed.
logged :: Process.CreateProcess
    -> IO (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle,
        Maybe Process.ProcessHandle)
logged proc = do
    r@(_, _, _, hdl) <- create proc
    waitAndLog proc hdl
    return r

-- | Like 'Process.createProcess', but return a Nothing instead of a pid if
-- the binary doesn't exist.
create :: Process.CreateProcess
    -> IO (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle,
        Maybe Process.ProcessHandle)
create proc = File.ignoreEnoent (Process.createProcess proc) >>= \case
    Nothing -> return (Nothing, Nothing, Nothing, Nothing)
    Just (inh, outh, errh, hdl) -> return (inh, outh, errh, Just hdl)

cmdOf :: Process.CreateProcess -> String
cmdOf proc = case Process.cmdspec proc of
    Process.RawCommand fn args -> unwords $ fn : args
    Process.ShellCommand cmd -> cmd

binaryOf :: Process.CreateProcess -> FilePath
binaryOf proc = case Process.cmdspec proc of
    Process.RawCommand fn _ -> fn
    Process.ShellCommand cmd -> takeWhile (/=' ') cmd

isAlive :: Posix.ProcessID -> IO Bool
isAlive pid = (Posix.signalProcess Posix.nullSignal pid >> return True)
    `Exception.catch` (return . not . IO.Error.isDoesNotExistError)

commandName :: Posix.ProcessID -> IO (Maybe String)
commandName pid = do
    -- Ignore the exit code, because ps -p returns 1 when the pid doesn't
    -- exist.
    (_, output, _) <- Process.readProcessWithExitCode
        "ps" ["-p", show pid, "-o", "comm"] ""
    return $ case lines output of
        [_, cmd] -> Just cmd
        _ -> Nothing

exit :: Int -> IO a
exit 0 = Exit.exitSuccess
exit n = Exit.exitWith $ Exit.ExitFailure n

handleToPid :: Internals.ProcessHandle -> IO (Maybe Posix.ProcessID)
#if GHC_VERSION < 80200
handleToPid (Internals.ProcessHandle mvar _) =
#else
handleToPid (Internals.ProcessHandle mvar _ _) =
#endif
    MVar.readMVar mvar >>= \x -> case x of
        Internals.OpenHandle pid -> return (Just pid)
        _ -> return Nothing

showt :: Show a => a -> Text
showt = Text.pack . show
