module Util.Process where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as ByteString
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified System.Process as Process


-- | Similar to System.Process.readProcessWithExitCode but return ByteStrings
-- instead of String.
readProcessWithExitCode :: Maybe [(String, String)] -> FilePath -> [String]
    -> ByteString.ByteString
    -> IO (Exit.ExitCode, ByteString.ByteString, ByteString.ByteString)
readProcessWithExitCode env cmd args stdin = do
    (Just inh, Just outh, Just errh, pid) <-
        Process.createProcess (Process.proc cmd args)
            { Process.env = env
            , Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
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
    ex <- Process.waitForProcess pid
    return (ex, out, err)
