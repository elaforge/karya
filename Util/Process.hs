-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
-- | Utilities to deal with processes.
module Util.Process where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import           Control.Monad (forever, void)
import qualified Control.Monad.Fix as Fix

import qualified Data.ByteString as ByteString
import qualified Data.String as String
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified System.Exit
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Posix as Posix
import qualified System.Process as Process
import qualified System.Timeout as Timeout

import qualified Util.File as File
import qualified Util.Log as Log

import           Global


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
--
-- TODO use Process.withCreateProcess?
-- TODO I think 'multipleOutput' makes this obsolete.
supervised :: Process.CreateProcess -> IO ()
supervised proc = Exception.mask $ \restore -> do
    -- Hopefully this mask means that I can't get killed after starting the
    -- process but before installing the exception handler.
    (_, _, _, hdl) <- create proc
    Exception.onException (restore (waitAndLog proc hdl)) $ case hdl of
        Nothing -> return ()
        Just hdl -> do
            pid <- Process.getPid hdl
            Log.warn $ "received exception, killing " <> showt (cmdOf proc)
                <> maybe "" ((<>")") . (" (pid "<>) . showt) pid
            Process.terminateProcess hdl

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

-- | Like 'Process.callProcess', but log if the binary wasn't found or
-- failed.
call :: FilePath -> [String] -> IO ()
call cmd args = Exception.handle ioError $
    Process.withCreateProcess proc $ \_ _ _ hdl -> waitAndLog proc (Just hdl)
    where
    -- If I don't close the fds, the subprocess can inherit open fds, with
    -- confusing results.
    proc = (Process.proc cmd args) { Process.close_fds = True }
    -- In modern GHCs, this will happen when the binary doesn't exist.  The
    -- exception will have the binary name in it, so cmdOf not needed.
    ioError :: IO.Error.IOError -> IO ()
    ioError exc = Log.warn $ showt exc

-- | Like 'Process.createProcess', but return a Nothing instead of a pid if
-- the binary doesn't exist.
create :: Process.CreateProcess
    -> IO (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle,
        Maybe Process.ProcessHandle)
create proc = File.ignoreEnoent (Process.createProcess proc) >>= \case
    Nothing -> return (Nothing, Nothing, Nothing, Nothing)
    Just (inh, outh, errh, hdl) -> return (inh, outh, errh, Just hdl)

-- * conversation

data TalkOut = Stdout !Text | Stderr !Text
    -- | This always terminates the conversation, and effectively marks the
    -- channel closed.
    | Exit !Exit
    deriving (Eq, Ord, Show)
data TalkIn = Text !Text | EOF
    deriving (Eq, Show)

data Exit = ExitCode !Int | BinaryNotFound | KillTimeout
    deriving (Eq, Ord, Show)

instance String.IsString TalkIn where
    fromString = Text . Text.pack

-- | Have a conversation with a subprocess.  This doesn't use ptys, so this
-- will only work if the subprocess explicitly doesn't use block buffering.
conversation :: FilePath -> [String] -> Maybe [(String, String)]
    -> Chan.Chan TalkIn -> (Chan.Chan TalkOut -> IO a) -> IO a
conversation cmd args env input action = do
    output <- Chan.newChan
    conversationWith cmd args env (Chan.readChan input) (Chan.writeChan output)
        (action output)

-- | Get output from multiple subprocesses.  They'll be killed when the action
-- returns, if they haven't already exited.
multipleOutput :: [(FilePath, [String])]
    -> (Chan.Chan ((FilePath, [String]), TalkOut) -> IO a)
    -> IO a
multipleOutput cmds action = do
    when (null cmds) $
        errorIO "no command given"
    output <- Chan.newChan
    let run (cmd, args) = conversationWith cmd args Nothing (return EOF)
            (Chan.writeChan output . ((cmd, args),))
    foldr run (action output) cmds

conversationWith :: FilePath -> [String] -> Maybe [(String, String)]
    -> IO TalkIn -> (TalkOut -> IO ()) -> IO a -> IO a
conversationWith cmd args env getInput notifyOutput action = do
    -- Apparently binary not found is detected in createProcess.  I think in
    -- previous versions it was detected in waitForProcess.
    ok <- File.ignoreEnoent $ Process.withCreateProcess proc $
        \(Just stdin) (Just stdout) (Just stderr) pid -> do
            IO.hSetBuffering stdout IO.LineBuffering
            IO.hSetBuffering stderr IO.LineBuffering
            inThread <- Async.async $ Fix.fix $ \loop -> getInput >>= \case
                Text t -> Text.IO.hPutStrLn stdin t >> IO.hFlush stdin >> loop
                EOF -> IO.hClose stdin
            outThread <- Async.async $ void $ File.ignoreEOF $ forever $
                notifyOutput . Stdout =<< Text.IO.hGetLine stdout
            errThread <- Async.async $ void $ File.ignoreEOF $ forever $
                notifyOutput . Stderr =<< Text.IO.hGetLine stderr
            -- Ensure both stdout and stderr are flushed before exit.
            complete <- Async.async $ do
                Async.waitBoth outThread errThread
                code <- Process.waitForProcess pid
                notifyOutput $ Exit $ ExitCode $ case code of
                    System.Exit.ExitFailure code -> code
                    System.Exit.ExitSuccess -> 0
            result <- action `Exception.onException` do
                mapM_ Async.cancel [inThread, outThread, errThread, complete]
                -- I could also just return and let withCreateProcess kill it,
                -- but then I wouldn't get the exit code, and more importantly,
                -- confirmation that it actually died.  Of course this means
                -- I'm trusting that the process will actually exit on SIGTERM.
                -- And for some reason trying to get 'complete' to do this
                -- doesn't work.
                Process.terminateProcess pid
                code <- Timeout.timeout killTimeout $ Process.waitForProcess pid
                notifyOutput $ Exit $ case code of
                    Just (System.Exit.ExitFailure code) -> ExitCode code
                    Just System.Exit.ExitSuccess -> ExitCode 0
                    Nothing -> KillTimeout
            Async.cancel complete
            return result
    case ok of
        Nothing -> do
            notifyOutput $ Exit BinaryNotFound
            action
        Just a -> return a
    where
    killTimeout = toUsec 4
    proc = (Process.proc cmd args)
        { Process.std_in = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        , Process.close_fds = True
        , Process.env = env
        }

toUsec :: Time.NominalDiffTime -> Int
toUsec = round . (*1000000)

-- * util

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
