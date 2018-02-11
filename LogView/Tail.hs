-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to tail a log file, even if it gets rotated.
module LogView.Tail (
    log_filename
    -- * rotate
    , rotate_logs
    -- * tail
    , Handle, open, tail
    , deserialize_line
) where
import Prelude hiding (read, tail)
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as Error
import qualified System.Posix as Posix
import qualified System.Process as Process

import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified App.Config as Config
import Global


log_filename :: IO FilePath
log_filename = Config.get_app_dir >>= \app_dir -> return $
    Config.make_path app_dir Config.log_dir </> "seq.log"

-- * rotate

-- | Get a file handle for writing log msgs, first rotating logs if necessary.
rotate_logs :: Int -> Int -> FilePath -> IO IO.Handle
rotate_logs keep max_size log_fn = do
    let rotated_fn n = log_fn ++ "." ++ show n ++ ".gz"
    size <- maybe 0 Posix.fileSize <$> ignore (Posix.getFileStatus log_fn)
    when (size >= fromIntegral max_size) $ do
        forM_ (reverse (zip [1..keep] (drop 1 [1..keep]))) $ \(from, to) ->
            ignore $ Directory.renameFile (rotated_fn from) (rotated_fn to)
        let fn = FilePath.dropExtension (rotated_fn 1)
        putStrLn $ "rotate logs " ++ log_fn ++ " -> " ++ fn
        ignore $ Directory.renameFile log_fn fn
        Process.waitForProcess =<< Process.runProcess "gzip" [fn]
            Nothing Nothing Nothing Nothing Nothing
        return ()
    hdl <- IO.openFile log_fn IO.AppendMode
    -- Logs are per-line, so ensure they go out promptly.
    IO.hSetBuffering hdl IO.LineBuffering
    return hdl
    where ignore = File.ignoreEnoent


-- * tail

-- | The Handle remembers the file and the last file size so it can detect when
-- the logs have been rotated.
data Handle = Handle !FilePath !IO.Handle !Integer deriving (Show)

open :: FilePath
    -> Maybe Integer -- ^ No seek if Nothing, else seek n*200 bytes from end.
    -- TODO this should be the number of lines, but I'm too lazy to do that
    -- right.
    -> IO Handle
open filename seek = do
    -- ReadWriteMode makes it create the file if it doesn't exist, and not
    -- die here.
    hdl <- IO.openFile filename IO.ReadWriteMode
    IO.hSetBuffering hdl IO.LineBuffering -- See read_line.
    case seek of
        Nothing -> return ()
        Just n -> do
            IO.hSeek hdl IO.SeekFromEnd (-n * 200)
            when (n /= 0) $
                void $ IO.hGetLine hdl -- make sure I'm at a line boundary
    size <- IO.hFileSize hdl
    return $ Handle filename hdl size

tail :: Handle -> IO (Log.Msg, Handle)
tail hdl = do
    (line, hdl) <- read_line hdl
    return (deserialize_line line, hdl)

deserialize_line :: ByteString.ByteString -> Log.Msg
deserialize_line line = case Log.deserialize (Lazy.fromStrict line) of
    Left err -> Log.msg Log.Error Nothing $ "error parsing " <> showt line
        <> ": " <> txt err
    Right msg -> msg

-- | (handle, file size)
type TailState = (IO.Handle, Integer)

read_line :: Handle -> IO (ByteString.ByteString, Handle)
read_line (Handle filename hdl size) = go (hdl, size)
    where
    go state@(hdl, size) = IO.hIsEOF hdl >>= \case
        True -> do
            new_size <- IO.hFileSize hdl
            -- Check if the file was truncated.
            state <- if new_size < size
                then IO.hSeek hdl IO.AbsoluteSeek 0 >> return state
                else ifM (file_renamed filename new_size)
                    (reopen hdl new_size filename)
                    (Thread.delay 2 >> return state)
            go state
        False -> do
            -- Since hGetLine in its infinite wisdom chops the newline it's
            -- impossible to tell if this is a complete line or not.  I'll set
            -- LineBuffering and hope for the best.
            line <- ByteString.hGetLine hdl
            new_size <- IO.hFileSize hdl
            return (line, Handle filename hdl new_size)

-- | If the filename exists, open it and close the old file.
reopen :: IO.Handle -> Integer -> FilePath -> IO TailState
reopen old size filename = do
    fp <- ignoreException "reopen" (IO.openFile filename IO.ReadMode)
    case fp of
        Nothing -> return (old, size)
        Just new -> do
            IO.hClose old
            IO.hSetBuffering new IO.LineBuffering
            size <- IO.hFileSize new
            return (new, size)

ignoreException :: String -> IO a -> IO (Maybe a)
ignoreException name action = Exception.try action >>= \result -> case result of
    Left exc
        | Just e <- Exception.fromException exc, Error.isDoesNotExistError e ->
            return Nothing
        | otherwise -> do
            putStrLn $ name ++ ": ignoring hopefully transient exception: "
                ++ show exc
            return Nothing
    Right val -> return $ Just val

-- | Check if it looks like the file has been renamed.
file_renamed :: FilePath -> Integer -> IO Bool
file_renamed filename size = do
    -- I should really use inode, but ghc's crummy IO libs make that a pain,
    -- since handleToFd closes the handle.
    file_size <- maybe 0 Posix.fileSize <$>
        ignoreException "file_renamed" (Posix.getFileStatus filename)
    return $ fromIntegral file_size /= size && file_size /= 0
