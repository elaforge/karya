-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to tail a log file, even if it gets rotated.
module LogView.Tail (
    logFilename
    -- * tail
    , Handle, open, tail
    , deserializeLine
    -- * magic
    , startingMsg, quittingMsg
) where
import           Prelude hiding (read, tail)
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified System.IO as IO
import qualified System.IO.Error as Error
import qualified System.Posix as Posix

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified App.Config as Config
import qualified App.Path as Path

import           Global


logFilename :: IO FilePath
logFilename = do
    appDir <- Path.getAppDir
    pure $ Path.toAbsolute appDir (Config.logDir Path.</> "seq.log")

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
        Nothing -> pure ()
        Just n -> do
            IO.hSeek hdl IO.SeekFromEnd (-n * 200)
            when (n /= 0) $
                void $ IO.hGetLine hdl -- make sure I'm at a line boundary
    size <- IO.hFileSize hdl
    pure $ Handle filename hdl size

tail :: Handle -> IO (Log.Msg, Handle)
tail hdl = do
    (line, hdl) <- readLine hdl
    pure (deserializeLine line, hdl)

deserializeLine :: ByteString.ByteString -> Log.Msg
deserializeLine line = case Log.deserialize (Lazy.fromStrict line) of
    Left err -> Log.msg Log.Error Nothing $ "error parsing " <> showt line
        <> ": " <> txt err
    Right msg -> msg

-- | (handle, file size)
type TailState = (IO.Handle, Integer)

readLine :: Handle -> IO (ByteString.ByteString, Handle)
readLine (Handle filename hdl size) = go (hdl, size)
    where
    go state@(hdl, size) = IO.hIsEOF hdl >>= \case
        True -> do
            newSize <- IO.hFileSize hdl
            -- Check if the file was truncated.
            state <- if newSize < size
                then IO.hSeek hdl IO.AbsoluteSeek 0 >> pure state
                else ifM (fileRenamed filename newSize)
                    (reopen hdl newSize filename)
                    (Thread.delay 2 >> pure state)
            go state
        False -> do
            -- Since hGetLine in its infinite wisdom chops the newline it's
            -- impossible to tell if this is a complete line or not.  I'll set
            -- LineBuffering and hope for the best.
            line <- ByteString.Char8.hGetLine hdl
            newSize <- IO.hFileSize hdl
            pure (line, Handle filename hdl newSize)

-- | If the filename exists, open it and close the old file.
reopen :: IO.Handle -> Integer -> FilePath -> IO TailState
reopen old size filename = do
    fp <- ignoreException "reopen" (IO.openFile filename IO.ReadMode)
    case fp of
        Nothing -> pure (old, size)
        Just new -> do
            IO.hClose old
            IO.hSetBuffering new IO.LineBuffering
            size <- IO.hFileSize new
            pure (new, size)

ignoreException :: String -> IO a -> IO (Maybe a)
ignoreException name action = Exception.try action >>= \result -> case result of
    Left exc
        | Just e <- Exception.fromException exc, Error.isDoesNotExistError e ->
            pure Nothing
        | otherwise -> do
            putStrLn $ name ++ ": ignoring hopefully transient exception: "
                ++ show exc
            pure Nothing
    Right val -> pure $ Just val

-- | Check if it looks like the file has been renamed.
fileRenamed :: FilePath -> Integer -> IO Bool
fileRenamed filename size = do
    -- I should really use inode, but ghc's crummy IO libs make that a pain,
    -- since handleToFd closes the handle.
    fileSize <- maybe 0 Posix.fileSize <$>
        ignoreException "file_renamed" (Posix.getFileStatus filename)
    pure $ fromIntegral fileSize /= size && fileSize /= 0

-- * magic

startingMsg :: Text
startingMsg = "app starting"

quittingMsg :: Text
quittingMsg = "app quitting"
