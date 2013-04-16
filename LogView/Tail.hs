module LogView.Tail (Handle, log_filename, open, tail) where
import Prelude hiding (read, tail)
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Posix.Files as Posix.Files

import Util.Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified App.Config as Config


data Handle = Handle !FilePath !IO.Handle !Integer deriving (Show)

log_filename :: IO FilePath
log_filename = Config.get_app_dir >>= \app_dir -> return $
    Config.make_path app_dir Config.log_dir </> "seq.log"

open :: FilePath
    -> Maybe Integer -- ^ no seek if Nothing, else seek n*200 bytes from end
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
    msg <- deserialize_line line
    return (msg, hdl)

deserialize_line :: String -> IO Log.Msg
deserialize_line line = do
    err_msg <- Log.deserialize_msg line
    case err_msg of
        Left exc -> Log.initialized_msg Log.Error $ "error parsing: "
            ++ show exc ++ ", line was: " ++ show line
        Right msg -> return msg

type TailState = (IO.Handle, Integer)

read_line :: Handle -> IO (String, Handle)
read_line (Handle filename hdl size) = go (hdl, size)
    where
    go state@(hdl, last_size) = IO.hIsEOF hdl >>= \x -> case x of
        True -> do
            new_size <- IO.hFileSize hdl
            -- Check if the file was truncated.
            state <- if new_size < last_size
                then IO.hSeek hdl IO.AbsoluteSeek 0 >> return state
                else ifM (file_renamed filename new_size)
                    (reopen hdl new_size filename)
                    (Thread.delay 2 >> return state)
            go state
        False -> do
            -- Since hGetLine in its infinite wisdom chops the newline it's
            -- impossible to tell if this is a complete line or not.  I'll set
            -- LineBuffering and hope for the best.
            line <- IO.hGetLine hdl
            return (line, Handle filename hdl last_size)

-- | If the filename exists, open it and close the old file.
reopen :: IO.Handle -> Integer -> FilePath -> IO TailState
reopen hdl size filename =
    File.ignore_enoent (IO.openFile filename IO.ReadMode) >>= \x -> case x of
        Nothing -> return (hdl, size)
        Just new -> do
            IO.hClose hdl
            IO.hSetBuffering new IO.LineBuffering
            IO.hSeek new IO.AbsoluteSeek 0
            size <- IO.hFileSize new
            return (new, size)

-- | Check if it looks like the file has been renamed.
file_renamed :: FilePath -> Integer -> IO Bool
file_renamed filename size = do
    -- I should really use inode, but ghc's crummy IO libs make that a pain,
    -- since handleToFd closes the handle.
    file_size <- maybe 0 Posix.Files.fileSize <$>
        File.ignore_enoent (Posix.Files.getFileStatus filename)
    return $ fromIntegral file_size /= size && file_size /= 0
