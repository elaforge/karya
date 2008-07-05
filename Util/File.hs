{- | Do things with files.
-}
module Util.File where
import qualified Control.Exception as Exception
import qualified Data.Word as Word

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error


lazy_read_binary :: FilePath -> IO [Word.Word8]
lazy_read_binary fn = do
    hdl <- IO.openBinaryFile fn IO.ReadMode
    bytes <- IO.hGetContents hdl
    return (map (fromIntegral . fromEnum) bytes)

read_binary fn = do
    bytes <- lazy_read_binary fn
    return $ length bytes `seq` bytes

-- | Like Directory.getDirectoryContents except don't return dotfiles and
-- prepend the dir.
read_dir :: FilePath -> IO [FilePath]
read_dir dir = do
    fns <- Directory.getDirectoryContents dir
    return $ map (dir </>) $ filter ((/=".") . take 1) $ fns

-- | Move the file to file.last.  Do this before writing a new one that may
-- fail.
backup_file :: FilePath -> IO ()
backup_file fname = Exception.catchJust enoent_exc
    (Directory.renameFile fname (fname ++ ".last"))
    (\_exc -> return ())

enoent_exc exc = case Exception.ioErrors exc of
    Just io_error | IO.Error.isDoesNotExistError io_error -> Just io_error
    _ -> Nothing
