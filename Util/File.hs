{- | Do things with files.
-}
module Util.File where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process


lazy_read_binary :: FilePath -> IO [Word.Word8]
lazy_read_binary fn = do
    hdl <- IO.openBinaryFile fn IO.ReadMode
    bytes <- IO.hGetContents hdl
    return (map (fromIntegral . fromEnum) bytes)

read_binary :: FilePath -> IO [Word.Word8]
read_binary fn = do
    -- ByteString for strictness... would it be better to use plain readFile
    -- + seq?
    bytes <- ByteString.readFile fn
    return (ByteString.unpack bytes)

-- | Like Directory.getDirectoryContents except don't return dotfiles and
-- prepend the dir.
list_dir :: FilePath -> IO [FilePath]
list_dir dir = do
    fns <- Directory.getDirectoryContents dir
    return $ map (dir </>) $ filter ((/=".") . take 1) fns

recursive_list_dir :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
recursive_list_dir descend dir = do
    is_file <- Directory.doesFileExist dir
    if is_file then return [dir]
        else maybe_descend (descend dir) descend dir
    where
    maybe_descend True descend dir = do
        fns <- list_dir dir
        fmap concat $ mapM (recursive_list_dir descend) fns
    maybe_descend False _ _ = return []

-- | recursiveRemoveDirectory in System.Process crashes if the dir doesn't
-- exist, and follows symlinks.
recursive_rm_dir :: FilePath -> IO ()
recursive_rm_dir dir = void $ Process.rawSystem "rm" ["-rf", dir]

-- | Move the file to file.last.  Do this before writing a new one that may
-- fail.
backup_file :: FilePath -> IO ()
backup_file fname =
    void $ ignore_enoent $ Directory.renameFile fname (fname ++ ".last")

-- | If @op@ raised ENOENT, return Nothing.
ignore_enoent :: IO a -> IO (Maybe a)
ignore_enoent op = Exception.handleJust (guard . IO.Error.isDoesNotExistError)
    (const (return Nothing)) (fmap Just op)
