{- | Do things with files.
-}
module Util.File where
import qualified Data.Word as Word
import qualified System.IO as IO
import System.FilePath ((</>))
import qualified System.Directory as Directory


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
