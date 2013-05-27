{-# LANGUAGE ScopedTypeVariables #-}
{- | Do things with files.
-}
module Util.File where
import qualified Control.Exception as Exception
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

import Util.Control


-- | Like 'Directory.getDirectoryContents' except don't return dotfiles and
-- it prepends the directory.
list :: FilePath -> IO [FilePath]
list dir = do
    fns <- Directory.getDirectoryContents dir
    return $ map (strip . (dir </>)) $ filter ((/=".") . take 1) fns
    where
    strip ('.' : '/' : path) = path
    strip path = path

listRecursive :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listRecursive descend dir = do
    is_file <- Directory.doesFileExist dir
    if is_file then return [dir]
        else maybe_descend (dir == "." || descend dir) descend dir
    where
    maybe_descend True descend dir = do
        fns <- list dir
        fmap concat $ mapM (listRecursive descend) fns
    maybe_descend False _ _ = return []

-- | 'Directory.recursiveRemoveDirectory' crashes if the dir doesn't exist, and
-- follows symlinks.
rmDirRecursive :: FilePath -> IO ()
rmDirRecursive dir = void $ Process.rawSystem "rm" ["-rf", dir]

-- | If @op@ raised ENOENT, return Nothing.
ignoreEnoent :: IO a -> IO (Maybe a)
ignoreEnoent op = Exception.handleJust (guard . IO.Error.isDoesNotExistError)
    (const (return Nothing)) (fmap Just op)
