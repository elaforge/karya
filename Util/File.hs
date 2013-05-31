-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{- | Do things with files.
-}
module Util.File where
import qualified Codec.Compression.GZip as GZip
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

import Util.Control


-- | Read a file, ungzipping it if it ends with @.gz@.
readGz :: FilePath -> IO ByteString.ByteString
readGz fn
    | FilePath.takeExtension fn == ".gz" = do
        bytes <- Lazy.readFile fn
        return $ Lazy.toStrict $ GZip.decompress bytes
    | otherwise = ByteString.readFile fn

-- | Write a gzipped file.
writeGz :: FilePath -> ByteString.ByteString -> IO ()
writeGz fn = Lazy.writeFile fn . GZip.compress . Lazy.fromStrict

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
