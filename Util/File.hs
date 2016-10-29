-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{- | Do things with files.
-}
module Util.File where
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib.Internal as Zlib.Internal
import qualified Control.Exception as Exception
import Control.Monad (void, guard)
import Control.Monad.Extra (whenM, orM)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as Error
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process


-- | Read and decompress a gzipped file.
readGz :: FilePath -> IO (Either String ByteString.ByteString)
readGz fn = decompress =<< Lazy.readFile fn

decompress :: Lazy.ByteString -> IO (Either String ByteString.ByteString)
decompress bytes =
    Exception.handle (return . handle) $
        Right <$> Exception.evaluate (Lazy.toStrict (GZip.decompress bytes))
    where handle (exc :: Zlib.Internal.DecompressError) = Left (show exc)

-- | Write a gzipped file.  Try to do so atomically by writing to @fn.write@
-- first and renaming it.
--
-- Like @mv@, this will refuse to overwrite a file if it isn't writable.
writeGz :: FilePath -> ByteString.ByteString -> IO ()
writeGz fn bytes = do
    requireWritable fn
    Lazy.writeFile (fn ++ ".write") $ GZip.compress $ Lazy.fromStrict bytes
    Directory.renameFile (fn ++ ".write") fn

-- | Throw if this file exists but isn't writable.
requireWritable :: FilePath -> IO ()
requireWritable fn = whenM (not <$> writable fn) $
    Exception.throwIO $ Error.mkIOError Error.permissionErrorType
        "refusing to overwrite a read-only file" Nothing (Just fn)

-- | True if the file doesn't exist, or if it does but is writable.
writable :: FilePath -> IO Bool
writable fn = orM
    [ not <$> orM [Directory.doesFileExist fn, Directory.doesDirectoryExist fn]
    , Directory.writable <$> Directory.getPermissions fn
    ]

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
        else maybeDescend (dir == "." || descend dir) descend dir
    where
    maybeDescend True descend dir = do
        fns <- list dir
        fmap concat $ mapM (listRecursive descend) fns
    maybeDescend False _ _ = return []

-- | 'Directory.recursiveRemoveDirectory' crashes if the dir doesn't exist, and
-- follows symlinks.
rmDirRecursive :: FilePath -> IO ()
rmDirRecursive dir = void $ Process.rawSystem "rm" ["-rf", dir]

writeLines :: FilePath -> [Text] -> IO ()
writeLines fname lines = IO.withFile fname IO.WriteMode $ \hdl ->
    mapM_ (Text.IO.hPutStrLn hdl) lines

-- * IO errors

-- | If @op@ raised ENOENT, return Nothing.
ignoreEnoent :: IO a -> IO (Maybe a)
ignoreEnoent = ignoreError IO.Error.isDoesNotExistError

-- | Ignore all IO errors.  This is useful when you want to see if a file
-- exists, because some-file/x will not give ENOENT, but ENOTDIR, which is
-- probably isIllegalOperation.
ignoreIOError :: IO a -> IO (Maybe a)
ignoreIOError = ignoreError (\(_ :: IO.Error.IOError) -> True)

ignoreError :: Exception.Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
ignoreError ignore action = Exception.handleJust (guard . ignore)
    (const (return Nothing)) (fmap Just action)

-- | 'Exception.try' specialized to IOError.
tryIO :: IO a -> IO (Either IO.Error.IOError a)
tryIO = Exception.try
