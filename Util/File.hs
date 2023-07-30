-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
-- | Do things with files.
module Util.File (
    -- * read/write
    writeLines
    , writeAtomic
    , symlink
    -- * query
    , writable
    -- * directory
    , list, listRecursive
    , walk
    -- * compression
    , readGz, writeGz
) where
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib.Internal as Zlib.Internal
import qualified Control.Exception as Exception
import           Control.Monad (forM_, unless, when)
import           Control.Monad.Extra (filterM, ifM, orM, partitionM, whenM)
import           Control.Monad.Trans (liftIO)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as Error
import qualified System.Posix.Files as Posix.Files

import qualified Util.Exceptions as Exceptions


-- * read/write

writeLines :: FilePath -> [Text] -> IO ()
writeLines fname lines = IO.withFile fname IO.WriteMode $ \hdl ->
    mapM_ (Text.IO.hPutStrLn hdl) lines

writeAtomic :: FilePath -> ByteString.ByteString -> IO ()
writeAtomic fn bytes = do
    ByteString.writeFile tmp bytes
    Directory.renameFile tmp fn
    where
    tmp = fn ++ ".write.tmp"

-- | Make a symlink atomically.
symlink :: String -> FilePath -> IO ()
symlink dest fname = do
    oldDest <- Exceptions.ignoreEnoent $ Directory.getSymbolicLinkTarget fname
    -- Don't remake if it's already right.  Probably unnecessary, but it
    -- happens a lot and we can avoid touching the filesystem.
    unless (oldDest == Just dest) $ do
        -- If a previous process got killed, there might be stale .tmp files.
        Exceptions.ignoreEnoent $ Directory.removeFile (fname <> ".tmp")
        -- Atomically replace the old link, if any.
        Directory.createFileLink dest (fname <> ".tmp")
        Directory.renameFile (fname <> ".tmp") fname

-- * query

sameContents :: FilePath -> FilePath -> IO Bool
sameContents fn1 fn2 = do
    c1 <- Exceptions.ignoreEnoent $ Lazy.readFile fn1
    c2 <- Exceptions.ignoreEnoent $ Lazy.readFile fn2
    return $ c1 == c2

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

-- * directory

-- | Like 'Directory.listDirectory' except prepend the directory.
list :: FilePath -> IO [FilePath]
list dir = do
    fns <- Directory.listDirectory dir
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

-- | Walk the filesystem and stream (dir, fname).
walk :: (FilePath -> Bool) -> FilePath
    -> S.Stream (S.Of (FilePath, [FilePath])) IO ()
walk wantDir = go
    where
    go dir = do
        (dirs, fnames) <- liftIO $
            partitionM (Directory.doesDirectoryExist . (dir</>))
                =<< Directory.listDirectory dir
        S.yield (dir, fnames)
        dirs <- return $ map (dir</>) $ filter wantDir dirs
        dirs <- liftIO $ if followLinks then return dirs
            else filterM (fmap not . Directory.pathIsSymbolicLink) dirs
        mapM_ go dirs
    followLinks = False

-- * compression

-- | Read and decompress a gzipped file.
readGz :: FilePath -> IO (Either String ByteString.ByteString)
readGz fn = decompress =<< Lazy.readFile fn

decompress :: Lazy.ByteString -> IO (Either String ByteString.ByteString)
decompress bytes =
    Exception.handle (return . handle) $
        Right <$> Exception.evaluate (Lazy.toStrict (GZip.decompress bytes))
    where handle (exc :: Zlib.Internal.DecompressError) = Left (show exc)

-- | Write a gzipped file.  Try to do so atomically by writing to a tmp file
-- first and renaming it.
--
-- Like @mv@, this will refuse to overwrite a file if it isn't writable.  If
-- the file wouldn't have changed, abort the write and delete the tmp file.
-- The mtime won't change, and the caller gets a False, which can be used to
-- avoid rebuilds.
writeGz :: Int -- ^ save this many previous versions of the file
    -> FilePath -> ByteString.ByteString -> IO Bool
    -- ^ False if the file wasn't written because it wouldn't have changed.
writeGz rotations fn bytes = do
    requireWritable fn
    forM_ [0 .. rotations-1] $ requireWritable . rotation
    let tmp = fn ++ ".write.tmp"
    Lazy.writeFile tmp $ GZip.compress $ Lazy.fromStrict bytes
    ifM (sameContents fn tmp)
        (Directory.removeFile tmp >> return False) $
        do
            -- out.2 -> out.3, out.1 -> out.2, out.0 -> out.1
            forM_ [rotations-1, rotations-2 .. 1] $ \n ->
                Exceptions.ignoreEnoent_ $
                    Directory.renameFile (rotation (n-1)) (rotation n)
            -- Ensure there is never a moment where out doesn't exist:
            -- > rm -f out.0.tmp
            -- > ln out out.0.tmp
            -- > mv out.0.tmp out.0
            -- > mv out.write.tmp out
            when (rotations > 0) $ Exceptions.ignoreEnoent_ $ do
                let tmp0 = rotation 0 <> ".tmp"
                Exceptions.ignoreEnoent_ $ Directory.removeFile tmp0
                Posix.Files.createLink fn tmp0
                Directory.renameFile tmp0 (rotation 0)
            Directory.renameFile tmp fn
            return True
    where
    rotation n = fn <> "." <> show n
