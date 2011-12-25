{-# LANGUAGE OverloadedStrings #-}
module Shake.CcDeps where
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Shake.Util as Util


-- | Find files this file includes, relative to the current directory.
--
-- TODO do this transitively
includesOf :: [FilePath] -> FilePath -> IO ([FilePath], [FilePath])
    -- ^ (found, not found)
includesOf dirs fn = do
    includes <- readIncludes fn
    -- @#include "x"@ starts searching from the same directory as the source
    -- file.
    paths <- mapM (find (FilePath.dropFileName fn : dirs)) includes
    return $ Either.partitionEithers
        [maybe (Right inc) Left path | (path, inc) <- zip paths includes]

find :: [FilePath] -> FilePath -> IO (Maybe FilePath)
find [] _ = return Nothing
find (dir:dirs) fn = Util.ifM (Directory.doesFileExist (dir </> fn))
    (return $ Just (dir </> fn))
    (find dirs fn)

-- | TODO #includes can be anywhere, but stop parsing as soon as a function
-- definition is hit
readIncludes :: FilePath -> IO [FilePath]
readIncludes = fmap (Maybe.mapMaybe parseLine . B.lines) . B.readFile

-- | Intentionally ignore @#include <x>@ lines, they are just system includes.
parseLine :: B.ByteString -> Maybe FilePath
parseLine line
    | include `B.isPrefixOf` line = fmap B.unpack (extract line)
    | otherwise = Nothing
    where
    extract = quotes . B.dropWhile Char.isSpace . B.drop (B.length include)
    quotes s
        | not (B.null s) && B.index s 0 == '"' =
            Just $ B.takeWhile (/='"') (B.drop 1 s)
        | otherwise = Nothing
    include = "#include "
