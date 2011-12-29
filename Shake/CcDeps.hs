{-# LANGUAGE OverloadedStrings #-}
module Shake.CcDeps (transitiveIncludesOf) where
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Development.Shake as Shake
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Shake.Util as Util


-- -- | Find files this file includes.
-- includesOf :: [FilePath] -> FilePath -> Shake.Action ([FilePath], [FilePath])
--     -- ^ (found, not found)
-- includesOf dirs fn = Shake.need [fn] >> Trans.liftIO (includesOf_ dirs fn)

includesOf_ :: [FilePath] -> FilePath -> IO ([FilePath], [FilePath])
includesOf_ dirs fn = do
    includes <- readIncludes fn
    -- @#include "x"@ starts searching from the same directory as the source
    -- file.
    paths <- mapM (find (FilePath.dropFileName fn : dirs)) includes
    return $ Either.partitionEithers
        [maybe (Right inc) Left path | (path, inc) <- zip paths includes]

-- | Find files this files includes, transitively.  Includes the given file.
--
-- Can also be used for .hsc files since it looks for @^#include@.
transitiveIncludesOf :: [FilePath] -> FilePath
    -> Shake.Action ([FilePath], [FilePath]) -- ^ (found, notfound)
transitiveIncludesOf dirs fn =
    Shake.need [fn] >> Trans.liftIO (transitiveIncludesOf_ dirs fn)

transitiveIncludesOf_ :: [FilePath] -> FilePath -> IO ([FilePath], [FilePath])
transitiveIncludesOf_ dirs fn = go Set.empty Set.empty [fn]
    where
    go checked notfound (fn:fns)
        | fn `Set.member` checked || fn `Set.member` notfound =
            go checked notfound fns
        | otherwise = do
            (includes, fnNotfound) <- includesOf_ dirs fn
            let checked' = Set.insert fn checked
            go checked' (Set.union notfound (Set.fromList fnNotfound))
                (fns ++ filter (`Set.notMember` checked') includes)
    go checked notfound [] = return (Set.toList checked, Set.toList notfound)

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
