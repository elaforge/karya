-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
module Shake.CcDeps (enableDefines, includesOf, transitiveIncludesOf) where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Development.Shake as Shake
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Shake.Util as Util


-- * defines

-- | Rewrite a file, switching @#define@s to @#undef@s and vice versa.
enableDefines :: FilePath -> [String] -> [String] -> IO ()
enableDefines fn defines undefs = do
    orig <- B.readFile fn
    let out = B.unlines $ map process $ B.lines orig
    when (orig /= out) $
        B.writeFile fn out
    where
    trans = Map.fromList $
        [("#undef " <> define, "#define " <> define)
            | define <- map B.pack defines]
        ++ [("#define " <> undef, "#undef " <> undef)
            | undef <- map B.pack undefs]
    process line = Map.findWithDefault line line trans

-- * includes

-- | Same as 'Shake.HsDeps.Generated'.
type Generated = Set.Set FilePath

-- | Unlike HsDeps.importsOf, I return the not-found paths instead of ignoring
-- them.  This is because I assume not-found haskell imports are from external
-- packages, while #include with double-quotes is a sign it should be found
-- locally.
includesOf :: Generated -> [FilePath] -> FilePath
    -> Shake.Action ([FilePath], [FilePath])
    -- ^ (foundIncludes, notFoundIncludes)
includesOf generated dirs fn =
    Shake.need [fn] >> Trans.liftIO (includesOfIO generated dirs fn)

-- | Find files this files includes, transitively.  Includes the given file.
--
-- Can also be used for .hsc files since it looks for @^#include@.  There isn't
-- an IO version because it 'Shake.need's the intermediate files.
transitiveIncludesOf :: Generated -> [FilePath] -> FilePath
    -> Shake.Action ([FilePath], [FilePath]) -- ^ ([found], [notFound])
transitiveIncludesOf generated dirs fn = go Set.empty Set.empty [fn]
    where
    go checked notFound (fn:fns)
        | fn `Set.member` checked || fn `Set.member` notFound =
            go checked notFound fns
        | otherwise = do
            (includes, notFounds) <- includesOf generated dirs fn
            let checked' = Set.insert fn checked
            go checked' (Set.union notFound (Set.fromList notFounds))
                (fns ++ filter (`Set.notMember` checked') includes)
    go checked notFound [] = return
        (map FilePath.normalise (Set.toList checked), Set.toList notFound)

includesOfIO :: Generated -> [FilePath] -> FilePath
    -> IO ([FilePath], [FilePath]) -- ^ (foundIncludes, notFoundIncludes)
includesOfIO generated dirs fn = do
    includes <- readIncludes fn
    -- @#include "x"@ starts searching from the same directory as the source
    -- file.
    paths <- mapM find1 includes
    return $ Either.partitionEithers
        [maybe (Right inc) Left path | (path, inc) <- zip paths includes]
    where
    find1 include
        | include `Set.member` generated = return $ Just include
        | otherwise = find (FilePath.dropFileName fn : dirs) include

find :: [FilePath] -> FilePath -> IO (Maybe FilePath)
find [] _ = return Nothing
find (dir:dirs) fn = Util.ifM (Directory.doesFileExist (dir </> fn))
    (return $ Just $ FilePath.normalise $ dir </> fn)
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
