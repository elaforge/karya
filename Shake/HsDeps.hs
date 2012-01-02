{-# LANGUAGE OverloadedStrings #-}
module Shake.HsDeps (importsOf, transitiveImportsOf) where
import Control.Applicative ((<$>))
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans

import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Development.Shake as Shake
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified Shake.Util as Util


type ModuleName = B.ByteString

-- | Find files of modules this module imports, in the form A/B.hs or A/B.hsc.
-- Paths that don't exist are assumed to be package imports and are omitted.
importsOf :: FilePath -> Shake.Action [FilePath]
importsOf fn = Shake.need [fn] >> Trans.liftIO (importsOf_ fn)

importsOf_ :: FilePath -> IO [FilePath]
importsOf_ fn = do
    imports <- readImportBlock fn
    Maybe.catMaybes <$> mapM fileOf (parseImports imports)

-- | Like 'importsOf' but transitive.  Includes the given module.
transitiveImportsOf :: FilePath -> Shake.Action [FilePath]
transitiveImportsOf fn =
    Shake.need [fn] >> Trans.liftIO (transitiveImportsOf_ fn)

transitiveImportsOf_ :: FilePath -> IO [FilePath]
transitiveImportsOf_ fn = go Set.empty [fn]
    where
    go checked (fn:fns)
        | fn `Set.member` checked = go checked fns
        | otherwise = do
            imports <- importsOf_ fn
            let checked' = Set.insert fn checked
            go checked' (fns ++ filter (`Set.notMember` checked') imports)
    go checked [] = return $ Set.toList checked

fileOf :: ModuleName -> IO (Maybe FilePath)
fileOf mod =
    Util.ifM (Directory.doesFileExist fn) (return (Just fn)) $
    Util.ifM (Directory.doesFileExist (fn ++ "c"))
        (return (Just (fn ++ "c"))) (return Nothing)
    where
    fn = B.unpack $ B.map slash mod `B.append` ".hs"
    slash c = if c == '.' then '/' else c

parseImports :: [B.ByteString] -> [ModuleName]
parseImports = Maybe.mapMaybe (parse . B.words)
    where
    parse (w1:w2:w3:_) | (w1, w2) == ("import", "qualified") = Just w3
    parse (w1:w2:_) | w1 == "import" = Just w2
    parse _ = Nothing

readImportBlock :: FilePath -> IO [B.ByteString]
readImportBlock fn = withFile fn header
    where
    read accum hdl rest = do
        eof <- IO.hIsEOF hdl
        if eof then return accum else rest =<< B.hGetLine hdl
    header hdl = read [] hdl $ \line ->
        if isImport line then imports [line] hdl else header hdl
    imports accum hdl = read accum hdl $ \line ->
        if postImports line then return accum else imports (line:accum) hdl
    isImport = ("import " `B.isPrefixOf`)
    -- Icky.  The imports block can have CPP and other hackery, but if I see
    -- a type signature, I'm probably out of it.
    postImports = (" :: " `B.isInfixOf`)

withFile :: FilePath -> (IO.Handle -> IO a) -> IO a
withFile fn = Exception.bracket (IO.openFile fn IO.ReadMode) IO.hClose
