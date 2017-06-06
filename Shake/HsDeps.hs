-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
module Shake.HsDeps (
    Generated
    , importsOf, transitiveImportsOf
    , importsPackagagesOf_
    , loadPackageDb, savePackageDb
) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set

import qualified Development.Shake as Shake
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Util.Map
import qualified Shake.Util as Util


type Package = String
type ModuleName = B.ByteString

-- | Normally 'importsOf' and 'transitiveImportsOf' filter out files that don't
-- exist, under the assumption that they belong to external packages.  This
-- set contains exceptions to that, so they will get a need call, so they can
-- be generated.
type Generated = Set.Set FilePath

-- | Find files of modules this module imports, in the form A/B.hs or A/B.hsc.
-- Paths that don't exist are assumed to be package imports and are omitted.
importsOf :: Generated -> Maybe [String] -> FilePath -> Shake.Action [FilePath]
importsOf generated cppFlags fn = do
    Shake.need [fn]
    Trans.liftIO (importsOf_ generated cppFlags fn)

importsOf_ :: Generated
    -> Maybe [String] -- ^ If Just, first run CPP with these flags.
    -> FilePath -> IO [FilePath]
importsOf_ generated cppFlags fn = do
    mods <- parseImports <$> preprocess cppFlags fn
    Maybe.catMaybes <$> mapM (fileOf generated) mods

-- * PackageDb

-- | Map a module name to the package it comes from.
type PackageDb = Map.Map ModuleName Package

-- | Get local imports and package dependencies.
--
-- I thought I wound use this but wound up not wanting it.  But I'll leave the
-- basic implementation here in case I change my mind.  This still needs to
-- be integrated with 'importsOf' and 'transitiveImportsOf'.
importsPackagagesOf_ :: PackageDb
    -> Generated -> Maybe [String] -- ^ If Just, first run CPP with these flags.
    -> FilePath -> IO ([FilePath], [Package])
importsPackagagesOf_ packageDb generated cppFlags fn = do
    mods <- parseImports <$> preprocess cppFlags fn
    files <- Maybe.catMaybes <$> mapM (fileOf generated) mods
    return (files, Maybe.mapMaybe (`Map.lookup` packageDb) mods)

-- | Load cached module to package db.
loadPackageDb :: FilePath -> Shake.Action PackageDb
loadPackageDb fn = do
    Shake.need [fn]
    Trans.liftIO $ do
        result <- Serialize.decode <$> B.readFile fn
        either (Util.errorIO . (("load from " ++ fn ++ ":")++)) return result

-- | Call 'getModuleToPackage' and save its contents to the file.
-- If there are colliding modules, throw an IO exception.
savePackageDb :: [Package] -> FilePath -> IO ()
savePackageDb packages cacheFn = do
    (packageDb, collisions) <- getModuleToPackage packages
    unless (null collisions) $
        Util.errorIO $ "modules found in >1 package:\n" ++ unlines
            [B.unpack k ++ ": " ++ unwords vs | (k, vs) <- collisions]
    B.writeFile cacheFn $ Serialize.encode packageDb

getModuleToPackage :: [Package]
    -> IO (Map.Map ModuleName Package, [(ModuleName, [Package])])
    -- ^ also return any modules found under multiple packages
getModuleToPackage packages = do
    packageMods <- zip packages <$> mapM getExposedModules packages
    return $ Util.Map.unique2
        [(mod, package) | (package, mods) <- packageMods, mod <- mods]

getExposedModules :: Package -> IO [ModuleName]
getExposedModules package =
    parse <$> processStdout "ghc-pkg" ["field", package, "exposed-modules"]
    where
    -- The first word is "exposed-modules:".
    -- TODO gets duplicates if you have the same package installed under
    -- different versions.
    parse = drop 1 . B.words

-- | Like 'importsOf' but transitive.  Includes the given module.
--
-- TODO Technically I should run CPP on the output of hsc2hs, which means
-- this should map the module names to the appropriate .hs and 'need' it.
-- Otherwise the '#include' that belongs to hsc2hs will get processed by CPP.
transitiveImportsOf :: Generated -> (FilePath -> Maybe [String])
    -> FilePath -> Shake.Action [FilePath]
transitiveImportsOf generated cppFlagsOf fn = do
    Shake.need [fn]
    Trans.liftIO $ transitiveImportsOf_ generated cppFlagsOf fn

transitiveImportsOf_ :: Generated -> (FilePath -> Maybe [String])
    -> FilePath -> IO [FilePath]
transitiveImportsOf_ generated cppFlagsOf fn = go Set.empty [fn]
    where
    go checked (fn:fns)
        | fn `Set.member` checked = go checked fns
        | otherwise = do
            imports <- importsOf_ generated (cppFlagsOf fn) fn
            let checked' = Set.insert fn checked
            go checked' (fns ++ filter (`Set.notMember` checked') imports)
    go checked [] = return (Set.toList checked)

fileOf :: Generated -> ModuleName -> IO (Maybe FilePath)
fileOf generated mod
    | fn `Set.member` generated = return $ Just fn
    | otherwise = findExistingFile [fn, fn ++ "c"]
    where
    fn = B.unpack $ B.map slash mod `B.append` ".hs"
    slash c = if c == '.' then '/' else c

-- | Any line that starts with @import@ should be an import.  It's a reserved
-- word so I think that's safe?
parseImports :: B.ByteString -> [ModuleName]
parseImports = Maybe.mapMaybe (parse . B.words) . B.lines
    where
    parse (w1:w2:w3:_) | (w1, w2) == ("import", "qualified") = Just w3
    parse (w1:w2:_) | w1 == "import" = Just w2
    parse _ = Nothing

-- | Read the file, and preprocess with CPP if cppFlags are given.
preprocess :: Maybe [String] -> FilePath -> IO B.ByteString
preprocess Nothing fn = B.readFile fn
preprocess (Just flags) fn = processStdout "cpp" (["-w", "-P"] ++ flags ++ [fn])
    -- Use -w to suppress warnings.  CPP doesn't understand haskell comments
    -- and will warn about unterminated 's in comments.  -P suppresses the
    -- '# line' stuff I don't care about.


-- * util

processStdout :: FilePath -> [String] -> IO B.ByteString
processStdout cmd args = Exception.bracket open IO.hClose B.hGetContents
    where
    open = do
        (_, Just stdout, _, _) <- loggedProcess $
            (Process.proc cmd args) { Process.std_out = Process.CreatePipe }
        return stdout

-- | Like 'Process.createProcess', but actually report when the binary isn't
-- found.
loggedProcess :: Process.CreateProcess -> IO (Maybe IO.Handle,
       Maybe IO.Handle, Maybe IO.Handle, Process.ProcessHandle)
loggedProcess create = do
    r@(_, _, _, pid) <- Process.createProcess create
    Concurrent.forkIO $ do
        code <- Process.waitForProcess pid
        case code of
            Exit.ExitFailure c -> IO.hPutStrLn IO.stderr $
                "subprocess " ++ show (binaryOf create) ++ " failed: "
                ++ if c == 127 then "binary not found" else show c
            _ -> return ()
    return r
    where
    binaryOf create = case Process.cmdspec create of
        Process.RawCommand fn _ -> fn
        Process.ShellCommand cmd -> takeWhile (/=' ') cmd

findExistingFile :: [FilePath] -> IO (Maybe FilePath)
findExistingFile (fn:fns) =
    Util.ifM (Directory.doesFileExist fn) (return (Just fn))
        (findExistingFile fns)
findExistingFile [] = return Nothing
