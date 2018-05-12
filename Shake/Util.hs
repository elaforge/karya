-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
module Shake.Util (
    -- * shake specific
    Cmdline, cmdline, system, staunchSystem, shell, putQuietNormal
    , findFiles, findHs, runIO

    -- * ghc
    , sandboxPackageDb
    -- * platform
    , Platform(..), platform
    -- * general
    , ifM, whenM, errorIO
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (liftIO)

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as FilePath
import qualified System.Exit as Exit
import qualified System.FilePath
import System.FilePath ((</>))
import qualified System.Info
import qualified System.Process as Process

import qualified Util.File as File
import qualified Util.Seq as Seq


-- * shake specific

-- | (short_name_for_cmd, output_file_name, [cmd])
type Cmdline = (String, String, [String])

-- | This is like 'system', but expect a Cmdline.  It logs an abbreviated
-- cmdline at quiet, and a complete cmdline at normal.
--
-- Shake logs @# key@ msgs at and cmds at loud.  However, I think cmds should
-- be at normal, and the keys should be at loud, because the cmds give
-- a progress indication, while the keys just make any compiler errors scroll
-- off the screen.
cmdline :: Cmdline -> Shake.Action ()
cmdline = doCmdline False

doCmdline :: Bool -> Cmdline -> Shake.Action ()
doCmdline staunch (abbr, output, cmd_:args) = do
    let cmd = FilePath.toNative cmd_
    let desc = abbr ++ if null output then "" else ": " ++ output
    putQuietNormal desc (unwords (cmd:args))
    res <- Shake.traced ("cmdline: " ++ desc) $
        Process.rawSystem "nice" (cmd : args)
    when (not staunch && res /= Exit.ExitSuccess) $
        errorIO $ "Failed:\n" ++ unwords (cmd : args)
doCmdline _ (abbr, output, []) =
    errorIO $ "0 args for cmdline: " ++ show (abbr, output)

system :: FilePath -> [String] -> Shake.Action ()
system cmd args = cmdline (unwords (cmd:args), "", cmd:args)

-- | Like 'system', but don't ignore the exit code.
staunchSystem :: FilePath -> [String] -> Shake.Action ()
staunchSystem cmd args = doCmdline True (unwords (cmd:args), "", cmd:args)

-- | Run a shell command, and crash if it fails.
shell :: String -> Shake.Action ()
shell cmd = do
    Shake.putQuiet cmd
    res <- Shake.traced ("shell: " ++ cmd) $ Process.system cmd
    when (res /= Exit.ExitSuccess) $
        errorIO $ "Failed:\n" ++ cmd

-- | Log one thing at quiet, and another at normal or above.
putQuietNormal :: String -> String -> Shake.Action ()
putQuietNormal quiet normal = do
    verbosity <- Shake.getVerbosity
    if verbosity == Shake.Quiet then Shake.putQuiet quiet
        else if verbosity > Shake.Quiet then Shake.putNormal normal
        else return ()

-- | Recursively find files below a directory.
findFiles :: (FilePath -> Bool) -> Shake.FilePattern -> FilePath
    -> Shake.Action [FilePath]
findFiles acceptDir filePattern dir = do
    fns <- map (System.FilePath.normalise . (dir </>)) <$>
        Shake.getDirectoryFiles dir [filePattern]
    dirs <- map (dir </>) . filter acceptDir <$> Shake.getDirectoryDirs dir
    rest <- mapM (findFiles acceptDir filePattern) dirs
    return $ concat (fns:rest)

findHs :: Shake.FilePattern -> FilePath -> Shake.Action [FilePath]
findHs = findFiles $ all Char.isUpper . take 1

-- | Run an Action, useful for interactive testing.
runIO :: FilePath -> Shake.Action a -> IO a
runIO shakeDir action = do
    mvar <- MVar.newEmptyMVar
    Shake.shake options $ Shake.action $ do
        result <- action
        liftIO $ MVar.putMVar mvar result
    MVar.takeMVar mvar
    where
    options = Shake.shakeOptions { Shake.shakeFiles = shakeDir }

-- * platform

data Platform = Mac | Linux deriving (Eq, Show)

platform :: Platform
platform = case System.Info.os of
    "darwin" -> Mac
    "linux" -> Linux
    _ -> error $ "unknown platform: " <> show System.Info.os

-- * ghc

-- | If there is a cabal sandbox in the current directory, return the path to
-- its package db.
sandboxPackageDb :: IO (Maybe FilePath)
sandboxPackageDb = do
    text <- File.ignoreEnoent $ Text.IO.readFile "cabal.sandbox.config"
    return $ parse =<< text
    where
    -- package-db: /Users/elaforge/src/seq/sandbox/.cabal-sandbox/...
    parse = fmap Text.unpack . Seq.head
        . Maybe.mapMaybe (Text.stripPrefix "package-db: ") . Text.lines

-- * general

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond consequent alternative =
    cond >>= \b -> if b then consequent else alternative

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond consequent = cond >>= \b -> when b consequent

errorIO :: Trans.MonadIO m => String -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall
