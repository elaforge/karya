-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Shake.Util (
    -- * shake specific
    Cmdline, cmdline, system, staunchSystem, shell, putQuietNormal
    , findFiles, findHs, runIO

    -- * ghc
    , sandboxPackageDb
    -- * general
    , ifM, whenM
) where
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Trans (liftIO)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as FilePath
import qualified System.Exit as Exit
import qualified System.FilePath
import System.FilePath ((</>))
import qualified System.Process as Process


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
        error $ "Failed:\n" ++ unwords (cmd : args)
doCmdline _ (abbr, output, []) =
    error $ "0 args for cmdline: " ++ show (abbr, output)

system :: FilePath -> [String] -> Shake.Action ()
system cmd args = cmdline (unwords (cmd:args), "", cmd:args)

-- | Like 'system', but don't ignore the exit code.
staunchSystem :: FilePath -> [String] -> Shake.Action ()
staunchSystem cmd args = doCmdline True (unwords (cmd:args), "", cmd:args)

shell :: String -> Shake.Action ()
shell cmd = do
    Shake.putQuiet cmd
    res <- Shake.traced ("shell: " ++ cmd) $ Process.system cmd
    when (res /= Exit.ExitSuccess) $
        error $ "Failed:\n" ++ cmd

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
runIO :: Show a => Shake.Action a -> IO ()
runIO action = Shake.shake Shake.shakeOptions $ Shake.action $ do
    result <- action
    liftIO $ print result

-- * ghc

-- | If there is a cabal sandbox in the current directory, return the path to
-- its package db.
sandboxPackageDb :: IO (Maybe FilePath)
sandboxPackageDb = do
    -- I don't care about ghc-pkg's output, but cabal -v will print the cmdline
    -- when it runs it.  As far as I know, this is the only way to get that
    -- info.
    (code, stdout, _stderr) <- Process.readProcessWithExitCode
        "cabal" ["-v", "sandbox", "hc-pkg", "list", "base"] ""
    return $ case code of
        Exit.ExitFailure {} -> Nothing
        Exit.ExitSuccess -> msum $ map extract (lines stdout)
    where
    extract = msum . map packageDb . words
    packageDb w | flag `List.isPrefixOf` w = Just $ drop (length flag) w
    packageDb _ = Nothing
    flag = "--package-db="

-- * general

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond consequent alternative =
    cond >>= \b -> if b then consequent else alternative

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond consequent = cond >>= \b -> when b consequent
