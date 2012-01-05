module Shake.Util (
    -- * shake specific
    Cmdline, system, shell, putNormalLoud
    , findFiles, findHs, runIO

    -- * general
    , ifM, whenM
) where
import Control.Applicative ((<$>))
import Control.Monad
import qualified Control.Monad.Trans as Trans

import qualified Data.Char as Char
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as FilePath
import qualified System.Cmd as Cmd
import qualified System.Exit as Exit
import qualified System.FilePath
import System.FilePath ((</>))


-- * shake specific

type Cmdline = (String, String, [String])

system :: Cmdline -> Shake.Action ()
system (abbr, output, cmd_:args) = do
    let cmd = FilePath.toNative cmd_
    let desc = abbr ++ ": " ++ output
    putNormalLoud desc (unwords (cmd:args))
    res <- Shake.traced (crunch ("system: " ++ desc)) $ Cmd.rawSystem cmd args
    when (res /= Exit.ExitSuccess) $
        error $ "Failed:\n" ++ unwords (cmd : args)
system (abbr, output, []) =
    error $ "0 args for system: " ++ show (abbr, output)

shell :: String -> Shake.Action ()
shell cmd = do
    Shake.putLoud cmd
    res <- Shake.traced (crunch ("shell: " ++ cmd)) $ Cmd.system cmd
    when (res /= Exit.ExitSuccess) $
        error $ "Failed:\n" ++ cmd

-- Work around shake bug where only the first word is taken.
crunch = filter (/=' ')

putNormalLoud :: String -> String -> Shake.Action ()
putNormalLoud normal loud = do
    verbosity <- Shake.getVerbosity
    case verbosity of
        Shake.Normal -> Shake.putNormal normal
        Shake.Loud -> Shake.putLoud loud
        _ -> return ()

-- | Recursively find files below a directory.
findFiles :: (FilePath -> Bool) -> Shake.FilePattern -> FilePath
    -> Shake.Action [FilePath]
findFiles acceptDir filePattern dir = do
    fns <- map (System.FilePath.normalise . (dir </>)) <$>
        Shake.getDirectoryFiles dir filePattern
    dirs <- map (dir </>) . filter acceptDir <$> Shake.getDirectoryDirs dir
    rest <- mapM (findFiles acceptDir filePattern) dirs
    return $ concat (fns:rest)

findHs :: Shake.FilePattern -> FilePath -> Shake.Action [FilePath]
findHs = findFiles $ all Char.isUpper . take 1

runIO :: (Show a) => Shake.Action a -> IO ()
runIO action = Shake.shake Shake.shakeOptions $ Shake.action $ do
    result <- action
    Trans.liftIO $ print result

-- * general

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond consequent alternative =
    cond >>= \b -> if b then consequent else alternative

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond consequent = cond >>= \b -> when b consequent
