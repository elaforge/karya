module Shake.Util (
    -- * shake specific
    Cmdline, system, shell, putNormalLoud

    -- * general
    , findHs, ifM, whenM
) where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.Char as Char
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as FilePath
import qualified System.Cmd as Cmd
import qualified System.Directory as Directory
import qualified System.Exit as Exit
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

putNormalLoud :: String -> String -> Shake.Action ()
putNormalLoud normal loud = do
    verbosity <- Shake.getVerbosity
    case verbosity of
        Shake.Normal -> Shake.putNormal normal
        Shake.Loud -> Shake.putLoud loud
        _ -> return ()


findHs :: (Trans.MonadIO m) => (FilePath -> Bool) -> FilePath -> m [FilePath]
findHs acceptHs dir = Trans.liftIO $ findFiles capital
    (\fn -> capital fn && FilePath.takeExtension fn == ".hs" && acceptHs fn)
    dir
    where capital = all Char.isUpper . take 1 . FilePath.takeFileName

-- Work around shake bug where only the first word is taken.
crunch = filter (/=' ')

-- * general

-- | Recursively find files below a directory.
findFiles :: (FilePath -> Bool) -> (FilePath -> Bool) -> FilePath
    -> IO [FilePath]
findFiles acceptDir acceptFile dir = do
    (dirs, files) <- partitionM Directory.doesDirectoryExist =<< listDir dir
    subs <- mapM (findFiles acceptDir acceptFile) (filter acceptDir dirs)
    return $ concat (filter acceptFile files : subs)

listDir :: FilePath -> IO [FilePath]
listDir dir =
    fmap (map add . filter (not . (`elem` [".", ".."])))
        (Directory.getDirectoryContents dir)
    where add = if dir == "." then id else (dir </>)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = ifM (f x)
    (partitionM f xs >>= \(ts, fs) -> return (x:ts, fs))
    (partitionM f xs >>= \(ts, fs) -> return (ts, x:fs))

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond consequent alternative =
    cond >>= \b -> if b then consequent else alternative

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond consequent = cond >>= \b -> when b consequent
