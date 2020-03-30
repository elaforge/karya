-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
module Shake.Util (
    -- * shake specific
    Cmdline, cmdline, system, systemKeepGoing, shell
    , findFiles, findHs, runIO

    -- * ghc
    , sandboxPackageDb
    -- * platform
    , Platform(..), platform
    -- * general
    , ifM, whenM, errorIO
) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans
import           Control.Monad.Trans (liftIO)

import qualified Data.Char as Char
import qualified Data.Function as Function
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as FilePath
import qualified System.CPUTime as CPUTime
import qualified System.Console.Concurrent as Concurrent
import qualified System.Console.Regions as Regions
import qualified System.Exit as Exit
import qualified System.FilePath
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Info
import qualified System.Process as Process

import qualified Text.Printf as Printf

import qualified Util.File as File
import qualified Util.Seq as Seq

import           Control.Monad


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
cmdline cmd@(abbr, _, cmdline) = do
    fancy <- fancyOutput
    Shake.putNormal $ unwords $ "%" : cmdline
    Shake.traced ("cmdline:" <> abbr) . liftIO $ doCmdline fancy False cmd

data Metric = Metric {
    metricCpu :: !Double
    , metricWall :: !Time.UTCTime
    } deriving (Show)

metric :: IO Metric
metric = Metric <$> (cpuToSec <$> CPUTime.getCPUTime) <*> Time.getCurrentTime
    where cpuToSec s = fromIntegral s / fromIntegral (10^12)

diffMetric :: Metric -> Metric -> String
diffMetric (Metric _cpu1 time1) (Metric _cpu2 time2) =
    -- I used to print the cpu time too, but it's always really low.
    Printf.printf "%.2fs" (toSecs (time1 `Time.diffUTCTime` time2))
    where
    toSecs :: Time.NominalDiffTime -> Double
    toSecs = realToFrac

-- | If true, use concurrent output, otherwise use shake's default output.
fancyOutput :: Shake.Action Bool
fancyOutput = (==Shake.Quiet) <$> Shake.getVerbosity

doCmdline :: Bool -> Bool -> Cmdline -> IO ()
doCmdline _ _ (abbr, output, []) =
    errorIO $ "0 args for cmdline: " ++ show (abbr, output)
doCmdline fancyOutput keepGoing (abbr, output, cmd_:args) = do
    start <- metric
    notRequired <- withRegion (do
        (exit, ghcNotRequired) <- createProcessConcurrent "nice" (cmd:args)
        when (not keepGoing && exit /= Exit.ExitSuccess) $
            errorIO $ "Failed:\n" ++ unwords (cmd : args)
        return ghcNotRequired
        ) `Exception.onException` do
            timing <- showMetric start
            put $ unwords [desc, timing, "(aborted)"]
    timing <- showMetric start
    put $ unwords [desc, timing] <> (if notRequired then " (skipped)" else "")
    where
    showMetric start = do
        end <- metric
        return $ unwords ["-", diffMetric end start]
    cmd = FilePath.toNative cmd_
    desc = ellipsis 127 abbr <> if null output then "" else ": " <> output
    put | fancyOutput = Concurrent.outputConcurrent . (<>"\n")
        | otherwise = putStrLn
    withRegion action
        | fancyOutput = Regions.withConsoleRegion Regions.Linear $ \region -> do
            Regions.setConsoleRegion region desc
            action
        | otherwise = action

-- | Some command lines are really long.
ellipsis :: Int -> String -> String
ellipsis len line
    | lineLen > len = take len line <> "... [" <> show lineLen <> " chars]"
    | otherwise = line
    where lineLen = length line

-- | This is basically like 'Concurrent.createProcessConcurrent', except that
-- one doesn't get along with ghc's colorized stderr, for some reason.  I tried
-- to see why, but its implementation is crazy complicated due to it wanting
-- to buffer in temp files if the output is too large.
createProcessConcurrent :: FilePath -> [String] -> IO (Exit.ExitCode, Bool)
createProcessConcurrent cmd args = do
    let proc = (Process.proc cmd args)
            { Process.std_in = Process.NoStream
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }
    ghcNotRequired <- IORef.newIORef False
    Process.withCreateProcess proc $ \Nothing (Just outh) (Just errh) pid -> do
        outWriter <- Async.async (streamHandle ghcNotRequired outh)
        errWriter <- Async.async (streamHandle ghcNotRequired errh)
        Async.wait outWriter
        Async.wait errWriter
        (,) <$> Process.waitForProcess pid <*> IORef.readIORef ghcNotRequired
    where
    streamHandle ghcNotRequired hdl = Function.fix $ \loop ->
        File.ignoreEOF (Text.IO.hGetLine hdl) >>= \x -> case x of
            Nothing -> IO.hClose hdl
            Just line -> do
                -- This shows up on ghc's stdout.
                if line == "compilation IS NOT required"
                    then IORef.writeIORef ghcNotRequired True
                    else if ignoreLine line then return ()
                    else Concurrent.outputConcurrent (line <> "\n")
                loop
    -- A bug in OS X 10.13.6 causes harmless but annoying warnings.
    -- This is supposedly a solution:
    -- https://gist.github.com/wawiesel/eba461de5f5e38f7f0ac93ae3676b484
    -- No solution from Apple as of 2019-03-09:
    -- https://github.com/golang/go/issues/26073
    --
    -- It seems simpler and safer to just ignore it.
    ignoreLine = ("ld: warning: text-based stub file " `Text.isPrefixOf`)

system :: FilePath -> [String] -> Shake.Action ()
system cmd args = cmdline (unwords (cmd:args), "", cmd:args)

-- | Like 'system', but don't ignore the exit code.
systemKeepGoing :: FilePath -> [String] -> Shake.Action ()
systemKeepGoing cmd args = do
    fancy <- fancyOutput
    liftIO $ doCmdline fancy True (unwords (cmd:args), "", cmd:args)

-- | Run a shell command, and crash if it fails.
shell :: String -> Shake.Action ()
shell cmd = do
    Shake.putNormal cmd
    res <- Shake.traced ("shell: " ++ cmd) $ Process.system cmd
    when (res /= Exit.ExitSuccess) $
        errorIO $ "Failed:\n" ++ cmd

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
