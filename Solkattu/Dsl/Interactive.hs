-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for ghci.
module Solkattu.Dsl.Interactive (
    diff, diffw, printInstrument
) where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Process as Process

import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Solkattu as Solkattu

import Global


printInstrument :: Solkattu.Notation stroke => Bool -> Bool
    -> Korvai.Instrument stroke -> [Korvai.Sequence] -> Format.Abstraction
    -> Korvai.Korvai -> IO ()
printInstrument lint writeDiff inst defaultStrokes abstraction korvai = do
    let config = Terminal.defaultConfig { Terminal._abstraction = abstraction }
    let (out, hasError) = Terminal.formatInstrument config inst korvai
    mapM_ Text.IO.putStrLn out
    when (not hasError && lint) $
        Text.IO.putStr $ Korvai.lint inst defaultStrokes korvai
    let write = Text.IO.writeFile (gitRepo </> korvaiPath)
            (Text.unlines out)
    when (not hasError && writeDiff) $
        ifM (Directory.doesDirectoryExist (gitRepo </> ".git"))
            (commit gitRepo >> write)
            (createRepo gitRepo >> write >> commit gitRepo)

-- | Line-oriented diff against the previous realize.
diff :: IO ()
diff = mapM_ putStrLn . drop 5 . lines
    =<< readCwd gitRepo "git" diffArgs
    -- The first 5 lines are diff hunk metadata that I don't care about.

-- | Word-oriented diff against the previous realize.
diffw :: IO ()
diffw = mapM_ putStrLn . drop 5 . lines
    =<< readCwd gitRepo "git"
        (diffArgs ++ ["--word-diff", "--word-diff-regex=."])

diffArgs :: [String]
diffArgs = ["diff", "--unified=100", "--color=always"]

korvaiPath :: FilePath
korvaiPath = "korvai.txt"

createRepo :: FilePath -> IO ()
createRepo dir = do
    Directory.createDirectoryIfMissing True dir
    callCwd dir "git" ["init"]
    writeFile (gitRepo </> korvaiPath) "\n"
    callCwd dir "git" ["add", korvaiPath]

commit :: FilePath -> IO ()
commit dir =
    void $ readCwd dir "git" ["commit", "--quiet", "-m", "update", korvaiPath]

readCwd :: FilePath -> FilePath -> [String] -> IO String
readCwd cwd cmd args = do
    (_, stdout, _stderr) <- Process.readCreateProcessWithExitCode
        ((Process.proc cmd args) { Process.cwd = Just cwd }) ""
    return stdout

callCwd :: FilePath -> FilePath -> [String] -> IO ()
callCwd cwd cmd args = do
    exit <- Process.withCreateProcess
        ((Process.proc cmd args)
            { Process.delegate_ctlc = True
            , Process.cwd = Just cwd
            })
        (\_ _ _ p -> Process.waitForProcess p)
    case exit of
        Exit.ExitSuccess -> return ()
        Exit.ExitFailure r -> errorIO $ "subprocess: " <> showt r


gitRepo :: FilePath
gitRepo = "../data/solkattu-korvai"
