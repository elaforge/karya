-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for ghci.
module Solkattu.Interactive (
    _ghciSave, diff, diffw, printInstrument
) where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Process as Process

import qualified Util.File as File
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Solkattu as Solkattu

import Global


-- . save korvai variable or get last one
-- . make a tmp git dir, if it doesn't exist
-- . commit in git dir
-- . writeText to the git dir, :kdiff will diff changes

-- TODO call from ghci macro
_ghciSave :: String -> IO String
_ghciSave name = do
    name <- resolveName name
    saveName name
    return $ unwords ["realizep", show name, name]
    -- return the code to execute

printInstrument :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> [Korvai.Sequence] -> Format.Abstraction -> Korvai.Korvai -> IO ()
printInstrument inst defaultStrokes abstraction korvai = do
    let config = Terminal.defaultConfig { Terminal._abstraction = abstraction }
    let out = Terminal.formatInstrument config inst korvai
    mapM_ Text.IO.putStrLn out
    Text.IO.putStr $ Korvai.lint inst defaultStrokes korvai
    let write = Text.IO.writeFile (gitRepo </> korvaiPath)
            (Text.unlines out)
    ifM (Directory.doesDirectoryExist (gitRepo </> ".git"))
        (commit gitRepo >> write)
        (createRepo gitRepo >> write >> commit gitRepo)

-- TODO drop first 5 lines
diff :: IO ()
diff = callCwd gitRepo "git" ["diff", "--unified=100"]

diffw :: IO ()
diffw = callCwd gitRepo "git"
    ["diff", "--unified=40", "--word-diff", "--word-diff-regex=."]

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
    readCwd dir "git" ["commit", "--quiet", "-m", "update", korvaiPath]

resolveName :: String -> IO String
resolveName name
    | name /= "" = return name
    | otherwise =
        fromMaybe "" <$> File.ignoreEnoent (readFile (gitRepo </> korvaiPath))

saveName :: String -> IO ()
saveName = writeFile (gitRepo </> "korvai")

readCwd :: FilePath -> FilePath -> [String] -> IO ()
readCwd cwd cmd args = do
    (_, _stdout, _stderr) <- Process.readCreateProcessWithExitCode
        ((Process.proc cmd args) { Process.cwd = Just cwd }) ""
    return ()

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
