-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Quick hack to print out the binary save files.
module App.Dump where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment
import qualified System.Exit
import Text.Printf

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Ui.State as State
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import Global


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [fname]
            | SaveGit.is_git fname -> dump_git fname Nothing
            | otherwise -> dump_simple fname
        [fname, commit] | SaveGit.is_git fname -> dump_git fname (Just commit)
        _ -> usage $ "expected a single filename, got: " ++ Seq.join ", " args
    where
    usage msg = do
        putStrLn msg
        putStrLn "usage: dump name [ git-commit ]"
        System.Exit.exitWith (System.Exit.ExitFailure 1)

die :: Text -> IO a
die msg = do
    Text.IO.putStrLn $ "Error: " <> msg
    System.Exit.exitWith (System.Exit.ExitFailure 1)

dump_simple :: FilePath -> IO ()
dump_simple fname = do
    save <- either (die . (("reading " <> showt fname <> ":") <>)) return
        =<< Save.read_state_ fname
    state <- maybe (die $ "file not found: " <> showt fname) return save
    pprint_state state

-- | Either a commit hash or a save point ref.
dump_git :: FilePath -> Maybe String -> IO ()
dump_git repo maybe_arg = do
    maybe_commit <- case maybe_arg of
        Nothing -> return Nothing
        Just arg -> do
            commit <- maybe (die $ "couldn't find commit for " <> showt arg)
                return =<< SaveGit.infer_commit repo arg
            return (Just commit)
    (state, commit, names) <- either
        (die . (("reading " <> showt repo <> ":") <>)) return
            =<< SaveGit.load repo maybe_commit
    printf "commit: %s, names: %s\n" (prettys commit)
        (untxt (Text.intercalate ", " names))
    pprint_state state

pprint_state :: State.State -> IO ()
pprint_state = Text.IO.putStrLn . clean . Pretty.formatted

clean :: Text -> Text
clean text = case Text.lines text of
    "State" : first : rest ->
        -- +2 to drop the '{ ' and ', ' on each line.
        let indent = Text.length (Text.takeWhile (==' ') first) + 2
        in Text.unlines $ map (Text.drop indent) (first : rest)
    _ -> text
