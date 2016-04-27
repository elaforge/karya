-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utility to print out any of the binary formats used.
module App.Dump where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified System.Environment as Environment
import qualified System.Exit
import qualified Text.Printf as Printf

import qualified Util.Git as Git
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize

import qualified Synth.Shared.Note
import Global


main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    case args of
        [fname]
            | SaveGit.is_git fname -> dump_git fname Nothing
            | otherwise -> dump fname >>= \x -> case x of
                Nothing -> return ()
                Just err -> do
                    Text.IO.putStrLn $ txt fname <> ": " <> err
                    System.Exit.exitWith (System.Exit.ExitFailure 1)
        [fname, commit] | SaveGit.is_git fname -> dump_git fname (Just commit)
        _ -> usage $ "expected a single filename, got: " ++ Seq.join ", " args
    where
    usage msg = do
        putStrLn msg
        putStrLn "usage: dump file [ git-commit ]"
        System.Exit.exitWith (System.Exit.ExitFailure 1)

dump :: FilePath -> IO (Maybe Text)
dump fname =
    try fname Cmd.Serialize.score_magic ((:[]) . dump_score) $
    try fname Cmd.Serialize.allocations_magic ((:[]) . Pretty.formatted) $
    try fname Cmd.Serialize.views_magic ((:[]) . Pretty.formatted) $
    try fname DiffPerformance.midi_magic dump_midi $
    try fname Synth.Shared.Note.notes_magic (map Pretty.formatted) $
    return $ Just "no magic codes match"

-- | Try to unserialize the file, and try the passed continuation if it failed
-- with Serialize.BadMagic.
try :: Serialize.Serialize a => FilePath -> Serialize.Magic a
    -> (a -> [Text]) -> IO (Maybe Text) -> IO (Maybe Text)
try fname magic dump next = do
    val <- Serialize.unserialize magic fname
    case val of
        Right val ->
            mapM_ (Text.IO.putStrLn . Text.strip) (dump val) >> return Nothing
        Left Serialize.BadMagic {} -> next
        Left err -> return $ Just (pretty err)

die :: Text -> IO a
die msg = do
    Text.IO.putStrLn $ "Error: " <> msg
    System.Exit.exitWith (System.Exit.ExitFailure 1)

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
    Printf.printf "commit: %s, names: %s\n" (prettys commit)
        (untxt (Text.intercalate ", " names))
    Text.IO.putStrLn $ dump_score state

dump_score :: State.State -> Text
dump_score = clean . Pretty.formatted

dump_midi :: Vector.Vector Midi.WriteMessage -> [Text]
dump_midi = map Pretty.formatted . Vector.toList

clean :: Text -> Text
clean text = case Text.lines text of
    "State" : first : rest ->
        -- +2 to drop the '{ ' and ', ' on each line.
        let indent = Text.length (Text.takeWhile (==' ') first) + 2
        in Text.unlines $ map (Text.drop indent) (first : rest)
    _ -> text
