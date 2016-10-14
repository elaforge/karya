-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utility to print out any of the binary formats used.
module App.Dump (main) where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit

import qualified Text.Printf as Printf

import qualified Util.Git as Git
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize

import qualified Synth.Shared.Note
import Global


data Flag = DumpAll | DumpAllocations | DumpConfig
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["allocations"] (GetOpt.NoArg DumpAllocations)
        "dump allocations"
    , GetOpt.Option [] ["config"] (GetOpt.NoArg DumpConfig) "dump config"
    ]

main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    let flag = fromMaybe DumpAll (Seq.last flags)
    case args of
        [fname]
            | SaveGit.is_git fname -> dump_git flag fname Nothing
            | otherwise -> dump flag fname >>= \x -> case x of
                Nothing -> return ()
                Just err -> do
                    Text.IO.putStrLn $ txt fname <> ": " <> err
                    System.Exit.exitWith (System.Exit.ExitFailure 1)
        [fname, commit] | SaveGit.is_git fname ->
            dump_git flag fname (Just commit)
        _ -> usage $ "expected a single filename, got: " ++ show args
    where
    usage msg = do
        putStrLn "usage: dump file [ git-commit ]"
        putStr (GetOpt.usageInfo msg options)
        System.Exit.exitWith (System.Exit.ExitFailure 1)

dump :: Flag -> FilePath -> IO (Maybe Text)
dump flag fname =
    try fname Cmd.Serialize.score_magic ((:[]) . dump_score flag) $
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
dump_git :: Flag -> FilePath -> Maybe String -> IO ()
dump_git flag repo maybe_arg = do
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
    Text.IO.putStrLn $ dump_score flag state

dump_score :: Flag -> State.State -> Text
dump_score flag = strip_name . format
    where
    format state = case flag of
        DumpAll -> Pretty.formatted state
        DumpAllocations -> Pretty.formatted $
            StateConfig.config_allocations $ State.state_config state
        DumpConfig -> Pretty.formatted $ State.state_config state

dump_midi :: Vector.Vector Midi.WriteMessage -> [Text]
dump_midi = map Pretty.formatted . Vector.toList

-- | Remove the record name and dedent, since it's redundant.
strip_name :: Text -> Text
strip_name text = case Text.lines text of
    name : first : rest | is_record name ->
        -- +2 to drop the '{ ' and ', ' on each line.
        let indent = Text.length (Text.takeWhile (==' ') first) + 2
        in Text.unlines $ map (Text.drop indent) (first : rest)
    _ -> text
    where
    is_record = (`elem` ["State", "Config"])
