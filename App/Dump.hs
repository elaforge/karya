-- | Quick hack to print out the binary save files.
--
-- TODO flags not actually used.  Maybe I don't need them?
module App.Dump where
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List as List
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit

import Text.Printf

import qualified Util.Git.Git as Git
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize as Serialize


options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["track"] (GetOpt.NoArg TrackContents)
        "show track contents"
    , GetOpt.Option [] ["ruler"] (GetOpt.NoArg RulerContents)
        "show ruler contents"
    ]

data Flag = TrackContents | RulerContents deriving (Show, Eq)

main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors: " ++ Seq.join ", " errs
    let is_git = (".git" `List.isSuffixOf`)
    case args of
        [fn]
            | is_git fn -> dump_git flags fn Nothing
            | otherwise -> dump_simple flags fn
        [fn, commit] | is_git fn -> dump_git flags fn (Just commit)
        _ -> usage $ "expected a single filename: " ++ Seq.join ", " args
    where
    usage msg = do
        putStr (GetOpt.usageInfo msg options)
        System.Exit.exitWith (System.Exit.ExitFailure 1)

die :: String -> IO a
die msg = do
    putStrLn $ "Error: " ++ msg
    System.Exit.exitWith (System.Exit.ExitFailure 1)

dump_simple :: [Flag] -> FilePath -> IO ()
dump_simple flags fn = do
    Serialize.SaveState state date <-
        either (die . (("reading " ++ show fn ++ ":") ++)) return
            =<< Serialize.unserialize fn
    printf "saved at %s:\n" (show date)
    pprint_state flags state

-- | Either a commit hash or a save point ref.
dump_git :: [Flag] -> FilePath -> Maybe String -> IO ()
dump_git flags repo maybe_arg = do
    maybe_commit <- case maybe_arg of
        Nothing -> return Nothing
        Just arg -> do
            commit <- maybe (die $ "couldn't find commit for " ++ show arg)
                return =<< infer_commit repo arg
            return (Just commit)
    (state, commit, names) <- either
        (die . (("reading " ++ show repo ++ ":") ++)) return
            =<< SaveGit.load repo maybe_commit
    printf "commit: %s, names: %s\n" (Pretty.pretty commit)
        (Seq.join ", " names)
    pprint_state flags state

infer_commit :: Git.Repo -> String -> IO (Maybe Git.Commit)
infer_commit repo arg
    | length arg == 40 = return (commit arg)
    | otherwise = Git.read_ref repo arg
    where
    commit = Just . Git.Commit . Git.parse_hash . ByteString.pack

pprint_state :: [Flag] -> State.State -> IO ()
pprint_state flags = putStrLn . clean . Pretty.formatted

clean :: String -> String
clean text = case lines text of
    "State" : first : rest ->
        -- +2 to drop the '{ ' and ', ' on each line.
        let indent = length (takeWhile (==' ') first) + 2
        in unlines $ map (drop indent) (first : rest)
    _ -> text
