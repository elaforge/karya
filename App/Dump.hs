-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utility to print out any of the binary formats used.
module App.Dump (main) where
import qualified Control.Monad.Except as Except
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.IO as IO

import qualified Util.Git as Git
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Processes as Processes
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize

import qualified Derive.Stack as Stack
import qualified Midi.Midi as Midi
import qualified Synth.Shared.Note
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


data Mode = DumpAll | DumpAllocations | DumpConfig | DumpCalls
    deriving (Eq, Show)

data Flag = Mode !Mode | PPrint
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["allocations"] (GetOpt.NoArg (Mode DumpAllocations))
        "dump allocations"
    , GetOpt.Option [] ["config"] (GetOpt.NoArg (Mode DumpConfig))
        "dump config"
    , GetOpt.Option [] ["calls"] (GetOpt.NoArg (Mode DumpCalls))
        "Dump call text. Use this with grep to find where calls are used."
    , GetOpt.Option [] ["pprint"] (GetOpt.NoArg PPrint)
        "Use haskell pprint instead of Pretty."
    ]

usage_doc :: String
usage_doc = "usage: dump file[,git-commit] file ..."

main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors: " ++ unwords errs
    let mode = fromMaybe DumpAll (Seq.last [m | Mode m <- flags])
        pprint = any (==PPrint) flags
    ok <- mapM (dump_file pprint mode) args
    Processes.exit $ length (filter not ok)
    where
    usage msg = do
        putStrLn usage_doc
        putStr (GetOpt.usageInfo msg options)
        Processes.exit 1

dump_file :: Bool -> Mode -> FilePath -> IO Bool
dump_file pprint mode fname = dump pprint mode fname >>= \case
    Right lines -> do
        let prefix = txt fname <> ": "
        mapM_ Text.IO.putStrLn $
            if mode == DumpCalls then map (prefix<>) lines else lines
        return True
    Left err -> do
        Text.IO.hPutStrLn IO.stderr $ txt fname <> ": " <> err
        return False

dump :: Bool -> Mode -> FilePath -> IO (Either Text [Text])
dump pprint mode fname
    | [repo, commit] <- Seq.split "," fname =
        dump_git pprint mode repo (Just commit)
    | SaveGit.is_git fname = dump_git pprint mode fname Nothing
dump pprint mode fname =
    try fname Cmd.Serialize.score_magic (dump_score pprint mode) $
    try fname Cmd.Serialize.allocations_magic ((:[]) . format pprint) $
    try fname Cmd.Serialize.views_magic ((:[]) . format pprint) $
    try fname DiffPerformance.midi_magic dump_midi $
    try fname Synth.Shared.Note.notesMagic (map (format pprint)) $
    return $ Left "no magic codes match"

format :: (Show a, Pretty a) => Bool -> a -> Text
format pprint = if pprint then txt . PPrint.pshow else Pretty.formatted

-- | Try to unserialize the file, and try the passed continuation if it failed
-- with Serialize.BadMagic.
try :: Serialize.Serialize a => FilePath -> Serialize.Magic a
    -> (a -> [Text]) -> IO (Either Text [Text]) -> IO (Either Text [Text])
try fname magic dump next = do
    val <- Serialize.unserialize magic fname
    case val of
        Right val -> return $ Right (dump val)
        Left Serialize.BadMagic {} -> next
        Left err -> return $ Left (pretty err)

-- | Either a commit hash or a save point ref.
dump_git :: Bool -> Mode -> FilePath -> Maybe String -> IO (Either Text [Text])
dump_git pprint mode repo maybe_arg = Except.runExceptT $ do
    maybe_commit <- case maybe_arg of
        Nothing -> return Nothing
        Just arg -> do
            commit <- tryJust ("couldn't find commit for " <> showt arg)
                =<< liftIO (SaveGit.infer_commit repo arg)
            return (Just commit)
    let prefix = "reading " <> showt repo <> ":"
    (state, commit, names) <- tryRight . first (prefix<>)
        =<< liftIO (SaveGit.load repo maybe_commit)
    let header = "commit: " <> pretty commit <> ", names: "
            <> Text.intercalate ", " names
    return $ header : dump_score pprint mode state

dump_score :: Bool -> Mode -> Ui.State -> [Text]
dump_score pprint mode state = case mode of
    DumpAll -> fmt state
    DumpAllocations -> fmt $
        UiConfig.config_allocations $ Ui.state_config state
    DumpConfig -> fmt $ Ui.state_config state
    DumpCalls -> extract_calls state ++ extract_event_text state
    where
    fmt :: (Show a, Pretty a) => a -> [Text]
    fmt = (:[]) . strip_name . format pprint

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

extract_calls :: Ui.State -> [Text]
extract_calls state = filter (not . Text.null) $
    label "ky" (Ui.config#UiConfig.ky #$ state)
    : map block_title (Map.toList (Ui.state_blocks state))
    ++ map track_title (Map.toList (Ui.state_tracks state))
    where
    block_title (block_id, block) =
        stack_label (Just block_id, Nothing, Nothing) (Block.block_title block)
    track_title (track_id, track) =
        stack_label (Nothing, Just track_id, Nothing) (Track.track_title track)

extract_event_text :: Ui.State -> [Text]
extract_event_text state =
    [ stack_label (frame track_id event) (Event.text event)
    | (track_id, track) <- Map.toList (Ui.state_tracks state)
    , event <- Events.ascending (Track.track_events track)
    , not (Text.null (Event.text event))
    ]
    where
    frame track_id event =
        (Nothing, Just track_id, Just (Event.range event))

stack_label :: Stack.UiFrame -> Text -> Text
stack_label frame = label (Stack.log_ui_frame frame)

label :: Text -> Text -> Text
label name val
    | Text.null val = ""
    | otherwise = name <> ": " <> val
