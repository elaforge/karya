-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
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
import qualified Util.Pretty as Pretty
import qualified Util.Process as Process
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize

import qualified Derive.Stack as Stack
import qualified Synth.Shared.Note
import Global


data Flag = DumpAll | DumpAllocations | DumpConfig | DumpCalls
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["allocations"] (GetOpt.NoArg DumpAllocations)
        "dump allocations"
    , GetOpt.Option [] ["config"] (GetOpt.NoArg DumpConfig) "dump config"
    , GetOpt.Option [] ["calls"] (GetOpt.NoArg DumpCalls)
        "Dump call text. Use this with grep to find where calls are used."
    ]

usage_doc :: String
usage_doc = "usage: dump file[,git-commit] file ..."

main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors: " ++ unwords errs
    let flag = fromMaybe DumpAll (Seq.last flags)
    ok <- mapM (dump_file flag) args
    Process.exit $ length (filter not ok)
    where
    usage msg = do
        putStrLn usage_doc
        putStr (GetOpt.usageInfo msg options)
        Process.exit 1

dump_file :: Flag -> FilePath -> IO Bool
dump_file flag fname = dump flag fname >>= \case
    Right lines -> do
        let prefix = txt fname <> ": "
        mapM_ Text.IO.putStrLn $
            if flag == DumpCalls then map (prefix<>) lines else lines
        return True
    Left err -> do
        Text.IO.hPutStrLn IO.stderr $ txt fname <> ": " <> err
        return False

dump :: Flag -> FilePath -> IO (Either Text [Text])
dump flag fname
    | [repo, commit] <- Seq.split "," fname = dump_git flag repo (Just commit)
    | SaveGit.is_git fname = dump_git flag fname Nothing
dump flag fname =
    try fname Cmd.Serialize.score_magic (dump_score flag) $
    try fname Cmd.Serialize.allocations_magic ((:[]) . Pretty.formatted) $
    try fname Cmd.Serialize.views_magic ((:[]) . Pretty.formatted) $
    try fname DiffPerformance.midi_magic dump_midi $
    try fname Synth.Shared.Note.notes_magic (map Pretty.formatted) $
    return $ Left "no magic codes match"

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
dump_git :: Flag -> FilePath -> Maybe String -> IO (Either Text [Text])
dump_git flag repo maybe_arg = Except.runExceptT $ do
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
    return $ header : dump_score flag state

dump_score :: Flag -> Ui.State -> [Text]
dump_score flag state = case flag of
    DumpAll -> format state
    DumpAllocations -> format $
        UiConfig.config_allocations $ Ui.state_config state
    DumpConfig -> format $ Ui.state_config state
    DumpCalls -> extract_calls state ++ extract_event_text state
    where
    format :: Pretty.Pretty a => a -> [Text]
    format = (:[]) . strip_name . Pretty.formatted

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
    label "global" (Ui.config#Ui.global_transform #$ state)
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
