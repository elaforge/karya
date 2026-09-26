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
import qualified Util.Lists as Lists
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Processes as Processes
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

usageDoc :: String
usageDoc = "usage: dump file[,git-commit] file ..."

main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> pure (flags, args)
        (_, _, errs) -> usage $ "flag errors: " ++ unwords errs
    let mode = fromMaybe DumpAll (Lists.last [m | Mode m <- flags])
        pprint = any (==PPrint) flags
    ok <- mapM (dumpFile pprint mode) args
    Processes.exit $ length (filter not ok)
    where
    usage msg = do
        putStrLn usageDoc
        putStr (GetOpt.usageInfo msg options)
        Processes.exit 1

dumpFile :: Bool -> Mode -> FilePath -> IO Bool
dumpFile pprint mode fname = dump pprint mode fname >>= \case
    Right lines -> do
        let prefix = txt fname <> ": "
        mapM_ Text.IO.putStrLn $
            if mode == DumpCalls then map (prefix<>) lines else lines
        pure True
    Left err -> do
        Text.IO.hPutStrLn IO.stderr $ txt fname <> ": " <> err
        pure False

dump :: Bool -> Mode -> FilePath -> IO (Either Text [Text])
dump pprint mode fname
    | [repo, commit] <- Lists.split (==',') fname =
        dumpGit pprint mode repo (Just commit)
    | SaveGit.isGit fname = dumpGit pprint mode fname Nothing
dump pprint mode fname =
    try fname Cmd.Serialize.scoreMagic (dumpScore pprint mode) $
    try fname Cmd.Serialize.allocationsMagic ((:[]) . format pprint) $
    try fname Cmd.Serialize.viewsMagic ((:[]) . format pprint) $
    try fname DiffPerformance.midiMagic dumpMidi $
    try fname Synth.Shared.Note.notesMagic (map (format pprint)) $
    pure $ Left "no magic codes match"

format :: (Show a, Pretty a) => Bool -> a -> Text
format pprint = if pprint then txt . PPrint.pshow else Pretty.formatted

-- | Try to unserialize the file, and try the passed continuation if it failed
-- with Serialize.BadMagic.
try :: Serialize.Serialize a => FilePath -> Serialize.Magic a
    -> (a -> [Text]) -> IO (Either Text [Text]) -> IO (Either Text [Text])
try fname magic dump next = do
    val <- Serialize.unserialize magic fname
    case val of
        Right val -> pure $ Right (dump val)
        Left Serialize.BadMagic {} -> next
        Left err -> pure $ Left (pretty err)

-- | Either a commit hash or a save point ref.
dumpGit :: Bool -> Mode -> FilePath -> Maybe String -> IO (Either Text [Text])
dumpGit pprint mode repo maybeArg = Except.runExceptT $ do
    maybeCommit <- case maybeArg of
        Nothing -> pure Nothing
        Just arg -> do
            commit <- tryJust ("couldn't find commit for " <> showt arg)
                =<< liftIO (SaveGit.inferCommit repo arg)
            pure (Just commit)
    let prefix = "reading " <> showt repo <> ":"
    (state, commit, names) <- tryRight . first (prefix<>)
        =<< liftIO (SaveGit.load repo maybeCommit)
    let header = "commit: " <> pretty commit <> ", names: "
            <> Text.intercalate ", " names
    pure $ header : dumpScore pprint mode state

dumpScore :: Bool -> Mode -> Ui.State -> [Text]
dumpScore pprint mode state = case mode of
    DumpAll -> fmt state
    DumpAllocations -> fmt $
        UiConfig.configAllocations $ Ui.stateConfig state
    DumpConfig -> fmt $ Ui.stateConfig state
    DumpCalls -> extractCalls state ++ extractEventText state
    where
    fmt :: (Show a, Pretty a) => a -> [Text]
    fmt = (:[]) . stripName . format pprint

dumpMidi :: Vector.Vector Midi.WriteMessage -> [Text]
dumpMidi = map Pretty.formatted . Vector.toList

-- | Remove the record name and dedent, since it's redundant.
stripName :: Text -> Text
stripName text = case Text.lines text of
    name : first : rest | isRecord name ->
        -- +2 to drop the '{ ' and ', ' on each line.
        let indent = Text.length (Text.takeWhile (==' ') first) + 2
        in Text.unlines $ map (Text.drop indent) (first : rest)
    _ -> text
    where
    isRecord = (`elem` ["State", "Config"])

extractCalls :: Ui.State -> [Text]
extractCalls state = filter (not . Text.null) $
    label "ky" (Ui.config#UiConfig.ky #$ state)
    : map blockTitle (Map.toList (Ui.stateBlocks state))
    ++ map trackTitle (Map.toList (Ui.stateTracks state))
    where
    blockTitle (blockId, block) =
        stackLabel (Just blockId, Nothing, Nothing) (Block.blockTitle block)
    trackTitle (trackId, track) =
        stackLabel (Nothing, Just trackId, Nothing) (Track.trackTitle track)

extractEventText :: Ui.State -> [Text]
extractEventText state =
    [ stackLabel (frame trackId event) (Event.text event)
    | (trackId, track) <- Map.toList (Ui.stateTracks state)
    , event <- Events.ascending (Track.trackEvents track)
    , not (Text.null (Event.text event))
    ]
    where
    frame trackId event =
        (Nothing, Just trackId, Just (Event.range event))

stackLabel :: Stack.UiFrame -> Text -> Text
stackLabel frame = label (Stack.logUiFrame frame)

label :: Text -> Text -> Text
label name val
    | Text.null val = ""
    | otherwise = name <> ": " <> val
