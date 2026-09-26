-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmdline program to verify that a saved score still derives the same MIDI
-- msgs or lilypond code as the last saved performance.
module App.VerifyPerformance (main) where
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.IO as IO

import qualified Util.Exceptions as Exceptions
import qualified Util.Git as Git
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Processes as Processes
import qualified Util.SourceControl as SourceControl
import qualified Util.Thread as Thread

import qualified Cmd.Cmd as Cmd
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.Save as Save

import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Midi.Midi as Midi
import qualified Perform.Im.Convert as Im.Convert
import qualified Synth.Shared.Note as Shared.Note
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


data Flag = Help | Mode Mode | Output !FilePath
    deriving (Eq, Show)

data Mode
    = Verify | Save | Derive | Perform | Profile | ProfileDerive
    | DumpMidi | CommitInfo
    | UpdateMidi
    deriving (Eq, Show, Bounded, Enum)

data PerformTo = ToDerive | ToMidi
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["mode"]
        (GetOpt.ReqArg readMode (show [minBound :: Mode ..])) $
        "Run in this mode, defaults to Verify.  Modes:\n\
        \Verify - Check saved performances against current performances.\n\
        \   If you give a directory and it has a file inside called\n\
        \   " ++ verifyMeTxt ++ ", use the contents of the file as\n\
        \   further files to verify.\n\
        \Save - Write saved performances to disk as binary.\n\
        \Perform - Perform to MIDI and write to $input.midi.\n\
        \Profile - Like Perform, but don't write any output.\n\
        \ProfileDerive - Like Profile, but derive only, don't render midi.\n\
        \DumpMidi - Pretty print binary saved MIDI to stdout.\n\
        \CommitInfo - Dump info on the current commit in JSON. This doesn't\n\
        \  belong here, but there's no other great place.\n\
        \UpdateMidi - Update the saved MIDI.\n"
    , GetOpt.Option [] ["out"] (GetOpt.ReqArg Output defaultOutDir) $
        "Write output to this directory. This is diffs, and timing .json."
    ]

-- | This is intentionally not the same as what the shakefile uses, so I don't
-- mingle the results.
defaultOutDir :: FilePath
defaultOutDir = "build/verify-cmdline"

readMode :: String -> Flag
readMode s =
    Mode $ fromMaybe (error ("unknown mode: " <> show s)) $ Map.lookup s modes
    where modes = Map.fromList [(show m, m) | m <- [minBound .. maxBound]]

main :: IO ()
main = Git.initialize $ do
    args <- System.Environment.getArgs
    Log.configure $ \state -> state
        { Log.statePriority = Log.Warn
        , Log.stateWriteMsg = Log.writeFormatted IO.stderr
        }
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> pure (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Lists.join ", " errs
    unless (null [Help | Help <- flags]) $ usage ""
    let outDir = fromMaybe defaultOutDir $ Lists.last [d | Output d <- flags]
    cmdConfig <- DeriveSaved.loadCmdConfig
    failures <- case fromMaybe Verify $ Lists.last [m | Mode m <- flags] of
        Verify -> do
            when (null args) $ usage "no inputs"
            fnames <- concatMapM expandVerifyMe args
            results <- forM fnames $ \fname -> do
                putStrLn $ "------------------------- verify " <> fname
                fails <- runError $ verifyPerformance outDir cmdConfig fname
                putStrLn $ if fails == 0
                    then "+++++++++++++++++++++++++ OK!"
                    else "_________________________ FAILED!"
                pure (fname, fails)
            when (length results > 1) $ do
                let (failed, ok) = List.partition ((>0) . snd) results
                unless (null ok) $
                    putStr $ "    passed:\n" <> unlines (map fst ok)
                unless (null failed) $
                    putStr $ "    failed:\n" <> unlines (map fst failed)
            pure $ Num.sum $ map snd results
        UpdateMidi -> do
            _ <- runError0 $ mapM_ (updateMidi cmdConfig) args
            pure 0
        Save -> do
            when (null args) $ usage "no inputs"
            runError $ concat <$> mapM (save outDir) args
        Profile -> do
            when (null args) $ usage "no inputs"
            runError $ mapM_ (perform Nothing cmdConfig) args >> pure []
        ProfileDerive -> do
            when (null args) $ usage "no inputs"
            runError $ mapM_ (derive Nothing cmdConfig) args >> pure []
        Derive -> do
            when (null args) $ usage "no inputs"
            runError $ mapM_ (derive (Just outDir) cmdConfig) args
                >> pure []
        Perform -> do
            when (null args) $ usage "no inputs"
            runError $ mapM_ (perform (Just outDir) cmdConfig) args
                >> pure []
        DumpMidi -> do
            when (null args) $ usage "no inputs"
            runError $ mapM_ dumpMidi args >> pure []
        CommitInfo -> do
            patch <- either (errorIO . txt) pure
                =<< SourceControl.current "."
            ByteString.Lazy.Char8.putStrLn $ Aeson.encode $ Map.fromList
                [ ("date" :: Text, Aeson.toJSON $ SourceControl._date patch)
                , ("hash", Aeson.toJSON $ SourceControl._hash patch)
                , ("name", Aeson.toJSON $ SourceControl._summary patch)
                ]
            pure 0
    Processes.exit failures
    where
    usage msg = do
        putStrLn $ "error: " ++ msg
        putStrLn "usage: verify_performance [ flags ] dirs or filenames"
        putStr (GetOpt.usageInfo "" options)
        Processes.exit 1

verifyMeTxt :: FilePath
verifyMeTxt = "verify-me.txt"

-- | If this is a directory with a 'verify_me_txt', expand to include its
-- contents.
expandVerifyMe :: FilePath -> IO [FilePath]
expandVerifyMe fname = do
    mContents <- Exceptions.ignoreIOError $
        Text.IO.readFile (fname </> verifyMeTxt)
    pure $ case mContents of
        Nothing -> [fname]
        Just contents -> map ((fname</>) . untxt) $ Text.lines contents

type ErrorM a = Except.ExceptT Text IO a

runError :: ErrorM [Text] -> IO Int
runError m = do
    errors <- either (\err -> pure [err]) pure =<< Except.runExceptT m
    mapM_ Text.IO.putStrLn errors
    pure (length errors)

runError0 :: ErrorM () -> IO Int
runError0 m = Except.runExceptT m >>= \case
    Left err -> do
        Text.IO.putStrLn err
        pure 1
    Right () -> pure 0

requireRight :: IO (Either Text a) -> Except.ExceptT Text IO a
requireRight io = tryRight =<< liftIO io

-- * implementation

-- | Extract saved performances and write them to disk.
save :: FilePath -> FilePath -> ErrorM [Text]
save outDir fname = do
    (state, _defsLib, _aliases, blockId) <- load fname
    let meta = Ui.config#UiConfig.meta #$ state
        look = Map.lookup blockId
    midi <- case look (UiConfig.metaMidiPerformances meta) of
        Nothing -> pure False
        Just perf -> liftIO $ do
            out <- getOutput outDir fname ".midi"
            putStrLn $ "write " <> out
            DiffPerformance.saveMidi out (UiConfig.perfEvents perf)
            pure True
    ly <- case look (UiConfig.metaLilypondPerformances meta) of
        Nothing -> pure False
        Just perf -> liftIO $ do
            out <- getOutput outDir fname ".midi"
            putStrLn $ "write " <> out
            Text.IO.writeFile out (UiConfig.perfEvents perf)
            pure True
    pure $ if midi || ly then []
        else [txt fname <> ": no midi or ly performance"]

-- | Perform to MIDI and possibly write to disk.
perform :: Maybe FilePath -> Cmd.Config -> FilePath -> ErrorM ()
perform maybeOutDir cmdConfig fname = do
    (state, library, aliases, blockId) <- load fname
    (msgs, _, _) <- performBlock fname
        (makeCmdState library aliases cmdConfig) state blockId
    whenJust maybeOutDir $ \outDir -> liftIO $ do
        out <- getOutput outDir fname ".midi"
        putStrLn $ "write " <> out
        DiffPerformance.saveMidi out (Vector.fromList msgs)

-- | Like 'perform', but don't perform to MIDI.
derive :: Maybe FilePath -> Cmd.Config -> FilePath
    -> ErrorM (Vector.Vector Score.Event, DeriveSaved.CPU)
derive maybeOutDir cmdConfig fname = do
    (state, library, aliases, blockId) <- load fname
    let cmdState = makeCmdState library aliases cmdConfig
    ((!events, logs), deriveCpu) <- liftIO $
        DeriveSaved.timedDerive fname state cmdState blockId
    liftIO $ mapM_ Log.write logs
    liftIO $ Text.IO.putStrLn $ "derived " <> showt (Vector.length events)
        <> " in " <> Num.showFloat 2 (toSecs deriveCpu)
    whenJust maybeOutDir $ \outDir -> liftIO $ do
        out <- getOutput outDir fname ".score"
        putStrLn $ "write " <> out
        Text.IO.writeFile out $ Text.unlines $
            map Pretty.formatted (Vector.toList events)
    pure (events, deriveCpu)

getOutput :: FilePath -> FilePath -> String -> IO FilePath
getOutput outDir fname extension = do
    Directory.createDirectoryIfMissing True (FilePath.takeDirectory out)
    pure out
    where
    out = outDir </> basename fname <> extension

dumpMidi :: FilePath -> ErrorM ()
dumpMidi fname = do
    msgs <- requireRight $ DiffPerformance.loadMidi fname
    liftIO $ mapM_ Pretty.pprint (Vector.toList msgs)

type Timings = [(Text, Thread.Seconds)]

verifyPerformance :: FilePath -> Cmd.Config -> FilePath -> ErrorM [Text]
verifyPerformance outDir cmdConfig fname = do
    (uiState, library, aliases, blockId) <- load fname
    let meta = Ui.config#UiConfig.meta #$ uiState
    let cmdState = makeCmdState library aliases cmdConfig
    let verify1 verify field =
            maybe (pure (Nothing, []))
                (verify outDir fname cmdState uiState blockId)
                (Map.lookup blockId (field meta))
    (midiErr, midiTimings) <-
        verify1 verifyMidi UiConfig.metaMidiPerformances
    (imErr, imTimings) <- verify1 verifyIm UiConfig.metaImPerformances
    (lyErr, lyTimings) <- verify1 verifyLilypond
        UiConfig.metaLilypondPerformances
    let timings = midiTimings ++ imTimings ++ lyTimings
    if null timings
        then pure ["no saved performances"]
        else do
            liftIO $ writeTiming (outDir </> basename fname <> ".json")
                timings
            pure $ Maybe.catMaybes [midiErr, imErr, lyErr]

updateMidi :: Cmd.Config -> FilePath -> ErrorM ()
updateMidi cmdConfig fname = do
    (uiState, library, aliases, blockId) <- load fname
    let cmdState = makeCmdState library aliases cmdConfig
    (msgs, _, _) <- performBlock fname cmdState uiState blockId
    perf <- liftIO $ UiConfig.makePerformance (Vector.fromList msgs)
    uiState <- pure $ (Ui.config#UiConfig.meta#UiConfig.midiPerformances
            %= Map.insert blockId perf)
        uiState
    liftIO $ Save.writeState fname uiState

-- | Perform from the given state and compare it to the old MidiPerformance.
verifyMidi :: FilePath -> FilePath -> Cmd.State -> Ui.State -> BlockId
    -> UiConfig.MidiPerformance -> ErrorM (Maybe Text, Timings)
verifyMidi outDir fname cmdState uiState blockId performance = do
    (msgs, deriveCpu, performCpu) <-
        performBlock fname cmdState uiState blockId
    (maybeDiff, wroteFiles) <- liftIO $
        DiffPerformance.diffMidi (basename fname ++ ".midi")
            outDir performance msgs
    pure
        ( (<> ("\nwrote " <> txt (unwords wroteFiles))) <$> maybeDiff
        , [("derive", deriveCpu), ("perform", performCpu)]
        )

verifyIm :: FilePath -> FilePath -> Cmd.State -> Ui.State -> BlockId
    -> UiConfig.ImPerformance -> ErrorM (Maybe Text, Timings)
verifyIm outDir fname cmdState uiState blockId performance = do
    ((events, logs), deriveCpu) <- liftIO $
        DeriveSaved.timedDerive fname uiState cmdState blockId
    liftIO $ mapM_ Log.write logs
    let (notes, convertLogs) = convertIm cmdState uiState blockId
            (Vector.toList events)
    liftIO $ mapM_ Log.write convertLogs
    (maybeDiff, wroteFiles) <- liftIO $
        DiffPerformance.diffIm (basename fname ++ ".im")
            outDir performance notes
    pure
        ( (<> ("\nwrote " <> txt (unwords wroteFiles))) <$> maybeDiff
        , [("derive", deriveCpu)]
        )

convertIm :: Cmd.State -> Ui.State -> BlockId -> [Score.Event]
    -> ([Shared.Note.Note], [Log.Msg])
convertIm cmdState uiState blockId events =
    extract $ DeriveSaved.runCmd uiState cmdState $ do
        lookupInst <- Cmd.getLookupInstrument
        pure $ Im.Convert.convert blockId lookupInst events
    where
    extract (Left err) = ([], [Log.msg Log.Error Nothing err])
    extract (Right (levents, logs)) = (events, logs ++ perfLogs)
        where (events, perfLogs) = LEvent.partition levents

performBlock :: FilePath -> Cmd.State -> Ui.State -> BlockId
    -> ErrorM ([Midi.WriteMessage], DeriveSaved.CPU, DeriveSaved.CPU)
performBlock fname cmdState state blockId = do
    ((events, logs), deriveCpu) <- liftIO $
        DeriveSaved.timedDerive fname state cmdState blockId
    liftIO $ mapM_ Log.write logs
    ((msgs, logs), performCpu) <- liftIO $
        DeriveSaved.timedPerform cmdState fname state events
    liftIO $ mapM_ Log.write logs
    pure (msgs, deriveCpu, performCpu)

verifyLilypond :: FilePath -> FilePath -> Cmd.State -> Ui.State
    -> BlockId -> UiConfig.LilypondPerformance -> ErrorM (Maybe Text, Timings)
verifyLilypond outDir fname cmdState state blockId performance = do
    ((result, logs), cpu) <- liftIO $
        DeriveSaved.timedLilypond fname state cmdState blockId
    liftIO $ mapM_ Log.write logs
    case result of
        Left err -> pure (Just $ "error deriving: " <> Log.formatMsg err, [])
        Right got -> do
            (maybeDiff, wroteFiles) <- liftIO $
                DiffPerformance.diffLilypond (basename fname ++ ".ly") outDir
                    performance got
            pure
                ( (<> ("\nwrote " <> txt (unwords wroteFiles))) <$> maybeDiff
                , [("lilypond", cpu)]
                )

-- * util

-- | Load a score and get its root block id.
load :: FilePath
    -> ErrorM (Ui.State, Derive.Builtins, Derive.InstrumentAliases, BlockId)
load fname = do
     (state, builtins, aliases) <- requireRight $ DeriveSaved.loadScore fname
     blockId <- requireRight $ pure $ getRoot state
     pure (state, builtins, aliases, blockId)

makeCmdState :: Derive.Builtins -> Derive.InstrumentAliases -> Cmd.Config
    -> Cmd.State
makeCmdState builtins aliases cmdConfig =
    DeriveSaved.addLibrary builtins aliases $ Cmd.initialState cmdConfig

getRoot :: Ui.State -> Either Text BlockId
getRoot state = justErr "no root block" $ Ui.config#UiConfig.root #$ state

basename :: FilePath -> FilePath
basename = FilePath.takeFileName . List.dropWhileEnd (=='/')

writeTiming :: FilePath -> Timings -> IO ()
writeTiming fname timings = ByteString.Lazy.writeFile fname $ (<>"\n") $
    Aeson.encode $ Map.fromList $ map (second toSecs) timings

toSecs :: Thread.Seconds -> Double
toSecs = realToFrac
