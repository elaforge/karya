-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to directly perform a saved score.
module Derive.DeriveSaved where
import qualified Control.Exception as Exception
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy
import qualified Data.Vector as Vector

import qualified System.IO as IO
import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Test.Testing as Testing
import qualified Util.Thread as Thread

import qualified App.Config as Config
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lilypond
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Save as Save
import qualified Cmd.SaveGitT as SaveGitT

import qualified Derive.C.All as C.All
import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Local.Config
import qualified Midi.Midi as Midi
import qualified Midi.StubMidi as StubMidi
import qualified Perform.Sc.Note as Sc.Note
import qualified Synth.Shared.Config as Shared.Config
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


performFile :: Cmd.Config -> FilePath -> IO [Midi.WriteMessage]
performFile cmdConfig fname = do
    (uiState, cmdState) <- either errorIO pure =<< loadScore cmdConfig fname
    rootId <- maybe (errorIO $ txt fname <> ": no root block") pure $
        Ui.config#UiConfig.root #$ uiState
    ((events, logs), _cpu) <- timedDerive fname uiState cmdState rootId
    mapM_ Log.write logs
    ((msgs, logs), _cpu) <- timedPerform cmdState ("perform " ++ fname)
        uiState events
    mapM_ Log.write logs
    pure msgs

timedPerform :: Cmd.State -> FilePath -> Ui.State
    -> Vector.Vector Score.Event -> IO (([Midi.WriteMessage], [Log.Msg]), CPU)
timedPerform cmdState fname state events =
    time ("perform " <> txt fname) (timerMsg (length . fst)) $ do
        let (msgs, logs) = LEvent.partition $
                performMidi cmdState state events
        Testing.force (msgs, logs)
        pure (msgs, logs)

timedDerive :: FilePath -> Ui.State -> Cmd.State -> BlockId
    -> IO ((Vector.Vector Score.Event, [Log.Msg]), CPU)
timedDerive fname uiState cmdState blockId = do
    let (perf, logs) = Performance.derive uiState cmdState blockId
    (_, cpu) <- time ("derive " <> txt fname) (timerMsg Vector.length) $ do
        () <- pure $ Msg.forcePerformance perf
        pure $! Cmd.perfEvents perf
    let warns = filter ((>=Log.Warn) . Log.msgPriority) (Cmd.perfLogs perf)
    pure ((Cmd.perfEvents perf, warns ++ logs), cpu)

-- | This is like 'timed_derive', except that it does more work itself
-- rather than calling Performance.derive.  This can be more convenient to
-- look at derivation results.
timedDerive2 :: FilePath -> Ui.State -> Cmd.State -> BlockId
    -> IO ((Vector.Vector Score.Event, [Log.Msg]), CPU)
timedDerive2 fname uiState cmdState blockId =
    case deriveBlock uiState cmdState blockId of
        Left err -> pure ((mempty, [Log.msg Log.Warn Nothing err]), 0)
        Right (result, cmdLogs) -> do
            let (events, deriveLogs) = first Vector.fromList $
                    Stream.partition $ Derive.rEvents result
                msg = "derive " <> txt fname <> " " <> pretty blockId
            (events, cpu) <- time msg (timerMsg Vector.length)
                (pure $! events)
            pure
                ( (events, cmdLogs ++ filter (not . boring) deriveLogs)
                , cpu
                )
    where
    boring = Cache.isCacheLog
    deriveBlock :: Ui.State -> Cmd.State -> BlockId
        -> Either Text (Derive.Result, [Log.Msg])
    deriveBlock uiState cmdState blockId =
        runCmd uiState cmdState $ PlayUtil.uncachedDerive blockId

timedLilypond :: FilePath -> Ui.State -> Cmd.State -> BlockId
    -> IO ((Either Log.Msg Text, [Log.Msg]), CPU)
timedLilypond fname uiState cmdState blockId = case result of
    Left err -> pure ((Left $ Log.msg Log.Warn Nothing err, []), 0)
    Right (levents, cmdLogs) -> do
        let (events, deriveLogs) = Stream.partition levents
        (events, cpu) <- time ("lilypond " <> txt fname)
            (timerMsg length) (pure $! events)
        let (result, lyLogs) = Cmd.Lilypond.extractMovements
                config "title" events
        let logs = cmdLogs ++ filter (not . boring) deriveLogs ++ lyLogs
        pure ((Lazy.toStrict <$> result, logs), cpu)
    where
    result = runCmd uiState cmdState $
        Derive.rEvents <$> Cmd.Lilypond.deriveBlock blockId
    config = Ui.config#UiConfig.lilypond #$ uiState
    boring = Cache.isCacheLog

timerMsg :: (a -> Int) -> Thread.Metric Thread.Seconds -> a -> String
timerMsg len (Thread.Metric cpuSecs wallSecs) events =
    Printf.printf "events: %d (%d / cpu, %d / sec)"
        eventsLen (per cpuSecs) (per wallSecs)
    where
    eventsLen = len events
    per :: Thread.Seconds -> Int
    per secs = round (fromIntegral eventsLen / toSecs secs)

runCmd :: Ui.State -> Cmd.State -> Cmd.CmdId a -> Either Text (a, [Log.Msg])
runCmd uiState cmdState cmd = case result of
    Left err -> Left $ pretty err
    Right (val, _, _) -> case val of
        Nothing -> Left "cmd had no result"
        Just val -> Right (val, logs)
    where (_, _, logs, result) = Cmd.runId uiState cmdState cmd

perform :: Cmd.State -> Ui.State -> Vector.Vector Score.Event
    -> ( ([LEvent.LEvent Midi.WriteMessage], [LEvent.LEvent Sc.Note.Note])
       , [Log.Msg]
       )
perform cmdState uiState =
    extract . runCmd uiState cmdState . PlayUtil.performFrom 0
    where
    extract = either (\err -> (([], []), [Log.msg Log.Error Nothing err])) id

performMidi :: Cmd.State -> Ui.State -> Vector.Vector Score.Event
    -> [LEvent.LEvent Midi.WriteMessage]
performMidi cmdState uiState events =
    extract $ runCmd uiState cmdState $ PlayUtil.performRaw $
        Vector.toList events
    where
    extract (Left err) = [LEvent.Log $ Log.msg Log.Error Nothing err]
    extract (Right (levents, logs)) = map LEvent.Log logs ++ levents

loadScore :: Cmd.Config -> FilePath -> IO (Either Text (Ui.State, Cmd.State))
loadScore cmdConfig fname =
    fmap fst $ time ("load " <> txt fname) (\_ _ -> "") $
        (extract =<<) $ Cmd.run_ Ui.empty (Cmd.initialState cmdConfig) $
            Save.loadDirect fname
    where
    extract (val, logs) = do
        mapM_ Log.write logs
        pure $ case val of
            Left err -> Left (txt err)
            Right ((), cmdState, uiState) -> Right (uiState, cmdState)

addLibrary :: Derive.Builtins -> Derive.InstrumentAliases
    -> Cmd.State -> Cmd.State
addLibrary builtins aliases state =
    state { Cmd.stateKyCache = Just $ Cmd.PermanentKy (builtins, aliases) }

-- | Load cmd config, which basically means the inst db.
loadCmdConfig :: IO Cmd.Config
loadCmdConfig = do
    config <- Local.Config.loadStaticConfig
    cmdConfig (StaticConfig.instrumentDb config)

cmdConfig :: Cmd.InstrumentDb -> IO Cmd.Config
cmdConfig instDb = do
    interface <- StubMidi.interface
    appDir <- Path.getAppDir
    saveDir <- Path.canonical $ Path.toAbsolute appDir Config.saveDir
    pure $ Cmd.Config
        { configAppDir = appDir
        , configSaveDir = saveDir
        , configMidiInterface = interface
        , configKyPaths = map (Path.toAbsolute appDir) Config.kyPaths
        , configRdevMap = mempty
        , configWdevMap = mempty
        , configInstrumentDb = instDb
        , configBuiltins = C.All.builtins
        , configHighlightColors = mempty
        , configIm = Shared.Config.config appDir
        -- You shouldn't be saving any checkpoints from here, so I can use
        -- dummy values.
        , configGitUser = SaveGitT.User "name" "email"
        , configImPlayDirect = False
        -- TODO This could mean tscore must have play-cache explicitly?
        , configPlayCache = Nothing
        }

-- * timer

-- | CPU seconds.
type CPU = Thread.Seconds

-- TODO this is mostly duplicated with Thread.printTimer, except I use
-- the timing info in the msg.
time :: Text -> (Thread.Metric Thread.Seconds -> a -> String) -> IO a
    -> IO (a, CPU)
time msg showVal op = do
    Text.IO.putStr $ msg <> " - "
    IO.hFlush IO.stdout
    result <- Exception.try $ Thread.timeAction $ do
        !val <- op
        pure val
    case result of
        Right (val, metric) -> do
            putStrLn $ showVal metric val
            pure (val, Thread.metricCpu metric)
        Left (exc :: Exception.SomeException) -> do
            -- Complete the line so the exception doesn't interrupt it.  This
            -- is important if it's a 'failure' line!
            putStrLn $ "threw exception: " <> show exc
            Exception.throwIO exc

toSecs :: Thread.Seconds -> Double
toSecs = realToFrac
