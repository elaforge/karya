-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to directly perform a saved score.
module Derive.DeriveSaved where
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy
import qualified Data.Vector as Vector

import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Test.Testing as Testing
import qualified Util.Thread as Thread

import qualified App.Config as Config
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Ky as Ky
import qualified Cmd.Lilypond
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.C.All as C.All
import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Local.Config
import qualified Midi.Midi as Midi
import qualified Midi.StubMidi as StubMidi
import qualified Synth.Shared.Config as Shared.Config
import qualified Ui.Ui as Ui

import           Global
import           Types


perform_file :: Cmd.Config -> FilePath -> IO [Midi.WriteMessage]
perform_file cmd_config fname = do
    (ui_state, cmd_state) <- load_score_states cmd_config fname
    root_id <- maybe (errorIO $ txt fname <> ": no root block") return $
        Ui.config#Ui.root #$ ui_state
    ((events, logs), _cpu) <- timed_derive fname ui_state cmd_state root_id
    mapM_ Log.write logs
    ((msgs, logs), _cpu) <- timed_perform cmd_state ("perform " ++ fname)
        ui_state events
    mapM_ Log.write logs
    return msgs

timed_perform :: Cmd.State -> FilePath -> Ui.State
    -> Vector.Vector Score.Event -> IO (([Midi.WriteMessage], [Log.Msg]), CPU)
timed_perform cmd_state fname state events =
    time ("perform " <> txt fname) (timer_msg (length . fst)) $ do
        let (msgs, logs) = perform cmd_state state events
        Testing.force (msgs, logs)
        return (msgs, logs)

timed_derive :: FilePath -> Ui.State -> Cmd.State -> BlockId
    -> IO ((Vector.Vector Score.Event, [Log.Msg]), CPU)
timed_derive fname ui_state cmd_state block_id = do
    let (perf, logs) = Performance.derive ui_state cmd_state block_id
    (_, cpu) <- time ("derive " <> txt fname) (timer_msg Vector.length) $ do
        () <- return $ Msg.force_performance perf
        return $! Cmd.perf_events perf
    let warns = filter ((>=Log.Warn) . Log.msg_priority) (Cmd.perf_logs perf)
    return ((Cmd.perf_events perf, warns ++ logs), cpu)

-- | This is like 'timed_derive', except that it does more work itself
-- rather than calling Performance.derive.  This can be more convenient to
-- look at derivation results.
timed_derive2 :: FilePath -> Ui.State -> Cmd.State -> BlockId
    -> IO ((Vector.Vector Score.Event, [Log.Msg]), CPU)
timed_derive2 fname ui_state cmd_state block_id =
    case derive_block ui_state cmd_state block_id of
        Left err -> return ((mempty, [Log.msg Log.Warn Nothing err]), 0)
        Right (result, cmd_logs) -> do
            let (events, derive_logs) = first Vector.fromList $
                    Stream.partition $ Derive.r_events result
                msg = "derive " <> txt fname <> " " <> pretty block_id
            (events, cpu) <- time msg (timer_msg Vector.length)
                (return $! events)
            return
                ( (events, cmd_logs ++ filter (not . boring) derive_logs)
                , cpu
                )
    where
    boring = Cache.is_cache_log
    derive_block :: Ui.State -> Cmd.State -> BlockId
        -> Either Text (Derive.Result, [Log.Msg])
    derive_block ui_state cmd_state block_id =
        run_cmd ui_state cmd_state $ PlayUtil.uncached_derive block_id

timed_lilypond :: FilePath -> Ui.State -> Cmd.State -> BlockId
    -> IO ((Either Log.Msg Text, [Log.Msg]), CPU)
timed_lilypond fname ui_state cmd_state block_id = case result of
    Left err -> return ((Left $ Log.msg Log.Warn Nothing err, []), 0)
    Right (levents, cmd_logs) -> do
        let (events, derive_logs) = Stream.partition levents
        (events, cpu) <- time ("lilypond " <> txt fname)
            (timer_msg length) (return $! events)
        let (result, ly_logs) = Cmd.Lilypond.extract_movements
                config "title" events
        let logs = cmd_logs ++ filter (not . boring) derive_logs ++ ly_logs
        return ((Lazy.toStrict <$> result, logs), cpu)
    where
    result = run_cmd ui_state cmd_state $
        Derive.r_events <$> Cmd.Lilypond.derive_block block_id
    config = Ui.config#Ui.lilypond #$ ui_state
    boring = Cache.is_cache_log

timer_msg :: (a -> Int) -> Thread.Metric Thread.Seconds -> a -> String
timer_msg len (Thread.Metric cpu_secs wall_secs) events =
    Printf.printf "events: %d (%d / cpu, %d / sec)"
        events_len (per cpu_secs) (per wall_secs)
    where
    events_len = len events
    per :: Thread.Seconds -> Int
    per secs = round (fromIntegral events_len / toSecs secs)

run_cmd :: Ui.State -> Cmd.State -> Cmd.CmdId a -> Either Text (a, [Log.Msg])
run_cmd ui_state cmd_state cmd = case result of
    Left err -> Left $ pretty err
    Right (val, _, _) -> case val of
        Nothing -> Left "cmd had no result"
        Just val -> Right (val, logs)
    where (_, _, logs, result) = Cmd.run_id ui_state cmd_state cmd

perform :: Cmd.State -> Ui.State -> Vector.Vector Score.Event
    -> ([Midi.WriteMessage], [Log.Msg])
perform cmd_state ui_state events =
    extract $ run_cmd ui_state cmd_state $ PlayUtil.perform_events events
    where
    extract (Left err) = ([], [Log.msg Log.Error Nothing err])
    extract (Right (levents, logs)) = (events, logs ++ perf_logs)
        where (events, perf_logs) = LEvent.partition levents

load_score_states :: Cmd.Config -> FilePath -> IO (Ui.State, Cmd.State)
load_score_states cmd_config fname = do
    (ui_state, library, aliases) <- either errorIO return =<< load_score fname
    return (ui_state,
        add_library library aliases (Cmd.initial_state cmd_config))

add_library :: Derive.Builtins -> Derive.InstrumentAliases
    -> Cmd.State -> Cmd.State
add_library builtins aliases state =
    state { Cmd.state_ky_cache = Just $ Cmd.PermanentKy (builtins, aliases) }

-- | Load a score and its accompanying local definitions library, if it has one.
load_score :: FilePath
    -> IO (Either Text (Ui.State, Derive.Builtins, Derive.InstrumentAliases))
load_score fname = fmap fst $ time ("load " <> txt fname) (\_ _ -> "") $
    Except.runExceptT $ do
        save <- require_right $ Save.infer_save_type fname
        (state, dir) <- case save of
            Cmd.SaveRepo repo -> do
                (state, _, _) <- require_right $
                    SaveGit.load (Path.to_path repo) Nothing
                return (state, FilePath.takeDirectory (Path.to_path repo))
            Cmd.SaveState fname -> do
                state <- require_right $ first pretty <$>
                    Save.read_state_ (Path.to_path fname)
                return (state, FilePath.takeDirectory (Path.to_path fname))
        app_dir <- liftIO Path.get_app_dir
        let paths = dir : map (Path.to_absolute app_dir) Config.ky_paths
        (builtins, aliases) <- require_right $ Ky.load paths state
        return (state, builtins, aliases)

require_right :: IO (Either Text a) -> Except.ExceptT Text IO a
require_right io = tryRight =<< liftIO io

-- | Load cmd config, which basically means the inst db.
load_cmd_config :: IO Cmd.Config
load_cmd_config = do
    config <- Local.Config.load_static_config
    cmd_config (StaticConfig.instrument_db config)

cmd_config :: Cmd.InstrumentDb -> IO Cmd.Config
cmd_config inst_db = do
    interface <- StubMidi.interface
    app_dir <- Path.get_app_dir
    save_dir <- Path.canonical $ Path.to_absolute app_dir Config.save_dir
    return $ Cmd.Config
        { config_app_dir = app_dir
        , config_save_dir = save_dir
        , config_midi_interface = interface
        , config_ky_paths = map (Path.to_absolute app_dir) Config.ky_paths
        , config_rdev_map = mempty
        , config_wdev_map = mempty
        , config_instrument_db = inst_db
        , config_builtins = C.All.builtins
        , config_highlight_colors = mempty
        , config_im = Shared.Config.config app_dir
        -- You shouldn't be saving any checkpoints from here, so I can use
        -- dummy values.
        , config_git_user = SaveGit.User "name" "email"
        }

-- * timer

-- | CPU seconds.
type CPU = Thread.Seconds

-- TODO this is mostly duplicated with Thread.printTimer, except I use
-- the timing info in the msg.
time :: Text -> (Thread.Metric Thread.Seconds -> a -> String) -> IO a
    -> IO (a, CPU)
time msg show_val op = do
    Text.IO.putStr $ msg <> " - "
    IO.hFlush IO.stdout
    result <- Exception.try $ Thread.timeAction $ do
        !val <- op
        return val
    case result of
        Right (val, metric) -> do
            putStrLn $ show_val metric val
            return (val, Thread.metricCpu metric)
        Left (exc :: Exception.SomeException) -> do
            -- Complete the line so the exception doesn't interrupt it.  This
            -- is important if it's a 'failure' line!
            putStrLn $ "threw exception: " <> show exc
            Exception.throwIO exc

toSecs :: Thread.Seconds -> Double
toSecs = realToFrac
