-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to directly perform a saved score.
module Derive.DeriveSaved where
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Lazy
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Log as Log
import Util.Test

import qualified Midi.Midi as Midi
import qualified Midi.StubMidi as StubMidi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lilypond
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.Cache as Cache
import qualified Derive.Call.All as Call.All
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.All as Scale.All

import qualified Local.Config
import qualified App.StaticConfig as StaticConfig
import Types


perform_file :: Cmd.Config -> FilePath -> IO [Midi.WriteMessage]
perform_file cmd_config fname = do
    (state, defs_lib) <- either errorIO return =<< load_score fname
    block_id <- maybe (errorIO $ fname <> ": no root block") return $
        State.config#State.root #$ state
    let cmd_state = add_definition_lib defs_lib (Cmd.initial_state cmd_config)
    (events, logs) <- either (errorIO . ((fname <> ": ") <>)) return
        =<< timed_derive fname state cmd_state block_id
    mapM_ Log.write logs
    (msgs, logs) <- timed_perform cmd_state ("perform " ++ fname) state events
    mapM_ Log.write logs
    return msgs

add_definition_lib :: Derive.Library -> Cmd.State -> Cmd.State
add_definition_lib lib state =
    state { Cmd.state_definition_cache = Just (day0, Right lib) }
    where day0 = Time.UTCTime (Time.ModifiedJulianDay 0) 0

timed_perform :: Cmd.State -> String -> State.State -> Cmd.Events
    -> IO ([Midi.WriteMessage], [Log.Msg])
timed_perform cmd_state msg state events =
    print_timer msg (timer_msg (length . fst)) $ do
        let (msgs, logs) = perform cmd_state state events
        force (msgs, logs)
        return (msgs, logs)

timed_derive :: String -> State.State -> Cmd.State -> BlockId
    -> IO (Either String (Cmd.Events, [Log.Msg]))
timed_derive name ui_state cmd_state block_id =
    case derive_block ui_state cmd_state block_id of
        Left err -> return $ Left err
        Right (result, cmd_logs) -> fmap Right $ do
            let (events, derive_logs) = first Vector.fromList $
                    LEvent.partition $ Derive.r_events result
            events <- print_timer ("derive " ++ name) (timer_msg Vector.length)
                (return $! events)
            return (events, cmd_logs ++ filter (not . boring) derive_logs)
    where boring msg = Cache.is_cache_log msg

timed_lilypond :: String -> State.State -> Cmd.State -> BlockId
    -> IO (Either Text Text, [Log.Msg])
timed_lilypond name ui_state cmd_state block_id = case result of
    Left err -> return (Left (txt err), [])
    Right (levents, cmd_logs) -> do
        let (events, derive_logs) = LEvent.partition levents
        events <- print_timer ("lilypond " ++ name) (timer_msg length)
            (return $! events)
        let (result, ly_logs) = Cmd.Lilypond.extract_movements
                config "title" events
        return (Lazy.toStrict <$> result,
            cmd_logs ++ filter (not . boring) derive_logs ++ ly_logs)
    where
    result = run_cmd ui_state cmd_state $
        Derive.r_events <$> Cmd.Lilypond.derive_block block_id
    config = State.config#State.lilypond #$ ui_state
    boring msg = Cache.is_cache_log msg

timer_msg :: (a -> Int) -> Double -> a -> String
timer_msg len secs events = Printf.printf "events: %d (%d / sec)"
    events_len (round (fromIntegral events_len / secs) :: Int)
    where events_len = len events

derive_block :: State.State -> Cmd.State -> BlockId
    -> Either String (Derive.Result, [Log.Msg])
derive_block ui_state cmd_state block_id =
    run_cmd ui_state cmd_state $ PlayUtil.uncached_derive block_id

run_cmd :: State.State -> Cmd.State -> Cmd.CmdId a
    -> Either String (a, [Log.Msg])
run_cmd ui_state cmd_state cmd = case result of
    Left err -> Left $ pretty err
    Right (val, _, _) -> case val of
        Nothing -> Left "cmd had no result"
        Just val -> Right (val, logs)
    where (_, _, logs, result) = Cmd.run_id ui_state cmd_state cmd

perform :: Cmd.State -> State.State -> Cmd.Events
    -> ([Midi.WriteMessage], [Log.Msg])
perform cmd_state ui_state events =
    extract $ run_cmd ui_state cmd_state $ PlayUtil.perform_events events
    where
    extract (Left err) = ([], [Log.msg Log.Error Nothing (txt err)])
    extract (Right (levents, logs)) = (events, logs ++ perf_logs)
        where (events, perf_logs) = LEvent.partition levents

-- | Load a score and its accompanying local definitions library, if it has one.
load_score :: FilePath -> IO (Either String (State.State, Derive.Library))
load_score fname =
    print_timer ("load " ++ fname) (\_ _ -> "") $ Error.runErrorT $ do
        save <- require_right $ Save.infer_save_type fname
        (state, dir) <- case save of
            Cmd.SaveRepo repo -> do
                (state, _, _) <- require_right $ SaveGit.load repo Nothing
                return (state, FilePath.takeDirectory repo)
            Cmd.SaveState fname -> do
                maybe_state <- require_right $ Save.read_state_ fname
                state <- maybe (Error.throwError "file not found") return
                    maybe_state
                return (state, FilePath.takeDirectory fname)
        case State.config#State.definition_file #$ state of
            Nothing -> return (state, mempty)
            Just defs_name -> do
                let defs_fname = dir FilePath.</> defs_name
                lib <- maybe
                    (Error.throwError $ "defs file not found: " <> defs_fname)
                    (either (Error.throwError . untxt) return)
                        =<< liftIO (PlayUtil.load_definitions defs_fname)
                return (state, lib)

require_right :: IO (Either String a) -> Error.ErrorT String IO a
require_right io = either Error.throwError return =<< liftIO io

-- | Load cmd config, which basically means the inst db.
load_cmd_config :: IO Cmd.Config
load_cmd_config = do
    config <- Local.Config.load_static_config
    cmd_config (StaticConfig.instrument_db config)

cmd_config :: Cmd.InstrumentDb -> IO Cmd.Config
cmd_config inst_db = do
    interface <- StubMidi.interface
    return $ Cmd.Config
        { Cmd.state_app_dir = "."
        , Cmd.state_midi_interface = interface
        , Cmd.state_rdev_map = mempty
        , Cmd.state_wdev_map = mempty
        , Cmd.state_instrument_db = inst_db
        , Cmd.state_library = Call.All.library
        , Cmd.state_lookup_scale = Cmd.LookupScale $
            \scale_id -> Map.lookup scale_id Scale.All.scales
        , Cmd.state_highlight_colors = mempty
        }
