-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to directly perform a saved score.
module Derive.DeriveSaved where
import qualified Data.Vector as Vector
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Log as Log
import Util.Test

import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Save as Save

import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent

import qualified Local.Config
import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Pianoteq as Pianoteq
import qualified Local.Instrument.Vsl as Vsl

import qualified App.StaticConfig as StaticConfig
import Types


perform_file :: FilePath -> IO [Midi.WriteMessage]
perform_file fname = do
    cmd_config <- load_cmd_config
    state <- either errorIO return =<< load_score fname
    (events, logs) <- timed_derive fname state
    mapM_ Log.write logs
    let msg = "perform " ++ fname
    (msgs, logs) <- print_timer msg (timer_msg (length . fst)) $ do
        let (msgs, logs) = perform cmd_config state events
        force (msgs, logs)
        return (msgs, logs)
    mapM_ Log.write logs
    return msgs

timed_derive :: String -> State.State -> IO (Cmd.Events, [Log.Msg])
timed_derive name state = do
    block_id <- maybe (errorIO $ name <> ": no root block") return $
        State.config#State.root #$ state
    let (events, logs) = first Vector.fromList $ LEvent.partition $
            Derive.r_events $ derive_block state block_id
    events <- print_timer ("derive " ++ name) (timer_msg Vector.length)
        (return $! events)
    return (events, filter (not . boring) logs)
    where
    boring msg = Cache.is_cache_log msg

timer_msg :: (a -> Int) -> Double -> a -> String
timer_msg len secs events = Printf.printf "events: %d (%.2f / sec)"
    events_len (fromIntegral events_len / secs)
    where events_len = len events

derive_block :: State.State -> BlockId -> Derive.Result
derive_block = DeriveTest.derive_block_standard instrument_db mempty mempty id

instrument_db :: Cmd.InstrumentDb
instrument_db = DeriveTest.synth_to_db mempty pure_synths

-- | These are synths that I happen to know are pure, and are used in the saved
-- performances.
pure_synths :: [Cmd.SynthDesc]
pure_synths = concat $ Unsafe.unsafePerformIO $
    sequence $ map ($"") [Fm8.load, Kontakt.load, Pianoteq.load, Vsl.load]

perform :: Cmd.Config -> State.State -> Cmd.Events
    -> ([Midi.WriteMessage], [Log.Msg])
perform cmd_config ui_state events =
    extract $ CmdTest.result_val $ CmdTest.run ui_state cmd_state $
        PlayUtil.perform_events events
    where
    cmd_state = Cmd.initial_state cmd_config
    extract val = case val of
        Left err -> error $ "perform: " ++ err
        Right Nothing -> error $ "perform: cmd aborted"
        Right (Just msgs) -> LEvent.partition msgs

load_score :: FilePath -> IO (Either String State.State)
load_score fname = do
    result <- print_timer ("unserialize " ++ fname) (\_ _ -> "") $
        Save.read_state_ fname
    return $ case result of
        Left err -> Left $ "loading " ++ show fname ++ ": " ++ err
        Right Nothing -> Left $ "loading " ++ show fname ++ ": doesn't exist"
        Right (Just state) -> Right state

-- | Load cmd config, which basically means the inst db.
load_cmd_config :: IO Cmd.Config
load_cmd_config = do
    config <- Local.Config.load_static_config
    return $ DeriveTest.cmd_config (StaticConfig.instrument_db config)
