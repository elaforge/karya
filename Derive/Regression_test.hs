-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Perform some scores and compare them against a saved version of what they
-- used to output.
module Derive.Regression_test where
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified System.IO.Unsafe as Unsafe

import qualified Util.ApproxEq as ApproxEq
import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import Util.Test
import qualified Util.Thread as Thread

import Midi.Instances ()
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Save as Save
import qualified Cmd.Serialize as Serialize

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


-- | Perform the input score and save the midi msgs to the output file.
-- This creates the -perf files.
save_performance :: FilePath -> FilePath -> IO ()
save_performance input output = do
    msgs <- perform_file input
    Serialize.serialize magic output msgs

test_bloom = check =<< compare_performance
    "data/bloom-perf.gz" "data/bloom.gz"
test_pnovla = check =<< compare_performance
    "data/pnovla-perf.gz" "data/pnovla.gz"
test_viola_sonata = check =<< compare_performance
    "data/viola-sonata-perf.gz" "data/viola-sonata.gz"

-- * implementation

compare_performance :: FilePath -> FilePath -> IO Bool
compare_performance saved score = timeout score $ do
    expected <- load_midi saved
    got <- perform_file score
    let diffs = diff_performances expected got
    -- Too many diffs aren't useful.
    mapM_ Text.IO.putStrLn (take 40 diffs)
    return $ null diffs

timeout :: String -> IO a -> IO a
timeout fname = maybe (errorIO msg) return <=< Thread.timeout 15
    where
    msg = fname
        ++ ": timed out, this can happen when too many msgs are different"

perform_file :: FilePath -> IO [Midi.WriteMessage]
perform_file fname = do
    cmd_config <- load_cmd_config
    state <- either errorIO return =<< load_score fname
    (events, logs) <- derive fname state
    mapM_ Log.write logs
    (msgs, logs) <- print_timer ("perform " ++ fname)
        (("MIDI msgs: "++) . show . length . fst) $ return $
            perform cmd_config state events
    mapM_ Log.write logs
    return msgs

load_midi :: FilePath -> IO [Midi.WriteMessage]
load_midi fname = Serialize.unserialize magic fname >>= \x -> case x of
    Left err -> errorIO $ "loading " ++ fname ++ ": " ++ err
    Right Nothing -> errorIO $ "not found: " ++ fname
    Right (Just msgs) -> return msgs

diff_performances :: [Midi.WriteMessage] -> [Midi.WriteMessage] -> [Text]
diff_performances expected got =
    mapMaybe show_diff $ Diff.getGroupedDiffBy wmsgs_equal expected got
    where
    show_diff (Diff.Both {}) = Nothing
    show_diff (Diff.First msgs) = Just $ Text.unlines $
        map (("- " <>) . Pretty.prettytxt) msgs
    show_diff (Diff.Second msgs) = Just $ Text.unlines $
        map (("+ " <>) . Pretty.prettytxt) msgs

wmsgs_equal :: Midi.WriteMessage -> Midi.WriteMessage -> Bool
wmsgs_equal (Midi.WriteMessage dev1 t1 m1) (Midi.WriteMessage dev2 t2 m2) =
    dev1 == dev2 && ApproxEq.approx_eq 0.001 t1 t2 && msgs_equal m1 m2

msgs_equal :: Midi.Message -> Midi.Message -> Bool
msgs_equal (Midi.ChannelMessage chan1 m1) (Midi.ChannelMessage chan2 m2) =
    chan1 == chan2 && chan_msgs_equal m1 m2
msgs_equal m1 m2 = m1 == m2

chan_msgs_equal :: Midi.ChannelMessage -> Midi.ChannelMessage -> Bool
chan_msgs_equal (Midi.PitchBend v1) (Midi.PitchBend v2) =
    ApproxEq.approx_eq 0.01 v1 v2
    -- PitchBends are serialized as 14-bit numbers, so when they get
    -- deserialized they change.
chan_msgs_equal m1 m2 = m1 == m2

magic :: Serialize.Magic
magic = Serialize.Magic 'm' 'i' 'd' 'i'

derive :: String -> State.State -> IO (Cmd.Events, [Log.Msg])
derive name state = do
    block_id <- maybe (errorIO $ name <> ": no root block") return $
        State.config#State.root #$ state
    let (events, logs) = first Vector.fromList $ LEvent.partition $
            Derive.r_events $ derive_block state block_id
    events <- print_timer ("derive " ++ name)
        (("events: "<>) . show . Vector.length) (return $! events)
    return (events, filter (not . boring) logs)
    where
    boring msg = Cache.is_cache_log msg
        || "Track signal" `Text.isPrefixOf` Log.msg_text msg

derive_block :: State.State -> BlockId -> Derive.Result
derive_block = DeriveTest.derive_block_standard instrument_db mempty mempty id

instrument_db :: Cmd.InstrumentDb
instrument_db = DeriveTest.synth_to_db mempty synths

-- | These are synths that I happen to know are pure, and are used in the saved
-- performances.
synths :: [Cmd.SynthDesc]
synths = concat $ Unsafe.unsafePerformIO $
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
    result <- print_timer ("unserialize " ++ fname) (const "") $
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
