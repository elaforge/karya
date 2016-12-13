-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This actually has tests, but they test memory usage, so this should be
-- compiled with optimization, like profiles.
module Cmd.MemoryLeak_profile where
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified GHC.Stats
import qualified System.Mem

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import Util.Testing

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.ResponderTest as ResponderTest

import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.Score as Score

import Global


test_generated_score = do
    Log.configure $ \st -> st { Log.state_log_level = Log.Notice }
    let times = 5
    let states = ResponderTest.mkstates_blocks (score 64 256)
    let pitches = ["5c", "5d", "5e", "5f"]
        mods = [UiTest.insert_event_in "sub3" 2 (1, 1, p) | p <- cycle pitches]
    mems <- thread states (take times mods)
    let diffs = zipWith (-) (drop 1 mems) mems
    Text.IO.putStrLn $ "diffs: " <> pretty diffs
    -- The first run takes extra memory for some reason, but it seems to
    -- stabilize after that.  TODO maybe track that one down too some day.
    diffs <- return $ drop 1 diffs
    check ("< 1mb: " <> pretty diffs) $ all ((<1) . megabytes) diffs

_test_putu_kreasi = do
    let fname = "save/bali/putu-kreasi-galungan"
    Log.configure $ \st -> st { Log.state_log_level = Log.Notice }
    let times = 4
    let pitches = ["5i", "5o", "5e", "5u"]
        mods =
            [ UiTest.insert_event_in "untitled/b3" 12 (1.25, 0.25, p)
            | p <- cycle pitches
            ]
    diffs <- memory_leak_file fname (take times mods)
    check ("< 1mb: " <> pretty diffs) $ all ((<1) . megabytes) diffs

memory_leak_file :: FilePath -> [Cmd.CmdT IO ()] -> IO [Bytes]
memory_leak_file fname mods = do
    cmd_config <- DeriveSaved.load_cmd_config
    states <- DeriveSaved.load_score_states cmd_config fname
    mems <- thread (only_derive_root states) mods
    Text.IO.putStrLn $ "mem: " <> pretty mems
    return $ zipWith (-) (drop 1 mems) mems

newtype Bytes = Bytes Int
    deriving (Show, Num)

instance Pretty.Pretty Bytes where
    pretty = (<>"mb") . Num.showFloat 2 . megabytes

megabytes :: Bytes -> Double
megabytes (Bytes b) = fromIntegral b / 1024^2

thread :: ResponderTest.States -> [Cmd.CmdT IO ()] -> IO [Bytes]
thread states (mod:mods) = do
    root_id <- maybe (errorIO "no root block") return $
        State.config#State.root #$ fst states
    states <- return $ strip_states states
    res <- ResponderTest.respond_until (is_complete root_id) timeout states mod
    force_performances (last res)
    mem <- memory_used
    (mem:) <$> thread (ResponderTest.result_states (last res)) mods
    where
    timeout = 64
    is_complete = ResponderTest.is_block_derive_complete
thread _ [] = return []

-- This still has the (commented out) code to strip out various bits of state
-- to try to find a leak.  I'll leave it in here in case I need to do it again
-- some day.

strip_states :: ResponderTest.States -> ResponderTest.States
strip_states = id -- second strip_cmd
    where
    strip_cmd state = state
        { Cmd.state_play = play
            -- { Cmd.state_performance = mempty
            -- , Cmd.state_current_performance = mempty
            -- , Cmd.state_performance_threads = mempty
            -- }

            { Cmd.state_performance = Map.map strip (Cmd.state_performance play)
            , Cmd.state_current_performance =
                Map.map strip (Cmd.state_current_performance play)
            }
        }
        where play = Cmd.state_play state
    strip = strip_performance

strip_performance :: Cmd.Performance -> Cmd.Performance
strip_performance perf = cache `seq` perf
    { Cmd.perf_derive_cache = cache
    , Cmd.perf_events = mempty -- strip_event <$> Cmd.perf_events perf
    -- , Cmd.perf_logs = []
    -- , Cmd.perf_logs_written = True
    , Cmd.perf_track_dynamic = mempty
    -- , Cmd.perf_integrated = mempty
    -- -- , Cmd.perf_damage = mempty
    -- , Cmd.perf_warps = []
    -- , Cmd.perf_track_signals = mempty
    }
    where
    -- cache = invalidate_cache (Cmd.perf_derive_cache perf)
    cache = strip_cache (Cmd.perf_derive_cache perf)

invalidate_cache :: Derive.Cache -> Derive.Cache
invalidate_cache (Derive.Cache cache) = Derive.Cache (Map.map strip cache)
    where strip _ = Derive.Invalid

strip_cache :: Derive.Cache -> Derive.Cache
strip_cache (Derive.Cache cache) = Derive.Cache (Map.map strip cache)
    where
    strip Derive.Invalid = Derive.Invalid
    strip (Derive.Cached entry) = Derive.Cached $ case entry of
        Derive.CachedEvents (Derive.CallType collect es) ->
            Derive.CachedEvents $
                Derive.CallType (strip_collect collect) (strip_event <$> es)
        _ -> entry

strip_collect :: Derive.Collect -> Derive.Collect
strip_collect c = c
    -- { Derive.collect_warp_map = mempty
    -- , Derive.collect_track_signals = mempty
    -- , Derive.collect_signal_fragments = mempty
    { Derive.collect_track_dynamic =
        Map.map strip_dynamic (Derive.collect_track_dynamic c)
    , Derive.collect_track_dynamic_inverted =
        Map.map strip_dynamic (Derive.collect_track_dynamic_inverted c)
    -- , Derive.collect_block_deps :: !BlockDeps
    -- , Derive.collect_cache :: !Cache
    -- , Derive.collect_integrated :: ![Integrated]
    -- , Derive.collect_control_mods :: ![ControlMod]
    -- , Derive.collect_score_duration :: !(CallDuration ScoreTime)
    -- , Derive.collect_real_duration :: !(CallDuration RealTime)
    }

strip_event :: Score.Event -> Score.Event
strip_event event = event
    -- { Score.event_untransformed_pitch = pitch
    -- }
    -- where pitch = PSignal.constant (PSignal.nn_pitch 40)

strip_dynamic :: Derive.Dynamic -> Derive.Dynamic
strip_dynamic dyn = dyn
    -- { Derive.state_controls = mempty -- !Score.ControlMap
    -- , Derive.state_control_functions = mempty -- :: !Score.ControlFunctionMap
    -- , Derive.state_control_merge_defaults = mempty
    --     -- Map.Map Score.Control (Merger Signal.Control)
    -- , Derive.state_pitches = mempty -- :: !Score.PitchMap
    -- , Derive.state_pitch = mempty -- :: !PSignal.PSignal
    -- , Derive.state_environ = mempty -- :: !BaseTypes.Environ
    -- -- , Derive.state_warp = -- :: !Score.Warp
    -- -- , Derive.state_scopes = mempty -- :: !Scopes
    -- , Derive.state_instrument_aliases = mempty
    --     -- :: !(Map.Map Score.Instrument Score.Instrument)

    -- , Derive.state_control_damage :: !ControlDamage
    -- , Derive.state_under_invert = id -- !(NoteDeriver -> NoteDeriver)
    -- , Derive.state_inversion :: !Inversion
    { Derive.state_pitch_map = Nothing
    -- , Derive.state_note_track = Nothing
    -- , Derive.state_stack :: !Stack.Stack
    -- , Derive.state_mode :: !Mode
    }

only_derive_root :: ResponderTest.States -> ResponderTest.States
only_derive_root = first $ State.views #= mempty

force_performances :: ResponderTest.Result -> IO ()
force_performances result = do
    let perfs = Cmd.state_performance $ Cmd.state_play $
            ResponderTest.result_cmd_state result
    forM_ (Map.toList perfs) $ \(block_id, perf) ->
        Text.IO.putStrLn $ showt block_id <> " -> " <> show_performance perf

show_performance :: Msg.Performance -> Text
show_performance perf
    | otherwise = showt (Vector.length (Cmd.perf_events perf)) <> " events"
        <> if null logs then "" else ": " <> pretty (take 4 logs)
    where logs = filter ((>=Log.Warn) . Log.msg_priority) $ Cmd.perf_logs perf

score :: Int -> Int -> [UiTest.BlockSpec]
score sub_blocks sub_notes =
    ("top", [(">", (zip3 (iterate (+dur) 0) (repeat dur) subs))])
        : map (, sub) subs
    where
    dur = fromIntegral sub_notes
    subs = map (("sub"<>) . show) [1..sub_blocks]
    sub = UiTest.regular_notes sub_notes

memory_used :: IO Bytes
memory_used = do
    System.Mem.performMajorGC
    stats <- GHC.Stats.getGCStats
    return $ Bytes $ fromIntegral $ GHC.Stats.currentBytesUsed stats
