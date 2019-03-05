-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This actually has tests, but they test memory usage, so this should be
-- compiled with optimization, like profiles.
module Cmd.MemoryLeak_profile where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Util.Memory as Memory
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.ResponderTest as ResponderTest

import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.Score as Score

import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


-- | Don't leak more than this much on each run.
max_memory_growth :: Memory.Size
max_memory_growth = Memory.fromM 1

-- | No cmd should take more than this many seconds.
max_cmd_latency :: Double
max_cmd_latency = 0.05

test_generated_score = do
    let times = 5
    let states = ResponderTest.mkstates_blocks (score 64 256)
    let pitches = ["5c", "5d", "5e", "5f"]
        mods = [UiTest.insert_event_in "sub3" 2 (1, 1, p) | p <- cycle pitches]
    check_results times states mods

_test_putu_kreasi = do
    let fname = "save/bali/putu-kreasi-galungan"
    let times = 4
    let pitches = ["5i", "5o", "5e", "5u"]
        mods =
            [ UiTest.insert_event_in "untitled/b3" 12 (1.25, 0.25, p)
            | p <- cycle pitches
            ]
    states <- load_file fname
    check_results times states mods

check_results :: Int -> ResponderTest.States -> [Cmd.CmdT IO ()] -> IO ()
check_results times states mods = do
    (latency, mems) <- unzip <$> thread states (take times mods)
    let diffs = zipWith (-) (drop 1 mems) mems
    Text.IO.putStrLn $ "diffs: " <> pretty diffs
    -- The first run takes extra memory for some reason, but it seems to
    -- stabilize after that.  TODO maybe track that one down too some day.
    diffs <- return $ drop 1 diffs
    -- If it's too lazy, memory will leak.
    equal (filter (>= max_memory_growth) diffs) []
    check ("< 1mb: " <> pretty diffs) $ all ((<1) . Memory.toM) diffs

    -- If it's too strict, the UI will get laggy.
    putStrLn "cmd latency:"
    let show_latency (msg, secs) = msg <> ": " <> pretty secs
    mapM_ Text.IO.putStrLn (map show_latency (concat latency))
    equal
        [ show_latency (msg, secs)
        -- drop 1 because the latency on the first cmd is much higher.  I don't
        -- know why, maybe it needs to evaluate more thunks, but it doesn't
        -- represent sustained use.
        | (msg, secs) <- concat (map (drop 1) latency)
        , secs >= max_cmd_latency
        ]
        []
    return ()

load_file :: FilePath -> IO ResponderTest.States
load_file fname = do
    cmd_config <- DeriveSaved.load_cmd_config
    only_derive_root <$> DeriveSaved.load_score_states cmd_config fname

-- | (msg, latency)
type Latency = (Text, ResponderTest.Seconds)

thread :: ResponderTest.States -> [Cmd.CmdT IO ()]
    -> IO [([Latency], Memory.Size)]
thread states (mod:mods) = do
    states <- return $ strip_states states
    (latency, result) <- strip_results <$>
        ResponderTest.respond_all timeout states mod
    force_performances result
    mem <- Memory.rtsAllocated
    ((latency, mem):) <$> thread (ResponderTest.result_states result) mods
    where
    timeout = 64
thread _ [] = return []

-- | Get rid of intermediate Results, otherwise I get the memory leak right
-- here!
strip_results :: [ResponderTest.Result]
    -> ([Latency], ResponderTest.Result)
strip_results results = latency `DeepSeq.deepseq` (latency, last results)
    where
    latency = map strip results
    strip r = (pretty (ResponderTest.result_msg r), ResponderTest.result_time r)

-- The commented out strips out various bits of state to try to find a leak.
-- I'll leave it in here in case I need to do it again some day.

strip_states :: ResponderTest.States -> ResponderTest.States
strip_states = id -- second strip_cmd
    where
    _strip_cmd state = state
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
    -- , Derive.state_control_functions = mempty
    --      -- :: !BaseTypes.ControlFunctionMap
    -- , Derive.state_control_merge_defaults = mempty
    --     -- Map ScoreT.Control (Merger Signal.Control)
    -- , Derive.state_pitches = mempty -- :: !Score.PitchMap
    -- , Derive.state_pitch = mempty -- :: !PSignal.PSignal
    -- , Derive.state_environ = mempty -- :: !BaseTypes.Environ
    -- -- , Derive.state_warp = -- :: !Score.Warp
    -- -- , Derive.state_scopes = mempty -- :: !Scopes
    -- , Derive.state_instrument_aliases = mempty
    --     -- :: !(Map ScoreT.Instrument ScoreT.Instrument)

    -- , Derive.state_control_damage :: !ControlDamage
    -- , Derive.state_under_invert = id -- !(NoteDeriver -> NoteDeriver)
    -- , Derive.state_inversion :: !Inversion
    { Derive.state_pitch_map = Nothing
    -- , Derive.state_note_track = Nothing
    -- , Derive.state_stack :: !Stack.Stack
    -- , Derive.state_mode :: !Mode
    }

only_derive_root :: ResponderTest.States -> ResponderTest.States
only_derive_root = first $ Ui.views #= mempty

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
    subs = map (("sub"<>) . showt) [1..sub_blocks]
    sub = UiTest.regular_notes sub_notes
