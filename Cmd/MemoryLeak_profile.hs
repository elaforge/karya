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
maxMemoryGrowth :: Memory.Size
maxMemoryGrowth = Memory.fromM 1

-- | No cmd should take more than this many seconds.
maxCmdLatency :: Double
maxCmdLatency = 0.05

test_generated_score = do
    let times = 5
    let states = ResponderTest.mkstatesBlocks (score 64 256)
    let pitches = ["5c", "5d", "5e", "5f"]
        mods = [UiTest.insertEventIn "sub3" 2 (1, 1, p) | p <- cycle pitches]
    checkResults times states mods

_testPutuKreasi = do
    let fname = "save/bali/putu-kreasi-galungan"
    let times = 4
    let pitches = ["5i", "5o", "5e", "5u"]
        mods =
            [ UiTest.insertEventIn "untitled/b3" 12 (1.25, 0.25, p)
            | p <- cycle pitches
            ]
    states <- loadFile fname
    checkResults times states mods

checkResults :: Int -> ResponderTest.States -> [Cmd.CmdT IO ()] -> IO ()
checkResults times states mods = do
    (latency, mems) <- unzip <$> thread states (take times mods)
    let diffs = zipWith (-) (drop 1 mems) mems
    Text.IO.putStrLn $ "diffs: " <> pretty diffs
    -- The first run takes extra memory for some reason, but it seems to
    -- stabilize after that.  TODO maybe track that one down too some day.
    diffs <- pure $ drop 1 diffs
    -- If it's too lazy, memory will leak.
    equal (filter (>= maxMemoryGrowth) diffs) []
    check ("< 1mb: " <> pretty diffs) $ all ((<1) . Memory.toM) diffs

    -- If it's too strict, the UI will get laggy.
    putStrLn "cmd latency:"
    let showLatency (msg, secs) = msg <> ": " <> pretty secs
    mapM_ Text.IO.putStrLn (map showLatency (concat latency))
    equal
        [ showLatency (msg, secs)
        -- drop 1 because the latency on the first cmd is much higher.  I don't
        -- know why, maybe it needs to evaluate more thunks, but it doesn't
        -- represent sustained use.
        | (msg, secs) <- concat (map (drop 1) latency)
        , secs >= maxCmdLatency
        ]
        []
    pure ()

loadFile :: FilePath -> IO ResponderTest.States
loadFile fname = do
    cmdConfig <- DeriveSaved.loadCmdConfig
    onlyDeriveRoot <$> DeriveSaved.loadScoreStates cmdConfig fname

-- | (msg, latency)
type Latency = (Text, ResponderTest.Seconds)

thread :: ResponderTest.States -> [Cmd.CmdT IO ()]
    -> IO [([Latency], Memory.Size)]
thread states (mod:mods) = do
    states <- pure $ stripStates states
    (latency, result) <- stripResults <$>
        ResponderTest.respondAll [] timeout states mod
    forcePerformances result
    mem <- Memory.rtsAllocated
    ((latency, mem):) <$> thread (ResponderTest.resultStates result) mods
    where
    timeout = 64
thread _ [] = pure []

-- | Get rid of intermediate Results, otherwise I get the memory leak right
-- here!
stripResults :: [ResponderTest.Result]
    -> ([Latency], ResponderTest.Result)
stripResults results = latency `DeepSeq.deepseq` (latency, last results)
    where
    latency = map strip results
    strip r = (pretty (ResponderTest.resultMsg r), ResponderTest.resultTime r)

-- The commented out strips out various bits of state to try to find a leak.
-- I'll leave it in here in case I need to do it again some day.

stripStates :: ResponderTest.States -> ResponderTest.States
stripStates = id -- second strip_cmd
    where
    _stripCmd state = state
        { Cmd.statePlay = play
            -- { Cmd.state_performance = mempty
            -- , Cmd.state_current_performance = mempty
            -- , Cmd.state_performance_threads = mempty
            -- }

            { Cmd.statePerformance = Map.map strip (Cmd.statePerformance play)
            , Cmd.stateCurrentPerformance =
                Map.map strip (Cmd.stateCurrentPerformance play)
            }
        }
        where play = Cmd.statePlay state
    strip = stripPerformance

stripPerformance :: Cmd.Performance -> Cmd.Performance
stripPerformance perf = cache `seq` perf
    { Cmd.perfDeriveCache = cache
    , Cmd.perfEvents = mempty -- strip_event <$> Cmd.perf_events perf
    -- , Cmd.perf_logs = []
    -- , Cmd.perf_logs_written = True
    , Cmd.perfTrackDynamic = mempty
    -- , Cmd.perf_integrated = mempty
    -- -- , Cmd.perf_damage = mempty
    -- , Cmd.perf_warps = []
    -- , Cmd.perf_track_signals = mempty
    }
    where
    -- cache = invalidate_cache (Cmd.perf_derive_cache perf)
    cache = stripCache (Cmd.perfDeriveCache perf)

invalidateCache :: Derive.Cache -> Derive.Cache
invalidateCache (Derive.Cache cache) = Derive.Cache (Map.map strip cache)
    where strip _ = Derive.Invalid

stripCache :: Derive.Cache -> Derive.Cache
stripCache (Derive.Cache cache) = Derive.Cache (Map.map strip cache)
    where
    strip Derive.Invalid = Derive.Invalid
    strip (Derive.Cached entry) = Derive.Cached $ case entry of
        Derive.CachedEvents (Derive.CallType collect es) ->
            Derive.CachedEvents $
                Derive.CallType (stripCollect collect) (stripEvent <$> es)
        _ -> entry

stripCollect :: Derive.Collect -> Derive.Collect
stripCollect c = c
    -- { Derive.collect_warp_map = mempty
    -- , Derive.collect_track_signals = mempty
    -- , Derive.collect_signal_fragments = mempty
    { Derive.collectTrackDynamic =
        Map.map stripDynamic (Derive.collectTrackDynamic c)
    , Derive.collectTrackDynamicInverted =
        Map.map stripDynamic (Derive.collectTrackDynamicInverted c)
    -- , Derive.collect_block_deps :: !BlockDeps
    -- , Derive.collect_cache :: !Cache
    -- , Derive.collect_integrated :: ![Integrated]
    -- , Derive.collect_control_mods :: ![ControlMod]
    -- , Derive.collect_score_duration :: !(CallDuration ScoreTime)
    -- , Derive.collect_real_duration :: !(CallDuration RealTime)
    }

stripEvent :: Score.Event -> Score.Event
stripEvent event = event
    -- { Score.event_untransformed_pitch = pitch
    -- }
    -- where pitch = PSignal.constant (PSignal.nn_pitch 40)

stripDynamic :: Derive.Dynamic -> Derive.Dynamic
stripDynamic dyn = dyn
    -- { Derive.state_controls = mempty -- !ScoreT.ControlMap
    -- , Derive.state_control_merge_defaults = mempty
    --     -- Map ScoreT.Control (Merger Signal.Control)
    -- , Derive.state_pitch = mempty -- :: !PSignal.PSignal
    -- , Derive.state_environ = mempty -- :: !BaseTypes.Environ
    -- -- , Derive.state_warp = -- :: !Score.Warp
    -- -- , Derive.state_scopes = mempty -- :: !Scopes
    -- , Derive.state_instrument_aliases = mempty
    --     -- :: !(Map ScoreT.Instrument ScoreT.Instrument)

    -- , Derive.state_control_damage :: !ControlDamage
    -- , Derive.state_under_invert = id -- !(NoteDeriver -> NoteDeriver)
    -- , Derive.state_inversion :: !Inversion
    { Derive.statePitchMap = Nothing
    -- , Derive.state_note_track = Nothing
    -- , Derive.state_stack :: !Stack.Stack
    -- , Derive.state_mode :: !Mode
    }

onlyDeriveRoot :: ResponderTest.States -> ResponderTest.States
onlyDeriveRoot = first $ Ui.views #= mempty

forcePerformances :: ResponderTest.Result -> IO ()
forcePerformances result = do
    let perfs = Cmd.statePerformance $ Cmd.statePlay $
            ResponderTest.resultCmdState result
    forM_ (Map.toList perfs) $ \(blockId, perf) ->
        Text.IO.putStrLn $ showt blockId <> " -> " <> showPerformance perf

showPerformance :: Msg.Performance -> Text
showPerformance perf
    | otherwise = showt (Vector.length (Cmd.perfEvents perf)) <> " events"
        <> if null logs then "" else ": " <> pretty (take 4 logs)
    where logs = filter ((>=Log.Warn) . Log.msgPriority) $ Cmd.perfLogs perf

score :: Int -> Int -> [UiTest.BlockSpec]
score subBlocks subNotes =
    ("top", [(">", (zip3 (iterate (+dur) 0) (repeat dur) subs))])
        : map (, sub) subs
    where
    dur = fromIntegral subNotes
    subs = map (("sub"<>) . showt) [1..subBlocks]
    sub = UiTest.regularNotes subNotes
