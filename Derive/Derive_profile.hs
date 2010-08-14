module Derive.Derive_profile where
import qualified Control.DeepSeq as DeepSeq
import Control.Monad
-- import qualified Control.Concurrent as Concurrent
import qualified Data.Time as Time
import qualified System.CPUTime as CPUTime
import qualified Text.Printf as Printf
import qualified System.IO as IO

import Util.Control
import Util.Test

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Midi.Perform as Perform
import qualified Perform.Timestamp as Timestamp


-- | Make a block with simple non-overlapping controls and tempo changes.
profile_simple = derive_profile $ make_simple "b1" 0 2000

make_simple :: (State.UiStateMonad m) => String -> Int -> Double -> m [TrackId]
make_simple bid offset size = UiTest.mkstate bid $
        track_until size simple_tempo_track
        : note_tracks inst1 0 ++ note_tracks inst2 2
    where
    note_tracks name offset2 =
        [ track (note_track name)
        , track simple_pitch_track
        , track vel_track
        ]
        where track = track_until size . track_drop (offset + offset2)

-- | Block with a control controlling multiple note tracks.  Intended to
-- profile channelize control sharing.
profile_shared = derive_profile $ make_shared_control "b1" 2000

make_shared_control bid size = UiTest.mkstate bid $
        track simple_tempo_track
        : track mod_track
        : track_set 0 ++ track_set 2 -- ++ track_set 4
    where
    track = track_until size
    track_set offset = map (track . track_drop offset)
        [ note_track inst1
        , simple_pitch_track
        , vel_track
        ]

-- | Block with non-tempered scale so pitches can't share.  Intended to profile
-- channelize pitch sharing.
profile_nontempered = derive_profile $ make_nontempered "b1" 1000
    where
    make_nontempered bid size = UiTest.mkstate bid $ map (track_until size)
            [ simple_tempo_track
            , note_track inst1
            , nontempered_pitch_track
            , vel_track
            ]

-- | Giant control track with lots of samples.  Intended to profile control
-- track derivation.
profile_control = derive_profile $ make_big_control "b1" 15000

make_big_control bid size = UiTest.mkstate bid $ map (track_until size)
    [ (inst1, [(0, size, "")])
    , mod_track
    ]

-- | Giant note track.  Profile note track derivation.
profile_notes = derive_profile $ make_big_notes "b1" 15000
    where
    make_big_notes bid size = UiTest.mkstate bid $ map (track_until size)
        [ note_track inst1
        ]

-- | Block with lots of calls to sub-blocks.
profile_subderive = derive_profile $ make_subderive "b1" 300

make_subderive bid size = do
    let sub_bids = [bid ++ "." ++ show sub | sub <- [0..15]]
    forM_ (zip sub_bids [0, 4..]) $ \(sub_bid, offset) ->
        make_simple sub_bid offset 16
    UiTest.mkstate bid [track_until size $ ctrack 1 ">" sub_bids]

profile_nested = derive_profile $ make_nested_simple "b1" 8 3 128

make_nested :: (State.UiStateMonad m) => String -> Double -> Int -> m [a]
make_nested bid size 0 = do
    UiTest.mkstate bid $ map (track_until size)
        [note_track ">", simple_pitch_track]
    return []
make_nested bid size depth = do
    let sub_bids = [bid ++ "." ++ show sub | sub <- [0 .. min 4 (floor size)]]
    UiTest.mkstate bid $ map (track_until (step * size))
        [ctrack step inst1 sub_bids, pitch_track]
    forM_ sub_bids $ \sub -> make_nested sub size (depth-1)
    return []
    where
    step = size^depth
    pitch_track = ctrack0 step "*twelve"
        ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"]


-- | Try to duplicate a "normal" score, only without control curves.
-- This means a lightly nested structure, with the terminal blocks having
-- a more events than the intermediate ones.
make_nested_simple :: (State.UiStateMonad m) => String -> Int -> Int -> Int
    -> m ()
make_nested_simple bid _ 1 bottom_size = do
    UiTest.mkstate bid $ map (track_until (fromIntegral bottom_size))
        [note_track ">", simple_pitch_track]
    return ()
make_nested_simple bid size depth bottom_size = do
    let sub_bids = [bid ++ "." ++ show sub | sub <- [0 .. size]]
        step = bottom_size * (size^(depth-2))
    UiTest.mkstate bid $ map (track_until (fromIntegral (step * size)))
        [ctrack (fromIntegral step) inst1 sub_bids]
    forM_ sub_bids $ \sub -> make_nested_simple sub size (depth-1) bottom_size


-- | Make a UI state and force it.
-- ui_exec :: State.State -> State.StateId a -> IO State.State
-- ui_exec state modify = return (UiTest.exec state modify)
type PerfResults = ([Perform.Event], [(Timestamp.Timestamp, Midi.Message)])


-- * implementation

derive_profile :: State.StateId a -> IO ()
derive_profile create = do
    run_profile False (UiTest.exec State.empty create)
    return ()

run_profile :: Bool -> State.State -> IO (Derive.Cache, PerfResults)
run_profile run_midi ui_state = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    let section :: (Show log) => String -> IO ((), [a], [log]) -> IO ()
        section = time_section 2 (start_cpu, start)

    section "generate" $ do
        force (UiTest.block_structure ui_state)
        return ((), [], []) :: IO ((), [()], [()])

    let result = DeriveTest.derive_block ui_state (UiTest.bid "b1")
    let events = either (error . ("Left: " ++) . show) id
            (Derive.r_result result)
    section "derive" $ do
        force events
        return ((), events, DeriveTest.quiet_filter_logs (Derive.r_logs result))

    let (perf_events, convert_warns, mmsgs, midi_warns) =
            DeriveTest.perform inst_config events
    when (run_midi) $ do
        section "convert" $ do
            force perf_events
            return ((), perf_events, convert_warns)
        section "midi" $ do
            force mmsgs
            return ((), mmsgs, midi_warns)
    return (Derive.r_cache result, (perf_events, mmsgs))

time_section :: (Show log) => Int -> (Integer, Time.DiffTime) -> String
    -> IO (result, [a], [log]) -> IO result
time_section maxlogs (start_cpu, start_time) title op = do
    putStr $ "--> " ++ title ++ ": "
    IO.hFlush IO.stdout
    ((result, vals, logs), cpu_secs, secs) <- cpu_time op
    cur_cpu <- CPUTime.getCPUTime
    cur_time <- now
    Printf.printf "%d vals -> %.2fcpu / %.2fs (running: %.2fcpu / %.2fs)\n"
        (length vals) cpu_secs (double secs)
        (cpu_to_sec (cur_cpu - start_cpu)) (double (cur_time - start_time))
    let show_logs = take maxlogs logs
    when (not (null show_logs)) $ do
        pprint show_logs
    return result
    where
    double :: Time.DiffTime -> Double
    double = realToFrac

cpu_time :: IO a -> IO (a, Double, Time.DiffTime)
cpu_time op = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    v <- op
    end_cpu <- CPUTime.getCPUTime
    end <- now
    return (v, cpu_to_sec (end_cpu - start_cpu), end - start)

now = fmap Time.utctDayTime Time.getCurrentTime
cpu_to_sec :: Integer -> Double
cpu_to_sec s = fromIntegral s / fromIntegral (10^12)

force :: (DeepSeq.NFData a) => a -> IO ()
force val = DeepSeq.deepseq val (return ())


-- * state building

inst1 = ">fm8/1"
inst2 = ">fm8/2"

note_track inst = ctrack 1 inst [""]
simple_tempo_track = ctrack0 10 "tempo" ["1", "2", "3", "i 1"]
mod_track = ctrack0 1 "cc1 | srate = .02" ["i 1", "i 0", "e 1", "i .5", "0"]
simple_pitch_track =
    ctrack0 1 "*twelve" ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"]
nontempered_pitch_track =
    ctrack0 1 "*semar" ["1", "2", "3", "5", "6", "`1^`", "`6.`"]
vel_track = ctrack0 1 "vel" ["1", ".2", ".4", ".6"]

ctrack0 :: Double -> String -> [String] -> UiTest.TrackSpec
ctrack0 step title ts =
    (title, [(p, 0, t) | (p, t) <- zip [0, step..] (cycle ts)])

ctrack :: Double -> String -> [String] -> UiTest.TrackSpec
ctrack step title ts =
    (title, [(p, step, t) | (p, t) <- zip [0, step..] (cycle ts)])

track_until :: Double -> UiTest.TrackSpec -> UiTest.TrackSpec
track_until until = second (takeWhile (\(p, _, _) -> p <= until))

track_drop n = second (shift . drop n)
    where
    shift [] = []
    shift ((p, d, t) : rest) =
        (0, d, t) : map (\(p', d', t') -> (p' - p, d', t')) rest

inst_config = DeriveTest.make_inst_config
    [(drop 1 inst1, [0, 1]), (drop 1 inst2, [2])]
