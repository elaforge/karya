module Derive.Derive_profile where
import qualified Control.DeepSeq as DeepSeq
import Control.Monad
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

import qualified Cmd.Create as Create

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Midi.Perform as Perform
import qualified Perform.Timestamp as Timestamp


-- q=90 means 1.5 quarters / sec
-- So 3 8ths, 6 16ths, 12 32nds
-- 32 * 60 * 60 * 2 = 230400 for 2 hours
-- two_hours = 230400
-- time_minutes events = events / 32 / 60

-- | Make a giant block with simple non-overlapping controls and tempo changes.
profile_big_block = derive_profile $ make_simple "b1" 2000

make_simple :: (State.UiStateMonad m) => String -> Int -> m [TrackId]
make_simple bid size = UiTest.mkstate bid $
        make_tempo size
        : note_tracks inst1 0 ++ note_tracks inst1 2 ++ note_tracks inst2 4
    where
    note_tracks name offset = map (track_take size . track_drop offset)
        [note_track name, simple_pitch_track, vel_track]


-- | Block with a control controlling multiple note tracks.  Intended to
-- profile channelize control sharing.
profile_shared = derive_profile $ make_shared_control "b1" 2000

make_shared_control bid size = UiTest.mkstate bid $
        make_tempo size
        : track_take size mod_track
        : track_set 0 ++ track_set 2 -- ++ track_set 4
    where
    track_set offset = map (track_take size . track_drop offset)
        [ note_track inst1
        , simple_pitch_track
        , vel_track
        ]

-- | Block with non-tempered scale so pitches can't share.  Intended to profile
-- channelize pitch sharing.
profile_nontempered = derive_profile $ make_nontempered "b1" 1000
    where
    make_nontempered bid size = UiTest.mkstate bid $ map (track_until size)
            [ make_tempo (floor size)
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

profile_nested_simple = derive_profile $ make_nested_notes "b1" 10 3 60
profile_nested_controls = derive_profile $ make_nested_controls "b1" 10 3 60

make_nested_notes :: (State.UiStateMonad m) => String -> Int -> Int -> Int
    -> m ()
make_nested_notes = make_nested [note_track inst1, simple_pitch_track]

make_nested_controls :: (State.UiStateMonad m) => String -> Int -> Int -> Int
    -> m ()
make_nested_controls = make_nested
    [ note_track inst1
    , simple_pitch_track
    , mod_track
    ]

-- | Try to generate a "normal" score.  This means a lightly nested structure,
-- with the terminal blocks having a more events than the intermediate ones.
--
-- This doesn't produce intermediate tempo or control curves.
make_nested :: (State.UiStateMonad m) => [UiTest.TrackSpec]
    -> String -> Int -> Int -> Int -> m ()
make_nested bottom_tracks block_name size depth bottom_size = do
    (ruler_id, _) <- Create.ruler "ruler" UiTest.default_ruler
    go ruler_id block_name depth
    where
    go ruler_id block_name 1 = do
        UiTest.mkstate_id_ruler (UiTest.bid block_name) ruler_id $
            map (track_take bottom_size) bottom_tracks
        return ()
    go ruler_id block_name depth = do
        let sub_bids = [block_name ++ "." ++ show sub | sub <- [0 .. size-1]]
            step = bottom_size * size^(depth-2)
        UiTest.mkstate_id_ruler (UiTest.bid block_name) ruler_id $
            map (track_take size) [ctrack (fromIntegral step) inst1 sub_bids]
        forM_ sub_bids $ \sub -> go ruler_id sub (depth-1)

-- * implementation

type PerfResults = ([Perform.Event], [(Timestamp.Timestamp, Midi.Message)])

-- | This runs the derivation several times to minimize the creation cost.
derive_profile :: State.StateId a -> IO ()
derive_profile create = sequence_ $ replicate 6 $
    run_profile False (UiTest.exec State.empty create)

run_profile :: Bool -> State.State -> IO (Derive.Cache, PerfResults)
run_profile perform_midi ui_state = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    let section :: (Show log) => String -> IO ((), [a], [log]) -> IO ()
        section = time_section 2 (start_cpu, start)

    let result = DeriveTest.derive_block ui_state (UiTest.bid "b1")
    let events = either (error . ("Left: " ++) . show) id
            (Derive.r_result result)
    section "derive" $ do
        force events
        return ((), events, DeriveTest.quiet_filter_logs (Derive.r_logs result))

    let (perf_events, convert_warns, mmsgs, midi_warns) =
            DeriveTest.perform DeriveTest.default_lookup_inst midi_config events
    when perform_midi $ do
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

inst1 = ">s/1"
inst2 = ">s/2"

note_track inst = ctrack 1 inst [""]
make_tempo n = track_take (ceiling (fromIntegral n / 10)) $
    ctrack0 10 "tempo" ["1", "2", "3", "i 1"]
mod_track = ctrack0 1 "cc1 | srate = .02" ["1", "i 0", "e 1", "i .5", "i 0"]
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
track_until until = second (takeWhile (\(p, _, _) -> p < until))

track_drop n = second (shift . drop n)
    where
    shift [] = []
    shift ((p, d, t) : rest) =
        (0, d, t) : map (\(p', d', t') -> (p' - p, d', t')) rest

track_take :: Int -> UiTest.TrackSpec -> UiTest.TrackSpec
track_take = second . take

midi_config = DeriveTest.make_midi_config
    [(drop 1 inst1, [0, 1]), (drop 1 inst2, [2])]
