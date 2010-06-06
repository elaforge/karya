module Derive.Derive_profile where
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified System.CPUTime as CPUTime
import qualified Text.Printf as Printf

import Util.Control
import Util.Test
import qualified Util.Pretty as Pretty

import Ui
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Midi.Perform as Perform


profile_shared = run_profile $ make_shared_control "b1" 1000

profile_simple = run_profile $ make_simple "b1" 1000

profile_nontempered = run_profile $ make_nontempered "b1" 1000


run_profile create = do
    let (_, ui_state) = UiTest.run State.empty create
    let blocks = State.state_blocks ui_state

    start_cpu <- CPUTime.getCPUTime
    start <- now
    let section = time_section (start_cpu, start)
    section "generate" $ do
        print $ Map.map (Track.events_length . Track.track_events)
            (State.state_tracks ui_state)

    let e_event e = (Score.event_start e, Score.event_duration e,
            Score.event_text e)
    let (events, logs) = DeriveTest.e_val_right $
            DeriveTest.derive_block ui_state (UiTest.bid "b1")
    section "derive" $ do
        putStrLn $ "events: " ++ show (length events)
        pprint logs

    let e_pevent e = (Perform.event_start e, Perform.event_duration e)
    let (perf_events, convert_warns, mmsgs, midi_warns) =
            DeriveTest.perform inst_config events
    section "convert" $ do
        -- pprint (take 5 (map e_pevent perf_events))
        putStrLn $ "perf events: " ++ show (length perf_events)
        pprint (take 5 convert_warns)

    section "midi" $ do
        putStrLn $ "msgs: " ++ show (length mmsgs)
        pprint (take 5 midi_warns)

time_section :: (Integer, Time.DiffTime) -> String -> IO a -> IO a
time_section (start_cpu, start_time) title op = do
    putStrLn $ "**** " ++ title
    (v, cpu_secs, secs) <- cpu_time op
    cur_cpu <- CPUTime.getCPUTime
    cur_time <- now
    Printf.printf "---> %s: %.2f cpu / %.2fs (running: %.2f cpu / %.2fs)\n\n"
        title cpu_secs (double secs)
        (cpu_to_sec (cur_cpu - start_cpu)) (double (cur_time - start_time))
    return v
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


-- | Make a block with simple non-overlapping controls and tempo changes.
make_simple :: (State.UiStateMonad m) => String -> Double -> m [TrackId]
make_simple bid size = UiTest.mkstate bid $
        track_until size simple_tempo_track
        : note_tracks inst1 0 ++ note_tracks inst2 2
    where
    note_tracks name offset =
        [ track (note_track name)
        , track simple_pitch_track
        , track vel_track
        ]
        where track = track_until size . track_drop offset

-- | Block with a control controlling multiple notes.
make_shared_control :: (State.UiStateMonad m) => String -> Double -> m [TrackId]
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

-- | Block with non-tempered scale so pitches can't share.
make_nontempered bid size = UiTest.mkstate bid $ map (track_until size)
        [ simple_tempo_track
        , note_track inst1
        , nontempered_pitch_track
        , vel_track
        ]

-- | Block with lots of calls to sub-blocks.

inst1 = ">fm8/1"
inst2 = ">fm8/2"

note_track inst = (inst, [(p, 1, "") | p <- [0..]])
simple_tempo_track = ctrack "tempo" ["1", "2", "3", "i 1"] 10
mod_track = ctrack "srate = .02 | cc1" ["i 1", "i 0", "e 1", "i .5", "0"] 1
simple_pitch_track =
    ctrack "*twelve" ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"] 1
nontempered_pitch_track =
    ctrack "*semar" ["1", "2", "3", "5", "6", "1^", "6."] 1
vel_track = ctrack "vel" ["1", ".2", ".4", ".6"] 1

ctrack :: String -> [String] -> Double -> UiTest.TrackSpec
ctrack title ts step =
    (title, [(p, 0, t) | (p, t) <- zip [0, step..] (cycle ts)])

track_until :: Double -> UiTest.TrackSpec -> UiTest.TrackSpec
track_until until = second (takeWhile (\(p, _, _) -> p <= until))

track_drop n = second (shift . drop n)
    where
    shift [] = []
    shift ((p, d, t) : rest) =
        (0, d, t) : map (\(p', d', t') -> (p' - p, d', t')) rest

inst_config = DeriveTest.make_inst_config
    [(drop 1 inst1, [0, 1]), (drop 1 inst2, [2])]
