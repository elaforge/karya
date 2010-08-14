-- | Profile performance with and without the cache.
--
-- This module is about performance, correctness is tested in
-- "Derive.Cache_test".
module Derive.Cache_profile where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.CPUTime as CPUTime

import Util.Test
import qualified Util.Log as Log

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Update as Update
import qualified Ui.UiTest as UiTest

import qualified Derive.Cache_test as Cache_test
import qualified Derive.Derive as Derive
import qualified Derive.Derive_profile as Derive_profile
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Perform as Perform


profile_normal = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_simple "b1" 10 3 128)
        modify block pos = modify_note ("b1.0." ++ show block ++ ".t0")
            ("b1.0." ++ show block ++ ".t1") pos
    rederive ui_state [modify 0 2, modify 1 0, modify 4 4]

profile_small = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_simple "b1" 4 3 128)
    -- pprint (Map.keys (State.state_tracks ui_state))
    -- rederive ui_state [modify_note "b1.0.0.t0" "b1.0.0.t1" 2]
    rederive ui_state [modify_pitch "b1.0.0.t1" 2]
    -- rederive ui_state []

modify_note :: (State.UiStateMonad m) => String -> String -> ScoreTime -> m ()
modify_note note_tid pitch_tid pos = do
    State.insert_event (UiTest.tid note_tid) pos (Event.event "" 1)
    State.insert_event (UiTest.tid pitch_tid) pos (Event.event "1c" 0)

modify_pitch :: (State.UiStateMonad m) => String -> ScoreTime -> m ()
modify_pitch pitch_tid pos = do
    State.insert_event (UiTest.tid pitch_tid) pos (Event.event "1c" 0)


-- | Run the state transform a few times and rederive each time.
rederive :: State.State -> [State.StateId ()] -> IO ()
rederive initial_state modifications = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    go (start_cpu, start) initial_state Derive.empty_cache
        (return () : modifications)
    where
    go _ _ _ [] = return ()
    go start_times state1 cache (modify:rest) = do
        let (_, state2, updates) = Cache_test.run state1 modify
        cached <- Derive_profile.time_section 10 start_times "cached" $ do
            let result = Cache_test.derive_block cache state2 updates
                    (UiTest.bid "b1")
            let events = extract_events result
            Derive_profile.force events
            return (result, events, filter_logs (Derive.r_logs result))
        uncached <- Derive_profile.time_section 0 start_times "derive" $ do
            let result = Cache_test.derive_block Derive.empty_cache state2 []
                    (UiTest.bid "b1")
            let events = extract_events result
            Derive_profile.force events
            return (result, events, filter_logs (Derive.r_logs result))
        equal (Cache_test.diff_events cached uncached) (Right [])
        go start_times state2 (Derive.r_cache cached) rest
    filter_logs = map show_msg . filter (not . is_cache_msg)

extract_events result = either (error . ("Left: " ++) . show) id
    (Derive.r_result result)

-- | Rederive and check correctness.
rederive_check = undefined

is_cache_msg m = Text.pack "using cache" `Text.isInfixOf` Log.msg_text m
show_msg = Cache_test.show_msg_stack

now = fmap Time.utctDayTime Time.getCurrentTime

{-

type PerfResults = ([Perform.Event], [(Timestamp.Timestamp, Midi.Message)])

-- Make a nested structure with each call only appearing in one place and
-- a large number of events per block.  Run repeated modifications against
-- notes and controls.  Compare cached and uncached output, and cached and
-- uncached time.
--
-- Test with and without midi.
-- profile_rederive = do
--     let ui_state = UiTest.exec State.empty (make_nested "b1" 8 3)
--     (cache, perf_results) <- run_profile ui_state
--     rederive perf_results ui_state cache [append_note "b1" "b1.t0" "b1.t1"]

append_note bid note_tid pitch_tid = do
    pos <- State.track_end (UiTest.tid note_tid)
    State.insert_event (UiTest.tid note_tid) pos (Event.event "" 1)
    State.insert_event (UiTest.tid pitch_tid) pos (Event.event "1c" 0)

show_rederive_diff :: String -> PerfResults -> PerfResults -> String
show_rederive_diff msg (events1, mmsgs1) (events2, mmsgs2)
    | null ediff && null mmdiff = ""
    | otherwise = msg ++ " diffs:\n" ++ ediff ++ mmdiff
    where
    ediff = show_diff events1 events2
    mmdiff = show_diff mmsgs1 mmsgs2

show_diff :: (Eq a, Show a) => [a] -> [a] -> String
show_diff xs ys = concatMap f (Seq.diff (==) xs ys)
    where
    f (Just x, Nothing) = "<" ++ show x ++ "\n"
    f (Nothing, Just y) = ">" ++ show y ++ "\n"
    f _ = ""

-}
