-- | Profile performance with and without the cache.
--
-- This module is about performance, correctness is tested in
-- "Derive.Cache_test".
module Derive.Cache_profile where
import qualified Data.Monoid as Monoid
import qualified Data.Time as Time

import qualified Util.CPUTime as CPUTime
import Util.Control
import Util.Test

import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Derive.Cache_test as Cache_test
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Derive_profile as Derive_profile

import Types


profile_normal = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_controls 10 3 128)
        modify block pos = modify_note ("b1.0." ++ show block ++ ".t0")
            ("b1.0." ++ show block ++ ".t1") pos
    rederive ui_state [modify 0 2, modify 1 0, modify 4 4]

profile_small = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_controls 4 3 128)
    -- pprint (Map.keys (State.state_tracks ui_state))
    rederive ui_state [modify_pitch "b1.0.0.t1" 2]

modify_note :: (State.M m) => String -> String -> ScoreTime -> m ()
modify_note note_tid pitch_tid pos = do
    State.insert_event (UiTest.tid note_tid) (Event.event pos 1 "")
    State.insert_event (UiTest.tid pitch_tid) (Event.event pos 0 "1c")

modify_pitch :: (State.M m) => String -> ScoreTime -> m ()
modify_pitch pitch_tid pos =
    State.insert_event (UiTest.tid pitch_tid) (Event.event pos 0 "1c")

-- | Run the state transform a few times and rederive each time.
rederive :: State.State -> [State.StateId ()] -> IO ()
rederive initial_state modifications = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    go (start_cpu, start) initial_state mempty (return () : modifications)
    where
    go _ _ _ [] = return ()
    go start_times state1 cache (modify:rest) = do
        let section = Derive_profile.time_section start_times
        let (_, state2, cmd_updates) = Cache_test.run state1 modify
        cached <- section "cached" $
            eval_derivation cache state1 state2 cmd_updates

        uncached <- section "uncached" $ do
            let result = DeriveTest.derive_block_cache mempty
                    Monoid.mempty state2 (UiTest.bid "b1")
            let events = Derive.r_events result
            force events
            return (result, events)
        equal (Cache_test.diff_events cached uncached) []

        go start_times state2 (Derive.r_cache cached) rest

eval_derivation :: Derive.Cache -> State.State -> State.State
    -> [Update.CmdUpdate] -> IO (Derive.Result, Derive.Events)
eval_derivation cache state1 state2 cmd_updates = do
    force events
    return (result, events)
    where
    (ui_updates, _) = Diff.diff cmd_updates state1 state2
    damage = Diff.derive_diff state1 state2 ui_updates
    result = DeriveTest.derive_block_cache cache damage state2
        (UiTest.bid "b1")
    events = Derive.r_events result

now = fmap Time.utctDayTime Time.getCurrentTime
