-- | Profile performance with and without the cache.
--
-- This module is about performance, correctness is tested in
-- "Derive.Cache_test".
module Derive.Cache_profile where
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Data.Time as Time
import qualified System.CPUTime as CPUTime

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Derive.Cache as Cache
import qualified Derive.Cache_test as Cache_test
import qualified Derive.Derive as Derive
import Derive.Derive_profile (force)
import qualified Derive.Derive_profile as Derive_profile
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent

import qualified Perform.Midi.Cache as Midi.Cache
import qualified Perform.Midi.Cache_test as Midi.Cache_test
import qualified Perform.Midi.Convert as Convert


profile_normal = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_controls "b1" 10 3 128)
        modify block pos = modify_note ("b1.0." ++ show block ++ ".t0")
            ("b1.0." ++ show block ++ ".t1") pos
    rederive ui_state [modify 0 2, modify 1 0, modify 4 4]

profile_midi_normal = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_controls "b1" 10 3 128)
        modify block pos = modify_note ("b1.0." ++ show block ++ ".t0")
            ("b1.0." ++ show block ++ ".t1") pos
    rederive_midi ui_state [modify 0 2, modify 1 0, modify 4 4]

profile_small = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_controls "b1" 4 3 128)
    -- pprint (Map.keys (State.state_tracks ui_state))
    rederive ui_state [modify_pitch "b1.0.0.t1" 2]

profile_midi_small = do
    let ui_state = UiTest.exec State.empty
            (Derive_profile.make_nested_controls "b1" 4 3 128)
    -- pprint (UiTest.simplify ui_state)
    rederive_midi ui_state [modify_pitch "b1.0.0.t1" 2]

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
    go (start_cpu, start) initial_state mempty (return () : modifications)
    where
    go _ _ _ [] = return ()
    go start_times state1 cache (modify:rest) = do
        let section = Derive_profile.time_section start_times
        let (_, state2, updates) = Cache_test.run state1 modify
        cached <- section "cached" $ do
            eval_derivation cache state1 state2 updates

        uncached <- section "uncached" $ do
            let result = DeriveTest.derive_block_cache mempty
                    Monoid.mempty state2 (UiTest.bid "b1")
            let events = Derive.r_events result
            force events
            return (result, events)
        equal (Cache_test.diff_events cached uncached) []

        go start_times state2 (Derive.r_cache cached) rest

eval_derivation :: Derive.Cache -> State.State -> State.State -> [Update.Update]
    -> IO (Derive.Result, Derive.Events)
eval_derivation cache state1 state2 updates = do
    force events
    return (result, events)
    where
    damage = Cache.score_damage state1 state2 updates
    result = DeriveTest.derive_block_cache cache damage state2 (UiTest.bid "b1")
    events = Derive.r_events result

rederive_midi :: State.State -> [State.StateId ()] -> IO ()
rederive_midi initial_state modifications = do
    start_cpu <- CPUTime.getCPUTime
    start <- now
    go (start_cpu, start) initial_state mempty
        initial_midi (return () : modifications)
    where
    initial_midi = Midi.Cache.cache Derive_profile.midi_config
    go _ _ _ _ [] = return ()
    go start_times state1 derive_cache midi_cache (modify:rest) = do
        let section = Derive_profile.time_section start_times
        let (_, state2, updates) = Cache_test.run state1 modify
        cached <- section "cached" $ do
            eval_derivation derive_cache state1 state2 updates

        (cached_midi, stats) <- section "cached midi" $ do
            let (out, stats) = cached_perform midi_cache
                    (event_damage cached) (Derive.r_events cached)
                msgs = Midi.Cache.cache_messages out
            force msgs
            return ((out, stats), msgs)
        putStrLn $ "stats: " ++ Pretty.pretty stats

        (uncached_midi, _) <- section "uncached midi" $ do
            let (out, stats) = cached_perform initial_midi
                    (event_damage cached) (Derive.r_events cached)
                msgs = Midi.Cache.cache_messages out
            force msgs
            return ((out, stats), msgs)
        equal (Midi.Cache_test.diff_msgs cached_midi uncached_midi)
            []

        go start_times state2 (Derive.r_cache cached) cached_midi rest

    event_damage result = Midi.Cache.EventDamage d
        where Derive.EventDamage d = Derive.r_event_damage result

cached_perform :: Midi.Cache.Cache -> Midi.Cache.EventDamage -> Derive.Events
    -> (Midi.Cache.Cache, (RealTime, RealTime))
cached_perform cache damage events = (out, Midi.Cache.cache_stats splice out)
    where
    perf_events = Convert.convert DeriveTest.default_lookup_scale
        DeriveTest.default_lookup_inst events
    out = Midi.Cache.perform cache damage perf_events
    splice = fmap fst $
        List.find is_failure (zip [0..] (Midi.Cache.cache_chunks out))
    is_failure (_, chunk) = any Midi.Cache.is_splice_failure
        (LEvent.logs_of (Midi.Cache.chunk_messages chunk))

-- | Rederive and check correctness.
rederive_check = undefined

now = fmap Time.utctDayTime Time.getCurrentTime
