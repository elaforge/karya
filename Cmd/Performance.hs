-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This module manages the performance of music, specifically the creation
-- of performance threads.
--
-- Performance is relative to a toplevel block, so each block has its own set
-- of caches.  Since performance is lazy, a separate thread will force it
-- asynchronously.
module Cmd.Performance (SendStatus, update_performance, performance) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.State as Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.RealTime as RealTime
import qualified App.Config as Config
import Types


type SendStatus = BlockId -> Msg.DeriveStatus -> IO ()
type StateM = Monad.State.StateT Cmd.State IO ()

-- | Update the performances by rederiving if necessary.  This means figuring
-- out ScoreDamage, and if there has been damage, killing any in-progress
-- derivation and starting derivation for the focused and root blocks.
--
-- The majority of the calls here will bring neither score damage nor
-- a changed view id, and thus this will do nothing.
--
-- This is tricky, and I've gotten it wrong in the past, so here's a detailed
-- description:
--
-- Merge ui damage with each perf's damage.  Then for each perf, if it's
-- 'Cmd.state_current_performance' has damage, kill its thread, and remove its
-- entry in 'Cmd.state_performance_threads'.  The lack of a thread entry,
-- whether because was removed or never existed, means that a block should be
-- rederived.  Derivation creates a new 'Cmd.Performance' and an evaluate
-- thread, and puts them into 'Cmd.state_current_performance' and
-- 'Cmd.state_performance_threads' respectively, but due to laziness, no actual
-- derivation happens unless someone (like play) happens to look at the
-- performance.  This all happens synchronously, so the next time
-- 'update_performance' is called, it sees a nice clean new Performance with
-- no damage.
--
-- Meanwhile, the evaluate thread asynchronously waits for a bit, then
-- forces the contents of the Performance, and then sends it back to the
-- responder so it can stash it in 'Cmd.state_performance'.  If a new change
-- comes in while it's waiting it'll get killed off, and the out-of-date
-- derivation will never happen.  Yay for laziness!
update_performance :: SendStatus -> State.State -> Cmd.State
    -> Derive.ScoreDamage -> IO Cmd.State
update_performance send_status ui_state cmd_state damage =
    -- The update will be modifying Cmd.State, especially PlayState.
    Monad.State.execStateT (run_update send_status ui_state)
        (insert_damage damage cmd_state)

run_update :: SendStatus -> State.State -> StateM
run_update send_status ui_state = do
    kill_threads
    let visible = map Block.view_block $ Map.elems $ State.state_views $
            ui_state
        root_id = State.config_root (State.state_config ui_state)
        block_ids = Seq.unique $ maybe id (:) root_id visible
    mapM_ (try_generate_performance send_status ui_state) block_ids

try_generate_performance :: SendStatus -> State.State -> BlockId -> StateM
try_generate_performance send_status ui_state block_id = do
    state <- Monad.State.get
    when (needs_generate state block_id) $
        generate_performance ui_state (derive_wait state block_id)
            send_status block_id

-- | Theoretically I should be able to do away with the wait, but in practice
-- deriving constantly causes UI latency.
derive_wait :: Cmd.State -> BlockId -> Thread.Seconds
derive_wait cmd_state block_id
    | block_id `Set.member` Cmd.state_derive_immediately cmd_state = 0
    | otherwise = Config.default_derive_wait

-- | Since caches are stored per-performance, score damage is also
-- per-performance.  It accumulates in an existing performance and is cleared
-- when a new performance is created from the old one.
insert_damage :: Derive.ScoreDamage -> Cmd.State -> Cmd.State
insert_damage damage = modify_play_state $ \st -> st
    { Cmd.state_current_performance =
        Map.map update (Cmd.state_current_performance st)
    }
    where
    update perf = perf { Cmd.perf_damage = damage <> Cmd.perf_damage perf }


-- | Kill all performance threads with damage.  If they are still deriving
-- they're now out of date and should stop.
kill_threads :: StateM
kill_threads = do
    play_state <- Monad.State.gets Cmd.state_play
    let threads = Cmd.state_performance_threads play_state
        perfs = Cmd.state_current_performance play_state
        with_damage =
            [ block_id | (block_id, perf) <- Map.toList perfs
            , Cmd.perf_damage perf /= mempty
            ]
        kill = filter ((`elem` with_damage) . fst) $ Map.toList threads
    liftIO $ mapM_ Concurrent.killThread (map snd kill)
    Monad.State.modify $ modify_play_state $ const $ play_state
        { Cmd.state_performance_threads = Map.delete_keys (map fst kill)
            (Cmd.state_performance_threads play_state)
        }


-- * performance evaluation

-- | True if this BlockId should be regenerated.  This happens if there is any
-- damage, or if there is no existing performance.  I use
-- 'Cmd.state_performance_threads' and not 'Cmd.state_current_performance',
-- because the thread is filled in synchronously, while the current performance
-- is filled in later.
needs_generate :: Cmd.State -> BlockId -> Bool
needs_generate state block_id = not (Map.member block_id perfs)
    where perfs = Cmd.state_performance_threads $ Cmd.state_play state

-- | Start a new performance thread.
--
-- Pull previous caches from the existing performance, if any.  Use them to
-- generate a new performance, kick off a thread for it, and insert the new
-- thread into 'Cmd.state_performance_threads' and performance into
-- 'Cmd.state_current_performance'.
generate_performance :: State.State -> Thread.Seconds -> SendStatus -> BlockId
    -> StateM
generate_performance ui_state wait send_status block_id = do
    cmd_state <- Monad.State.get
    let (perf, logs) = derive ui_state cmd_state block_id
    mapM_ Log.write logs
    th <- liftIO $ Thread.start $
        evaluate_performance wait send_status block_id perf
    Monad.State.modify $ modify_play_state $ \st -> st
        { Cmd.state_performance_threads = Map.insert block_id
            th (Cmd.state_performance_threads st)
        , Cmd.state_current_performance = Map.insert block_id
            perf (Cmd.state_current_performance st)
        }
    -- If the derivation somehow failed, then the old performance will remain,
    -- and since there is no thread, this will try again the next time around.

derive :: State.State -> Cmd.State -> BlockId -> (Cmd.Performance, [Log.Msg])
derive ui_state cmd_state block_id = (perf, logs)
    where
    perf = case cmd_result of
        Left err -> broken_performance $
            "derivation for " <> showt block_id <> " failed: " <> prettyt err
        Right (derive_result, _, _) -> case derive_result of
            Nothing -> broken_performance $
                "derivation for " <> showt block_id <> " aborted"
            Just result -> performance result
    prev_perf = Map.lookup block_id $ Cmd.state_current_performance $
        Cmd.state_play cmd_state
    (_state, _midi, logs, cmd_result) = Cmd.run_id ui_state cmd_state $
        case prev_perf of
            Nothing -> PlayUtil.derive_block mempty mempty block_id
            Just perf -> PlayUtil.derive_block (Cmd.perf_derive_cache perf)
                (Cmd.perf_damage perf) block_id

evaluate_performance :: Thread.Seconds -> SendStatus -> BlockId
    -> Cmd.Performance -> IO ()
evaluate_performance wait send_status block_id perf = do
    send_status block_id Msg.OutOfDate
    Thread.delay wait
    send_status block_id Msg.Deriving
    -- I just force the logs here, and wait for a play to actually write them.
    ((), secs) <- Log.time_eval $ return $ Msg.force_performance perf
    when (secs > 1) $
        Log.notice $ "derived " ++ show block_id ++ " in "
            ++ pretty (RealTime.seconds secs)
    send_status block_id $ Msg.DeriveComplete perf

-- | Make a broken performance with just an error msg.  This ensures that
-- the msg is logged when you try to play, but will still suppress further
-- performance, so you don't get a million msgs.
broken_performance :: Text -> Cmd.Performance
broken_performance msg = Cmd.Performance
    { Cmd.perf_derive_cache = mempty
    , Cmd.perf_events = mempty
    , Cmd.perf_logs = [Log.msg Log.Warn Nothing msg]
    , Cmd.perf_logs_written = False
    , Cmd.perf_track_dynamic = mempty
    , Cmd.perf_integrated = mempty
    , Cmd.perf_damage = mempty
    , Cmd.perf_warps = mempty
    , Cmd.perf_track_signals = mempty
    }

-- | Constructor for 'Cmd.Performance'.
performance :: Derive.Result -> Cmd.Performance
performance result = Cmd.Performance
    { Cmd.perf_derive_cache = Derive.r_cache result
    , Cmd.perf_events = Vector.fromList events
    , Cmd.perf_logs = logs
    , Cmd.perf_logs_written = False
    , Cmd.perf_track_dynamic = Derive.r_track_dynamic result
    , Cmd.perf_integrated = Derive.r_integrated result
    , Cmd.perf_damage = mempty
    , Cmd.perf_warps = TrackWarp.collections $ Derive.collect_warp_map $
        Derive.state_collect $ Derive.r_state result
    , Cmd.perf_track_signals = Derive.r_track_signals result
    }
    where (events, logs) = LEvent.partition (Derive.r_events result)

modify_play_state :: (Cmd.PlayState -> Cmd.PlayState) -> Cmd.State -> Cmd.State
modify_play_state modify state =
    state { Cmd.state_play = modify (Cmd.state_play state) }
