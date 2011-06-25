-- | This module manages the performance of music, specifically the creation
-- of 'Cmd.Cmd.PerformanceThread's.
--
-- Performance is relative to a toplevel block, so each block has its own set
-- of caches.  Since performance is lazy, a separate thread will force it as
-- needed:  enough so that playing will probably be lag free, but not so much
-- to do too much unnecessary work (specifically, stressing the GC leads to
-- UI latency).
module Cmd.Performance (SendStatus, update_performance, performance) where
import qualified Control.Concurrent as Concurrent
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import Ui
import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Perform.RealTime as RealTime


-- | The background derive threads will wait this many seconds before starting
-- up, to avoid working too hard during an edit.
derive_wait :: Double
derive_wait = 1

type SendStatus = BlockId -> Msg.DeriveStatus -> IO ()

-- | Update 'Cmd.state_performance_threads'.  This means applying any new
-- ScoreDamage to them, halting performance of blocks which have changed, and
-- starting a performance for the focused block.
--
-- The majority of the calls here will bring neither score damage nor
-- a changed view id, and thus this will do nothing.
update_performance :: SendStatus -> State.State -> State.State -> Cmd.State
    -> [Update.Update] -> IO Cmd.State
update_performance send_status ui_pre ui_to cmd_state updates = do
    (cmd_state, _, logs, result) <- Cmd.run_io ui_to cmd_state $ do
        let damage = Diff.derive_diff ui_pre ui_to updates
        kill_obsolete_threads damage
        insert_score_damage damage
        focused <- Cmd.lookup_focused_block
        when_just focused (regenerate_performance send_status)
        when_just (State.state_root ui_to) (regenerate_performance send_status)
        return Cmd.Done
    mapM_ Log.write logs
    case result of
        Left err -> Log.error $ "ui error deriving: " ++ show err
        _ -> return ()
    return cmd_state

-- | This doesn't remove the PerformanceThreads because their caches are still
-- needed for the next performance.
kill_obsolete_threads :: Derive.ScoreDamage -> Cmd.CmdT IO ()
kill_obsolete_threads (Derive.ScoreDamage _ track_blocks blocks) = do
    threads <- Cmd.gets Cmd.state_performance_threads
    let block_ids = Set.toList (Set.union track_blocks blocks)
    let thread_ids = map Cmd.pthread_id $
            Maybe.mapMaybe (flip Map.lookup threads) block_ids
    Trans.liftIO $ mapM_ Concurrent.killThread thread_ids

-- | Since caches are stored per-block, score damage is also per-block.
-- It accumulates in an existing performance and is cleared when a new
-- performance is created from the old one.
insert_score_damage :: Derive.ScoreDamage -> Cmd.CmdT IO ()
insert_score_damage damage = Cmd.modify_state $ \st ->
    st { Cmd.state_performance_threads =
        Map.map update_pthread (Cmd.state_performance_threads st) }
    where
    update_pthread th = th { Cmd.pthread_perf = update (Cmd.pthread_perf th) }
    update perf =
        perf { Cmd.perf_score_damage = damage <> Cmd.perf_score_damage perf }


-- * performance evaluation

regenerate_performance :: SendStatus -> BlockId -> Cmd.CmdT IO ()
regenerate_performance send_status block_id = do
    threads <- Cmd.gets Cmd.state_performance_threads
    when (needs_regeneration threads block_id) $
        generate_performance send_status block_id

-- | A block should be rederived only if the it doesn't already have
-- a performance or its performance has damage.
needs_regeneration :: Map.Map BlockId Cmd.PerformanceThread -> BlockId -> Bool
needs_regeneration threads block_id = case Map.lookup block_id threads of
    Nothing -> True
    Just pthread ->
        -- It could be this damage is on a parent block and hence not
        -- relevant to this one.  But in that case it should hit the caches
        -- and be cheap all the same.
        Cmd.perf_score_damage (Cmd.pthread_perf pthread) /= mempty

-- | Start a new performance thread.
--
-- Pull previous caches from the existing performance, if any.  Use them to
-- generate a new performance, kick off a thread for it, and insert the new
-- PerformanceThread.
generate_performance :: SendStatus -> BlockId -> Cmd.CmdT IO ()
generate_performance send_status block_id = do
    old_thread <- fmap Cmd.pthread_id <$> Cmd.lookup_pthread block_id
    when_just old_thread (Trans.liftIO . Concurrent.killThread)
    derived <- (Just <$> PlayUtil.cached_derive block_id)
        `Error.catchError` \err -> do
            when (Cmd.is_abort err) $
                Log.warn $ "Error performing: " ++ show err
            return Nothing
    case performance <$> derived of
        Nothing -> Trans.liftIO $ send_status block_id Msg.DeriveFailed
        Just perf -> do
            th <- Trans.liftIO $ Thread.start $
                evaluate_performance send_status block_id perf
            let pthread = Cmd.PerformanceThread perf th
            Cmd.modify_state $ \st -> st { Cmd.state_performance_threads =
                Map.insert block_id pthread (Cmd.state_performance_threads st) }

evaluate_performance :: SendStatus -> BlockId -> Cmd.Performance -> IO ()
evaluate_performance send_status block_id perf = do
    send_status block_id Msg.OutOfDate
    Thread.delay derive_wait
    send_status block_id Msg.Deriving
    secs <- Log.time_eval (Cmd.perf_inv_tempo perf 0)
    Log.notice $ show block_id ++ ": primed evaluation in "
        ++ Pretty.pretty (RealTime.seconds secs)
    send_status block_id (Msg.DeriveComplete (Cmd.perf_track_signals perf))

-- | Constructor for 'Cmd.Performance'.
performance :: Derive.Result -> Cmd.Performance
performance result = Cmd.Performance (Derive.r_cache result)
    (Derive.r_events result)
    (Derive.r_track_environ result) mempty (Derive.r_tempo result)
    (Derive.r_closest_warp result) (Derive.r_inv_tempo result)
    (Derive.r_track_signals result)
