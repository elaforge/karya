-- | This module manages the performance of music, specifically the creation
-- of 'Cmd.Cmd.PerformanceThread's.
--
-- Performance is relative to a toplevel block, so each block has its own set
-- of caches.  Since performance is lazy, a separate thread will force it as
-- needed:  enough so that playing will probably be lag free, but not so much
-- to do too much unnecessary work (specifically, stressing the GC leads to
-- UI latency).
module Cmd.Performance (SendStatus, update_performance) where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import Ui
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Derive.Cache as Derive.Cache
import qualified Derive.Derive as Derive
import qualified Perform.RealTime as RealTime


-- | The background derive threads will wait this many seconds before starting
-- up, to avoid working too hard during an edit.
derive_wait :: Double
derive_wait = 1

type SendStatus = BlockId -> Msg.DeriveStatus -> IO ()

type PerfInfo = (SendStatus, Maybe BlockId, Maybe Selection.Point)

-- | Update 'Cmd.state_performance_threads'.  This means applying any new
-- ScoreDamage to them, halting performance of blocks which have changed, and
-- starting a performance for the focused block.
--
-- The majority of the calls here will bring neither score damage nor a changed
-- view id, and thus this will do nothing.
update_performance :: SendStatus -> State.State -> State.State -> Cmd.State
    -> [Update.Update] -> IO Cmd.State
update_performance send_status ui_from ui_to cmd_state updates = do
    (cmd_state, _, logs, result) <- Cmd.run_io ui_to cmd_state $ do
        let damage = Derive.Cache.score_damage ui_from ui_to updates
        kill_obsolete_threads damage
        insert_score_damage damage
        sel <- Selection.lookup_insert
        focused <- Cmd.lookup_focused_block
        let perf_info = (send_status, State.state_root ui_to, sel)
        when_just focused (regenerate_performance perf_info)
        when_just (State.state_root ui_to) (regenerate_performance perf_info)
        when_just sel (update_selection_pos (State.state_root ui_to))
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
            Seq.map_maybe (flip Map.lookup threads) block_ids
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

regenerate_performance :: PerfInfo -> BlockId -> Cmd.CmdT IO ()
regenerate_performance perf_info block_id = do
    threads <- Cmd.gets Cmd.state_performance_threads
    if needs_regeneration threads block_id
        then generate_performance perf_info block_id
        else return ()

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
generate_performance :: PerfInfo -> BlockId -> Cmd.CmdT IO ()
generate_performance (send_status, root_id, sel) block_id = do
    old_thread <- fmap Cmd.pthread_id <$> Cmd.lookup_pthread block_id
    when_just old_thread (Trans.liftIO . Concurrent.killThread)
    derived <- (Just <$> PlayUtil.cached_derive block_id)
        `Error.catchError` \err -> do
            when (Cmd.is_abort err) $
                Log.warn $ "Error performing: " ++ show err
            return Nothing
    case PlayUtil.performance <$> derived of
        Nothing -> Trans.liftIO $ send_status block_id Msg.DeriveFailed
        Just perf -> do
            root_sel <- maybe (return Nothing) (Selection.lookup_block_insert)
                root_id
            let cur = maybe 0
                    (Selection.relative_realtime_point perf root_sel) sel
            selection_pos <- Trans.liftIO $ IORef.newIORef cur
            th <- Trans.liftIO $ Thread.start $
                evaluate_performance send_status block_id perf
            let pthread = Cmd.PerformanceThread perf th selection_pos
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


-- * selection

update_selection_pos :: Maybe BlockId -> Selection.Point -> Cmd.CmdT IO ()
update_selection_pos root_id focused_sel = do
    -- for the selection in the focused block, find out what RealTime that
    -- means to each block_id in the performances, and send that
    threads <- Cmd.gets Cmd.state_performance_threads
    forM_ (Map.elems threads) $ \pthread -> do
        let perf = Cmd.pthread_perf pthread
        root_sel <- maybe (return Nothing) (Selection.lookup_block_insert)
            root_id
        let pos = Selection.relative_realtime_point perf root_sel focused_sel
        Trans.liftIO $ Cmd.write_selection pos (Cmd.pthread_selection pthread)
