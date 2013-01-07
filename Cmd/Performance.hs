-- | This module manages the performance of music, specifically the creation
-- of performance threads.
--
-- Performance is relative to a toplevel block, so each block has its own set
-- of caches.  Since performance is lazy, a separate thread will force it as
-- needed:  enough so that playing will probably be lag free, but not so much
-- to do too much unnecessary work (specifically, stressing the GC leads to
-- UI latency).
module Cmd.Performance (SendStatus, update_performance, performance) where
import qualified Control.Concurrent as Concurrent
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception

import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.TrackWarp as TrackWarp
import qualified Perform.RealTime as RealTime
import qualified App.Config as Config
import Types


type SendStatus = BlockId -> Msg.DeriveStatus -> IO ()

-- | Update 'Cmd.state_performance_threads'.  This means applying any new
-- ScoreDamage to them, halting performance of blocks which have changed, and
-- starting a performance for the focused block.
--
-- The majority of the calls here will bring neither score damage nor
-- a changed view id, and thus this will do nothing.
update_performance :: SendStatus -> State.State -> State.State -> Cmd.State
    -> [Update.UiUpdate] -> IO Cmd.State
update_performance send_status ui_pre ui_to cmd_state updates = do
    (cmd_state, _, logs, result) <- Cmd.run_io ui_to cmd_state $ do
        let damage = Diff.derive_diff ui_pre ui_to updates
        when (damage /= mempty) $ do
            kill_threads
            insert_score_damage damage
        focused <- Cmd.lookup_focused_block
        when_just focused $ \block_id ->
            regenerate_performance (derive_wait cmd_state block_id)
                send_status block_id
        when_just (State.config_root (State.state_config ui_to)) $ \block_id ->
            regenerate_performance (derive_wait cmd_state block_id)
                send_status block_id
        return Cmd.Done
    mapM_ Log.write logs
    case result of
        Left err -> Log.error $ "ui error deriving: " ++ show err
        _ -> return ()
    return cmd_state

derive_wait :: Cmd.State -> BlockId -> Thread.Seconds
derive_wait cmd_state block_id
    | block_id `Set.member` Cmd.state_derive_immediately cmd_state = 0
    | otherwise = Config.default_derive_wait

-- | Kill all performance threads and clear them out.  This will ensure that
-- 'needs_regenerate' will regenerate them if needed.
--
-- I do this if there is any damage at all.  It could be that the damage
-- doesn't affect to a particular block, but it's the job of the cache to
-- figure out what needs to be regenerated based on block dependencies.
kill_threads :: Cmd.CmdT IO ()
kill_threads = do
    threads <- Cmd.gets $
        Map.elems . Cmd.state_performance_threads . Cmd.state_play
    liftIO $ mapM_ Concurrent.killThread threads
    Cmd.modify_play_state $ \st ->
        st { Cmd.state_performance_threads = mempty }

-- | Since caches are stored per-block, score damage is also per-block.
-- It accumulates in an existing performance and is cleared when a new
-- performance is created from the old one.
insert_score_damage :: Derive.ScoreDamage -> Cmd.CmdT IO ()
insert_score_damage damage = Cmd.modify_play_state $ \st ->
    st { Cmd.state_performance = Map.map update (Cmd.state_performance st) }
    where
    update perf =
        perf { Cmd.perf_score_damage = damage <> Cmd.perf_score_damage perf }


-- * performance evaluation

-- | A block should be rederived only if the it doesn't already have
-- a performance or its performance has damage.
--
-- It could be this damage is on a parent block and hence not relevant to this
-- one.  But in that case it should hit the caches and be cheap all the same.
regenerate_performance :: Thread.Seconds -> SendStatus -> BlockId
    -> Cmd.CmdT IO ()
regenerate_performance wait send_status block_id =
    whenM (needs_regenerate block_id) $
        generate_performance wait send_status block_id

needs_regenerate :: (Cmd.M m) => BlockId -> m Bool
needs_regenerate block_id = do
    perf <- Cmd.lookup_performance block_id
    th <- lookup_thread block_id
    return $ case (perf, th) of
        (Nothing, Nothing) -> True
        -- Regen if there's damage, but only if there isn't already a thread.
        -- Otherwise it's an endless loop and the thread never has a chance
        -- to fill the performance.
        (Just perf, Nothing) -> Cmd.perf_score_damage perf /= mempty
        _ -> False

-- | Start a new performance thread.
--
-- Pull previous caches from the existing performance, if any.  Use them to
-- generate a new performance, kick off a thread for it, and insert the new
-- thread into 'Cmd.state_performance_threads'.
generate_performance :: Thread.Seconds -> SendStatus -> BlockId
    -> Cmd.CmdT IO ()
generate_performance wait send_status block_id = do
    old_thread <- lookup_thread block_id
    when_just old_thread (liftIO . Concurrent.killThread)
    ui_state <- State.get
    cmd_state <- Cmd.get
    th <- liftIO $ Thread.start $
        performance_thread ui_state cmd_state wait send_status block_id
            `Exception.onException` send_status block_id Msg.Killed
    Cmd.modify_play_state $ \st ->
        st { Cmd.state_performance_threads = Map.insert block_id
            th (Cmd.state_performance_threads st) }

lookup_thread :: (Cmd.M m) => BlockId -> m (Maybe Concurrent.ThreadId)
lookup_thread block_id = Map.lookup block_id <$>
    Cmd.gets (Cmd.state_performance_threads . Cmd.state_play)

performance_thread :: State.State -> Cmd.State
    -> Thread.Seconds -> SendStatus -> BlockId -> IO ()
performance_thread ui_state cmd_state wait send_status block_id = do
    mapM_ Log.write logs
    case res of
        -- These errors indicate not that derivation failed, but that the cmd
        -- threw before it even got started, which should never happen.
        Left err -> Log.error $
            "derivation for " ++ show block_id ++ " failed: " ++ show err
        Right  (Nothing, _, _) ->
            Log.error $ "derivation for " ++ show block_id ++ " aborted"
        Right  (Just derive_result, _, _) ->
            evaluate_performance wait send_status block_id
                (performance derive_result)
    where
    (_, _, logs, res) = Cmd.run_id ui_state cmd_state $
        PlayUtil.cached_derive block_id

evaluate_performance :: Thread.Seconds -> SendStatus -> BlockId
    -> Cmd.Performance -> IO ()
evaluate_performance wait send_status block_id perf = do
    send_status block_id (Msg.OutOfDate perf)
    Thread.delay wait
    send_status block_id Msg.Deriving
    secs <- Log.time_eval (DeepSeq.rnf perf)
    when (secs > 1) $
        Log.notice $ "derived " ++ show block_id ++ " in "
            ++ Pretty.pretty (RealTime.seconds secs)
    send_status block_id $ Msg.DeriveComplete perf

-- | Constructor for 'Cmd.Performance'.
performance :: Derive.Result -> Cmd.Performance
performance result = Cmd.Performance
    { Cmd.perf_derive_cache = Derive.r_cache result
    , Cmd.perf_events = Derive.r_events result
    , Cmd.perf_track_dynamic = Derive.r_track_dynamic result
    , Cmd.perf_integrated = Derive.r_integrated result
    , Cmd.perf_score_damage = mempty
    , Cmd.perf_warps = TrackWarp.collections $ Derive.collect_warp_map $
        Derive.state_collect $ Derive.r_state result
    , Cmd.perf_track_signals = Derive.r_track_signals result
    }
