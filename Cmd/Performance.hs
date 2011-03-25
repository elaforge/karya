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
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Text.Printf (printf)

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Derive.Cache as Derive.Cache
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Stack as Stack
import qualified Perform.Midi.Cache as Midi.Cache


-- | The background derive threads will wait this many seconds before starting
-- up, to avoid working too hard during an edit.
derive_wait_focused :: Double
derive_wait_focused = 1

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
    maybe_perf <- (Just <$>
        (PlayUtil.cached_perform block_id =<< PlayUtil.cached_derive block_id))
        `Error.catchError` \err -> do
            when (Cmd.is_abort err) $
                Log.warn $ "Error performing: " ++ show err
            return Nothing
    case maybe_perf of
        Nothing -> Trans.liftIO $ send_status block_id Msg.DeriveFailed
        Just perf -> do
            root_sel <- maybe (return Nothing) (Selection.lookup_block_insert)
                root_id
            let cur = maybe 0
                    (Selection.relative_realtime_point perf root_sel) sel
            selection_pos <- Trans.liftIO $ IORef.newIORef cur
            th <- Trans.liftIO $ do
                Log.notice_stack (Stack.block block_id) "start deriving"
                Thread.start $ evaluate_performance send_status selection_pos
                    block_id perf
            let pthread = Cmd.PerformanceThread perf th selection_pos
            Cmd.modify_state $ \st -> st { Cmd.state_performance_threads =
                Map.insert block_id pthread (Cmd.state_performance_threads st) }

evaluate_performance :: SendStatus -> Cmd.SelectionPosition -> BlockId
    -> Cmd.Performance -> IO ()
evaluate_performance send_status selection_pos block_id perf = do
    send_status block_id Msg.Deriving
    Thread.delay derive_wait_focused
    send_status block_id Msg.StartedDeriving
    let cache = Cmd.perf_midi_cache perf
    evaluate_midi block_id cache selection_pos False 0
        (zip [0..] (Midi.Cache.cache_chunks cache))
    send_status block_id (Msg.DeriveComplete (Cmd.perf_track_signals perf))

-- | Evaluate all chunks before 'eval_until'.  If I hit a splice failed msg,
-- log the overall cache stats.  Otherwise, cache stats can only be logged
-- once I'm out of chunks.
evaluate_midi :: BlockId -> Midi.Cache.Cache -> Cmd.SelectionPosition -> Bool
    -> RealTime -> [(Int, Midi.Cache.Chunk)] -> IO ()
evaluate_midi block_id cache _ logged_stats _ [] =
    when (not logged_stats) $ log_stats block_id Nothing cache
evaluate_midi block_id cache selection_pos logged_stats eval_pos chunks = do
    pos <- Cmd.read_selection selection_pos
    let eval_until = max pos eval_pos
    -- when (pos > eval_pos) $
    --     Log.notice $ block_id ++ "jumping forward to cursor: " ++ show pos
    let (pre, post) = break ((>=eval_until) . chunk_time . snd) chunks
    mapM_ evaluate_chunk (map snd pre)
    let splice_failed = fst <$>
            List.find (Midi.Cache.chunk_splice_failed . snd) pre
    when (not logged_stats && Maybe.isJust splice_failed) $
        log_stats block_id splice_failed cache
    -- Log.notice $ block_id ++ "eval until " ++ Pretty.pretty eval_pos
    -- The delay here should be long enough to go easy on the GC but short
    -- enough to have a good chance of having already evaluated the output
    -- before the user hits "play".
    -- TODO skip the delay for cached chunks
    when (not (null post)) $
        Thread.delay 0.5
    evaluate_midi block_id cache selection_pos
        (logged_stats || Maybe.isJust splice_failed)
        (eval_until + Midi.Cache.cache_chunk_size) post

log_stats :: BlockId -> Maybe Midi.Cache.ChunkNum -> Midi.Cache.Cache -> IO ()
log_stats block_id splice_failed_at cache = do
    Log.notice_stack (Stack.block block_id) $ printf
        "midi reperformed: %s, total %s, cache damage: %s"
        (Pretty.pretty reperformed) (Pretty.pretty total)
        (Pretty.pretty (Midi.Cache.cache_damage cache))
    where (reperformed, total) = Midi.Cache.cache_stats splice_failed_at cache

chunk_time :: Midi.Cache.Chunk -> RealTime
chunk_time chunk = case LEvent.events_of (Midi.Cache.chunk_messages chunk) of
    [] -> 0
    wmsg : _ -> Midi.wmsg_ts wmsg

evaluate_chunk :: Midi.Cache.Chunk -> IO ()
evaluate_chunk chunk = mapM_ eval (Midi.Cache.chunk_messages chunk)
    where
    eval (LEvent.Log log) = Log.write log
    -- Midi.WriteMessage is strict, so deepseq is unnecessary.
    eval (LEvent.Event midi) = midi `seq` return ()

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
