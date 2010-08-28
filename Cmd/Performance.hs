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
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.Cache as Derive.Cache

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Transport as Transport
import qualified Perform.Midi.Cache as Midi.Cache
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.Db

import qualified App.Config as Config


-- | The background derive threads will wait this many seconds before starting
-- up, to avoid working too hard during an edit.
derive_wait_focused :: Double
derive_wait_focused = 1

type SendStatus = BlockId -> Msg.DeriveStatus -> IO ()

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
        regenerate_performance send_status =<< Cmd.lookup_focused_block
        update_selection_pos
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
    update perf = perf { Cmd.perf_score_damage =
        Monoid.mappend damage (Cmd.perf_score_damage perf) }

regenerate_performance :: SendStatus -> Maybe BlockId -> Cmd.CmdT IO ()
regenerate_performance _ Nothing = return ()
regenerate_performance send_status (Just block_id) = do
    threads <- Cmd.gets Cmd.state_performance_threads
    if needs_regeneration threads block_id
        then generate_performance send_status block_id
        else return ()

-- | A block should be rederived only if the it doesn't already have
-- a performance or its performance has damage.
needs_regeneration :: Map.Map BlockId Cmd.PerformanceThread -> BlockId -> Bool
needs_regeneration threads block_id = case Map.lookup block_id threads of
    Nothing -> True
    Just pthread ->
        Cmd.perf_score_damage (Cmd.pthread_perf pthread) /= Monoid.mempty

update_selection_pos :: Cmd.CmdT IO ()
update_selection_pos = update =<< current_selection
    where
    update Nothing = return ()
    update (Just (sel_block, sel_track, sel_pos)) = do
        -- for the selection in the focused block, find out what RealTime that
        -- means to each block_id in the performances, and send that
        threads <- Cmd.gets Cmd.state_performance_threads
        forM_ (Map.elems threads) $ \pthread -> do
            let perf = Cmd.pthread_perf pthread
            let pos = find_appropriate_time (Cmd.perf_tempo perf)
                    sel_block sel_track sel_pos
            Trans.liftIO $ Cmd.write_selection pos
                (Cmd.pthread_selection pthread)

-- | Start a new performance thread.
--
-- Pull previous caches from the existing performance, if any.  Use them to
-- generate a new performance, kick off a thread for it, and insert the new
-- PerformanceThread.
generate_performance :: SendStatus -> BlockId -> Cmd.CmdT IO ()
generate_performance send_status block_id = do
    cmd_state <- Cmd.get_state
    midi_config <- State.get_midi_config
    let (old_thread, derive_cache, midi_cache, damage) =
            performance_info (Cmd.state_instrument_db cmd_state)
                midi_config block_id cmd_state
    when_just old_thread (Trans.liftIO . Concurrent.killThread)

    -- Log.debug $ "re-performing midi with " ++ show damage
    maybe_perf <- (Just <$>
            Play.perform derive_cache midi_cache damage
                (Cmd.state_schema_map cmd_state) block_id)
        -- This is likely a Cmd.abort so the ultimate failure should have
        -- already been logged.
        `Error.catchError` \_ -> return Nothing
    case maybe_perf of
        Nothing -> Trans.liftIO $ send_status block_id Msg.DeriveFailed
        Just perf -> do
            sel <- current_selection
            let cur = case sel of
                    Nothing -> 0
                    Just (_, track_id, score_time) ->
                        find_appropriate_time (Cmd.perf_tempo perf) block_id
                            track_id score_time
            selection_pos <- Trans.liftIO $ IORef.newIORef cur
            th <- Trans.liftIO $
                Thread.start_thread ("derive " ++ show block_id)
                    (evaluate_performance send_status selection_pos block_id
                        perf)
            let pthread = Cmd.PerformanceThread perf th selection_pos
            Cmd.modify_state $ \st -> st { Cmd.state_performance_threads =
                Map.insert block_id pthread (Cmd.state_performance_threads st) }

-- | This is different than 'Cmd.Selection.get' because it doesn't fail
-- when there is no focused view.
current_selection :: (Monad m) =>
    Cmd.CmdT m (Maybe (BlockId, TrackId, ScoreTime))
current_selection =
    justm Cmd.lookup_focused_view $ \view_id ->
    justm (State.get_selection view_id Config.insert_selnum) $ \sel -> do
        block_id <- State.block_id_of_view view_id
        justm (State.event_track_at block_id (Types.sel_cur_track sel)) $
            \track_id -> return $
                Just (block_id, track_id,
                    max (Types.sel_cur_pos sel) (Types.sel_start_pos sel))

-- | This is sort of like a monad transformer, but the Maybe is on the inside
-- instead of the outside.
justm :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
justm op1 op2 = maybe (return Nothing) op2 =<< op1

-- | Transport.TempoFunction only works when called with the ScoreTime from the
-- root block.  But what I need is 'BlockId -> ScoreTime -> [RealTime]', since
-- if a block is called multiple times, a given score time may occur at
-- multiple real times.
--
-- So I need a way to figure which real time is appropriate.  I can look at
-- the selection position on the root block, and its RealTime (should only be
-- one), and pick the one closest to that.
--
-- So modify TempoFunction to return multiple RealTimes, and have another
-- function pick the closest one to the selection at the root block.
--
-- I think I also need this if I want to control a DAW position.  Also if I
-- want to play a block in the context of the root.  If the root is several
-- steps removed it might be not too easy to know, but I can deal with that
-- if it comes up.
--
-- So I need two play commands, but that waits until I have a root block.
find_appropriate_time :: Transport.TempoFunction -> BlockId
    -> TrackId -> ScoreTime -> RealTime
find_appropriate_time tempo block_id track_id pos =
    maybe 0 id (tempo block_id track_id pos)

-- | The tricky thing about the MIDI cache is that it depends on the inst
-- lookup function and inst config.  If either of those change, it has to be
-- cleared.
--
-- I can't compare functions so I just have to make sure to reinitialize the
-- MIDI cache when the function changes (which should be rare if ever).  The
-- instrument config /can/ be compared, so I just compare on play, and clear
-- the cache if it's changed.
performance_info :: Instrument.Db.Db -> Instrument.Config -> BlockId
    -> Cmd.State -> (Maybe Concurrent.ThreadId, Derive.Cache, Midi.Cache.Cache,
        Derive.ScoreDamage)
performance_info inst_db config block_id state = case maybe_pthread of
        Nothing -> (Nothing, Derive.empty_cache, empty_midi, Monoid.mempty)
        Just (Cmd.PerformanceThread perf th_id _) ->
            ( Just th_id
            , Cmd.perf_derive_cache perf
            , get_midi (Cmd.perf_midi_cache perf)
            , Cmd.perf_score_damage perf
            )
    where
    maybe_pthread = Map.lookup block_id (Cmd.state_performance_threads state)
    get_midi old
        | config == Midi.Cache.cache_config old = old
        | otherwise = empty_midi
    empty_midi = Midi.Cache.cache (Instrument.Db.db_lookup_midi inst_db) config

evaluate_performance :: SendStatus -> Cmd.SelectionPosition -> BlockId
    -> Cmd.Performance -> IO ()
evaluate_performance send_status selection_pos block_id perf = do
    send_status block_id Msg.Deriving
    Thread.delay derive_wait_focused
    send_status block_id Msg.StartedDeriving
    -- Force the performance to actually be evaluated.  Writing out the logs
    -- should do it.
    let prefix = Text.append (Text.pack ("deriving " ++ show block_id ++ ": "))
    let logs = map (\log -> log { Log.msg_text = prefix (Log.msg_text log) })
            (Cmd.perf_logs perf)
    mapM_ Log.write logs
    send_status block_id (Msg.DeriveComplete (Cmd.perf_track_signals perf))
    let cache = Cmd.perf_midi_cache perf
    evaluate_midi cache selection_pos False 0 (Midi.Cache.cache_chunks cache)

evaluate_midi :: Midi.Cache.Cache -> Cmd.SelectionPosition -> Bool -> RealTime
    -> Midi.Cache.Chunks -> IO ()
evaluate_midi cache _ logged_stats _ [] = when (not logged_stats) $
    log_stats Nothing cache
evaluate_midi cache selection_pos logged_stats eval_pos chunks = do
    pos <- Cmd.read_selection selection_pos
    let eval_until = max pos eval_pos
    when (pos > eval_pos) $
        Log.notice $ "jumping forward to cursor: " ++ show pos
    let (pre, post) = break ((>=eval_until) . chunk_time) chunks
    splice_failed <- any id <$> mapM evaluate_chunk pre
    when (not logged_stats && splice_failed) $
        log_stats (Just eval_until) cache
    -- Log.notice $ "eval until " ++ Pretty.pretty eval_pos
    -- The delay here should be long enough to go easy on the GC but short
    -- enough to have a good chance of having already evaluated the output
    -- before the user hits "play".
    -- TODO skip the delay for cached chunks
    when (not (null post)) $
        Thread.delay 0.5
    evaluate_midi cache selection_pos (logged_stats || splice_failed)
        (eval_until + Midi.Cache.cache_chunk_size) post

log_stats :: Maybe RealTime -> Midi.Cache.Cache -> IO ()
log_stats splice_failed cache =
    Log.notice $ "midi cached/reperformed: " ++ Pretty.pretty cached
        ++ "/" ++ Pretty.pretty reperformed
        ++ maybe " (splice succeeded)"
            (\p -> " (splice failed around " ++ Pretty.pretty p ++ ")")
            splice_failed
        ++ " chunk damage: " ++ Pretty.pretty (Midi.Cache.cache_damage cache)
    where
    (cached, reperformed) = Midi.Cache.cache_stats
        (Midi.Cache.to_chunknum <$> splice_failed) cache

chunk_time :: Midi.Cache.Chunk -> RealTime
chunk_time chunk = case Midi.Cache.chunk_messages chunk of
    [] -> 0
    wmsg : _ -> Timestamp.to_real_time (Midi.wmsg_ts wmsg)

evaluate_chunk :: Midi.Cache.Chunk -> IO Bool
evaluate_chunk chunk =
    fmap (any id) $ forM (Midi.Cache.chunk_warns chunk) $ \warn -> do
        let log = Play.warn_to_log "perform" warn
            splice = Midi.Cache.is_splice_failure warn
        -- A splice failure isn't really a warning.
        Log.write $ if splice then log { Log.msg_prio = Log.Notice } else log
        return splice
