-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Debugging utilities.
module Cmd.Repl.LDebug where
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified GHC.Stats
import qualified System.IO as IO
import qualified System.Mem

import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.TextUtil as TextUtil

import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LPerf as LPerf
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Types as Types
import Global
import Types


memory :: Trans.MonadIO m => m Int
memory = liftIO $ do
    System.Mem.performMajorGC
    stats <- GHC.Stats.getGCStats
    return $ fromIntegral $ GHC.Stats.currentBytesUsed stats `div` 1024

stats :: Trans.MonadIO m => m GHC.Stats.GCStats
stats = liftIO $ do
    System.Mem.performMajorGC
    GHC.Stats.getGCStats

-- Also see 'LPerf.extract_debug' and similar functions.

-- * block

-- | Save state in a format that can be copy-pasted into a test, and loaded
-- with 'Derive.DeriveTest.derive_dump'.
dump_blocks :: FilePath -> Cmd.CmdL ()
dump_blocks fname = do
    state <- Simple.dump_state
    liftIO $ write_dump fname state

-- | Like 'dump_blocks', but only dump a single block.
dump_block :: FilePath -> BlockId -> Cmd.CmdL ()
dump_block fname block_id = do
    state <- Ui.get
    block <- Ui.get_block block_id
    state <- Ui.eval_rethrow "dump_block"
        (state { Ui.state_blocks = Map.singleton block_id block })
        Simple.dump_state
    liftIO $ write_dump fname state

write_dump :: FilePath -> Simple.State -> IO ()
write_dump fname = writeFile fname . PPrint.pshow

-- * track

-- | Check 'Events.Events'.
check_events_invariants :: Ui.M m => m [(TrackId, Text)]
check_events_invariants = do
    tracks <- Ui.gets $ Map.toAscList . Ui.state_tracks
    return
        [ (track_id, err)
        | (track_id, track) <- tracks
        , err <- Events.check_invariants (Track.track_events track)
        ]

-- * perf events

dump_block_perf_events :: FilePath -> BlockId -> Cmd.CmdL ()
dump_block_perf_events fname block_id = do
    events <- LPerf.convert . LEvent.events_of =<< LPerf.block_events block_id
    dump_perf_events fname (LEvent.events_of events)

dump_perf_events :: FilePath -> [Types.Event] -> Cmd.CmdL ()
dump_perf_events fname events = liftIO $ IO.writeFile fname $
    PPrint.pshow (map Simple.dump_exact_perf_event events)

-- * undo

show_history :: Cmd.CmdL Text
show_history = do
    save_file <- ("save file: "<>) . showt <$> Cmd.gets Cmd.state_save_file
    hist <- Pretty.formatted <$> Cmd.gets Cmd.state_history
    config <- Pretty.formatted <$> Cmd.gets Cmd.state_history_config
    collect <- Pretty.formatted <$> Cmd.gets Cmd.state_history_collect
    return $ Text.unlines $ map Text.strip [save_file, hist, config, collect]

-- * extract from Performance

track_signal :: Cmd.CmdL (Maybe Track.TrackSignal)
track_signal = do
    (block_id, _, track_id, _) <- Selection.get_insert
    perf <- LPerf.get block_id
    return $ Map.lookup (block_id, track_id) (Cmd.perf_track_signals perf)

-- | Get collected warps of a specific track.
get_warps :: Cmd.M m => BlockId -> TrackId -> m [TrackWarp.TrackWarp]
get_warps block_id track_id = do
    perf <- LPerf.get_root
    let track_warps = Cmd.perf_warps perf
        warps = [tw | tw <- track_warps, TrackWarp.tw_block tw == block_id,
            Set.member track_id (TrackWarp.tw_tracks tw)]
    return $ map (\w -> w { TrackWarp.tw_warp = Score.id_warp }) warps

-- | Get all raw uncollected TrackWarps from the root, and strip out the
-- signals so they don't take up tons of space.
get_track_warps :: Cmd.M m => m (Map Stack.Stack TrackWarp.Track)
get_track_warps = do
    result <- LPerf.derive =<< Ui.get_root_id
    let wmap = Derive.collect_warp_map $ Derive.state_collect $
            Derive.r_state result
    let strip (TrackWarp.Track s e _warp bid tid) =
            TrackWarp.Track s e Score.id_warp bid tid
    return $ strip <$> wmap

-- ** cache

-- | Extract the cache logs, with no summarizing.
cache_logs :: BlockId -> Cmd.CmdL Text
cache_logs block_id = do
    perf <- Cmd.get_performance block_id
    let logs = filter wanted $ Cmd.perf_logs perf
    return $ Text.unlines
        [ TextUtil.joinWith ": " (format_stack msg) (Log.msg_text msg)
        | msg <- logs
        ]
    where
    wanted log = Cache.is_cache_log log || PlayUtil.is_score_damage_log log
    format_stack = maybe "" Stack.pretty_ui_ . Log.msg_stack

-- | Stats for both block and track caches from the given block.
cache_stats :: BlockId -> Cmd.CmdL String
cache_stats block_id = do
    block <- block_cache block_id
    track <- track_cache block_id
    return $ unlines
        ["block:", format_stats block, "track:", format_stats track]

-- | Get summarized stats for cached blocks.
block_cache :: BlockId -> Cmd.CmdL ([(Text, [BlockId])], [(BlockId, Int)])
block_cache block_id =
    Play.extract_cache_stats Play.get_block_id . Cmd.perf_logs <$>
        Cmd.get_performance block_id

-- | Get summarized stats for cached tracks on the given block.
track_cache :: BlockId -> Cmd.CmdL ([(Text, [TrackId])], [(TrackId, Int)])
track_cache block_id = do
    (rederived, cached) <- (rederived_block *** cached_block)
        . Play.extract_cache_stats Play.get_track_id . Cmd.perf_logs
        <$> Cmd.get_performance block_id
    return (rederived, cached)
    where
    rederived_block = map (second $ map snd . filter ((==block_id) . fst))
    cached_block = map (first snd) . filter ((==block_id) . fst . fst)

format_stats :: Id.Ident id => ([(Text, [id])], [(id, Int)]) -> String
format_stats (rederived, cached) =
    "cached: " <> format_cached cached <> "\n"
        <> unlines (map format_rederived rederived)
    where
    format_rederived (because, ids) =
        untxt because <> ": [" <> show (length ids) <> "] "
        <> unwords (map (untxt . Id.ident_name) ids)
    format_cached cached =
        show (length cached) <> " [" <> show (sum (map snd cached)) <> "] "
        <> unwords (map (untxt . Id.ident_name . fst) cached)
