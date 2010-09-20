-- | Cmds to deal with Cmd.Performance, derivation, and performance.
module Cmd.Lang.LPerf where
import qualified Data.Map as Map
import Util.Control
import qualified Util.Seq as Seq

import Ui

import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Perform as Midi.Perform
import qualified Perform.Midi.Cache as Midi.Cache


-- * performance

get_performance :: BlockId -> Cmd.CmdL Cmd.Performance
get_performance block_id = do
    threads <- Cmd.gets Cmd.state_performance_threads
    maybe (Cmd.throw $ "no performance for block " ++ show block_id)
        (return . Cmd.pthread_perf) (Map.lookup block_id threads)

get_midi_cache :: BlockId -> Cmd.CmdL Midi.Cache.Cache
get_midi_cache block_id =
    Cmd.perf_midi_cache <$> get_performance block_id

-- * derive

-- These are mostly for testing, to find problems in performer output.

-- | Clear out all caches and rederive from scratch for the given block.
rederive :: BlockId -> Cmd.CmdL ()
rederive = PlayUtil.clear_cache

-- | Compare performances with and without the midi cache.
compare_cached_midi :: BlockId
    -> Cmd.CmdL [Either Midi.WriteMessage Midi.WriteMessage]
compare_cached_midi block_id = do
    result <- PlayUtil.cached_derive block_id
    uncached <- PlayUtil.uncached_perform result
    cached <- PlayUtil.cached_perform block_id result
    return $ diff_events cached uncached
    where
    diff_events perf1 perf2 = Seq.diff (==) (msgs perf1) (msgs perf2)
    msgs = Midi.Cache.cache_messages . Cmd.perf_midi_cache

compare_cached_events :: BlockId
    -> Cmd.CmdL [Either Simple.ScoreEvent Simple.ScoreEvent]
compare_cached_events block_id = do
    uncached <- PlayUtil.uncached_derive block_id
    cached <- PlayUtil.cached_derive block_id
    either (Cmd.throw . show) return (diff_events cached uncached)
    where
    diff_events r1 r2 = do
        e1 <- Derive.r_result r1
        e2 <- Derive.r_result r2
        return $ Seq.diff (==)
            (map Simple.score_event e1) (map Simple.score_event e2)

derive_to_events :: BlockId -> Cmd.CmdL Derive.Events
derive_to_events block_id = do
    result <- derive block_id
    case Derive.r_result result of
        Left err -> Cmd.throw $ "derive error: " ++ show err
        Right events -> return events

-- | Derive the current block and return events that fall within the current
-- selection.
derive_selection :: Cmd.CmdL Derive.Events
derive_selection = do
    (start, end) <- Selection.realtime
    block_id <- Cmd.get_focused_block
    events <- derive_to_events block_id
    -- TODO filter out events from other tracks
    return $ takeWhile ((<=end) . Score.event_end) $
        dropWhile ((<start) . Score.event_start) events

derive :: BlockId -> Cmd.CmdL (Derive.Result Derive.Events)
derive = PlayUtil.cached_derive

uncached_derive :: BlockId -> Cmd.CmdL (Derive.Result Derive.Events)
uncached_derive = PlayUtil.uncached_derive

-- | Test the tempo map output.
-- TODO broken?
derive_tempo :: BlockId -> Cmd.CmdL [[(BlockId, [(TrackId, ScoreTime)])]]
derive_tempo block_id = do
    result <- PlayUtil.cached_derive block_id
    return $ map (Derive.r_inv_tempo result) (map Timestamp.seconds [0..10])

-- * perform

derive_to_perf :: BlockId -> Cmd.CmdL ([Midi.Perform.Event], [Warning.Warning])
derive_to_perf block_id = do
    events <- derive_to_events block_id
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    return $ Midi.Convert.convert lookup_scale lookup_inst events

cached_perform :: BlockId -> Cmd.CmdL Midi.Perform.Messages
cached_perform block_id = do
    perf <- PlayUtil.cached_perform block_id =<< PlayUtil.cached_derive block_id
    return $ Midi.Cache.cache_messages (Cmd.perf_midi_cache perf)

perform_selection :: Cmd.CmdL Midi.Perform.Messages
perform_selection = do
    (start, end) <- Selection.realtime
    block_id <- Cmd.get_focused_block
    perf <- PlayUtil.cached_perform block_id =<< PlayUtil.cached_derive block_id
    let end_ts = Timestamp.from_real_time (end - start)
    return $ takeWhile ((<=end_ts) . Midi.wmsg_ts) $
        Midi.Cache.messages_from (Timestamp.from_real_time start)
            (Cmd.perf_midi_cache perf)

perform_events :: Derive.Events -> Cmd.CmdL (Midi.Perform.Messages, [String])
perform_events = PlayUtil.perform_events
