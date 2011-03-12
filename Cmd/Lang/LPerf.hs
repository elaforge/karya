-- | Cmds to deal with Cmd.Performance, derivation, and performance.
module Cmd.Lang.LPerf where
import Util.Control
import qualified Util.Seq as Seq

import Ui

import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Perform as Midi.Perform
import qualified Perform.Midi.Cache as Midi.Cache
import qualified Perform.RealTime as RealTime


-- * performance

get_midi_cache :: BlockId -> Cmd.CmdL Midi.Cache.Cache
get_midi_cache block_id =
    Cmd.perf_midi_cache <$> Cmd.get_performance block_id

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
    uncached <- PlayUtil.uncached_perform block_id result
    cached <- PlayUtil.cached_perform block_id result
    return $ diff_events cached uncached
    where
    diff_events perf1 perf2 = Seq.diff (==) (msgs perf1) (msgs perf2)
    msgs = LEvent.events_of . Midi.Cache.cache_messages . Cmd.perf_midi_cache

compare_cached_events :: BlockId
    -> Cmd.CmdL [Either Simple.ScoreEvent Simple.ScoreEvent]
compare_cached_events block_id = do
    uncached <- PlayUtil.uncached_derive block_id
    cached <- PlayUtil.cached_derive block_id
    return $ diff (LEvent.events_of (Derive.r_events cached))
        (LEvent.events_of (Derive.r_events uncached))
    where
    diff e1 e2 = Seq.diff (==)
        (map Simple.score_event e1) (map Simple.score_event e2)

derive :: BlockId -> Cmd.CmdL Derive.Result
derive = PlayUtil.cached_derive

uncached_derive :: BlockId -> Cmd.CmdL Derive.Result
uncached_derive = PlayUtil.uncached_derive

-- | Test the tempo map output.
-- TODO broken?
derive_tempo :: BlockId -> Cmd.CmdL [[(BlockId, [(TrackId, ScoreTime)])]]
derive_tempo block_id = do
    result <- PlayUtil.cached_derive block_id
    return $ map (Derive.r_inv_tempo result) (map RealTime.seconds [0..10])

-- * block

block_events :: BlockId -> Cmd.CmdL Derive.Events
block_events block_id = Derive.r_events <$> derive block_id

block_uncached_events :: BlockId -> Cmd.CmdL Derive.Events
block_uncached_events block_id = Derive.r_events <$> uncached_derive block_id

-- | Derive all the way to MIDI.
block_midi :: BlockId -> Cmd.CmdL Midi.Perform.MidiEvents
block_midi block_id = do
    perf <- PlayUtil.cached_perform block_id =<< PlayUtil.cached_derive block_id
    return $ Midi.Cache.cache_messages (Cmd.perf_midi_cache perf)

-- * selection

-- | Derive the current block and return events that fall within the current
-- selection.
sel_events :: Cmd.CmdL Derive.Events
sel_events = get_sel block_events Score.event_start

sel_midi :: Cmd.CmdL Midi.Perform.MidiEvents
sel_midi = get_sel perf Midi.wmsg_ts
    where
    perf bid = do
        p <- PlayUtil.cached_perform bid =<< PlayUtil.cached_derive bid
        return $ Midi.Cache.messages_from 0 $ Cmd.perf_midi_cache p

-- | Easier to read midi.
simple_midi :: Cmd.CmdL Midi.Perform.MidiEvents
    -> Cmd.CmdL [(RealTime, Midi.Message)]
simple_midi = fmap (map f . LEvent.events_of)
    where f wmsg = (Midi.wmsg_ts wmsg, Midi.wmsg_msg wmsg)

-- ** implementation

get_sel :: (BlockId -> Cmd.CmdL [LEvent.LEvent d]) -> (d -> RealTime)
    -> Cmd.CmdL [LEvent.LEvent d]
get_sel block_events event_start = do
    (block_id, start, end) <- Selection.local_realtime
    events <- block_events block_id
    -- TODO filter out events from other tracks
    return $ events_in_range event_start start end events

events_in_range :: (Ord k) => (d -> k) -> k -> k
    -> [LEvent.LEvent d] -> [LEvent.LEvent d]
events_in_range start_of start end =
    takeWhile (is_event ((<=end) . start_of))
        . dropWhile (is_event ((<start) . start_of))
    where is_event f = LEvent.either f (const True)

-- * conversion

perform_events :: Derive.Events -> Cmd.CmdL Midi.Perform.MidiEvents
perform_events = PlayUtil.perform_events

convert :: Derive.Events -> Cmd.CmdL [LEvent.LEvent Midi.Perform.Event]
convert events = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    return $ Midi.Convert.convert lookup_scale lookup_inst events

-- * midi

get_chunk :: Int -> Cmd.CmdT IO Midi.Cache.Chunk
get_chunk n = do
    cache <- get_midi_cache =<< Cmd.get_focused_block
    return $ Midi.Cache.cache_chunks cache !! n
