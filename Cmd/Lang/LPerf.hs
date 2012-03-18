-- | Cmds to deal with Cmd.Performance, derivation, and performance.
module Cmd.Lang.LPerf where
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Perform as Midi.Perform
import qualified Perform.RealTime as RealTime

import Types


get_perf :: Cmd.CmdL Cmd.Performance
get_perf = Cmd.require =<< Perf.lookup_root

-- * derive

-- These are mostly for testing, to find problems in performer output.

-- | Clear out all caches and rederive from scratch for the given block.
rederive :: BlockId -> Cmd.CmdL ()
rederive = PlayUtil.clear_cache

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
    return $ map (TrackWarp.inverse_tempo_func (Derive.r_track_warps result)
        . RealTime.seconds) [0..10]

-- * block

block_events :: BlockId -> Cmd.CmdL Derive.Events
block_events block_id = Derive.r_events <$> derive block_id

block_uncached_events :: BlockId -> Cmd.CmdL Derive.Events
block_uncached_events block_id = Derive.r_events <$> uncached_derive block_id

-- | Derive all the way to MIDI.
block_midi :: BlockId -> Cmd.CmdL Midi.Perform.MidiEvents
block_midi block_id = do
    perf <- Performance.performance <$> PlayUtil.cached_derive block_id
    PlayUtil.perform_from 0 perf

-- * selection

-- | Derive the current block and return events that fall within the current
-- selection.
sel_events :: Cmd.CmdL Derive.Events
sel_events = get_sel_events False block_events

-- | Like 'sel_events' but take the root derivation.
root_sel_events :: Cmd.CmdL Derive.Events
root_sel_events = get_sel_events True block_events

-- * play from

events_from :: Cmd.CmdL [Score.Event]
events_from = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    perf <- Cmd.get_performance block_id
    start <- Perf.get_realtime perf block_id (Just track_id) pos
    return $ LEvent.events_of $
        PlayUtil.events_from start (Cmd.perf_events perf)

perform_from :: Cmd.CmdL Midi.Perform.MidiEvents
perform_from = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    perf <- Cmd.get_performance block_id
    start <- Perf.get_realtime perf block_id (Just track_id) pos
    msgs <- PlayUtil.perform_from start perf
    return msgs

-- ** implementation

type Events d = [LEvent.LEvent d]

get_sel_events :: Bool -> (BlockId -> Cmd.CmdL (Events Score.Event))
    -> Cmd.CmdL (Events Score.Event)
get_sel_events = get_sel Score.event_start Score.event_stack

get_sel :: (d -> RealTime) -> (d -> Stack.Stack)
    -> Bool -> (BlockId -> Cmd.CmdL (Events d)) -> Cmd.CmdL (Events d)
get_sel event_start event_stack from_root derive_events = do
    (block_id, start, end) <-
        if from_root then Selection.realtime else Selection.local_realtime
    (_, track_ids, _, _) <- Selection.tracks
    events <- derive_events block_id
    return $ in_tracks event_stack track_ids $
        in_range event_start start end events

in_tracks :: (d -> Stack.Stack) -> [TrackId] -> Events d -> Events d
in_tracks event_stack track_ids =
    filter $ is_event (has . tracks_of . event_stack)
    where
    is_event f = LEvent.either f (const True)
    tracks_of = Maybe.mapMaybe Stack.track_of . Stack.innermost
    has tids = any (`elem` tids) track_ids

in_range :: (Ord k) => (d -> k) -> k -> k -> Events d -> Events d
in_range start_of start end =
    takeWhile (is_event ((<=end) . start_of))
        . dropWhile (is_event ((<start) . start_of))
    where is_event f = LEvent.either f (const True)

-- * conversion

perform_events :: Derive.Events -> Cmd.CmdL Midi.Perform.MidiEvents
perform_events = PlayUtil.perform_events

convert :: Derive.Events -> Cmd.CmdL (Events Midi.Perform.Event)
convert events = do
    lookup <- PlayUtil.get_convert_lookup
    return $ Midi.Convert.convert lookup events

-- * util

-- | Reduce MIDI to an easier to read form.
simple_midi :: Midi.Perform.MidiEvents -> [(RealTime, Midi.Message)]
simple_midi = map f . LEvent.events_of
    where f wmsg = (Midi.wmsg_ts wmsg, Midi.wmsg_msg wmsg)
