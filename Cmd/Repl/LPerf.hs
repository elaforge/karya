{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Cmds to deal with Cmd.Performance, derivation, and performance.
module Cmd.Repl.LPerf where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Track as Track
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform

import Types


get_root :: Cmd.CmdL Cmd.Performance
get_root = Cmd.require =<< Perf.lookup_root

get :: BlockId -> Cmd.CmdL Cmd.Performance
get = Cmd.get_performance

track_signals :: Cmd.CmdL (Maybe Track.TrackSignal)
track_signals = do
    (block_id, _, track_id, _) <- Selection.get_insert
    perf <- get block_id
    return $ Map.lookup track_id (Cmd.perf_track_signals perf)

-- * info

environ :: Cmd.CmdL (Maybe TrackLang.Environ)
environ = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Perf.lookup_environ block_id (Just track_id)

-- | Controls in scope at the insert point.
controls :: Cmd.CmdL Score.ControlMap
controls = maybe mempty Derive.state_controls <$> dynamic

warp :: Cmd.CmdL Score.Warp
warp = maybe Score.id_warp Derive.state_warp <$> dynamic

dynamic :: Cmd.CmdL (Maybe Derive.Dynamic)
dynamic = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Perf.lookup_dynamic block_id (Just track_id)

sel_to_real :: Cmd.CmdL [RealTime]
sel_to_real = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    tempo <- Cmd.perf_tempo <$> get block_id
    return $ tempo block_id track_id pos

get_realtime :: Bool -> Cmd.CmdL RealTime
get_realtime root = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    perf <- if root then get_root else get block_id
    Perf.get_realtime perf block_id (Just track_id) pos

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

inverse_tempo_func :: RealTime -> Cmd.CmdL [(BlockId, [(TrackId, ScoreTime)])]
inverse_tempo_func time = do
    perf <- get =<< Cmd.get_focused_block
    return $ TrackWarp.inverse_tempo_func (Cmd.perf_warps perf) time

-- * block

block_events :: BlockId -> Cmd.CmdL Derive.Events
block_events block_id = Derive.r_events <$> PlayUtil.cached_derive block_id

block_uncached_events :: BlockId -> Cmd.CmdL Derive.Events
block_uncached_events block_id = Derive.r_events <$> uncached_derive block_id

-- | Derive all the way to MIDI.
block_midi :: BlockId -> Cmd.CmdL Perform.MidiEvents
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

events_from :: Cmd.CmdL Cmd.Events
events_from = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    perf <- Cmd.get_performance block_id
    start <- Perf.get_realtime perf block_id (Just track_id) pos
    return $ PlayUtil.events_from start (Cmd.perf_events perf)

perform_from :: Cmd.CmdL Perform.MidiEvents
perform_from = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    perf <- Cmd.get_performance block_id
    start <- Perf.get_realtime perf block_id (Just track_id) pos
    PlayUtil.perform_from start perf

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
    (_, _, track_ids, _, _) <- Selection.tracks
    events <- derive_events block_id
    return $ in_tracks event_stack track_ids $
        in_range event_start start end events

score_in_selection :: [TrackId] -> RealTime -> RealTime
    -> Derive.Events -> Derive.Events
score_in_selection track_ids start end =
    in_tracks Score.event_stack track_ids
    . in_range Score.event_start start end

in_tracks :: (d -> Stack.Stack) -> [TrackId] -> Events d -> Events d
in_tracks event_stack track_ids =
    filter $ is_event (has . tracks_of . event_stack)
    where
    is_event f = LEvent.either f (const True)
    tracks_of = mapMaybe Stack.track_of . Stack.innermost
    has tids = any (`elem` tids) track_ids

in_range :: (Ord k) => (d -> k) -> k -> k -> Events d -> Events d
in_range start_of start end =
    takeWhile (is_event ((<=end) . start_of))
        . dropWhile (is_event ((<start) . start_of))
    where is_event f = LEvent.either f (const True)

-- * perform_events

convert :: [Score.Event] -> Cmd.CmdL (Events Perform.Event)
convert events = do
    lookup <- PlayUtil.get_convert_lookup
    return $ Midi.Convert.convert lookup events

perf_event_inst :: Perform.Event -> String
perf_event_inst =
    Score.inst_name . Instrument.inst_score . Perform.event_instrument

-- * midi

perform_events :: Cmd.Events -> Cmd.CmdL Perform.MidiEvents
perform_events = PlayUtil.perform_events

-- | This is the local block's performance, and the events are filtered to the
-- selection range, and the filtering is done post-derivation, so they reflect
-- what would actually be played.
sel_midi :: Cmd.CmdL Perform.MidiEvents
sel_midi = do
    (block_id, start, end) <- Selection.local_realtime
    events <- block_midi block_id
    return $ takeWhile (LEvent.log_or $ (<=end) . Midi.wmsg_ts) $
        dropWhile (LEvent.log_or $ (<start) . Midi.wmsg_ts) events

play_midi :: Perform.MidiEvents -> Cmd.CmdL ()
play_midi msgs = do
    let status = Cmd.PlayMidi $ Cmd.PlayMidiArgs "repl" (to_zero msgs) Nothing
            Nothing
    Cmd.modify $ \st -> st { Cmd.state_repl_status = status }
    where
    to_zero msgs = PlayUtil.shift_messages 1 (PlayUtil.first_time msgs) msgs

filter_chan :: Midi.Channel -> Perform.MidiEvents -> Perform.MidiEvents
filter_chan chan events =
    [LEvent.Event msg | (msg, Just mchan) <- zip msgs chans, mchan == chan]
    where
    msgs = LEvent.events_of events
    chans = map (Midi.message_channel . Midi.wmsg_msg) msgs

-- | Reduce MIDI to an easier to read form.
simple_midi :: Perform.MidiEvents -> [(RealTime, Midi.Message)]
simple_midi = map f . LEvent.events_of
    where f wmsg = (Midi.wmsg_ts wmsg, Midi.wmsg_msg wmsg)

-- * cache

cache_stats :: BlockId -> Cmd.CmdL String
cache_stats block_id = do
    perf <- Cmd.get_performance block_id
    let logs = filter Cache.is_cache_log $ Cmd.perf_logs perf
    return $ unlines
        [format_stack msg ++ ": " ++ Log.msg_string msg | msg <- logs]
    where
    format_stack =
        maybe "" (Stack.show_ui_ . Stack.from_strings) . Log.msg_stack

show_cache :: (Cmd.M m) => BlockId -> m String
show_cache block_id = do
    perf <- Cmd.get_performance block_id
    return $ unlines (pretty_cache (Cmd.perf_derive_cache perf))

pretty_cache :: Derive.Cache -> [String]
pretty_cache (Derive.Cache cache) =
    [Stack.show_ui_ stack ++ ": " ++ pretty_cached cached
        | (stack, cached) <- Map.toAscList cache]
    where
    pretty_cached Derive.Invalid = "Invalid"
    pretty_cached (Derive.Cached entry) =
        show (entry_events entry) ++ " events"

entry_events :: Derive.CacheEntry -> Int
entry_events entry = case entry of
    Derive.CachedEvents (Derive.CallType _ events) -> length events
    Derive.CachedControl (Derive.CallType _ events) -> length events
    Derive.CachedPitch (Derive.CallType _ events) -> length events
