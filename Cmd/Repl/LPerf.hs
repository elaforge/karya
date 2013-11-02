-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Cmds to deal with Cmd.Performance, derivation, and performance.
module Cmd.Repl.LPerf where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance
import qualified Cmd.Play as Play
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
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified App.Config as Config
import Types


get_root :: (Cmd.M m) => m Cmd.Performance
get_root = Perf.get_root

get :: (Cmd.M m) => BlockId -> m Cmd.Performance
get = Cmd.get_performance

get_current :: (Cmd.M m) => BlockId -> m Cmd.Performance
get_current block_id = Cmd.require =<< Map.lookup block_id <$>
    Cmd.gets (Cmd.state_current_performance . Cmd.state_play)

track_signals :: Cmd.CmdL (Maybe Track.TrackSignal)
track_signals = do
    (block_id, _, track_id, _) <- Selection.get_insert
    perf <- get block_id
    return $ Map.lookup (block_id, track_id) (Cmd.perf_track_signals perf)

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

get_realtime :: (Cmd.M m) => Bool -> m RealTime
get_realtime root = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    perf <- if root then get_root else get block_id
    Perf.get_realtime perf block_id (Just track_id) pos

-- * analysis

type ControlVals =
    Map.Map Score.Control ((RealTime, Signal.Y), (RealTime, Signal.Y))

-- | Get the first and last values for each instrument.  This can be used to
-- show which controls the each instrument uses, which in turn can be used
-- to set its 'Instrument.config_controls', to make sure it is always
-- initialized consistently.
inst_controls :: BlockId -> Cmd.CmdL (Map.Map Score.Instrument ControlVals)
inst_controls block_id =
    List.foldl' merge mempty <$> block_perform_events block_id
    where
    merge insts event =
        Map.insertWith (Map.unionWith merge1) (event_inst event)
            (control_vals (Perform.event_controls event))
            insts
    control_vals = Map.mapMaybe $ \sig ->
        case (Signal.head sig, Signal.last sig) of
            (Just a, Just b) -> Just (a, b)
            _ -> Nothing
    merge1 (start1, end1) (start2, end2) =
        (Seq.min_on fst start1 start2, Seq.max_on fst end1 end2)
    event_inst = Instrument.inst_score . Perform.event_instrument

-- * derive

-- These are mostly for testing, to find problems in performer output.

-- | Clear out all caches and rederive from scratch.
rederive :: Cmd.CmdL ()
rederive = PlayUtil.clear_all_caches

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

block_perform_events :: BlockId -> Cmd.CmdL [Perform.Event]
block_perform_events block_id =
    LEvent.events_of <$> (convert . LEvent.events_of =<< block_events block_id)

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

sel_pevents :: Cmd.CmdL (Events Perform.Event)
sel_pevents = convert . LEvent.events_of =<< sel_events

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
    takeWhile (is_event ((<end) . start_of))
        . dropWhile (is_event ((<start) . start_of))
    where is_event f = LEvent.either f (const True)

-- * perform_events

convert :: [Score.Event] -> Cmd.CmdL (Events Perform.Event)
convert events = do
    lookup <- PlayUtil.get_convert_lookup
    return $ Midi.Convert.convert lookup events

perf_event_inst :: Perform.Event -> Text
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

-- | Get all logs whose 'Log.msg_text' matches a regex.
logs_like :: BlockId -> String -> Cmd.CmdL [Log.Msg]
logs_like block_id regex = do
    logs <- LEvent.logs_of <$> block_midi block_id
    let reg = Regex.make regex
    return $ filter (Regex.matches reg . Log.msg_string) logs

-- | Get logs that include a stack frame that matches the given block, tracks,
-- and range.
logs_matching :: BlockId -> BlockId -> [TrackId] -> TrackTime -> TrackTime
    -> Cmd.CmdL [Log.Msg]
logs_matching perf_block block_id track_ids start end = do
    logs <- LEvent.logs_of <$> block_midi perf_block
    let pattern = (Just block_id, Just (Set.fromList track_ids),
            Just (start, end))
        match = maybe False (Stack.match pattern . Stack.from_strings)
            . Log.msg_stack
    return $ filter match logs

play_midi :: Perform.MidiEvents -> Cmd.CmdL ()
play_midi msgs = do
    let status = Cmd.PlayMidi $ Cmd.PlayMidiArgs Nothing "repl" (to_zero msgs)
            Nothing Nothing
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

-- Should this go in LDebug?

-- | Extract the cache logs, with no summarizing.
cache_logs :: BlockId -> Cmd.CmdL String
cache_logs block_id = do
    perf <- Cmd.get_performance block_id
    let logs = filter Cache.is_cache_log $ Cmd.perf_logs perf
    return $ unlines
        [format_stack msg ++ ": " ++ Log.msg_string msg | msg <- logs]
    where
    format_stack =
        maybe "" (Stack.show_ui_ . Stack.from_strings) . Log.msg_stack

-- | Stats for both block and track caches on the focused block.
block_stats :: Cmd.CmdL String
block_stats = do
    block <- block_cache =<< Cmd.get_focused_block
    track <- track_cache =<< Cmd.get_focused_block
    return $ unlines
        ["block:", format_stats block, "track:", format_stats track]

-- | Get summarized stats for cached blocks.
block_cache :: BlockId -> Cmd.CmdL ([(Text, [BlockId])], [(BlockId, Int)])
block_cache block_id =
    Play.extract_cache_stats Play.get_block_id . Cmd.perf_logs <$> get block_id

-- | Get summarized stats for cached tracks on the given block.
track_cache :: BlockId -> Cmd.CmdL ([(Text, [TrackId])], [(TrackId, Int)])
track_cache block_id = do
    (rederived, cached) <- (rederived_block *** cached_block)
        . Play.extract_cache_stats Play.get_track_id . Cmd.perf_logs
        <$> get block_id
    return (rederived, cached)
    where
    rederived_block = map (second $ map snd . filter ((==block_id) . fst))
    cached_block = map (first snd) . filter ((==block_id) . fst . fst)

format_stats :: (Id.Ident id) => ([(Text, [id])], [(id, Int)]) -> String
format_stats (rederived, cached) =
    "cached: " <> format_cached cached <> "\n"
        <> unlines (map format_rederived rederived)
    where
    format_rederived (because, ids) =
        untxt because <> ": [" <> show (length ids) <> "] "
        <> unwords (map Id.ident_name ids)
    format_cached cached =
        show (length cached) <> " [" <> show (sum (map snd cached)) <> "] "
        <> unwords (map (Id.ident_name . fst) cached)

-- ** cache contents

get_cache :: (Cmd.M m) => BlockId -> m (Map.Map Stack.Stack Derive.Cached)
get_cache block_id = do
    Derive.Cache cache <- Cmd.perf_derive_cache <$>
        Cmd.get_performance block_id
    return cache

get_cache_events :: (Cache.Cacheable d, Cmd.M m) => BlockId
    -> m (Map.Map Stack.Stack (LEvent.LEvents d))
get_cache_events block_id = do
    cache <- get_cache block_id
    return $ Map.mapMaybe get cache
    where
    get Derive.Invalid = Nothing
    get (Derive.Cached c) = case Cache.from_cache_entry c of
        Nothing -> Nothing
        Just (Derive.CallType _ events) -> Just events

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


-- * pitches

type Ratio = Ratio.Rational

-- | A hook for 'Cmd.hooks_selection'.
chord_hook :: [(ViewId, Maybe Types.Selection)] -> Cmd.CmdId ()
chord_hook = mapM_ (uncurry set_chord_status)

-- | Show chord ratios at current selection.
chord :: Cmd.CmdL Text
chord = do
    (view_id, sel) <- Selection.get
    show_chord <$> chord_at_sel view_id sel

show_chord :: [(Pitch.NoteNumber, Pitch.Note, Ratio)] -> Text
show_chord = Text.intercalate ", " . map pretty
    where
    pretty (_, note, ratio) = Pitch.note_text note <> ":" <> show_ratio ratio

show_ratio :: Ratio -> Text
show_ratio = go 0
    where
    go oct ratio
        | ratio >= 2 = go (oct+1) (ratio/2)
        | ratio <= 1%2 = go (oct-1) (ratio*2)
        | oct == 0 = pretty ratio
        | otherwise = showt oct <> "+" <> pretty ratio
    pretty ratio =
        showt (Ratio.numerator ratio) <> "/" <> showt (Ratio.denominator ratio)

set_chord_status :: (Cmd.M m) => ViewId -> Maybe Types.Selection -> m ()
set_chord_status view_id maybe_sel = case maybe_sel of
    Nothing -> set Nothing
    Just sel -> set . Just . show_chord =<< chord_at_sel view_id sel
    where set = Cmd.set_view_status view_id Config.status_chord

-- | Show the ratios of the frequencies of the notes at the time of the current
-- selection.  They are sorted by their track-order, and the track with the
-- selection is considered unity.
chord_at_sel :: (Cmd.M m) => ViewId -> Types.Selection
    -> m [(Pitch.NoteNumber, Pitch.Note, Ratio)]
chord_at_sel view_id sel = do
    block_id <- State.block_id_of view_id
    maybe_track_id <- State.event_track_at block_id (Selection.point_track sel)
    perf <- Perf.get_root
    pos <- Perf.get_realtime perf block_id maybe_track_id (Selection.point sel)
    events <- PlayUtil.overlapping_events pos . Cmd.perf_events <$> get block_id
    events <- sort_by_track block_id events
    let selected = do
            track_id <- maybe_track_id
            List.find (on_track track_id) events
    let nns = mapMaybe (Score.nn_at pos) events
        notes = mapMaybe (Score.note_at pos) events
    return $ List.zip3 nns notes (nn_ratios (Score.nn_at pos =<< selected) nns)
    where
    on_track track_id = (== Just track_id) . fmap snd . Stack.block_track_of
        . Score.event_stack

-- | Sort events by the tracknum of the tracks they fall on.  Filter out
-- events that don't directly originate from a track on the given block.
--
-- TODO I should look for the block anywhere in the stack, and sort it by the
-- corresponding track.
sort_by_track :: (State.M m) => BlockId -> [Score.Event] -> m [Score.Event]
sort_by_track block_id events = do
    let by_track = Seq.key_on_maybe
            (fmap snd . Stack.block_track_of . Score.event_stack) events
    tracknums <- mapM (State.tracknum_of block_id . fst) by_track
    let by_tracknum = [(tracknum, event)
            | (Just tracknum, event) <- zip tracknums (map snd by_track)]
    return $ map snd $ Seq.sort_on fst by_tracknum

nn_ratios :: Maybe Pitch.NoteNumber -> [Pitch.NoteNumber] -> [Ratio]
nn_ratios unity nns = case (unity, nns) of
    (Just unity, nns) -> ratios unity nns
    (Nothing, nn : nns) -> ratios nn nns
    (_, []) -> []
    where
    ratios unity = map (flip Ratio.approxRational 0.01 . (/ hz unity) . hz)
    hz = Pitch.nn_to_hz

overlapping_events :: Bool -> Cmd.CmdL (RealTime, [Score.Event])
overlapping_events from_root = do
    (block_id, start, _) <-
        if from_root then Selection.realtime else Selection.local_realtime
    events <- PlayUtil.overlapping_events start . Cmd.perf_events <$>
        get block_id
    return (start, events)
