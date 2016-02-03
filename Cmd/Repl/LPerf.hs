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

import qualified Util.Log as Log
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Types as Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified App.Config as Config
import Global
import Types


get_root :: Cmd.M m => m Cmd.Performance
get_root = Perf.get_root

get :: Cmd.M m => BlockId -> m Cmd.Performance
get = Cmd.get_performance

get_current :: Cmd.M m => BlockId -> m Cmd.Performance
get_current block_id = Cmd.abort_unless =<< Map.lookup block_id <$>
    Cmd.gets (Cmd.state_current_performance . Cmd.state_play)

track_signals :: Cmd.CmdL (Maybe Track.TrackSignal)
track_signals = do
    (block_id, _, track_id, _) <- Selection.get_insert
    perf <- get block_id
    return $ Map.lookup (block_id, track_id) (Cmd.perf_track_signals perf)

-- * info

environ :: Cmd.CmdL (Maybe Env.Environ)
environ = Perf.lookup_environ =<< Selection.track

-- | Controls in scope at the insert point.
controls :: Bool -> Cmd.CmdL Score.ControlMap
controls from_root = Derive.state_controls <$> dynamic from_root

-- | The control vals at the insertion point, taking the control functions into
-- account.
control_vals :: Bool -> Cmd.CmdL Score.ControlValMap
control_vals from_root = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    ruler_id <- fromMaybe State.no_ruler <$>
        State.ruler_track_at block_id tracknum
    mlists <- Ruler.ruler_marklists <$> State.get_ruler ruler_id
    dyn <- dynamic from_root
    pos <- get_realtime from_root
    return $ Derive.state_controls_at pos mlists dyn

-- | Like 'control_vals', but without control functions.
raw_control_vals :: Bool -> Cmd.CmdL Score.TypedControlValMap
raw_control_vals from_root = do
    dyn <- dynamic from_root
    pos <- get_realtime from_root
    return $ fmap (fmap (Signal.at pos)) (Derive.state_controls dyn)

aliases :: Cmd.CmdL (Map.Map Score.Instrument Score.Instrument)
aliases = Derive.state_instrument_aliases <$> dynamic True

warp :: Bool -> Cmd.CmdL Score.Warp
warp from_root = Derive.state_warp <$> dynamic from_root

dynamic :: Bool -> Cmd.CmdL Derive.Dynamic
dynamic from_root = do
    track <- Selection.track
    Cmd.require ("no dynamic for track " <> pretty track) =<< if from_root
        then Perf.lookup_root_dynamic track
        else Perf.lookup_dynamic (fst track) track

sel_to_real :: Cmd.CmdL [RealTime]
sel_to_real = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    tempo <- Cmd.perf_tempo <$> get block_id
    return $ tempo block_id track_id pos

get_realtime :: Cmd.M m => Bool -> m RealTime
get_realtime from_root = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    perf <- if from_root then get_root else get block_id
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
            (control_vals (Types.event_controls event))
            insts
    control_vals = Map.mapMaybe $ \sig ->
        case (Signal.head sig, Signal.last sig) of
            (Just a, Just b) -> Just (a, b)
            _ -> Nothing
    merge1 (start1, end1) (start2, end2) =
        (Seq.min_on fst start1 start2, Seq.max_on fst end1 end2)
    event_inst = Types.patch_name . Types.event_patch

-- * derive

-- These are mostly for testing, to find problems in performer output.

-- | Clear out all caches and rederive from scratch.
rederive :: Cmd.CmdL ()
rederive = PlayUtil.clear_caches

compare_cached_events :: BlockId
    -> Cmd.CmdL [Either Simple.ScoreEvent Simple.ScoreEvent]
compare_cached_events block_id = do
    uncached <- PlayUtil.uncached_derive block_id
    cached <- PlayUtil.cached_derive block_id
    return $ diff (Stream.events_of (Derive.r_events cached))
        (Stream.events_of (Derive.r_events uncached))
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

-- | Get this block's performance from the cache.
block_events :: BlockId -> Cmd.CmdL [LEvent.LEvent Score.Event]
block_events block_id = normalize_events =<< block_events_unnormalized block_id

-- | 'normalize_events' is important for display, but I can't do it if I'm
-- going to pass to 'convert', because convert should do the normalization
-- itself.
block_events_unnormalized :: BlockId -> Cmd.CmdL [LEvent.LEvent Score.Event]
block_events_unnormalized block_id =
    Stream.to_list . Derive.r_events <$> PlayUtil.cached_derive block_id

block_uncached_events :: BlockId -> Cmd.CmdL [LEvent.LEvent Score.Event]
block_uncached_events block_id = normalize_events . Stream.to_list
    . Derive.r_events =<< uncached_derive block_id

-- | Apply 'Score.normalize' to the events, so that they have their final
-- control positions and pitches.  Normally 'convert' does this, but if you
-- display it before convert it's nice to see the \"cooked\" versions.
normalize_events :: Cmd.M m => [LEvent.LEvent Score.Event]
    -> m [LEvent.LEvent Score.Event]
normalize_events events = do
    lookup <- Cmd.get_lookup_instrument
    let lookup_env = maybe mempty (Common.get_environ . Inst.inst_common . fst)
            . lookup
    return $ map (fmap (Score.normalize lookup_env)) events

block_perform_events :: BlockId -> Cmd.CmdL [Types.Event]
block_perform_events block_id = (LEvent.events_of <$>) $
    convert . LEvent.events_of =<< block_events_unnormalized block_id

-- | Derive all the way to MIDI.
block_midi :: BlockId -> Cmd.CmdL Perform.MidiEvents
block_midi block_id = do
    perf <- Performance.performance <$> PlayUtil.cached_derive block_id
    PlayUtil.perform_from 0 perf

-- * selection

-- | Derive the current block from the cache and return events that fall within
-- the current selection.
sel_events :: Cmd.CmdL [Score.Event]
sel_events = get_sel_events False block_events

sel_pevents :: Cmd.CmdL (Events Types.Event)
sel_pevents = convert =<< get_sel_events False block_events_unnormalized

-- | Like 'sel_events' but take the root derivation.
root_sel_events :: Cmd.CmdL [Score.Event]
root_sel_events = get_sel_events True block_events

root_sel_pevents :: Cmd.CmdL (Events Types.Event)
root_sel_pevents = convert =<< get_sel_events True block_events_unnormalized

-- ** extract

-- | Extract events logged via the @debug@ call.
extract_debug :: BlockId -> Cmd.CmdL [(Text, [Score.Event])]
extract_debug block_id = do
    logs <- LEvent.logs_of <$> block_events block_id
    return
        [ (Log.msg_text log, events)
        | (log, Just events) <- zip logs (map Log.get_data logs)
        ]

extract_debug_tag :: BlockId -> Text -> Cmd.CmdL [Score.Event]
extract_debug_tag block_id tag =
    Cmd.require ("tag not found: " <> tag) . lookup tag
        =<< extract_debug block_id

extract_insts :: BlockId -> Text -> [Text] -> Cmd.CmdL [Score.Event]
extract_insts block_id tag insts =
    with_insts insts . strip_stack <$> extract_debug_tag block_id tag

control :: Score.Control -> Score.Event -> Maybe Score.TypedVal
control c e = Score.control_at (Score.event_start e) c e

event_controls :: Score.Event -> Score.ControlValMap
event_controls e = Score.event_controls_at (Score.event_start e) e

only_controls :: [Score.Control] -> [LEvent.LEvent Score.Event] -> [Score.Event]
only_controls controls = map strip . LEvent.events_of
    where
    strip e = e
        { Score.event_untransformed_controls =
            Map.filterWithKey (\c _ -> c `elem` controls)
                (Score.event_untransformed_controls e)
        }

with_insts :: [Text] -> [Score.Event] -> [Score.Event]
with_insts instruments = filter ((`elem` is) . Score.event_instrument)
    where is = map Util.instrument instruments

strip_stack :: [Score.Event] -> [Score.Event]
strip_stack = map $ \event -> event { Score.event_stack = Stack.empty }


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

-- | Like 'get_sel_events_logs', but filter out the LEvent.Logs.
get_sel_events :: Bool -- ^ from root
    -> (BlockId -> Cmd.CmdL (Events Score.Event))
    -> Cmd.CmdL [Score.Event]
get_sel_events from_root = (LEvent.events_of <$>)
    . get_sel Score.event_start Score.event_stack from_root

-- | Get events derived in the selected range.
get_sel_events_logs :: Bool -- ^ from root
    -> (BlockId -> Cmd.CmdL (Events Score.Event)) -- ^ derive events in the
    -- given block, e.g. via 'block_events' or 'block_events_unnormalized'
    -> Cmd.CmdL (Events Score.Event)
get_sel_events_logs = get_sel Score.event_start Score.event_stack

get_sel :: (d -> RealTime) -> (d -> Stack.Stack) -> Bool -- ^ from root
    -> (BlockId -> Cmd.CmdL (Events d)) -> Cmd.CmdL (Events d)
get_sel event_start event_stack from_root derive_events = do
    (block_id, start, end) <-
        if from_root then Selection.realtime else Selection.local_realtime
    (_, _, track_ids, _, _) <- Selection.tracks
    events <- derive_events block_id
    return $ in_tracks event_stack track_ids $
        in_range event_start start end events

score_in_selection :: [TrackId] -> RealTime -> RealTime
    -> [LEvent.LEvent Score.Event] -> [LEvent.LEvent Score.Event]
score_in_selection track_ids start end =
    in_tracks Score.event_stack track_ids
    . in_range Score.event_start start end

in_tracks :: (d -> Stack.Stack) -> [TrackId] -> Events d -> Events d
in_tracks event_stack track_ids =
    filter $ LEvent.log_or (has . tracks_of . event_stack)
    where
    tracks_of = mapMaybe Stack.track_of . Stack.innermost
    has tids = any (`elem` tids) track_ids

in_range :: (a -> RealTime) -> RealTime -> RealTime -> [LEvent.LEvent a]
    -> [LEvent.LEvent a]
in_range start_of start end =
    takeWhile (LEvent.log_or $ (<end) . start_of)
    . dropWhile (LEvent.log_or $ before start . start_of)
    where
    before start ts
        -- I can't put a selection before 0, so assume that means I want
        -- everything before 0 too.
        | start == 0 = False
        | otherwise = ts < start

-- * perform_events

convert :: [Score.Event] -> Cmd.CmdL (Events Types.Event)
convert events = do
    lookup <- PlayUtil.get_convert_lookup
    return $ Midi.Convert.convert lookup events

perf_event_inst :: Types.Event -> Text
perf_event_inst = Score.instrument_name . Types.patch_name . Types.event_patch

-- * midi

perform_events :: Cmd.Events -> Cmd.CmdL Perform.MidiEvents
perform_events = PlayUtil.perform_events

-- | This is the local block's performance, and the events are filtered to the
-- selection range, and the filtering is done post-derivation, so they reflect
-- what would actually be played.  I can't filter by selected track because
-- MIDI events don't retain the stack.
sel_midi :: Cmd.CmdL Perform.MidiEvents
sel_midi = get_sel_midi False

root_sel_midi :: Cmd.CmdL Perform.MidiEvents
root_sel_midi = get_sel_midi True

get_sel_midi :: Bool -> Cmd.CmdL Perform.MidiEvents
get_sel_midi from_root = do
    (block_id, start, end) <-
        if from_root then Selection.realtime else Selection.local_realtime
    events <- block_midi block_id
    return $ in_range Midi.wmsg_ts start end events

-- | Get all logs whose 'Log.msg_text' matches a regex.
logs_like :: BlockId -> String -> Cmd.CmdL [Log.Msg]
logs_like block_id regex = do
    logs <- LEvent.logs_of <$> block_midi block_id
    let reg = Regex.compileUnsafe "LPerf.logs_like" regex
    return $ filter (Regex.matches reg . Log.msg_text) logs

-- | Get logs that include a stack frame that matches the given block, tracks,
-- and range.
logs_matching :: BlockId -> BlockId -> [TrackId] -> TrackTime -> TrackTime
    -> Cmd.CmdL [Log.Msg]
logs_matching perf_block block_id track_ids start end = do
    logs <- LEvent.logs_of <$> block_midi perf_block
    let pattern = (Just block_id, Just (Set.fromList track_ids),
            Just (start, end))
        match = maybe False (Stack.match pattern) . Log.msg_stack
    return $ filter match logs

play_midi :: Perform.MidiEvents -> Cmd.CmdL ()
play_midi msgs = do
    let status = Cmd.PlayMidi $ Cmd.PlayMidiArgs Nothing "repl" (to_zero msgs)
            Nothing Nothing
    Cmd.modify $ \st -> st { Cmd.state_repl_status = status }
    where
    to_zero msgs = PlayUtil.shift_messages 1 (PlayUtil.first_time msgs) msgs

-- ** extract

with_chans :: [Midi.Channel] -> [Midi.WriteMessage] -> [Midi.WriteMessage]
with_chans chans = filter $ (`elem` map Just chans) . Midi.message_channel
    . Midi.wmsg_msg

-- | Reduce MIDI to an easier to read form.
simple_midi :: [Midi.WriteMessage] -> [(RealTime, Midi.Message)]
simple_midi = map $ \wmsg -> (Midi.wmsg_ts wmsg, Midi.wmsg_msg wmsg)


-- ** cache contents

get_cache :: Cmd.M m => BlockId -> m (Map.Map Stack.Stack Derive.Cached)
get_cache block_id = do
    Derive.Cache cache <- Cmd.perf_derive_cache <$>
        Cmd.get_performance block_id
    return cache

get_cache_events :: (Cache.Cacheable d, Cmd.M m) => BlockId
    -> m (Map.Map Stack.Stack [LEvent.LEvent d])
get_cache_events block_id = do
    cache <- get_cache block_id
    return $ fmap Stream.to_list $ Map.mapMaybe get cache
    where
    get Derive.Invalid = Nothing
    get (Derive.Cached c) = case Cache.from_cache_entry c of
        Nothing -> Nothing
        Just (Derive.CallType _ events) -> Just events

show_cache :: Cmd.M m => BlockId -> m Text
show_cache block_id = do
    perf <- Cmd.get_performance block_id
    return $ Text.unlines (pretty_cache (Cmd.perf_derive_cache perf))

pretty_cache :: Derive.Cache -> [Text]
pretty_cache (Derive.Cache cache) =
    [Stack.pretty_ui_ stack <> ": " <> pretty_cached cached
        | (stack, cached) <- Map.toAscList cache]
    where
    pretty_cached Derive.Invalid = "Invalid"
    pretty_cached (Derive.Cached entry) =
        showt (entry_events entry) <> " events"

entry_events :: Derive.CacheEntry -> Int
entry_events entry = case entry of
    Derive.CachedEvents (Derive.CallType _ events) -> Stream.length events
    Derive.CachedControl (Derive.CallType _ events) -> Stream.length events
    Derive.CachedPitch (Derive.CallType _ events) -> Stream.length events


-- * pitches

type Ratio = Ratio.Rational

-- | A hook for 'Cmd.hooks_selection'.
chord_hook :: [(ViewId, Maybe Cmd.TrackSelection)] -> Cmd.CmdId ()
chord_hook = mapM_ (uncurry set_chord_status)

-- | Show chord ratios at current selection.
chord :: Cmd.CmdL Text
chord = do
    (view_id, sel) <- Selection.get
    block_id <- State.block_id_of view_id
    maybe_track_id <- State.event_track_at block_id (Selection.point_track sel)
    show_chord <$> chord_at block_id maybe_track_id (Selection.point sel)

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

set_chord_status :: Cmd.M m => ViewId -> Maybe Cmd.TrackSelection -> m ()
set_chord_status view_id maybe_sel = case maybe_sel of
    Nothing -> set Nothing
    Just (sel, block_id, maybe_track_id) ->
        set . Just . show_chord
            =<< chord_at block_id maybe_track_id (Selection.point sel)
    where set = Cmd.set_view_status view_id Config.status_chord

-- | Show the ratios of the frequencies of the notes at the time of the current
-- selection.  They are sorted by their track-order, and the track with the
-- selection is considered unity.
chord_at :: Cmd.M m => BlockId -> Maybe TrackId -> ScoreTime
    -> m [(Pitch.NoteNumber, Pitch.Note, Ratio)]
chord_at block_id maybe_track_id pos = do
    perf <- Perf.get_root
    pos <- Perf.get_realtime perf block_id maybe_track_id pos
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
sort_by_track :: State.M m => BlockId -> [Score.Event] -> m [Score.Event]
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
