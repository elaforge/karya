{-# LANGUAGE OverloadedStrings #-}
module Cmd.Integrate.Convert where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.TrackInfo as TrackInfo

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.MidiDb as MidiDb
import Types


-- | A simplified description of a UI track, as collected by
-- "Derive.Call.Integrate".
data Track = Track {
    track_title :: !String
    , track_events :: ![Event.Event]
    } deriving (Show)

-- | (note track, control tracks)
type Tracks = [(Track, [Track])]
type Config = (LookupAttrs, Pitch.ScaleId)
type LookupAttrs = Score.Instrument -> Instrument.AttributeMap

convert :: (Cmd.M m) => BlockId -> Derive.Events -> m Tracks
convert source_block levents = do
    lookup_inst <- Cmd.get_lookup_instrument
    let lookup_attrs = fromMaybe mempty
            . fmap (Instrument.patch_attribute_map . MidiDb.info_patch)
            . lookup_inst
    default_scale_id <- State.get_default State.default_scale
    track_ids <- State.all_track_ids_of source_block
    let tracknums = Map.fromList
            [(track_id, n) | (n, Just track_id) <- zip [0..] track_ids]
    let (events, logs) = LEvent.partition levents
        (tracks, errs) = integrate (lookup_attrs, default_scale_id)
            tracknums events
    mapM_ Log.write (Log.add_prefix "integrate" logs)
    -- If something failed to derive I shouldn't integrate that into the block.
    when (any ((>=Log.Warn) . Log.msg_prio) logs) $
        Cmd.throw "aborting integrate due to warnings"
    unless (null errs) $
        Cmd.throw $ "integrating events: " ++ Seq.join "; " errs
    return tracks

-- | Convert derived score events back into UI events.
--
-- TODO optionally quantize the ui events
integrate :: Config -> Map.Map TrackId TrackNum -> [Score.Event]
    -> (Tracks, [String])
    -- ^ (tracks, errs)
integrate config tracknums =
    Tuple.swap . Seq.partition_either . map (integrate_track config)
    . allocate_tracks tracknums

-- | Allocate the events to separate tracks.
allocate_tracks :: Map.Map TrackId TrackNum -> [Score.Event]
    -> [(TrackKey, [Score.Event])]
allocate_tracks tracknums = concatMap overlap . Seq.keyed_group_on group_key
    where
    overlap (key, events) = map ((,) key) (split_overlapping events)
    -- Sort by tracknum so an integrated block's tracks come out in the same
    -- order as the original.
    group_key :: Score.Event -> TrackKey
    group_key event =
        (tracknum_of =<< track_of event, Score.event_instrument event,
            PitchSignal.sig_scale_id (Score.event_pitch event),
            event_voice event)
    tracknum_of tid = Map.lookup tid tracknums

-- | Split events into separate lists of non-overlapping events.
split_overlapping :: [Score.Event] -> [[Score.Event]]
split_overlapping [] = []
split_overlapping events = track : split_overlapping rest
    where
    -- Go through the track and collect non-overlapping events, then do it
    -- recursively until there are none left.
    (track, rest) = Seq.partition_either (strip events)
    strip [] = []
    strip (event:events) = Left event : map Right overlapping ++ strip rest
        where
        (overlapping, rest) =
            break ((>= Score.event_end event) . Score.event_start) events

event_voice :: Score.Event -> Score.Attributes
event_voice = Score.set_to_attrs
    . Set.intersection (Score.attrs_set Attrs.voices)
    . Score.attrs_set . Score.event_attributes

track_of :: Score.Event -> Maybe TrackId
track_of = Seq.head . mapMaybe Stack.track_of . Stack.innermost
    . Score.event_stack

type TrackKey = (Maybe TrackNum, Score.Instrument, Pitch.ScaleId,
    Score.Attributes)

integrate_track :: Config -> (TrackKey, [Score.Event])
    -> Either String (Track, [Track])
integrate_track (lookup_attrs, default_scale_id)
        ((_, inst, scale_id, _), events) = do
    pitch_track <- if no_pitch_signals events then return []
        else case pitch_events default_scale_id scale_id events of
            (track, []) -> return [track]
            (_, errs) -> Left $ Seq.join "; " errs
    return (note_events inst (lookup_attrs inst) events,
        pitch_track ++ control_events events)

-- ** note

note_events :: Score.Instrument -> Instrument.AttributeMap -> [Score.Event]
    -> Track
note_events inst attr_map events =
    make_track note_title (map (note_event attr_map) events)
    where note_title = TrackInfo.instrument_to_title inst

note_event :: Instrument.AttributeMap -> Score.Event -> Event.Event
note_event attr_map event = ui_event (Score.event_stack event)
    (RealTime.to_score (Score.event_start event))
    (RealTime.to_score (Score.event_duration event))
    (Map.findWithDefault "" (Score.event_attributes event) attr_map)

-- ** pitch

-- | Unlike 'control_events', this only drops dups that occur within the same
-- event.  This is because it's more normal to think of each note as
-- establishing a new pitch, even if it's the same as the last one.
pitch_events :: Pitch.ScaleId -> Pitch.ScaleId -> [Score.Event]
    -> (Track, [String])
pitch_events default_scale_id scale_id events =
    (make_track pitch_title (tidy_pitches ui_events), concat errs)
    where
    pitch_title = TrackInfo.scale_to_title $
        if scale_id == default_scale_id then Pitch.empty_scale else scale_id
    (ui_events, errs) = unzip $ map pitch_signal_events events
    tidy_pitches = clip_to_zero . clip_concat . map drop_dups

no_pitch_signals :: [Score.Event] -> Bool
no_pitch_signals = all (PitchSignal.null . Score.event_pitch)

-- | Convert an event's pitch signal to symbolic note names.  This uses
-- 'PitchSignal.pitch_note', which handles a constant transposition, but not
-- continuous pitch changes (it's not even clear how to spell those).  I could
-- try to convert back from NoteNumbers, but I still have the problem of how
-- to convert the curve back to high level pitches.
pitch_signal_events :: Score.Event -> ([Event.Event], [String])
pitch_signal_events event = (ui_events, pitch_errs)
    where
    (xs, ys) = unzip $ align_pitch_signal (Score.event_start event) $
        Score.event_pitch event
    pitches = zip3 xs ys (map PitchSignal.pitch_note ys)
    pitch_errs =
        [Pretty.pretty x ++ ": converting " ++ Pretty.pretty p ++ " "
                ++ Pretty.pretty err
            | (x, p, Left err) <- pitches]
    ui_events = [ui_event (Score.event_stack event) (RealTime.to_score x) 0
            (Pitch.note_text note)
        | (x, _, Right note) <- pitches]

-- ** control

control_events :: [Score.Event] -> [Track]
control_events events =
    filter (not . empty_track) $ map (control_track events) controls
    where
    controls = List.sort $ Seq.unique $ concatMap
        (map typed_control . Map.toList . Score.event_controls) events
    typed_control (control, sig) = Score.Typed (Score.type_of sig) control

control_track :: [Score.Event] -> Score.Typed Score.Control -> Track
control_track events control =
    make_track (TrackInfo.unparse_typed control) ui_events
    where
    ui_events = drop_dyn $ tidy_controls $
        map (signal_events (Score.typed_val control)) events
    -- Don't emit a dyn track if it's just the default.
    -- TODO generalize this to everything in in Derive.initial_controls
    drop_dyn [event]
        | Score.typed_val control == Score.c_dynamic
            && Event.event_string event == default_dyn = []
    drop_dyn events = events
    default_dyn = ShowVal.show_hex_val Derive.default_dynamic
    tidy_controls = clip_to_zero . drop_dups . clip_concat

signal_events :: Score.Control -> Score.Event -> [Event.Event]
signal_events control event = case Map.lookup control controls of
    Nothing -> []
    Just sig -> [ui_event (Score.event_stack event)
            (RealTime.to_score x) 0 (ShowVal.show_hex_val y)
        | (x, y) <- align_signal start (Score.typed_val sig)]
    where
    controls = Score.event_controls event
    start = Score.event_start event

-- * util

-- | Make sure the first sample of the signal lines up with the start.  This
-- is called after 'Signal.drop_before', which may still emit a sample before
-- the start time.
align_signal :: Signal.X -> Signal.Signal y -> [(Signal.X, Signal.Y)]
align_signal start sig = case Signal.unsignal (Signal.drop_before start sig) of
    [] -> []
    (_, y) : xs -> (start, y) : xs

align_pitch_signal :: Signal.X -> PitchSignal.Signal
    -> [(Signal.X, PitchSignal.Pitch)]
align_pitch_signal start sig =
    case PitchSignal.unsignal (PitchSignal.drop_before start sig) of
        [] -> []
        (_, y) : xs -> (start, y) : xs

event_stack :: Score.Event -> Event.Stack
event_stack event = Event.Stack (Score.event_stack event)
    (RealTime.to_score (Score.event_start event))

ui_event :: Stack.Stack -> ScoreTime -> ScoreTime -> String -> Event.Event
ui_event stack pos dur text =
    Event.set_stack (Event.Stack stack pos) $ Event.event pos dur text

-- | Concatenate the events, dropping ones that are out of order.  The
-- durations are not modified, so they still might overlap in duration, but the
-- start times will be increasing.
clip_concat :: [[Event.Event]] -> [Event.Event]
clip_concat = Seq.drop_with out_of_order . concat
    where out_of_order e1 e2 = Event.start e2 <= Event.start e1

-- | Drop subsequent events with the same text, since those are redundant for
-- controls.
drop_dups :: [Event.Event] -> [Event.Event]
drop_dups = Seq.drop_dups Event.event_string

-- | Drop events before 0, keeping at least one at 0.  Controls can wind up
-- with samples before 0 (e.g. after using 'Derive.Score.move'), but events
-- can't start before 0.
clip_to_zero :: [Event.Event] -> [Event.Event]
clip_to_zero (e1 : rest@(e2 : _))
    | Event.start e1 <= 0 && Event.start e2 <= 0 = clip_to_zero rest
    | otherwise = Event.move (max 0) e1 : rest
clip_to_zero [e] = [Event.move (max 0) e]
clip_to_zero [] = []

make_track :: String -> [Event.Event] -> Track
make_track title events = Track title (Seq.sort_on Event.start events)

empty_track :: Track -> Bool
empty_track (Track _ []) = True
empty_track _ = False
