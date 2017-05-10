-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate.Convert where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.Inst as Inst
import Global
import Types


-- | A simplified description of a UI track, as collected by
-- "Derive.Call.Integrate".
data Track = Track {
    track_title :: !Text
    , track_events :: ![Event.Event]
    } deriving (Show)

instance Pretty.Pretty Track where
    format (Track title events) = Pretty.record "Track"
        [ ("title", Pretty.format title)
        , ("events", Pretty.format events)
        ]

-- | (note track, control tracks)
type Tracks = [(Track, [Track])]
type Config = (LookupCall, Pitch.ScaleId)
type LookupCall = Score.Instrument -> Patch.CallMap

convert :: Cmd.M m => BlockId -> Stream.Stream Score.Event -> m Tracks
convert source_block stream = do
    lookup_inst <- Cmd.get_lookup_instrument
    -- TODO CallMap could go in Common but I don't know how much I care about
    -- this feature.
    let lookup_call = maybe mempty Patch.patch_call_map
            . (Inst.inst_midi . Cmd.inst_instrument <=< lookup_inst)
    default_scale_id <- Perf.default_scale_id
    tracknums <- Map.fromList <$> Ui.tracknums_of source_block
    let (events, logs) = Stream.partition stream
        (tracks, errs) = integrate (lookup_call, default_scale_id)
            tracknums events
    mapM_ Log.write $ Log.add_prefix "integrate" logs
    -- If something failed to derive I shouldn't integrate that into the block.
    when (any ((>=Log.Warn) . Log.msg_priority) logs) $
        Cmd.throw "aborting integrate due to warnings"
    unless (null errs) $
        Cmd.throw $ "integrating events: " <> Text.intercalate "; " errs
    return tracks

-- | Convert derived score events back into UI events.
--
-- TODO optionally quantize the ui events
integrate :: Config -> Map TrackId TrackNum -> [Score.Event]
    -> (Tracks, [Text])
    -- ^ (tracks, errs)
integrate config tracknums =
    Tuple.swap . Either.partitionEithers . map (integrate_track config)
    . allocate_tracks tracknums

-- | Allocate the events to separate tracks.
allocate_tracks :: Map TrackId TrackNum -> [Score.Event]
    -> [(TrackKey, [Score.Event])]
allocate_tracks tracknums = concatMap overlap . Seq.keyed_group_sort group_key
    where
    overlap (key, events) = map ((,) key) (split_overlapping events)
    -- Sort by tracknum so an integrated block's tracks come out in the same
    -- order as the original.
    group_key :: Score.Event -> TrackKey
    group_key event =
        (tracknum_of =<< track_of event, Score.event_instrument event,
            PSignal.sig_scale_id (Score.event_untransformed_pitch event),
            event_voice event)
    tracknum_of tid = Map.lookup tid tracknums

-- | Split events into separate lists of non-overlapping events.
split_overlapping :: [Score.Event] -> [[Score.Event]]
split_overlapping [] = []
split_overlapping events = track : split_overlapping rest
    where
    -- Go through the track and collect non-overlapping events, then do it
    -- recursively until there are none left.
    (track, rest) = Either.partitionEithers (strip events)
    strip [] = []
    strip (event:events) = Left event : map Right overlapping ++ strip rest
        where
        (overlapping, rest) =
            break ((>= Score.event_end event) . Score.event_start) events

event_voice :: Score.Event -> Voice
event_voice = fromMaybe 0 . Env.maybe_val EnvKey.voice . Score.event_environ

track_of :: Score.Event -> Maybe TrackId
track_of = Seq.head . mapMaybe Stack.track_of . Stack.innermost
    . Score.event_stack

-- | This determines how tracks are split when integration recreates track
-- structure.
type TrackKey = (Maybe TrackNum, Score.Instrument, Pitch.ScaleId, Voice)
type Voice = Int

integrate_track :: Config -> (TrackKey, [Score.Event])
    -> Either Text (Track, [Track])
integrate_track (lookup_call, default_scale_id)
        ((_, inst, scale_id, _), events) = do
    pitch_track <- if no_pitch_signals events then return []
        else case pitch_events default_scale_id scale_id events of
            (track, []) -> return [track]
            (_, errs) -> Left $ Text.intercalate "; " errs
    return (note_events inst (lookup_call inst) events,
        pitch_track ++ control_events events)

-- ** note

note_events :: Score.Instrument -> Patch.CallMap -> [Score.Event]
    -> Track
note_events inst call_map events =
    make_track note_title (map (note_event call_map) events)
    where note_title = ParseTitle.instrument_to_title inst

note_event :: Patch.CallMap -> Score.Event -> Event.Event
note_event call_map event = ui_event (Score.event_stack event)
    (RealTime.to_score (Score.event_start event))
    (RealTime.to_score (Score.event_duration event))
    (Expr.unsym $
        Map.findWithDefault "" (Score.event_attributes event) call_map)

-- ** pitch

-- | Unlike 'control_events', this only drops dups that occur within the same
-- event.  This is because it's more normal to think of each note as
-- establishing a new pitch, even if it's the same as the last one.
pitch_events :: Pitch.ScaleId -> Pitch.ScaleId -> [Score.Event]
    -> (Track, [Text])
pitch_events default_scale_id scale_id events =
    (make_track pitch_title (tidy_pitches ui_events), concat errs)
    where
    pitch_title = ParseTitle.scale_to_title $
        if scale_id == default_scale_id then Pitch.empty_scale else scale_id
    (ui_events, errs) = unzip $ map pitch_signal_events events
    tidy_pitches = clip_to_zero . clip_concat . map drop_dups

no_pitch_signals :: [Score.Event] -> Bool
no_pitch_signals = all (PSignal.null . Score.event_untransformed_pitch)

-- | Convert an event's pitch signal to symbolic note names.  This uses
-- 'PSignal.pitch_note', which handles a constant transposition, but not
-- continuous pitch changes (it's not even clear how to spell those).  I could
-- try to convert back from NoteNumbers, but I still have the problem of how
-- to convert the curve back to high level pitches.
pitch_signal_events :: Score.Event -> ([Event.Event], [Text])
pitch_signal_events event = (ui_events, pitch_errs)
    where
    start = Score.event_start event
    (xs, ys) = unzip $ align_pitch_signal start $
        Score.event_transformed_pitch event
    pitches = zip3 xs ys
        (map (PSignal.pitch_note . Score.apply_controls event start) ys)
    pitch_errs =
        [ pretty x <> ": converting " <> pretty p <> " " <> pretty err
        | (x, p, Left err) <- pitches
        ]
    ui_events =
        [ ui_event (Score.event_stack event) (RealTime.to_score x) 0
            (Pitch.note_text note)
        | (x, _, Right note) <- pitches
        ]

-- ** control

control_events :: [Score.Event] -> [Track]
control_events events =
    filter (not . empty_track) $ map (control_track events) controls
    where
    controls = List.sort $ Seq.unique $ concatMap
        (map typed_control . Map.toList . Score.event_transformed_controls)
        events
    typed_control (control, sig) = Score.Typed (Score.type_of sig) control

control_track :: [Score.Event] -> Score.Typed Score.Control -> Track
control_track events control =
    make_track (ParseTitle.unparse_typed control) ui_events
    where
    ui_events = drop_dyn $ tidy_controls $
        map (signal_events (Score.typed_val control)) events
    -- Don't emit a dyn track if it's just the default.
    -- TODO generalize this to everything in in Derive.initial_controls
    drop_dyn [event]
        | Score.typed_val control == Score.c_dynamic
            && Event.text event == default_dyn = []
    drop_dyn events = events
    default_dyn = ShowVal.show_hex_val Derive.default_dynamic
    tidy_controls = clip_to_zero . drop_dups . clip_concat

signal_events :: Score.Control -> Score.Event -> [Event.Event]
signal_events control event = case Score.event_control control event of
    Nothing -> []
    Just sig -> [ui_event (Score.event_stack event)
            (RealTime.to_score x) 0 (ShowVal.show_hex_val y)
        | (x, y) <- align_signal start (Score.typed_val sig)]
    where start = Score.event_start event

-- * util

-- | Make sure the first sample of the signal lines up with the start.  This
-- is called after 'Signal.drop_before', which may still emit a sample before
-- the start time.
align_signal :: Signal.X -> Signal.Signal y -> [(Signal.X, Signal.Y)]
align_signal start sig = case Signal.unsignal (Signal.drop_before start sig) of
    [] -> []
    (_, y) : xs -> (start, y) : xs

align_pitch_signal :: Signal.X -> PSignal.PSignal -> [(Signal.X, PSignal.Pitch)]
align_pitch_signal start sig =
    case PSignal.unsignal (PSignal.drop_before start sig) of
        [] -> []
        (_, y) : xs -> (start, y) : xs

event_stack :: Score.Event -> Event.Stack
event_stack event = Event.Stack (Score.event_stack event)
    (RealTime.to_score (Score.event_start event))

ui_event :: Stack.Stack -> ScoreTime -> ScoreTime -> Text -> Event.Event
ui_event stack pos dur text =
    Event.stack_ #= Just (Event.Stack stack pos) $ Event.event pos dur text

-- | Concatenate the events, dropping ones that are out of order.  The
-- durations are not modified, so they still might overlap in duration, but the
-- start times will be increasing.
clip_concat :: [[Event.Event]] -> [Event.Event]
clip_concat = Seq.drop_with out_of_order . concat
    where out_of_order e1 e2 = Event.start e2 <= Event.start e1

-- | Drop subsequent events with the same text, since those are redundant for
-- controls.
drop_dups :: [Event.Event] -> [Event.Event]
drop_dups = Seq.drop_dups Event.text

-- | Drop events before 0, keeping at least one at 0.  Controls can wind up
-- with samples before 0 (e.g. after using 'Derive.Score.move'), but events
-- can't start before 0.
clip_to_zero :: [Event.Event] -> [Event.Event]
clip_to_zero (e1 : rest@(e2 : _))
    | Event.start e1 <= 0 && Event.start e2 <= 0 = clip_to_zero rest
    | otherwise = (Event.start_ %= max 0 $ e1) : rest
clip_to_zero [e] = [Event.start_ %= max 0 $ e]
clip_to_zero [] = []

make_track :: Text -> [Event.Event] -> Track
make_track title events = Track title (Seq.sort_on Event.start events)

empty_track :: Track -> Bool
empty_track (Track _ []) = True
empty_track _ = False
