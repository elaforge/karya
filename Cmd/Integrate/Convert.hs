-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | The 'convert' function and support.
module Cmd.Integrate.Convert (
    Track(..), Tracks
    , convert
#ifdef TESTING
    , module Cmd.Integrate.Convert
#endif
) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Instrument.Common as Common
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Event as Event
import qualified Ui.Ui as Ui

import           Global
import           Types


type Error = Text
type Title = Text

-- | A simplified description of a UI track, as collected by
-- "Derive.Call.Integrate".
data Track = Track {
    track_title :: !Title
    , track_events :: ![Event.Event]
    } deriving (Eq, Show)

instance Pretty Track where
    format (Track title events) = Pretty.record "Track"
        [ ("title", Pretty.format title)
        , ("events", Pretty.format events)
        ]

-- | (note track, control tracks)
type Tracks = [(Track, [Track])]
type Config = (LookupCall, Pitch.ScaleId)
type LookupCall = ScoreT.Instrument -> Common.CallMap

-- | Convert 'Score.Event's to 'Tracks'.  This involves splitting overlapping
-- events into tracks, and trying to map low level notation back to high level.
convert :: Cmd.M m => BlockId -> Stream.Stream Score.Event -> m Tracks
convert source_block stream = do
    lookup_inst <- Cmd.get_lookup_instrument
    let lookup_call = maybe mempty (Common.common_call_map . Cmd.inst_common)
            . lookup_inst
    default_scale_id <- Perf.default_scale_id
    tracknums <- Map.fromList <$> Ui.tracknums_of source_block
    let (events, logs) = Stream.partition stream
        (errs, tracks) = integrate (lookup_call, default_scale_id)
            tracknums events
    mapM_ (Log.write . Log.add_prefix "integrate") logs
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
    -> ([Error], Tracks)
integrate config tracknums =
    Either.partitionEithers . map (integrate_track config)
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
        ( tracknum_of =<< track_of event
        , Score.event_instrument event
        , PSignal.sig_scale_id (Score.event_pitch event)
        , event_voice event
        )
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

event_voice :: Score.Event -> Maybe Voice
event_voice = Env.maybe_val EnvKey.voice . Score.event_environ

track_of :: Score.Event -> Maybe TrackId
track_of = Seq.head . mapMaybe Stack.track_of . Stack.innermost
    . Score.event_stack

-- | This determines how tracks are split when integration recreates track
-- structure.
type TrackKey = (Maybe TrackNum, ScoreT.Instrument, Pitch.ScaleId, Maybe Voice)
type Voice = Int

integrate_track :: Config -> (TrackKey, [Score.Event])
    -> Either Error (Track, [Track])
integrate_track (lookup_call, default_scale_id)
        ((_, inst, scale_id, voice), events) = do
    pitch_track <- if no_pitch_signals events then return []
        else case pitch_events default_scale_id scale_id events of
            (track, []) -> return [track]
            (_, errs) -> Left $ Text.intercalate "; " errs
    return
        ( note_events inst voice (lookup_call inst) events
        , pitch_track ++ control_events events
        )

-- ** note

note_events :: ScoreT.Instrument -> Maybe Voice -> Common.CallMap
    -> [Score.Event] -> Track
note_events inst voice call_map events =
    make_track note_title (map (note_event call_map) events)
    where
    note_title = ParseTitle.instrument_to_title inst <> case voice of
        Nothing -> ""
        Just v -> " | v=" <> ShowVal.show_val v

note_event :: Common.CallMap -> Score.Event -> Event.Event
note_event call_map event = ui_event (Score.event_stack event)
    (RealTime.to_score (Score.event_start event))
    (RealTime.to_score (Score.event_duration event))
    (note_call call_map event)

note_call :: Common.CallMap -> Score.Event -> Text
note_call call_map event = Texts.join2
    (maybe "" Expr.unsym (Map.lookup attrs call_map))
    -- Append flags to help with debugging.  The presence of a flag probably
    -- means some postproc step wasn't applied.
    (if flags == mempty then "" else " -- " <> pretty flags)
    where
    attrs = Score.event_attributes event
    flags = Score.event_flags event


-- ** pitch

-- | Unlike 'control_events', this only drops dups that occur within the same
-- event.  This is because it's more normal to think of each note as
-- establishing a new pitch, even if it's the same as the last one.
pitch_events :: Pitch.ScaleId -> Pitch.ScaleId -> [Score.Event]
    -> (Track, [Error])
pitch_events default_scale_id scale_id events =
    (make_track pitch_title (tidy_pitches ui_events), concat errs)
    where
    pitch_title = ParseTitle.scale_to_title $
        if scale_id == default_scale_id then Pitch.empty_scale else scale_id
    (ui_events, errs) = unzip $ map pitch_signal_events events
    tidy_pitches = clip_to_zero . clip_concat . map drop_dups

no_pitch_signals :: [Score.Event] -> Bool
no_pitch_signals = all (PSignal.null . Score.event_pitch)

-- | Convert an event's pitch signal to symbolic note names.  This uses
-- 'PSignal.pitch_note', which handles a constant transposition, but not
-- continuous pitch changes (it's not even clear how to spell those).  I could
-- try to convert back from NoteNumbers, but I still have the problem of how
-- to convert the curve back to high level pitches.
pitch_signal_events :: Score.Event -> ([Event.Event], [Error])
pitch_signal_events event = (ui_events, pitch_errs)
    where
    start = Score.event_start event
    (xs, ys) = unzip $ PSignal.to_pairs $ PSignal.clip_before start $
        Score.event_pitch event
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
        (map typed_control . Map.toList . Score.event_controls)
        events
    typed_control (control, sig) = ScoreT.Typed (ScoreT.type_of sig) control

control_track :: [Score.Event] -> ScoreT.Typed ScoreT.Control -> Track
control_track events control =
    make_track (ParseTitle.control_to_title control) ui_events
    where
    ui_events = drop_dyn $ tidy_controls $
        map (signal_events (ScoreT.typed_val control)) events
    -- Don't emit a dyn track if it's just the default.
    -- TODO generalize this to everything in in Derive.initial_controls
    drop_dyn [event]
        | ScoreT.typed_val control == Controls.dynamic
            && Event.text event == default_dyn = []
    drop_dyn events = events
    default_dyn = ShowVal.show_hex_val Derive.default_dynamic
    tidy_controls = clip_to_zero . drop_dups . clip_concat

signal_events :: ScoreT.Control -> Score.Event -> [Event.Event]
signal_events control event = case Score.event_control control event of
    Nothing -> []
    Just sig ->
        [ ui_event (Score.event_stack event)
            (RealTime.to_score x) 0 (ShowVal.show_hex_val y)
        | (x, y) <- Signal.to_pairs $
            Signal.clip_before start (ScoreT.typed_val sig)
        ]
    where start = Score.event_start event

-- * util

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

make_track :: Title -> [Event.Event] -> Track
make_track title events = Track title (Seq.sort_on Event.start events)

empty_track :: Track -> Bool
empty_track (Track _ []) = True
empty_track _ = False
