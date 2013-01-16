-- | Utilities for calls to cooperate with the lilypond backend.
module Derive.Call.Lily where
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import Types


-- | Used to turn RealTime into 'Lilypond.Time'.
newtype RealTimePerQuarter = RealTimePerQuarter RealTime
    deriving (Show)

when_lilypond :: (RealTimePerQuarter -> Derive.Deriver a)
    -- ^ Run if this is a lilypond derive.
    -> Derive.Deriver a -- ^ Run if this is a normal derive.
    -> Derive.Deriver a
when_lilypond lily not_lily =
    Derive.lookup_val TrackLang.v_lilypond_derive >>= \x -> case x of
        Nothing -> not_lily
        Just q -> lily (RealTimePerQuarter q)

append :: Derive.PassedArgs d -> String -> Derive.EventDeriver
    -> Derive.EventDeriver
append args code = when_lilypond $
    const $ append_code code $ Util.place args Util.note

-- * note transformer

-- | Replace a note transformer with one that derives its children as-is,
-- but add lilypond code.
notes_code :: Code -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_code code = notes_with (add_code code)

-- | This is like 'notes_code', but the first event in each track gets the
-- start code, and the last event in each track gets the end code.
notes_around :: Code -> Code -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_around start end args = when_lilypond $
    const $ Note.place $ concat $ map around $
        Note.sub_events args
    where
    around :: [Note.Event] -> [Note.Event]
    around = Seq.first_last (Note.map_event (add_code start))
        (Note.map_event (add_code end))

notes_with :: (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.PassedArgs d
    -> Derive.EventDeriver -> Derive.EventDeriver
notes_with f args = when_lilypond $
    const $ Note.place $ Note.map_events f $ concat (Note.sub_events args)

-- * code

-- | Either prepend or append some code to a lilypond note.
data Code = Prefix String | Suffix String deriving (Show)

prepend_code :: String -> Derive.EventDeriver -> Derive.EventDeriver
prepend_code = add_code . Prefix

append_code :: String -> Derive.EventDeriver -> Derive.EventDeriver
append_code = add_code . Suffix

add_code :: Code -> Derive.EventDeriver -> Derive.EventDeriver
add_code (Prefix code) = Derive.modify_val Lilypond.v_ly_code_prepend $
    (code++) . fromMaybe ""
add_code (Suffix code) = Derive.modify_val Lilypond.v_ly_code_append $
    (++code) . fromMaybe ""

-- | Emit a note that carries raw lilypond code.  The code is emitted
-- literally, and assumed to have the duration of the event.  The event's pitch
-- is ignored.  This can be used to emit lilypond that doesn't fit into
-- a 'Lilypond.Event'.
code :: (ScoreTime, ScoreTime) -> String -> Derive.EventDeriver
code (start, dur) code = Derive.with_val Lilypond.v_ly_code code
    (Derive.d_place start dur Util.note)

-- * convert

-- | Round the RealTime to the nearest NoteDuration.
note_duration :: RealTimePerQuarter -> RealTime -> Lilypond.NoteDuration
note_duration (RealTimePerQuarter q) =
    Lilypond.time_to_note_dur . Lilypond.real_to_time q

-- | Like 'note_duration', but only succeeds if the RealTime is exactly
-- a NoteDuration.
is_note_duration :: RealTimePerQuarter -> RealTime
    -> Maybe Lilypond.NoteDuration
is_note_duration (RealTimePerQuarter q) =
    Lilypond.is_note_dur . Lilypond.real_to_time q

is_duration :: RealTimePerQuarter -> RealTime -> Maybe Lilypond.Duration
is_duration per_quarter t = case is_note_duration per_quarter t of
    Just (Lilypond.NoteDuration dur False) -> Just dur
    _ -> Nothing

note_pitch :: Derive.EventDeriver -> Derive.Deriver String
note_pitch deriver = do
    events <- deriver
    event <- require "had no event" $ Seq.head (LEvent.events_of events)
    pitch <- require "note had no pitch" $ Score.initial_pitch event
    let controls = Score.event_controls_at (Score.event_start event) event
    pitch_to_lily $ PitchSignal.apply controls pitch
    -- Wow, there are a lot of ways to fail.
    where
    require = Derive.require . (prefix <>)
    prefix = "Lily.note_pitch: "

pitch_to_lily :: PitchSignal.Pitch -> Derive.Deriver String
pitch_to_lily pitch = do
    note <- right $ PitchSignal.pitch_note pitch
    pitch <- require ("unparseable note: " <> Pretty.pretty note) $
        Theory.parse_pitch (Pitch.note_text note)
    right $ Lilypond.show_pitch pitch
    where
    require = Derive.require . (prefix <>)
    right :: (Pretty.Pretty a) => Either a b -> Derive.Deriver b
    right = Derive.require_right ((prefix <>) . Pretty.pretty)
    prefix = "Lily.pitch_to_lily: "
