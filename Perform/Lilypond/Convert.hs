-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert where
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (require, throw)
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch

import Types


-- * convert

type ConvertT a = ConvertUtil.ConvertT () a

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: RealTime -- ^ this length of time becomes a quarter note
    -> Derive.Events -> [LEvent.LEvent Lilypond.Event]
convert quarter = ConvertUtil.convert () (convert_event quarter)

-- | Normally events have a duration and a pitch, and the lilypond performer
-- converts this into a normal lilypond note.  However, the deriver can emit
-- lilypond code directly with either a zero duration event or one without
-- a pitch:
--
-- - If the event has 0 duration, it must have prepend or append code or have
-- the magic 'Lilypond.ly_global' instrument.  The code will go before or after
-- other events at the same time.  Any pitch is ignored.
--
-- - If it doesn't have a pitch, then it must have prepend or append code.
-- prepend ++ append will be emitted and considered to have the given amount of
-- duration.
--
-- - If the event has a pitch it will be emitted as a note, with optional
-- prepended or appended code.
convert_event :: RealTime -> Score.Event -> ConvertT Lilypond.Event
convert_event quarter event = do
    let dur = Lilypond.real_to_time quarter (Score.event_duration event)
    maybe_pitch <- convert_pitch (Score.event_start event)
        (Score.event_controls event) (Score.event_pitch event)
    pitch <- case (dur, maybe_pitch) of
        (0, _) -> check_0dur >> return ""
        (_, Nothing)
            | not (has_prepend || has_append) ->
                throw "event with non-zero duration and no code requires pitch"
            | otherwise -> return ""
        (_, Just pitch) -> either (throw . ("show_pitch: "<>)) return
            (Lilypond.show_pitch pitch)
    return $ Lilypond.Event
        { Lilypond.event_start =
            Lilypond.real_to_time quarter (Score.event_start event)
        , Lilypond.event_duration =
            Lilypond.real_to_time quarter (Score.event_duration event)
        , Lilypond.event_pitch = pitch
        , Lilypond.event_instrument = Score.event_instrument event
        , Lilypond.event_environ = Score.event_environ event
        , Lilypond.event_stack = Score.event_stack event
        , Lilypond.event_clipped = False
        }
    where
    check_0dur
        | not is_ly_global && not has_prepend && not has_append = throw $
            "zero duration event must have one of "
            <> Seq.join ", " (map ShowVal.show_val code_attrs)
            <> "; had " <> Pretty.pretty (Score.event_environ event)
        | has_prepend && has_append = throw $
            "zero duration event with both prepend and append is ambiguous"
        | otherwise = return ()
    is_ly_global = Score.event_instrument event == Lilypond.ly_global
    has_prepend = has Lilypond.v_ly_prepend
    has_append = has Lilypond.v_ly_append_all
    has v = Maybe.isJust $
        TrackLang.lookup_val v (Score.event_environ event)
    code_attrs = [Lilypond.v_ly_prepend, Lilypond.v_ly_append_all]

convert_pitch :: RealTime -> Score.ControlMap -> PitchSignal.Signal
    -> ConvertT (Maybe Theory.Pitch)
convert_pitch start controls psig = case PitchSignal.at start psig of
    Nothing -> return Nothing
    Just pitch -> Just <$> go pitch
    where
    go pitch = do
        note <- either (throw . ("convert_pitch: "++) . show) return $
            PitchSignal.pitch_note $
                PitchSignal.apply (PitchSignal.controls_at start controls) pitch
        require ("parseable note: " ++ Pretty.pretty note) $
            Theory.parse_pitch (Pitch.note_text note)

-- * util

quantize :: Lilypond.Duration -> [Lilypond.Event] -> [Lilypond.Event]
quantize dur = map $ \e -> e
    { Lilypond.event_start = q (Lilypond.event_start e)
    , Lilypond.event_duration = q (Lilypond.event_duration e)
    }
    where q = quantize_time (Lilypond.dur_to_time dur)

quantize_time :: Lilypond.Time -> Lilypond.Time -> Lilypond.Time
quantize_time time t =
    round (fromIntegral t / fromIntegral time :: Double) * time
