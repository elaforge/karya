-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert where
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score
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

convert_event :: RealTime -> Score.Event -> ConvertT Lilypond.Event
convert_event quarter event = do
    maybe_pitch <- convert_pitch (Score.event_start event)
        (Score.event_controls event) (Score.event_pitch event)
    let env = Score.event_environ event
    pitch <- case maybe_pitch of
        Nothing
            | Maybe.isNothing $ TrackLang.lookup_val Lilypond.v_ly_code env ->
                throw $ "event without pitch requires "
                    <> Pretty.pretty Lilypond.v_ly_code
            | otherwise -> return "nopitch" -- I trust lilypond won't see this
        Just pitch -> either (throw . ("show_pitch: "<>)) return
            (Lilypond.show_pitch pitch)
    return $ Lilypond.Event
        { Lilypond.event_start =
            Lilypond.real_to_time quarter (Score.event_start event)
        , Lilypond.event_duration =
            Lilypond.real_to_time quarter (Score.event_duration event)
        , Lilypond.event_pitch = pitch
        , Lilypond.event_instrument = Score.event_instrument event
        , Lilypond.event_dynamic = Score.initial_dynamic event
        , Lilypond.event_environ = env
        , Lilypond.event_stack = Score.event_stack event
        }

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

