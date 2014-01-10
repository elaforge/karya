-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert where
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (require, throw)
import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Types as Types
import qualified Perform.Pitch as Pitch

import Types


-- * convert

type ConvertT a = ConvertUtil.ConvertT () a

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: RealTime -- ^ this length of time becomes a quarter note
    -> [Score.Event] -> [LEvent.LEvent Types.Event]
convert quarter = ConvertUtil.convert () (convert_event quarter)

-- | Normally events have a duration and a pitch, and the lilypond performer
-- converts this into a normal lilypond note.  However, the deriver can emit
-- lilypond code directly with either a zero duration event or one without
-- a pitch:
--
-- - If the event has 0 duration, it must have prepend or append code or have
-- the magic 'Constants.ly_global' instrument.  The code will go before or after
-- other events at the same time.  Any pitch is ignored.
--
-- - If it doesn't have a pitch, then it must have prepend or append code.
-- prepend ++ append will be emitted and considered to have the given amount of
-- duration.
--
-- - If the event has a pitch it will be emitted as a note, with optional
-- prepended or appended code.
convert_event :: RealTime -> Score.Event -> ConvertT (Types.Event, [a])
convert_event quarter event = do
    let dur = Types.real_to_time quarter (Score.event_duration event)
    maybe_pitch <- convert_pitch event
    pitch <- case (dur, maybe_pitch) of
        (0, _) -> check_0dur >> return ""
        (_, Nothing)
            | not (has_prepend || has_append) ->
                throw "event with non-zero duration and no code requires pitch"
            | otherwise -> return ""
        (_, Just pitch) -> either (throw . ("show_pitch: "<>)) return
            (Types.show_pitch pitch)
    let converted = Types.Event
            { Types.event_start =
                Types.real_to_time quarter (Score.event_start event)
            , Types.event_duration =
                Types.real_to_time quarter (Score.event_duration event)
            , Types.event_pitch = pitch
            , Types.event_instrument = Score.event_instrument event
            , Types.event_environ = Score.event_environ event
            , Types.event_stack = Score.event_stack event
            , Types.event_clipped = False
            }
    return (converted, [])
    where
    check_0dur
        | not is_ly_global && not has_prepend && not has_append = throw $
            "zero duration event must have one of "
            <> Seq.join ", " (map (untxt . ShowVal.show_val) code_attrs)
            <> "; had " <> Pretty.pretty (Score.event_environ event)
        | has_prepend && has_append = throw
            "zero duration event with both prepend and append is ambiguous"
        | otherwise = return ()
    is_ly_global = Score.event_instrument event == Constants.ly_global
    has_prepend = has Constants.v_ly_prepend
    has_append = has Constants.v_ly_append_all
    has v = Maybe.isJust $
        TrackLang.lookup_val v (Score.event_environ event)
    code_attrs = [Constants.v_ly_prepend, Constants.v_ly_append_all]

convert_pitch :: Score.Event -> ConvertT (Maybe Pitch.Pitch)
convert_pitch event = case PitchSignal.at start (Score.event_pitch event) of
    Nothing -> return Nothing
    Just pitch -> Just <$> go pitch
    where
    start = Score.event_start event
    go pitch = do
        note <- either (throw . ("convert_pitch: "++) . show) return $
            PitchSignal.pitch_note $ PitchSignal.apply
                (Score.event_environ event)
                (Score.event_controls_at start event)
                pitch
        require ("parseable note: " ++ Pretty.pretty note) $
            Twelve.read_absolute_pitch note

-- * util

quantize :: Types.Duration -> [Types.Event] -> [Types.Event]
quantize dur = map $ \e -> e
    { Types.event_start = q (Types.event_start e)
    , Types.event_duration = q (Types.event_duration e)
    }
    where q = quantize_time (Types.dur_to_time dur)

quantize_time :: Types.Time -> Types.Time -> Types.Time
quantize_time time t =
    round (fromIntegral t / fromIntegral time :: Double) * time
