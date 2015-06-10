-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert (convert, pitch_to_lily, quantize) where
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (throw)
import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Types as Types

import Global
import Types


-- * convert

type ConvertT a = ConvertUtil.ConvertT () a

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Types.Config -> [Score.Event] -> [LEvent.LEvent Types.Event]
convert config =
    ConvertUtil.convert () (convert_event quarter) . want_instrument
    where
    quarter = Types.config_quarter_duration config
    want_instrument
        | null (Types.config_staves config) = id
        | otherwise = filter ((`Set.member` insts) . Score.event_instrument)
        where
        insts = Set.fromList $ Constants.ly_global : [inst | (inst, staff)
            <- Types.config_staves config, Types.staff_display staff]

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
convert_event :: RealTime -> Score.Event -> ConvertT Types.Event
convert_event quarter event = do
    let dur = Types.real_to_time quarter (Score.event_duration event)
    maybe_pitch <- convert_pitch event
    pitch <- case (dur, maybe_pitch) of
        (0, _) -> check_0dur >> return ""
        (_, Nothing)
            | not (has_prepend || has_append) ->
                throw "event with non-zero duration and no code requires pitch"
            | otherwise -> return ""
        (_, Just pitch) -> return pitch
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
    return converted
    where
    check_0dur
        | not is_ly_global && not has_prepend && not has_append = throw $
            "zero duration event must have one of "
            <> Text.intercalate ", " (map ShowVal.show_val code_attrs)
            <> "; had " <> pretty (Score.event_environ event)
        | has_prepend && has_append = throw
            "zero duration event with both prepend and append is ambiguous"
        | otherwise = return ()
    is_ly_global = Score.event_instrument event == Constants.ly_global
    has_prepend = has Constants.v_ly_prepend
    has_append = has Constants.v_ly_append_all
    has v = Maybe.isJust $
        TrackLang.lookup_val v (Score.event_environ event)
    code_attrs = [Constants.v_ly_prepend, Constants.v_ly_append_all]

type Pitch = Text

convert_pitch :: Score.Event -> ConvertT (Maybe Pitch)
convert_pitch event = case Score.initial_pitch event of
    Nothing -> return Nothing
    Just pitch -> either (throw . ("convert_pitch: "<>)) (return . Just) $
        pitch_to_lily pitch

-- * util

-- | If it's @*twelve@ then use pitch_note, else use pitch_nn and pick the
-- closest pitch.
pitch_to_lily :: PSignal.Transposed -> Either Text Pitch
pitch_to_lily pitch
    | PSignal.pitch_scale_id pitch == Twelve.scale_id = do
        note <- first showt $ PSignal.pitch_note pitch
        show_note note
    | otherwise = do
        nn <- first showt $ PSignal.pitch_nn pitch
        note <- require ("nn out of range: " <> pretty nn) $
            Twelve.nn_to_note nn
        show_note note
    where
    show_note note = do
        pitch <- require ("unparseable note: " <> pretty note) $
            Twelve.read_absolute_pitch note
        Types.show_pitch pitch
    require msg Nothing = Left msg
    require _ (Just x) = Right x

quantize :: Types.Duration -> [Types.Event] -> [Types.Event]
quantize dur = map $ \e -> e
    { Types.event_start = q (Types.event_start e)
    , Types.event_duration = q (Types.event_duration e)
    }
    where q = quantize_time (Types.dur_to_time dur)

quantize_time :: Types.Time -> Types.Time -> Types.Time
quantize_time time t =
    round (fromIntegral t / fromIntegral time :: Double) * time
