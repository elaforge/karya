-- | Convert Derive.Score output into Lilypond.Events.
module Perform.Lilypond.Convert where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.TwelveUtil as TwelveUtil
import qualified Derive.Score as Score

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (require)
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import Types


type ConvertT a = ConvertUtil.ConvertT () a

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lilypond.Duration -- ^ 1 second becomes this Duration, e.g. 4 means
    -- one second is a quarter note
    -> Derive.Events -> [LEvent.LEvent Lilypond.Event]
convert dur1 = ConvertUtil.convert () (convert_event dur1)

convert_event :: Lilypond.Duration -> Score.Event -> ConvertT Lilypond.Event
convert_event dur1 event = do
    pitch <- convert_pitch (Score.event_start event)
        (Score.event_controls event) (Score.event_pitch event)
    pitch <- either (ConvertUtil.throw . ("show_pitch: " ++)) return
        (Lilypond.show_pitch pitch)
    return $ Lilypond.Event
        { Lilypond.event_start =
            real_to_time dur1 (Score.event_start event)
        , Lilypond.event_duration =
            real_to_time dur1 (Score.event_duration event)
        , Lilypond.event_pitch = pitch
        }

-- TODO This loses the enharmonics.
-- I need to record pitch signals as Strings and not PitchSignal.  This is
-- appropriate for e.g. trill, which shouldn't actually trill the note.
-- I still need the pitch signal in case other things depend on it, but I
-- should record the text of the pitch along with the Score.Event.
-- So events can get metadata too, and 'ly.pitch' holds the text of the pitch.
convert_pitch :: RealTime -> Score.ControlMap -> PitchSignal.Signal
    -> ConvertT Theory.Pitch
convert_pitch start controls psig = do
    when (PitchSignal.sig_scale_id psig /= Twelve.scale_id) $
        ConvertUtil.throw $ "scale must be " ++ Pretty.pretty Twelve.scale_id
            ++ ": " ++ Pretty.pretty (PitchSignal.sig_scale_id psig)
    pitch <- require "pitch" $ PitchSignal.at start psig
    nn <- either (ConvertUtil.throw . ("convert_pitch: "++) . show) return $
        PitchSignal.pitch_nn $
            PitchSignal.apply (PitchSignal.controls_at start controls) pitch
    require "pitch in range" $
        Map.lookup (Pitch.Degree (floor nn)) degree_to_pitch

degree_to_pitch :: Map.Map Pitch.Degree Theory.Pitch
degree_to_pitch =
    Map.fromList $ Maybe.mapMaybe (Seq.minimum_on (simplicity . snd)) $
        Seq.group_on fst $ map reverse $ Map.elems $
        TwelveUtil.sys_note_to_degree Twelve.system
    where
    reverse (a, b) = (b, a)
    simplicity pitch = (accs < 0, abs accs)
        where accs = Theory.note_accidentals (Theory.pitch_note pitch)

real_to_time :: Lilypond.Duration -> RealTime -> Lilypond.Time
real_to_time (Lilypond.Duration dur1) real = Lilypond.Time $ floor $
    RealTime.to_seconds real * (fromIntegral whole / fromIntegral dur1)
    where Lilypond.Time whole = Lilypond.time_per_whole
