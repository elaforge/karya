-- | Ornaments for gender.  The unique thing about gender technique is the
-- delayed damping, so these calls deal with delayed damping.
module Derive.Call.Gender where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (control, optional, typed_control)
import qualified Derive.Derive as Derive
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("'", c_tick Nothing)
    , ("'^", c_tick (Just (Pitch.Diatonic (-1))))
    , ("'v", c_tick (Just (Pitch.Diatonic 1)))
    ]

-- TODO prev note duration also must be extended!  But to do this I have to
-- either insert a hack where a call can modify previous events, or do a hybrid
-- postproc thing.  E.g. tick creates its note but adds an attr.  Postproc
-- goes and extends the duration of events previous to one with that attr.
c_tick :: Maybe Pitch.Transpose -> Derive.NoteCall
c_tick transpose = Derive.stream_generator "tick"
    ("Insert an intermediate grace note in the \"ngoret\" rambat style."
    <> " The grace note moves up for `'^`, down for `'v`, or is based"
    <> " on the previous note's pitch for `'`."
    ) $ CallSig.call3g
    ( optional "time" (typed_control "ngoret-time" 0.1 Score.Real) $
        "Time between the grace note start and the main note. If there isn't"
        <> " enough room after the previous note, it will be halfway between"
        <> " the previous note and this one."
    , optional "damp" (typed_control "ngoret-damp" 0.5 Score.Real) $
        "Time that the grace note overlaps with this one. So the total"
        <> " duration is time+damp, though it will be clipped to the"
        <> " end of the current note."
    , optional "dyn" (control "ngoret-dyn" 0.75) $
        "The grace note's dyn will be this multiplier of the current dyn."
    ) $ \time damp dyn_scale -> Note.inverting_around (2, 1) $ \args -> do
        start <- Args.real_start args
        transpose <- maybe (infer_transpose args start) return transpose
        time <- Util.real_time =<< Util.time_control_at Util.Real time start
        damp <- Util.time_control_at Util.Real damp start
        dyn_scale <- Util.control_at dyn_scale start
        dyn <- Util.dynamic start

        grace_start <- Derive.score (start - time)
        -- If there isn't room for the grace note, use the midpoint between the
        -- prev note and this one.
        grace_start <- return $ max grace_start $
            maybe 0 ((/2) . (+ Args.start args)) (Args.prev_start args)
        overlap <- Util.duration_from (Args.start args) damp
        let grace_end = min (Args.end args) (Args.start args + overlap)

        pitch <- Derive.require "pitch" =<< Derive.pitch_at start
        Derive.d_place grace_start (grace_end - grace_start)
                (Util.pitched_note (Pitches.transpose transpose pitch)
                    (dyn * dyn_scale))
            <> Derive.d_place (Args.start args) (Args.duration args) Util.note

infer_transpose :: Derive.PassedArgs d -> RealTime
    -> Derive.Deriver Pitch.Transpose
infer_transpose args start = do
    prev <- Derive.real =<< Derive.require "previous event"
        (Args.prev_start args)
    prev_pitch <- Derive.require "previous pitch" =<< Derive.pitch_at prev
    this_pitch <- Derive.require "this pitch" =<< Derive.pitch_at start
    ifM ((<=) <$> Pitches.pitch_nn prev_pitch <*> Pitches.pitch_nn this_pitch)
        (return (Pitch.Diatonic (-1))) (return (Pitch.Diatonic 1))
