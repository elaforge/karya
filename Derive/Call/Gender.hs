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
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("'", c_tick Nothing)
    , ("'^", c_tick (Just (Pitch.Diatonic (-1))))
    , ("'v", c_tick (Just (Pitch.Diatonic 1)))
    , ("realize-damp", c_realize_damp)
    ]

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
    , optional "dyn" (control "ngoret-dyn" 0.75)
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
        grace_start <- return $ case Args.prev_start args of
            Nothing -> grace_start
            Just prev -> max grace_start $ (prev + Args.start args) / 2

        overlap <- Util.duration_from (Args.start args) damp
        let grace_end = min (Args.end args) (Args.start args + overlap)

        pitch <- Derive.require "pitch" =<< Derive.pitch_at start
        Derive.d_place grace_start (grace_end - grace_start)
                (Util.add_attrs damped_tag $
                    Util.pitched_note (Pitches.transpose transpose pitch)
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

c_realize_damp :: Derive.NoteCall
c_realize_damp = Derive.transformer "realize-damp"
    ("Extend the duration of events preceding one with a "
    <> ShowVal.show_val damped_tag <> " to the end of the event with the attr."
    <> " This is because the tick call can't modify its previous note."
    <> " To avoid having to re-extract the next note of a track once they are"
    <> " all mixed together, this should be applied on the lowest level, which"
    <> " means it may be applied redundantly. But it's fast so it should be ok."
    ) $ CallSig.call0t $ \_ deriver -> do
        events <- deriver
        return $ Util.map_around_asc events $ \_prev event next ->
            Score.remove_attributes damped_tag $ case next of
                next : _ | Score.has_attribute damped_tag next ->
                    Score.set_duration
                        (Score.event_end next - Score.event_start event) event
                _ -> event

-- | Mark events that were damped late, and whose previous event should be
-- extended to be damped together.
damped_tag :: Score.Attributes
damped_tag = Score.attr "damped-tag"
