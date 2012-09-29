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
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("'", c_tick)
    , ("up", c_neighbor (Pitch.Diatonic (-1)))
    , ("dn", c_neighbor (Pitch.Diatonic 1))
    ]

-- | Insert an intermediate grace note in the rambat style.  The grace note
-- precedes the following note, and is one step above or one step below
-- depending on the preceding note.
--
-- The controls are called @ngoret-whatever@, and all instruments that have
-- variations on the ngoret glissando should use the same control names.
--
-- [time /Control/ @%ngoret-time,.15@] Time from the grace note to the following
-- note.  This is in absolute time, but will be halfway between the previous
-- and next note if there isn't this much time.
--
-- [damp /Control/ @%ngoret-damp,.5s@] Time that the grace note overlaps with
-- the following note.  So the total duration is time+damp, though it will be
-- clipped to the following note's damp time.
--
-- [dyn /Control/ @%ngoret-dynamic,.5@] Grace note dynamic will be this
-- percentage of the following note.
c_tick :: Derive.NoteCall
c_tick = Derive.stream_generator "tick" $ Note.inverting_n 2 $ \args ->
    CallSig.call3 args
    ( optional "time" (control "ngoret-time" 0.15)
    , optional "damp" (control "ngoret-damp" 0.5)
    , optional "dyn" (control "ngoret-dyn" 0.5)
    ) $ \time damp dyn -> do
        prev <- Derive.require "previous event" $ Args.prev_start args
        next <- Derive.require "next event" $ Args.next_start args
        start <- Args.real_start args
        (time, damp, dyn) <- time_damp_dyn start time damp dyn
        tick time damp dyn prev next (Args.next_end args)

tick :: RealTime -> TrackLang.RealOrScore -> Signal.Y
    -> ScoreTime -> ScoreTime -> ScoreTime -> Derive.EventDeriver
tick time damp_dur dyn prev next next_end = do
    prev_pitch <- Derive.require "previous pitch"
        =<< Derive.pitch_at =<< Derive.real prev
    next_pitch <- Derive.require "next pitch"
        =<< Derive.pitch_at =<< Derive.real next
    next_dyn <- Util.dynamic =<< Derive.real next
    neighbor <- ifM
        ((<=) <$> Pitches.pitch_nn prev_pitch <*> Pitches.pitch_nn next_pitch)
        (return (Pitch.Chromatic (-1))) (return (Pitch.Chromatic 1))
    (start, end) <- stretch prev next time
    overlap <- Util.duration_from end damp_dur
    Derive.d_place start (min next_end (end + overlap) - start) $
        Util.pitched_note (Pitches.transpose neighbor next_pitch)
            (next_dyn * dyn)

-- TODO if I need to do more note shifting and placing, I could dream up some
-- sort of constraint language like TeX's notion of stretchiness
stretch :: ScoreTime -> ScoreTime -> RealTime
    -> Derive.Deriver (ScoreTime, ScoreTime)
stretch prev next offset = do
    real_prev <- Derive.real prev
    real_next <- Derive.real next
    -- Try to use the offset, but use the midpoint if there isn't room.
    let real_pos = max (real_next - offset)
            ((real_prev + real_next) `RealTime.div` 2)
    pos <- Derive.score real_pos
    return (pos, next)

time_damp_dyn :: RealTime -> TrackLang.ValControl -> TrackLang.ValControl
    -> TrackLang.ValControl
    -> Derive.Deriver (RealTime, TrackLang.RealOrScore, Signal.Y)
time_damp_dyn start time damp dyn = do
    time <- Util.real_time =<< Util.time_control_at Util.Real time start
    damp <- Util.time_control_at Util.Real damp start
    dyn <- Util.control_at dyn start
    return (time, damp, dyn)


-- * neighbor

-- | Glissando from a neighboring note.
--
-- This has to be a generator, not a transformer, because otherwise the
-- inversion doesn't work out.
--
-- The controls are called @ngoret-whatever@, and all instruments that have
-- variations on the ngoret glissando should use the same control names.
--
-- [time /Control/ @%ngoret-time,.1s@] Grace note falls this much time before
-- the main note.
--
-- [damp /Control/ @%ngoret-damp,.5s@] Time that the grace note overlaps with
-- the main note.  So the total duration is time+damp, though it will be
-- clipped to the main note's damp time.
--
-- [dyn /Control/ @%ngoret-dynamic,.75@] Grace note dynamic will be this
-- percentage of the following note.
c_neighbor :: Pitch.Transpose -> Derive.NoteCall
c_neighbor transpose = Derive.stream_generator "neighbor" $
    Note.inverting $ \args -> CallSig.call3 args
    ( optional "time" (typed_control "ngoret-time" 0.1 Score.Real)
    , optional "damp" (typed_control "ngoret-damp" 0.5 Score.Real)
    , optional "dyn" (control "ngoret-dyn" 0.75)
    ) $ \time damp dyn_scale -> do
        start <- Args.real_start args
        (time, damp, dyn_scale) <- time_damp_dyn start time damp dyn_scale
        grace_start <- Derive.score (start - time)
        overlap <- Util.duration_from (Args.start args) damp
        let grace_end = min (Args.end args) (Args.start args + overlap)
        dyn <- Util.dynamic start

        pitch <- Derive.require "pitch" =<< Derive.pitch_at start
        Derive.d_place grace_start (grace_end - grace_start)
                (Util.pitched_note (Pitches.transpose transpose pitch)
                    (dyn * dyn_scale))
            <> Derive.d_place (Args.start args) (Args.duration args) Util.note
