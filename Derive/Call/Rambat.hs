module Derive.Call.Rambat where

import Ui

import Derive.CallSig (optional, control)
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Call as Call
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("tick", c_tick)
    ]

-- | Insert an intermediate grace note in the rambat style.  The grace note
-- precedes the following note, and is one step above or one step below
-- depending on the preceding note.
--
-- TODO damping
--
-- [time /Control/ @%tick-time,.2@] Time from the grace note to the following
-- note.  This is in absolute time, but will be halfway between the previous
-- and next note if there isn't this much time.
--
-- [vel /Control/ @%tick-velocity,.3@] Grace note velocity will be this
-- percentage of the following note.
c_tick :: Derive.NoteCall
c_tick = Derive.stream_generator "tick" $ \args -> CallSig.call2 args
    ( optional "time" (control "tick-time" 0.15)
    , optional "vel" (control "tick-velocity" 0.5)) $ \time vel ->
    case (Derive.passed_prev_begin args, Derive.passed_next_begin args) of
        (Just ppos, Just npos) ->
            Call.with_controls [time, vel] $ \[time, vel] ->
                tick (Signal.y_to_real time) vel ppos npos
        (Nothing, Just _) -> Derive.throw "no previous event"
        _ -> Derive.throw "no next event"

tick :: RealTime -> Signal.Y -> ScoreTime -> ScoreTime -> Derive.EventDeriver
tick time vel prev next = do
    prev_pitch <- Derive.pitch_degree_at =<< Derive.score_to_real prev
    next_pitch <- Derive.pitch_degree_at =<< Derive.score_to_real next
    next_vel <- Derive.velocity_at next
    let transpose = if prev_pitch <= next_pitch then -1 else 1
    (start, dur) <- stretch prev next time
    Derive.with_constant_pitch Nothing (next_pitch + transpose) $
        Derive.with_velocity (Signal.constant (next_vel * vel)) $
        Call.eval_one start dur [TrackLang.call ""]

-- TODO if I need to do more note shifting and placing, I could dream up some
-- sort of constraint language like TeX's notion of stretchiness
stretch :: ScoreTime -> ScoreTime -> RealTime
    -> Derive.Deriver (ScoreTime, ScoreTime)
stretch prev next offset = do
    real_prev <- Derive.score_to_real prev
    real_next <- Derive.score_to_real next
    -- Try to use the offset, but use the midpoint if there isn't room.
    let real_pos = max (real_next - offset) ((real_prev + real_next) / 2)
    pos <- Derive.real_to_score real_pos
    return (pos, next - pos)
