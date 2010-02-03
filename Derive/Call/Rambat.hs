module Derive.Call.Rambat where

import Ui

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (optional, signal)
import qualified Derive.Call as Call

import qualified Perform.Signal as Signal


note_calls :: Derive.CallMap
note_calls = Derive.make_calls
    [ ("tick", c_tick)
    ]

-- | Insert an intermediate grace note in the rambat style.  The grace note
-- precedes the following note, and is one step above or one step below
-- depending on the preceding note.
--
-- TODO damping
--
-- [@time@ /signal/ @%tick-time,.2@] Time from the grace note to the following
-- note.  This is in absolute time, but will be halfway between the previous
-- and next note if there isn't this much time.
--
-- [@vel@ /signal/ @%tick-velocity,.3@] Grace note velocity will be this
-- percentage of the following note.
c_tick :: Derive.Call
c_tick = Derive.generate_one $ \args prev (pos, _) next -> TrackLang.call2 args
    ( optional "time" (signal 0.15 "tick-time")
    , optional "vel" (signal 0.5 "tick-velocity")) $
    \time vel -> case (prev, next) of
        ((ppos, _) : _, (npos, _) : _) ->
            Call.with_signals pos [time, vel] $ \[time, vel] ->
                tick (Signal.y_to_x time) vel ppos npos
        _ -> do
            Derive.throw $ "no "
                ++ (if null prev then "previous" else "next") ++ " event"
            return []

tick :: TrackPos -> Signal.Y -> TrackPos -> TrackPos -> Derive.EventDeriver
tick time vel prev next = do
    prev_pitch <- Derive.pitch_degree_at prev
    next_pitch <- Derive.pitch_degree_at next
    next_vel <- Derive.velocity_at next
    let transpose = if prev_pitch <= next_pitch then -1 else 1
    (start, dur) <- stretch prev next time
    Derive.with_constant_pitch (next_pitch + transpose) $
        Derive.with_velocity (Signal.constant (next_vel * vel)) $
        Call.eval_one "tick" start dur [TrackLang.call ""]

-- TODO if I need to do more note shifting and placing, I could dream up some
-- sort of constraint language like TeX's notion of stretchiness
stretch :: TrackPos -> TrackPos -> TrackPos
    -> Derive.Deriver (TrackPos, TrackPos)
stretch prev next offset = do
    gprev <- Derive.local_to_global prev
    gnext <- Derive.local_to_global next
    -- Try to use the offset, but use the midpoint if there isn't room.
    let posg = max (gnext - offset) ((gprev + gnext) / 2)
    pos <- Derive.global_to_local posg
    return (pos, next - pos)
