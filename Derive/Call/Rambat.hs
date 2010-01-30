module Derive.Call.Rambat where

import Ui

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (optional, signal)
import qualified Derive.Score as Score
import qualified Derive.Call as Call

import qualified Perform.Signal as Signal


note_calls :: Derive.CallMap
note_calls = Derive.make_calls []
    -- [ ("tick", c_tick)
    -- ]

control_calls :: Derive.CallMap
control_calls = Derive.make_calls []

{-
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
c_tick args events = TrackLang.call2 args
    ( optional "time" (signal 0.15 "tick-time")
    , optional "vel" (signal 0.5 "tick-velocity")) $ \time vel ->
    Call.map_asc events () $ Call.with_directive_calls ["tick"] $ \_call ->
    Call.with_signals [time, vel] $ \[time, vel] (_, prev, _, next) -> do
        out <- case (prev, next) of
            (p:_, n:_) -> return [tick (Signal.y_to_x time) vel p n]
            _ -> do
                Derive.warn $ "no "
                    ++ (if null prev then "previous" else "next") ++ " event"
                return []
        return ((), out)

-- TODO these calls are in terms of Events, but to be more flexible they should
-- be in terms of derivers.  Then positioning the event is shift and stretch,
-- and a call can be manipulated properly.  Of course in this case, tick should
-- emit a plain note, so I'm not sure how to handle that.  A tick to a sequence
-- clearly shouldn't reproduce the sequence... but derivers that want an event
-- should be able to evaluate the first event out of the deriver, just like
-- nyquist functions can pull out samples.

tick :: TrackPos -> Signal.Y -> Score.Event -> Score.Event -> Score.Event
tick time vel prev next = modify next
    where
    prev_degree = Score.initial_pitch prev
    degree = Score.initial_pitch next
    transpose = if prev_degree <= degree then -1 else 1
    (start, dur) = stretch
        (Score.event_start prev) (Score.event_start next) time
    modify = Score.modify_velocity (*vel) . Score.place start dur
        . Score.transpose transpose

-- TODO if I need to do more note shifting and placing, I could dream up some
-- sort of constraint language like TeX's notion of stretchiness
stretch :: TrackPos -> TrackPos -> TrackPos -> (TrackPos, TrackPos)
stretch start end offset = (pos, end - pos)
    where
    -- Try to use the offset, but use the midpoint if there isn't room.
    pos = max (end - offset) ((start + end) / 2)
-}
