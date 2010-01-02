-- | A collection of basic calls.
module Derive.Call.Basic where

import Ui
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (optional, required_signal, signal)
import qualified Derive.Score as Score
import qualified Derive.Call as Call

import qualified Perform.Signal as Signal


note_calls :: Derive.CallMap
note_calls = Derive.make_calls
    [ ("delay", c_delay)
    , ("echo", c_echo)
    ]

control_calls :: Derive.CallMap
control_calls = Derive.make_calls []

-- * note calls

c_delay :: Derive.Call
c_delay args events = TrackLang.call1 args
    (optional "time" (required_signal "delay-time")) $ \time ->
    Call.map_asc events () $ Call.with_signals [time] $
    \[time] (_, _, event, _) ->
        return ((), [Score.move (+ TrackPos time) event])

-- TODO If the feedback signal is constant, as it is likely to be, then I could
-- preserve sharing by reusing the velocity of the first event.  However, this
-- only works if each event has the same signal, which may not be the case if
-- there is sub-derivation involved.  Signal serial numbers or StablePtr might
-- be a solution, but I think if signals were lazy then only the necessary bit
-- of the signal will be transformed which achieves the same effect.
c_echo :: Derive.Call
c_echo args events = TrackLang.call3 args
    ( optional "delay" (signal 1 "echo-delay")
    , optional "feedback" (signal 0.4 "echo-feedback")
    , optional "times" (signal 1 "echo-times")) $ \delay feedback times ->
    Call.map_any events () $ Call.with_signals [delay, feedback, times] $
    \[delay, feedback, times] (_, _, event, _) ->
        return ((), echo (Signal.y_to_x delay) feedback (floor times) event)

echo :: TrackPos -> Double -> Int -> Score.Event -> [Score.Event]
echo delay feedback times event = event : [modify n event | n <- [1..times]]
    where
    modify n = Score.modify_velocity (*vel)
        . Score.move (+ fromIntegral n * delay)
        where vel = feedback ** (fromIntegral n)
