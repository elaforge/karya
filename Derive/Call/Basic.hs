-- | A collection of basic calls.
module Derive.Call.Basic where
import Util.Control

import Ui
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (optional, signal)
import qualified Derive.Score as Score
import qualified Derive.Call as Call

import qualified Perform.Signal as Signal


note_calls :: [(TrackLang.CallId, Derive.Call)]
note_calls = map (first TrackLang.CallId)
    [ ("delay", c_delay)
    , ("echo", c_echo)
    ]

control_calls :: [(TrackLang.CallId, Derive.Call)]
control_calls = []

c_delay :: Derive.Call
c_delay args events = TrackLang.call1 args
    (optional "time" (signal 1 "delay-time")) $ \time ->
    Call.map_events events [time] $ \event [time] ->
        return [Score.move (+ TrackPos time) event]

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
    , optional "times" (signal 1 "echo-times")
    ) $ \a b c ->
    Call.map_events events [a, b, c] $ \event [delay, feedback, times] ->
        return (echo (Signal.y_to_x delay) feedback (floor times) event)

echo :: TrackPos -> Double -> Int -> Score.Event -> [Score.Event]
echo delay feedback times event = [modify n event | n <- [0..times]]
    where
    modify n = Score.modify_signal Score.c_velocity (*feedback)
        . Score.move (+ fromIntegral n * delay)

modify_vel :: Double -> Score.Event -> Score.Event
modify_vel n = Score.modify_signal Score.c_velocity (*n)
