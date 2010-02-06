-- | Echo and delay oriented calls.
module Derive.Call.Echo where

import Ui

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (optional, required_signal, signal)

import qualified Perform.Signal as Signal


note_calls :: Derive.CallMap
note_calls = Derive.make_calls
    [ ("delay", c_delay)
    , ("echo", c_echo)
    ]

-- * note calls

c_delay :: Derive.Call
c_delay = Derive.transformer $ \args pos deriver -> TrackLang.call1 args
    (optional "time" (required_signal "delay-time")) $ \time ->
    Call.with_signals pos [time] $ \[time] ->
        Derive.d_at (ScoreTime time) deriver

-- | This echo works on Derivers instead of Events, which means that the echoes
-- happen in score time, so they will change tempo with the rest of the score,
-- and the realization may change due to a different velocity.
--
-- TODO implement a event-echo variant that works directly on events for a
-- concrete echo
c_echo :: Derive.Call
c_echo = Derive.transformer $ \args pos deriver -> TrackLang.call3 args
    ( optional "delay" (signal 1 "echo-delay")
    , optional "feedback" (signal 0.4 "echo-feedback")
    , optional "times" (1 :: Double)) $ \delay feedback times ->
    Call.with_signals pos [delay, feedback] $ \[delay, feedback] ->
        echo (Signal.y_to_score delay) feedback (floor times) deriver

echo :: ScoreTime -> Double -> Int -> Derive.EventDeriver -> Derive.EventDeriver
echo delay feedback times deriver
    | times <= 0 = deriver
    | otherwise = Derive.d_merge deriver
        (Derive.d_control_at delay (scale_vel feedback
            (echo delay feedback (times - 1) deriver)))

scale_vel :: Signal.Y -> Derive.EventDeriver -> Derive.EventDeriver
scale_vel d = Derive.with_relative_control
    Score.c_velocity Signal.sig_multiply (Signal.constant d)
