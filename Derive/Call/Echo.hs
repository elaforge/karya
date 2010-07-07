-- | Echo and delay oriented calls.
--
-- TODO implement a event-echo variant that works directly on events for a
-- concrete echo
--
-- TODO echo with RealTime delay
--
-- TODO echo and delay are broken here because they will delay even things that
-- should not be delayed, like a global volume control
module Derive.Call.Echo where

import Ui

import qualified Derive.Call as Call
import Derive.CallSig (optional, required_control, control)
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("delay", c_delay)
    , ("echo", c_echo)
    , ("e-echo", c_event_echo)
    ]

-- * note calls

-- | Simple delay.
--
-- [time /Signal/ @%delay-time@] Delay this much score time.
c_delay :: Derive.NoteCall
c_delay = Derive.transformer "delay" $ \args deriver -> CallSig.call1 args
    (optional "time" (required_control "delay-time")) $ \time ->
    Call.with_controls [time] $ \[time] -> Derive.d_at (ScoreTime time) deriver

-- | This echo works on Derivers instead of Events, which means that the echoes
-- happen in score time, so they will change tempo with the rest of the score,
-- and the realization may change due to a different velocity.
--
-- The controls are only sampled at the beginning of the echo, so you can't
-- vary them over the scope of an echo call like you can with @event-echo@.
-- You would have to wrap every event in an @echo@ for that.
--
-- [delay /Control/ @%echo-delay,1@] Each echo is delayed this long in score
-- time.
--
-- [feedback /Control/ @%echo-feedback,.4@] The dynamics of each echo are
-- multiplied by this amount.
--
-- [times /Control/ @%echo-times,1@] This many echoes, not counting the
-- un-echoed notes.
c_echo :: Derive.NoteCall
c_echo = Derive.transformer "echo" $ \args deriver -> CallSig.call3 args
    ( optional "delay" (control "echo-delay" 1)
    , optional "feedback" (control "echo-feedback" 0.4)
    , optional "times" (control "echo-times" 1)) $ \delay feedback times ->
    Call.with_controls [delay, feedback, times] $ \[delay, feedback, times] ->
        echo (Signal.y_to_score delay) feedback (floor times) deriver

echo :: ScoreTime -> Double -> Int -> Derive.EventDeriver -> Derive.EventDeriver
echo delay feedback times deriver
    | times <= 0 = deriver
    | otherwise = do
        Derive.d_merge deriver
            (Derive.d_control_at delay (Derive.d_at delay (scale_vel feedback
                (echo delay feedback (times - 1) deriver))))

scale_vel :: Signal.Y -> Derive.EventDeriver -> Derive.EventDeriver
scale_vel d = Derive.with_relative_control
    Score.c_velocity Signal.sig_multiply (Signal.constant d)


-- | This echo works directly on Events.
--
-- Args are the same as 'c_echo', except that their signals are sampled at
-- every event, so parameters can vary over the course of the effect.
c_event_echo :: Derive.NoteCall
c_event_echo = Derive.transformer "post echo" $ \args deriver ->
    CallSig.call3 args
    ( optional "delay" (control "echo-delay" 1)
    , optional "feedback" (control "echo-feedback" 0.4)
    , optional "times" (control "echo-times" 1)) $ \delay feedback times -> do
        events <- deriver
        ((), result) <- Call.map_signals
            [delay, feedback, times] [] (\[delay, feedback, times] [] ->
                go delay feedback times) () events
        Call.cue (Derive.merge_asc_events result)
    where
    go delay feedback times () event =
        return ((), echo_event (RealTime delay) feedback (floor times) event)

echo_event :: RealTime -> Double -> Int -> Score.Event -> [Score.Event]
echo_event delay feedback times event = event : map (echo event) [1..times]
    where
    echo event n = Score.modify_velocity (*feedback^n) $ Score.move (+d) event
        where d = delay * RealTime (fromIntegral n)
    -- about efficiency... should be ok with lazy signals?  Or clip signals
    -- on event creation?
