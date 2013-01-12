-- | Echo and delay oriented calls.
--
-- TODO event delay
module Derive.Call.Echo where
import Data.FixedList (Cons(..), Nil(..))

import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Util as Util
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, typed_control, control)
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("d", c_delay)
    , ("echo", c_echo)
    , ("e-echo", c_event_echo)
    ]

-- * note calls

c_delay :: Derive.NoteCall
c_delay = Derive.transformer "delay"
    ("Simple abstract delay. As with `echo`, abstract means it happens in the\
    \ score, so events may not be delayed evenly if the tempo is changing."
    ) $ Sig.callt
    ( defaulted "time" (typed_control "delay-time" 0.1 Score.Real) "Delay time."
    ) $ \time args deriver -> do
        delay <- Util.duration_from (Args.start args)
            =<< Util.time_control_at Util.Real time
            =<< Args.real_start args
        Derive.d_at delay deriver

-- TODO typed delay time
c_echo :: Derive.NoteCall
c_echo = Derive.transformer "echo"
    ("Abstract echo. This means the echoes happen in score time, so they will\
    \ change tempo with the rest of the score, and their derivation may\
    \ change due to different dynamics.\
    \\nThe controls are only sampled at the beginning of the echo,\
    \ so you can't vary them over the scope of the echo like you can\
    \ with `e-echo`."
    ) $ Sig.callt ((,,)
    <$> defaulted "delay" (control "echo-delay" 1) "Delay time."
    <*> defaulted "feedback" (control "echo-feedback" 0.4)
        "The %dyn of each echo are multiplied by this amount."
    <*> defaulted "times" (control "echo-times" 1)
        "Number of echoes, not counting the original."
    ) $ \(delay, feedback, times) args deriver ->
        Util.with_controls args (delay :. feedback :. times :. Nil) $
            \(delay :. feedback :. times :. Nil) ->
                echo (Signal.y_to_score delay) feedback (floor times) deriver

echo :: ScoreTime -> Double -> Int -> Derive.EventDeriver
    -> Derive.EventDeriver
echo delay feedback times deriver
    | times <= 0 = deriver
    | otherwise = deriver <> Derive.shift_control delay (Derive.d_at delay
        (scale_dyn feedback $ echo delay feedback (times - 1) deriver))

scale_dyn :: Signal.Y -> Derive.EventDeriver -> Derive.EventDeriver
scale_dyn = Derive.multiply_control Score.c_dynamic


-- | This echo works directly on Events.
--
-- Args are the same as 'c_echo', except that their signals are sampled at
-- every event, so parameters can vary over the course of the effect.
c_event_echo :: Derive.NoteCall
c_event_echo = Derive.transformer "event echo"
    ("Concrete echo.  All events are delayed by the same amount.  Also, the\
    \ parameter signals are sampled at every event, so they can vary\
    \ over the course of the echo."
    ) $ Sig.callt ((,,)
    <$> defaulted "delay" (control "echo-delay" 1) "Delay time."
    <*> defaulted "feedback" (control "echo-feedback" 0.4)
        "The %dyn of each echo are multiplied by this amount."
    <*> defaulted "times" (control "echo-times" 1)
        "Number of echoes, not counting the original."
    ) $ \(delay, feedback, times) _args ->
        Util.map_controls_asc (delay :. feedback :. times :. Nil) () $
            \(delay :. feedback :. times :. Nil) ->
                go (Score.typed_val delay) (Score.typed_val feedback)
                    (Score.typed_val times)
    where
    go delay feedback times () event = return
        ((), echo_event (RealTime.seconds delay) feedback (floor times) event)

-- TODO this modifies the signals to shift by the given amount of time, which
-- is inefficient if there is a lot of signal data.  I could store a shift
-- with the event or the signals, but I'm not sure that would actually be
-- more efficient unless the signals are shifted more than once.
echo_event :: RealTime -> Double -> Int -> Score.Event -> [Score.Event]
echo_event delay feedback times event = event : map (echo event) [1..times]
    where
    echo event n = Score.modify_dynamic (*feedback^n) $ Score.move (+d) event
        where d = delay * RealTime.seconds (fromIntegral n)
