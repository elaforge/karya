-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Echo and delay oriented calls.
--
-- TODO event delay
module Derive.Call.Echo where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, typed_control, control)

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("d", c_delay)
    , ("echo", c_echo)
    , ("e-echo", c_event_echo)
    ]

-- * note calls

c_delay :: Derive.Transformer Derive.Note
c_delay = Derive.transformer Module.prelude "delay" Tags.ly
    ("Simple abstract delay. As with `echo`, abstract means it happens in the\
    \ score, so events may not be delayed evenly if the tempo is changing."
    ) $ Sig.callt
    ( defaulted "time" (typed_control "delay-time" 0.1 Score.Real) "Delay time."
    ) $ \time args deriver -> Lily.when_lilypond deriver $ do
        start <- Args.real_start args
        delay <- Util.score_duration start
            =<< Util.time_control_at Util.Real time start
        Derive.at delay deriver

-- TODO typed delay time
c_echo :: Derive.Transformer Derive.Note
c_echo = Derive.transformer Module.prelude "echo" mempty
    ("Abstract echo. This means the echoes happen in score time, so they will\
    \ change tempo with the rest of the score, and their derivation may\
    \ change due to different dynamics.\
    \\nThe controls are only sampled at the beginning of the echo,\
    \ so you can't vary them over the scope of the echo like you can\
    \ with `e-echo`."
    ) $ Sig.callt ((,,)
    <$> defaulted "delay" (control "echo-delay" 1) "Delay time."
    <*> defaulted "feedback" (control "echo-feedback" 0.4)
        "The %dyn of each echo is multiplied by this amount."
    <*> defaulted "times" (control "echo-times" 1)
        "Number of echoes, not counting the original."
    ) $ \(delay, feedback, times) args deriver -> do
        now <- Args.real_start args
        delay <- Signal.y_to_score <$> Util.control_at delay now
        feedback <- Util.control_at feedback now
        times <- floor <$> Util.control_at times now
        echo delay feedback times deriver

echo :: ScoreTime -> Double -> Int -> Derive.NoteDeriver -> Derive.NoteDeriver
echo delay feedback times deriver
    | times <= 0 = deriver
    | otherwise = deriver <> Derive.shift_control delay (Derive.at delay
        (scale_dyn feedback $ echo delay feedback (times - 1) deriver))

scale_dyn :: Signal.Y -> Derive.NoteDeriver -> Derive.NoteDeriver
scale_dyn = Derive.multiply_control Score.c_dynamic


-- | This echo works directly on Events.
--
-- Args are the same as 'c_echo', except that their signals are sampled at
-- every event, so parameters can vary over the course of the effect.
c_event_echo :: Derive.Transformer Derive.Note
c_event_echo = Derive.transformer Module.prelude "event echo" Tags.postproc
    ("Concrete echo.  All events are delayed by the same amount.  Also, the\
    \ parameter signals are sampled at every event, so they can vary\
    \ over the course of the echo."
    ) $ Sig.callt ((,,)
    <$> defaulted "delay" (control "echo-delay" 1) "Delay time."
    <*> defaulted "feedback" (control "echo-feedback" 0.4)
        "The %dyn of each echo is multiplied by this amount."
    <*> defaulted "times" (control "echo-times" 1)
        "Number of echoes, not counting the original."
    ) $ \(delay, feedback, times) _args deriver -> do
        events <- deriver
        delay <- Post.time_control delay events
        feedback <- Post.control Score.typed_val feedback events
        times <- Post.control (floor . Score.typed_val) times events
        return $ Post.emap_ (Post.uncurry4 echo_event)
            (LEvent.zip4 delay feedback times events)

-- TODO this modifies the signals to shift by the given amount of time, which
-- is inefficient if there is a lot of signal data.  I could store a shift
-- with the event or the signals, but I'm not sure that would actually be
-- more efficient unless the signals are shifted more than once.
echo_event :: RealTime -> Double -> Int -> Score.Event -> [Score.Event]
echo_event delay feedback times event = event : map (echo event) [1..times]
    where
    echo event n = Score.modify_dynamic (*feedback^n) $ Score.move (+d) event
        where d = delay * RealTime.seconds (fromIntegral n)
