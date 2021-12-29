-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Echo and delay oriented calls.
--
-- TODO event delay
module Derive.C.Prelude.Delay (library) where
import qualified Util.Control as Control
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Types


library :: Library.Library
library = Library.transformers
    [ ("d", c_delay)
    , ("echo", c_echo)
    , ("e-echo", c_event_echo)
    ]
    <> Library.transformers
    [ ("d", c_delay_c :: Derive.Transformer Derive.Control)
    ]
    <> Library.transformers
    [ ("d", c_delay_c :: Derive.Transformer Derive.Pitch)
    ]

-- * note calls

c_delay :: Derive.Transformer Derive.Note
c_delay = Derive.transformer Module.prelude "delay" Tags.ly
    ("Simple abstract delay. As with `echo`, abstract means it happens in the\
    \ score, so events may not be delayed evenly if the tempo is changing."
    ) $ Sig.callt
    ( Sig.defaulted "time" (Sig.typed_control "delay-time" 0.1 ScoreT.Real)
        "Delay time."
    ) $ \time args deriver -> Ly.when_lilypond deriver $ do
        start <- Args.real_start args
        delay <- Call.score_duration start
            =<< Call.time_control_at Typecheck.Real time start
        Derive.at delay deriver

c_delay_c :: Derive.Taggable a => Derive.Transformer a
c_delay_c = Derive.transformer Module.prelude "delay" mempty
    "Simple delay for control and pitch tracks."
    $ Sig.callt
    ( Sig.defaulted "time" (Typecheck.real 0.1) "Delay time."
    ) $ \(Typecheck.DefaultReal time) args deriver -> do
        start <- Args.real_start args
        delay <- Call.score_duration start time
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
    <$> Sig.defaulted "delay" (Sig.control "echo-delay" 1) "Delay time."
    <*> Sig.defaulted "feedback" (Sig.control "echo-feedback" 0.4)
        "The %dyn of each echo is multiplied by this amount."
    <*> Sig.defaulted "times" (Sig.control "echo-times" 1)
        "Number of echoes, not counting the original."
    ) $ \(delay, feedback, times) args deriver -> do
        now <- Args.real_start args
        delay <- Signal.y_to_score <$> Call.control_at delay now
        feedback <- Call.control_at feedback now
        times <- floor <$> Call.control_at times now
        echo delay feedback times deriver

echo :: ScoreTime -> Double -> Int -> Derive.NoteDeriver -> Derive.NoteDeriver
echo delay feedback times deriver
    | times <= 0 = deriver
    | otherwise = deriver <> Derive.shift_controls delay (Derive.at delay
        (Call.multiply_dynamic feedback $ echo delay feedback (times - 1)
            deriver))

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
    <$> Sig.defaulted "delay" (Sig.control "echo-delay" 1) "Delay time."
    <*> Sig.defaulted "feedback" (Sig.control "echo-feedback" 0.4)
        "The %dyn of each echo is multiplied by this amount."
    <*> Sig.defaulted "times" (Sig.control "echo-times" 1)
        "Number of echoes, not counting the original."
    ) $ \(delay, feedback, times) _args deriver -> do
        events <- deriver
        delay <- Post.time_control delay events
        feedback <- Post.control ScoreT.typed_val feedback events
        times <- Post.control (floor . ScoreT.typed_val) times events
        return $ Post.emap_asc_ (Control.uncurry4 echo_event)
            (Stream.zip4 delay feedback times events)

-- TODO this modifies the signals to shift by the given amount of time, which
-- is inefficient if there is a lot of signal data.  I could store a shift
-- with the event or the signals, but I'm not sure that would actually be
-- more efficient unless the signals are shifted more than once.
echo_event :: RealTime -> Double -> Int -> Score.Event -> [Score.Event]
echo_event delay feedback times event = event : map (echo event) [1..times]
    where
    echo event n = Score.modify_dynamic (*feedback^n) $ Score.move (+d) event
        where d = delay * RealTime.seconds (fromIntegral n)
