-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic high level pitch calls.
--
-- High level calls do something a little more abstract and \"musical\"
-- than the low level calls in "Derive.Call.Pitch".  Generally they have
-- complete-word names, while low level calls are just single letters.
module Derive.Call.PitchHigh where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("lift", c_lift_note)
    , ("drop", c_drop_note)
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [ ("drop", c_drop)
    , ("lift", c_lift)
    , ("ad", c_approach_dyn)
    ]
    []

c_drop :: Derive.Generator Derive.Pitch
c_drop = make_drop "drop" "Drop pitch and `dyn`." True

c_lift :: Derive.Generator Derive.Pitch
c_lift = make_drop "lift"
    "Lift pitch and drop `dyn`. This is the same as `drop`, except that it\
    \ defaults to going up instead of down."
    False

make_drop :: Text -> Text -> Bool -> Derive.Generator Derive.Pitch
make_drop name doc down = Derive.generator1 name Tags.cmod doc $
    Sig.call ((,)
    <$> defaulted "interval" (Left (Pitch.Chromatic 7))
        "Interval or destination pitch."
    <*> defaulted "time" (TrackLang.real 0.25)
        "Time to drop the given interval and fade to nothing."
    ) $ \(interval, TrackLang.DefaultReal time) args ->
        Args.prev_val args >>= \x -> case x of
            Nothing -> return mempty
            Just (_, prev_pitch) -> do
                next <- Derive.real (Args.next args)
                (start, end) <- Util.duration_from_start args time
                drop_signal start end next prev_pitch interval down

c_drop_note :: Derive.Transformer Derive.Note
c_drop_note = make_drop_note "drop"
    "Drop pitch and `dyn` at the end of the note." True

c_lift_note :: Derive.Transformer Derive.Note
c_lift_note = make_drop_note "lift"
    "Raise pitch and drop `dyn` at the end of the note. Same as the `drop`\
    \ note call, except it defaults to going up instead of down."
    False

make_drop_note :: Text -> Text -> Bool -> Derive.Transformer Derive.Note
make_drop_note name doc down = Derive.transformer name Tags.under_invert doc
    $ Sig.callt ((,,)
    <$> defaulted "interval" (Left (Pitch.Chromatic 7))
        "Interval or destination pitch."
    <*> defaulted "time" (TrackLang.real 0.25) "Time to drop by the interval."
    <*> defaulted "fade" (TrackLang.real 0.25) "Time to fade to nothing.\
        \ If the fade is longer than the pitch time, the pitch will finish\
        \ moving before the dyn has faded out."
    ) $ \(interval, TrackLang.DefaultReal time, TrackLang.DefaultReal fade)
            args deriver -> do
        (dyn_start, end) <- Util.duration_from_end args fade
        let dyn_end = end
        pitch_dur <- Util.real_dur' (Args.end args) time
        let pitch_start = min dyn_start (end - pitch_dur)
            pitch_end = min (dyn_start + pitch_dur) end
        Derive.pitch_at pitch_start >>= \x -> case x of
            Nothing -> deriver
            Just pitch -> do
                next <- Derive.real (Args.next args)
                slide <- drop_signal pitch_start pitch_end next pitch interval
                    down
                pitch <- Internal.get_dynamic Derive.state_pitch
                dyn <- segment id dyn_start 1 dyn_end 0
                multiply_dyn dyn $
                    Derive.with_pitch Nothing (pitch <> slide) deriver

multiply_dyn :: Signal.Control -> Derive.Deriver a -> Derive.Deriver a
multiply_dyn = Derive.with_multiplied_control Score.c_dynamic . Score.untyped

segment :: (Double -> Double) -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
segment f x1 y1 x2 y2 = do
    sig <- Control.make_signal f x1 y1 x2 y2
    return $ Signal.signal [(0, y1)] <> sig

drop_signal :: RealTime -> RealTime -> RealTime -> PitchSignal.Pitch
    -> Either Pitch.Transpose PitchSignal.Pitch -> Bool
    -> Derive.Deriver PitchSignal.Signal
drop_signal start end next prev_pitch interval down = do
    let dest = case interval of
            Left degrees -> Pitches.transpose
                (if down then Pitch.modify_transpose negate degrees
                    else degrees)
                prev_pitch
            Right pitch -> pitch
    Control.multiply_dyn next id start 1 end 0
    Call.Pitch.make_interpolator id False start prev_pitch end dest

c_approach_dyn :: Derive.Generator Derive.Pitch
c_approach_dyn = Derive.generator1 "approach-dyn" (Tags.cmod <> Tags.next)
    "Like `approach`, slide to the next pitch, but also drop the `dyn`."
    $ Sig.call ((,)
    <$> defaulted "time" (TrackLang.real 0.2)
        "Time to get to destination pitch and dyn."
    <*> defaulted "dyn" 0.25 "Drop `dyn` by this factor."
    ) $ \(TrackLang.DefaultReal time, dyn) args -> do
        (start, end) <- Util.duration_from_start args time
        Control.multiply_dyn end id start 1 end dyn
        Call.Pitch.approach args start end
