-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Carnatic style pitch ornaments.
--
-- The names don't correspond directly with anything traditional, as far as
-- I know, but are inspired by <http://www.gswift.com/article-2.html>.
module Derive.Call.Gamakam where
import qualified Data.List.NonEmpty as NonEmpty

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Make as Make
import qualified Derive.Call.SignalTransform as SignalTransform
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Trill as Trill
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, defaulted_env, required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [ ("kam", c_kampita Trill.UnisonFirst)
    , ("kam^", c_kampita Trill.NeighborFirst)
    , ("dip", c_dip)
    , ("jaru", c_jaru)
    , ("sgr", c_jaru_intervals Util.Diatonic [-1, 1])
    ]
    [ ("h", c_hold)
    ]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.call_maps
    [ ("kam", c_kampita_c Trill.UnisonFirst)
    , ("kam^", c_kampita_c Trill.NeighborFirst)
    , ("dip", c_dip_c)
    , ("jaru", c_jaru_c)
    , ("sgr", c_jaru_intervals_c [-1, 1])
    ]
    [ ("h", c_hold)
    ]

c_hold :: Derive.Transformer d
c_hold = Make.with_environ "hold"
    (defaulted "time" (TrackLang.real 1) "Hold first value for this long.")
    TrackLang.defaulted_real

-- * standard parameters

speed_arg :: Sig.Parser TrackLang.ValControl
speed_arg = defaulted "speed" (Sig.typed_control "trill-speed" 6 Score.Real)
    "Alternate pitches at this speed."

transition_default :: RealTime
transition_default = 0.15

jaru_time_default :: RealTime
jaru_time_default = 0.15


-- * pitch calls

-- TODO note version that ends on up or down
-- variations:
-- start high or low
-- end high or low - For this I need to know how when the note ends.  It can
-- either adjust the speed of the trill, or adjust the length of the note.
--
-- It seems like it might be nice to have ornaments be able to adjust tempo,
-- otherwise you have to coordinate changes in the tempo track.  But you can't
-- do it if you don't want it to affect other instruments.
--
-- To lengthen the note, it's actually in a position to do that since it's
-- above the note.  It could set a value which the note call will use for the
-- end, like an absolute duration sustain.  But it needs to know what it
-- originally was to round up, which requires peeking in the subs.
--
-- Or, tweak the speed to make it work out.
c_kampita :: Trill.Mode -> Derive.Generator Derive.Pitch
c_kampita mode = Derive.generator1 "kam" Tags.india
    "This is a kind of trill, but its interval defaults to NNs,\
    \ and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    $ Sig.call ((,,,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "neighbor" (Sig.typed_control "trill-neighbor" 1 Score.Nn)
        "Alternate with a pitch at this interval."
    <*> speed_arg
    <*> defaulted_env "transition" Sig.Both transition_default
        "Time for each slide."
    <*> Sig.environ (TrackLang.unsym Environ.hold) Sig.Unprefixed
        (TrackLang.real 0) "Time to hold the first pitch."
    ) $ \(pitch, neighbor, speed, transition, TrackLang.DefaultReal hold)
            args -> do
        srate <- Util.get_srate
        (neighbor, control) <- Util.to_transpose_signal Util.Nn neighbor
        let (val1, val2) = case mode of
                Trill.UnisonFirst -> (Signal.constant 0, neighbor)
                Trill.NeighborFirst -> (neighbor, Signal.constant 0)
        hold <- Util.duration_from (Args.start args) hold
        start <- Args.real_start args
        transpose <- SignalTransform.smooth id srate (-transition / 2) <$>
            trill_signal (Args.range_or_next args) hold speed val1 val2
        return $ PitchSignal.apply_control control
            (Score.untyped transpose) $ PitchSignal.signal [(start, pitch)]

trill_signal :: (ScoreTime, ScoreTime) -> ScoreTime -> TrackLang.ValControl
    -> Signal.Control -> Signal.Control -> Derive.Deriver Signal.Control
trill_signal (start, end) hold speed val1 val2 = do
    transitions <- Trill.trill_transitions (start + hold, end) speed
    transitions <- if hold > 0 then (: drop 1 transitions) <$> Derive.real start
        else return transitions
    return $ trill_from_transitions transitions val1 val2

-- | Make a trill signal from a list of transition times.
trill_from_transitions :: [RealTime] -> Signal.Control -> Signal.Control
    -> Signal.Control
trill_from_transitions transitions val1 val2 = Signal.signal
    [(x, Signal.at x sig) | (x, sig) <- zip transitions (cycle [val1, val2])]

-- | Ok, this name is terrible but what else is better?
c_dip :: Derive.Generator Derive.Pitch
c_dip = Derive.generator1 "dip" Tags.india
    "Alternate two intervals, dropping `dyn` on the second. This is useful\
    \ when avoiding a swaram, since it doesn't necessarily emit the base\
    \ pitch."
    $ Sig.call ((,,,,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "high" (TrackLang.default_diatonic 1) "High interval."
    <*> defaulted "low" (-1) "Low interval."
    <*> speed_arg
    <*> defaulted "dyn" 0.5 "Multiply dyn by this amount."
    <*> Sig.environ "transition" Sig.Both transition_default
        "Time for each slide."
    ) $ \(pitch, TrackLang.DefaultDiatonic high_, low, speed, dyn_scale,
            transition) args -> do
        srate <- Util.get_srate
        let (high, control) = Controls.transpose_control high_
        transitions <- Trill.trill_transitions (Args.range_or_next args) speed
        let transpose = SignalTransform.smooth id srate (-transition / 2) $
                trill_from_transitions transitions
                    (Signal.constant high) (Signal.constant low)
            dyn = SignalTransform.smooth id srate (-transition / 2) $
                trill_from_transitions transitions
                    (Signal.constant 1) (Signal.constant dyn_scale)
        (start, end) <- Args.real_range_or_next args
        Control.multiply_dyn end dyn
        return $ PitchSignal.apply_control control
            (Score.untyped transpose) $ PitchSignal.signal [(start, pitch)]

c_jaru :: Derive.Generator Derive.Pitch
c_jaru = Derive.generator1 "jaru" Tags.india
    "This is a series of grace notes whose pitches are relative to the given\
    \ base pitch."
    $ Sig.call ((,,,)
    <$> required "pitch" "Base pitch."
    <*> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both jaru_time_default "Time for each note."
    <*> Sig.environ "transition" Sig.Both Nothing
        "Time for each slide, defaults to `time`."
    ) $ \(pitch, intervals, time, maybe_transition) args -> do
        start <- Args.real_start args
        srate <- Util.get_srate
        (intervals, control) <- parse intervals
        let transition = fromMaybe time maybe_transition
        let sig = jaru srate start time transition (NonEmpty.toList intervals)
        return $ PitchSignal.apply_control control
            (Score.untyped sig) $ PitchSignal.signal [(start, pitch)]
    where
    parse intervals
        | all (==control) controls = return (xs, control)
        | otherwise = Derive.throw "all intervals must have the same type"
        where
        (xs, control :| controls) = NonEmpty.unzip $ NonEmpty.map
            (Controls.transpose_control . TrackLang.defaulted_diatonic)
            intervals

c_jaru_intervals :: Util.TransposeType -> [Signal.Y]
    -> Derive.Generator Derive.Pitch
c_jaru_intervals transpose intervals = Derive.generator1 "jaru" Tags.india
    ("This is `jaru` hardcoded to " <> Pretty.prettytxt intervals <> ".")
    $ Sig.call ((,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "time" jaru_time_default "Time for each note."
    <*> defaulted "transition" Nothing
        "Time for each slide, defaults to `time`."
    ) $ \(pitch, time, maybe_transition) args -> do
        start <- Args.real_start args
        srate <- Util.get_srate
        let sig = jaru srate start time (fromMaybe time maybe_transition)
                intervals
        return $ PitchSignal.apply_control (Util.transpose_control transpose)
            (Score.untyped sig) $ PitchSignal.signal [(start, pitch)]

jaru :: RealTime -> RealTime -> RealTime -> RealTime -> [Signal.Y]
    -> Signal.Control
jaru srate start time transition intervals =
    SignalTransform.smooth id srate (-transition) $
        Signal.signal (zip (Seq.range_ start time) (intervals ++ [0]))


-- * control calls

c_kampita_c :: Trill.Mode -> Derive.Generator Derive.Control
c_kampita_c mode = Derive.generator1 "kam" Tags.india
    "This is a trill with smooth transitions between the notes.  It's intended\
    \ for the vocal microtonal trills common in Carnatic music."
    $ Sig.call ((,,,)
    <$> defaulted "neighbor"
        (Sig.typed_control "trill-neighbor" 1 Score.Untyped)
        "Alternate between 0 and this value."
    <*> speed_arg
    <*> defaulted_env "transition" Sig.Both transition_default
        "Time for each slide."
    <*> Sig.environ (TrackLang.unsym Environ.hold) Sig.Unprefixed
        (TrackLang.real 0) "Time to hold the first pitch."
    ) $ \(neighbor, speed, transition, TrackLang.DefaultReal hold) args -> do
        srate <- Util.get_srate
        neighbor <- Util.to_untyped_signal neighbor
        let (val1, val2) = case mode of
                Trill.UnisonFirst -> (Signal.constant 0, neighbor)
                Trill.NeighborFirst -> (neighbor, Signal.constant 0)
        hold <- Util.duration_from (Args.start args) hold
        SignalTransform.smooth id srate (-transition / 2) <$>
            trill_signal (Args.range_or_next args) hold speed val1 val2

-- | Ok, this name is terrible but what else is better?
c_dip_c :: Derive.Generator Derive.Control
c_dip_c = Derive.generator1 "dip" Tags.india
    "Alternate two intervals, dropping `dyn` on the second. This is useful\
    \ when avoiding a swaram, since it doesn't necessarily emit the base\
    \ pitch."
    $ Sig.call ((,,,,)
    <$> defaulted "high" 1 "High interval."
    <*> defaulted "low" (-1) "Low interval."
    <*> speed_arg
    <*> defaulted "dyn" 0.5 "Multiply dyn by this amount."
    <*> Sig.environ "transition" Sig.Both transition_default
        "Time for each slide."
    ) $ \(high, low, speed, dyn_scale, transition) args -> do
        srate <- Util.get_srate
        transitions <- Trill.trill_transitions (Args.range_or_next args) speed
        let smooth = SignalTransform.smooth id srate (-transition / 2)
            transpose = smooth $ trill_from_transitions transitions
                (Signal.constant high) (Signal.constant low)
            dyn = smooth $ trill_from_transitions transitions
                (Signal.constant 1) (Signal.constant dyn_scale)
        end <- Derive.real (Args.next args)
        Control.multiply_dyn end dyn
        return transpose

c_jaru_c :: Derive.Generator Derive.Control
c_jaru_c = Derive.generator1 "jaru" Tags.india
    "This is a series of grace notes whose pitches are relative to the given\
    \ base pitch."
    $ Sig.call ((,,)
    <$> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both jaru_time_default "Time for each note."
    <*> Sig.environ "transition" Sig.Both Nothing
        "Time for each slide, defaults to `time`."
    ) $ \(intervals, time, maybe_transition) args -> do
        start <- Args.real_start args
        srate <- Util.get_srate
        let transition = fromMaybe time maybe_transition
        return $ jaru srate start time transition (NonEmpty.toList intervals)

c_jaru_intervals_c :: [Signal.Y] -> Derive.Generator Derive.Control
c_jaru_intervals_c intervals = Derive.generator1 "jaru" Tags.india
    ("This is `jaru` hardcoded to " <> Pretty.prettytxt intervals <> ".")
    $ Sig.call ((,)
    <$> defaulted "time" jaru_time_default "Time for each note."
    <*> defaulted "transition" Nothing
        "Time for each slide, defaults to `time`."
    ) $ \(time, maybe_transition) args -> do
        start <- Args.real_start args
        srate <- Util.get_srate
        return $ jaru srate start time (fromMaybe time maybe_transition)
            intervals
