-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Carnatic style pitch ornaments.

    The names don't correspond directly with anything traditional, as far as
    I know, but are inspired by <http://www.gswift.com/article-2.html>.

    Pitch ornaments can be expressed either as pitch calls, or as control
    calls meant for a transpose track.  They both have pros and cons:

    Transposition control signal:

    - I can keep the pitches separate and clear and collapse the pitch
    track.  This correctly reflects the underlying swaram, with the gamakam
    as separate ornamentation.

    - Related to the above, each call doesn't need to repeat the the pitch arg,
    so there's less redundancy.  Calls are also simpler with one fewer
    argument.

    Pitch signal:

    - A pitch call can use absolute (@t-nn@) or scalar (@t-diatonic@)
    transposition based on the type of its arguments, while the transpose
    signal has to either use a separate track, or the somewhat awkward @->@
    call.

    - The pitch signal can represent an ornament involving multiple pitches,
    e.g. a slide frome one pitch to another.  A transposition signal can only
    represent offsets from an external pitch.

    So the pitch signal is more powerful, but the transposition signal is often
    more convenient, and can lead to less redundant notation.  Unless I can
    think of a way to get the advantages of both, I might have to have both
    around, with their own versions of the same calls.
-}
module Derive.Call.India.Gamakam where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.SignalTransform as SignalTransform
import qualified Derive.Call.Prelude.Trill as Trill
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, defaulted_env, required)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map $
    [("dip", c_dip)
    , ("jaru", c_jaru)
    , ("sgr", c_jaru_intervals Typecheck.Diatonic [-1, 1])
    ] ++ kampita_variations "kam" c_kampita

module_ :: Module.Module
module_ = "india" <> "gamakam"

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.generator_call_map $
    [ ("dip", c_dip_c)
    , ("j)", jaru_transition_c "j)" Nothing
        "Time for each slide, defaults to `time`.")
    , ("j]", jaru_transition_c "j]" (Just (jaru_time_default / 2))
        "Time for each slide.")
    , ("sgr", c_jaru_intervals_c [-1, 1])
    ] ++ kampita_variations "kam" c_kampita_c
    ++ kampita_variations "nkam" c_nkampita_c

kampita_variations :: Text
    -> (Maybe Trill.Direction -> Maybe Trill.Direction -> call)
    -> [(BaseTypes.CallId, call)]
kampita_variations name call =
    [ (BaseTypes.Symbol $ affix s <> name <> affix e, call s e)
    | s <- dirs, e <- dirs
    ]
    where
    affix = Trill.direction_affix
    dirs = [Nothing, Just Trill.Low, Just Trill.High]

-- * standard parameters

transition_default :: RealTime
transition_default = 0.08

jaru_time_default :: RealTime
jaru_time_default = 0.15

speed_arg :: Sig.Parser BaseTypes.ControlRef
speed_arg = defaulted "speed" (Sig.typed_control "tr-speed" 6 Score.Real)
    "Alternate pitches at this speed."

neighbor_arg :: Sig.Parser BaseTypes.ControlRef
neighbor_arg = defaulted "neighbor"
    (Sig.typed_control "tr-neighbor" 1 Score.Untyped)
    "Alternate between 0 and this value."

lilt_env :: Sig.Parser Double
lilt_env = Sig.environ "lilt" Sig.Both 0 "Lilt is a horizontal bias to the\
    \ vibrato. A lilt of 1 would place each neighbor on top of the\
    \ following unison, while -1 would place it on the previous one.\
    \ So it should range from -1 < lilt < 1."

-- * pitch calls

c_kampita :: Maybe Trill.Direction -> Maybe Trill.Direction
    -> Derive.Generator Derive.Pitch
c_kampita start_dir end_dir = generator1 "kam" mempty
    "This is a kind of trill, but its interval defaults to NNs,\
    \ and transitions between the notes are smooth.  It's intended for\
    \ the vocal microtonal trills common in Carnatic music."
    $ Sig.call ((,,,,,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "neighbor" (Sig.typed_control "tr-neighbor" 1 Score.Nn)
        "Alternate with a pitch at this interval."
    <*> speed_arg
    <*> defaulted_env "transition" Sig.Both transition_default
        "Time for each slide."
    <*> Trill.hold_env <*> lilt_env <*> Trill.adjust_env
    ) $ \(pitch, neighbor, speed, transition, hold, lilt, adjust) args -> do
        (neighbor, control) <- Call.to_transpose_function Typecheck.Nn neighbor
        transpose <- kampita start_dir end_dir adjust neighbor speed
            transition hold lilt args
        start <- Args.real_start args
        return $ PSignal.apply_control control
            (Score.untyped transpose) $ PSignal.signal [(start, pitch)]

trill_transitions :: Maybe Bool -> Trill.Adjust -> Double -> ScoreTime
    -> BaseTypes.ControlRef -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [RealTime]
trill_transitions = Trill.adjusted_transitions include_end
    where
    -- Trills usually omit the transition that coincides with the end because
    -- that would create a zero duration note.  But these trills are smoothed
    -- and thus will still have a segment leading to the cut-off transition.
    include_end = True

-- | Make a trill signal from a list of transition times.
trill_from_transitions :: Typecheck.Function -> Typecheck.Function
    -> [RealTime] -> Signal.Control
trill_from_transitions val1 val2 transitions = Signal.signal
    [(x, sig x) | (x, sig) <- zip transitions (cycle [val1, val2])]

-- | Ok, this name is terrible but what else is better?
c_dip :: Derive.Generator Derive.Pitch
c_dip = generator1 "dip" mempty
    "Alternate two intervals, dropping `dyn` on the second. This is useful\
    \ when avoiding a swaram, since it doesn't necessarily emit the base\
    \ pitch."
    $ Sig.call ((,,,,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "high" (Typecheck.diatonic 1) "High interval."
    <*> defaulted "low" (-1) "Low interval."
    <*> speed_arg
    <*> defaulted "dyn" 0.5 "Multiply dyn by this amount."
    <*> Sig.environ "transition" Sig.Both transition_default
        "Time for each slide."
    ) $ \(pitch, Typecheck.DefaultDiatonic high_, low, speed, dyn_scale,
            transition) args -> do
        let (high, control) = Controls.transpose_control high_
        transpose <- dip high low speed dyn_scale transition
            (Args.range_or_next args)
        start <- Args.real_start args
        return $ PSignal.apply_control control
            (Score.untyped transpose) $ PSignal.signal [(start, pitch)]

c_jaru :: Derive.Generator Derive.Pitch
c_jaru = generator1 "jaru" mempty
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
        srate <- Call.get_srate
        (intervals, control) <- parse intervals
        let transition = fromMaybe time maybe_transition
        let sig = jaru srate start time transition (NonEmpty.toList intervals)
        return $ PSignal.apply_control control
            (Score.untyped sig) $ PSignal.signal [(start, pitch)]
    where
    parse intervals
        | all (==control) controls = return (xs, control)
        | otherwise = Derive.throw "all intervals must have the same type"
        where
        (xs, control :| controls) = NonEmpty.unzip $ NonEmpty.map
            (Controls.transpose_control . Typecheck.default_diatonic)
            intervals

c_jaru_intervals :: Typecheck.TransposeType -> [Signal.Y]
    -> Derive.Generator Derive.Pitch
c_jaru_intervals transpose intervals = generator1 "jaru" mempty
    ("This is `jaru` hardcoded to " <> Doc.pretty intervals <> ".")
    $ Sig.call ((,,)
    <$> required "pitch" "Base pitch."
    <*> defaulted "time" jaru_time_default "Time for each note."
    <*> defaulted "transition" Nothing
        "Time for each slide, defaults to `time`."
    ) $ \(pitch, time, maybe_transition) args -> do
        start <- Args.real_start args
        srate <- Call.get_srate
        let sig = jaru srate start time (fromMaybe time maybe_transition)
                intervals
        return $ PSignal.apply_control (Typecheck.transpose_control transpose)
            (Score.untyped sig) $ PSignal.signal [(start, pitch)]


-- * control calls

c_kampita_c :: Maybe Trill.Direction -> Maybe Trill.Direction
    -> Derive.Generator Derive.Control
c_kampita_c start_dir end_dir = generator1 "kam" mempty
    "This is a trill with smooth transitions between the notes. It's intended\
    \ for the microtonal vocal trills common in Carnatic music. `^` is high\
    \ and `_` is low, so `^kam_` starts on the upper note, and ends on the\
    \ lower one. Otherwise, it starts on the unison note and ends on either."
    $ Sig.call ((,,,,,)
    <$> neighbor_arg
    <*> speed_arg
    <*> defaulted_env "transition" Sig.Both transition_default
        "Time for each slide."
    <*> Trill.hold_env <*> lilt_env <*> Trill.adjust_env
    ) $ \(neighbor, speed, transition, hold, lilt, adjust) args -> do
        neighbor <- Call.to_function neighbor
        kampita start_dir end_dir adjust neighbor speed transition hold lilt
            args

-- | You don't think there are too many arguments, do you?
kampita :: Maybe Trill.Direction -> Maybe Trill.Direction -> Trill.Adjust
    -> Typecheck.Function -> BaseTypes.ControlRef -> RealTime
    -> BaseTypes.Duration -> Double -> Derive.PassedArgs a
    -> Derive.Deriver Signal.Control
kampita start_dir end_dir adjust neighbor speed transition hold lilt args = do
    start <- Args.real_start args
    let ((val1, val2), even_transitions) = convert_directions start neighbor
            start_dir end_dir
    hold <- Call.score_duration (Args.start args) hold
    smooth_trill (-transition) val1 val2
        =<< trill_transitions even_transitions adjust lilt hold speed
            (Args.range_or_next args)

smooth_trill :: RealTime -> Typecheck.Function -> Typecheck.Function
    -> [RealTime] -> Derive.Deriver Signal.Control
smooth_trill time val1 val2 transitions = do
    srate <- Call.get_srate
    return $ SignalTransform.smooth id srate time $
        trill_from_transitions val1 val2 transitions

convert_directions :: RealTime -> Typecheck.Function -> Maybe Trill.Direction
    -> Maybe Trill.Direction
    -> ((Typecheck.Function, Typecheck.Function), Maybe Bool)
convert_directions start_t neighbor start end = (vals, even_transitions)
    where
    first = case start of
        Nothing -> Trill.Unison
        Just Trill.Low -> if neighbor_low then Trill.Neighbor else Trill.Unison
        Just Trill.High -> if neighbor_low then Trill.Unison else Trill.Neighbor
    vals = case first of
        Trill.Unison -> (const 0, neighbor)
        Trill.Neighbor -> (neighbor, const 0)
    -- If I end Low, and neighbor is low, and I started with Unison, then val2
    -- is low, so I want even transitions.  Why is it so complicated just to
    -- get a trill to end high or low?
    first_low = case first of
        Trill.Unison -> not neighbor_low
        Trill.Neighbor -> neighbor_low
    even_transitions = case end of
        Nothing -> Nothing
        Just Trill.Low -> Just (not first_low)
        Just Trill.High -> Just first_low
    neighbor_low = neighbor start_t < 0

c_nkampita_c :: Maybe Trill.Direction -> Maybe Trill.Direction
    -> Derive.Generator Derive.Control
c_nkampita_c start_dir end_dir = generator1 "nkam" mempty
    "`kam` with a set number of cycles. The speed adjusts to fit the cycles in\
    \ before the next event."
    $ Sig.call ((,,,,)
    <$> neighbor_arg
    <*> (Typecheck.positive <$> defaulted "cycles" 1 "Number of cycles.")
    <*> lilt_env <*> Trill.hold_env
    <*> Sig.environ "transition" Sig.Both transition_default
        "Time for each slide."
    ) $ \(neighbor, cycles, lilt, hold, transition) args -> do
        (start, end) <- Args.real_range_or_next args
        neighbor <- Call.to_function neighbor
        let ((val1, val2), even_transitions) = convert_directions start
                neighbor start_dir end_dir
        hold <- Call.score_duration (Args.start args) hold
        -- In order to hear the cycles clearly, I leave a one transition of
        -- flat space at the end.  This means nkam can't transition into the
        -- next note, but for now this seems more convenient.
        let num_transitions = 1 + cycles * 2
                + (if even_transitions == Just True then 0 else 1)
        let speed = BaseTypes.constant_control $
                (num_transitions - 1) / RealTime.to_seconds (end - start)
        transitions <- trill_transitions Nothing Trill.Shorten lilt hold speed
                (Args.range_or_next args)
        smooth_trill (-transition) val1 val2 (Seq.rdrop 1 transitions)

-- | Ok, this name is terrible but what else is better?
c_dip_c :: Derive.Generator Derive.Control
c_dip_c = generator1 "dip" mempty
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
    ) $ \(high, low, speed, dyn_scale, transition) args ->
        dip high low speed dyn_scale transition (Args.range_or_next args)

dip :: Double -> Double -> BaseTypes.ControlRef -> Double
    -> RealTime -> (ScoreTime, ScoreTime) -> Derive.Deriver Signal.Control
dip high low speed dyn_scale transition (start, end) = do
    srate <- Call.get_srate
    transitions <- Trill.trill_transitions (start, end) False speed
    let smooth = SignalTransform.smooth id srate (-transition / 2)
        transpose = smooth $
            trill_from_transitions (const high) (const low) transitions
        dyn = smooth $
            trill_from_transitions (const 1) (const dyn_scale) transitions
    end <- Derive.real end
    ControlUtil.multiply_dyn end dyn
    return transpose

jaru_transition_c :: Derive.CallName -> Maybe RealTime -> Doc.Doc
    -> Derive.Generator Derive.Control
jaru_transition_c name default_transition transition_doc =
    generator1 name mempty
    "This is a series of grace notes with relative pitches."
    $ Sig.call ((,,)
    <$> Sig.many1 "interval" "Intervals from base pitch."
    <*> Sig.environ "time" Sig.Both jaru_time_default "Time for each note."
    <*> Sig.environ "transition" Sig.Both default_transition transition_doc
    ) $ \(intervals, time, maybe_transition) args -> do
        start <- Args.real_start args
        srate <- Call.get_srate
        let transition = fromMaybe time maybe_transition
        return $ jaru srate start time transition (NonEmpty.toList intervals)

c_jaru_intervals_c :: [Signal.Y] -> Derive.Generator Derive.Control
c_jaru_intervals_c intervals = generator1 "jaru" mempty
    ("This is `jaru` hardcoded to " <> Doc.pretty intervals <> ".")
    $ Sig.call ((,)
    <$> defaulted "time" jaru_time_default "Time for each note."
    <*> defaulted "transition" Nothing
        "Time for each slide, defaults to `time`."
    ) $ \(time, maybe_transition) args -> do
        start <- Args.real_start args
        srate <- Call.get_srate
        return $ jaru srate start time (fromMaybe time maybe_transition)
            intervals

jaru :: RealTime -> RealTime -> RealTime -> RealTime -> [Signal.Y]
    -> Signal.Control
jaru srate start time transition intervals =
    SignalTransform.smooth id srate (-transition) $
        Signal.signal (zip (Seq.range_ start time) (intervals ++ [0]))

generator1 :: Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 module_
