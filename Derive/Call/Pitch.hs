-- | Create val calls for scale degrees.
module Derive.Call.Pitch where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- | Create a pitch val call for the given scale degree.  This is intended to
-- be used by scales to generate their calls, but of course each scale may
-- define calls in its own way.
scale_degree :: Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scale_degree pitch_nn pitch_note = Derive.val_call
    "pitch" Tags.scale "Emit the pitch of a scale degree." $ Sig.call ((,)
    <$> defaulted "frac" 0
        "Add this many hundredths of a scale degree to the output."
    <*> defaulted "hz" 0 "Add an absolute hz value to the output."
    ) $ \(frac, hz) _ -> do
        environ <- Internal.get_dynamic Derive.state_environ
        -- TODO apparently lots of DRAG gets caught in the environ.  Properly
        -- I should make environ more strict so that doesn't happen, but it
        -- doesn't seem to be working.  Meanwhile, here's a hack.
        environ <- DeepSeq.rnf environ `seq` return environ
        return $ TrackLang.VPitch $ PitchSignal.pitch
            (call frac hz environ) (pitch_note environ)
    where
    call frac hz environ controls =
        Pitch.add_hz (hz + hz_sig) <$> pitch_nn environ
            (if frac == 0 then controls
                else Map.insertWith' (+) Score.c_chromatic (frac / 100)
                    controls)
        where hz_sig = Map.findWithDefault 0 Score.c_hz controls

-- | Convert a note and @frac@ arg into a tracklang expression representing
-- that note.
pitch_expr :: Pitch.Note -> Double -> String
pitch_expr (Pitch.Note note) frac
    | frac == 0 = note
    | otherwise = note ++ " " ++ show (floor (frac * 100))


-- * pitch

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("=", Util.c_equal)
    , ("", c_set)
    , ("set", c_set)
    , ("'", c_set_prev)

    , ("i", c_linear_prev)
    , ("i<<", c_linear_prev_const)
    , ("i>", c_linear_next)
    , ("i>>", c_linear_next_const)
    , ("e", c_exp_prev)
    , ("e<<", c_exp_prev_const)
    , ("e>", c_exp_next)
    , ("e>>", c_exp_next_const)

    , ("n", c_neighbor)
    , ("a", c_approach)
    , ("u", c_up)
    , ("d", c_down)

    , ("drop", c_drop)
    , ("ad", c_approach_dyn)
    ]

-- | This should contain the calls that require the previous value.  It's used
-- by a hack in 'Derive.Slice.slice'.
require_previous :: Set.Set String
require_previous = Set.fromList
    ["'", "i>", "i>>", "i<<", "e>", "e>>", "e<<", "a", "u", "d"]

c_set :: Derive.PitchCall
c_set = Derive.generator1 "set" mempty "Emit a pitch with no interpolation." $
    -- This could take a transpose too, but then set has to be in
    -- 'require_previous', it gets shadowed for "" because of scales that use
    -- numbers, and it's not clearly useful.
    Sig.call (required "pitch" "Destination pitch.") $ \pitch args -> do
        pos <- Args.real_start args
        Util.pitch_signal [(pos, pitch)]

-- | Re-set the previous val.  This can be used to extend a breakpoint.
c_set_prev :: Derive.PitchCall
c_set_prev = Derive.generator "set-prev" Tags.internal
    ("Re-set the previous pitch.  This can be used to extend a breakpoint."
    ) $ Sig.call0 $ \args -> case Args.prev_val args of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Args.real_start args
            if pos > prev_x then (:[]) <$> Util.pitch_signal [(pos, prev_y)]
                else return []

-- * linear

type Transpose = Either PitchSignal.Pitch Pitch.Transpose

-- | Linear interpolation, with different start times.
linear_interpolation :: (TrackLang.Typecheck time) =>
    String -> time -> String
    -> (Derive.PassedArgs PitchSignal.Signal -> time
        -> Derive.Deriver TrackLang.RealOrScore)
    -> Derive.PitchCall
linear_interpolation name time_default time_default_doc get_time =
    Derive.generator1 name Tags.prev doc $ Sig.call
        ((,) <$> pitch_arg <*> defaulted "time" time_default time_doc) $
    \(pitch, time) args -> interpolate id args pitch =<< get_time args time
    where
    doc = "Interpolate from the previous pitch to the given one in a straight\
        \ line."
    time_doc = "Time to reach destination. " ++ time_default_doc

c_linear_prev :: Derive.PitchCall
c_linear_prev = linear_interpolation "linear-prev" Nothing
    "If not given, start from the previous sample." Control.default_prev

c_linear_prev_const :: Derive.PitchCall
c_linear_prev_const =
    linear_interpolation "linear-prev-const" (TrackLang.real (-0.1)) "" $
        \_ -> return . default_real

c_linear_next :: Derive.PitchCall
c_linear_next =
    linear_interpolation "linear-next" Nothing
        "If not given, default to the start of the next event." $
    \args maybe_time -> return $ maybe (next_dur args) default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_linear_next_const :: Derive.PitchCall
c_linear_next_const =
    linear_interpolation "linear-next-const" (TrackLang.real 0.1) "" $
        \_ -> return . default_real


-- * exponential

-- | Exponential interpolation, with different start times.
exponential_interpolation :: (TrackLang.Typecheck time) =>
    String -> time -> String
    -> (Derive.PassedArgs PitchSignal.Signal -> time
        -> Derive.Deriver TrackLang.RealOrScore)
    -> Derive.PitchCall
exponential_interpolation name time_default time_default_doc get_time =
    Derive.generator1 name Tags.prev doc $ Sig.call ((,,)
    <$> pitch_arg
    <*> defaulted "exp" 2 Control.exp_doc
    <*> defaulted "time" time_default time_doc
    ) $ \(pitch, exp, time) args ->
        interpolate (Control.expon exp) args pitch =<< get_time args time
    where
    doc = "Interpolate from the previous pitch to the given one in a curve."
    time_doc = "Time to reach destination. " ++ time_default_doc

c_exp_prev :: Derive.PitchCall
c_exp_prev = exponential_interpolation "exp-prev" Nothing
    "If not given, start from the previous sample." Control.default_prev

c_exp_prev_const :: Derive.PitchCall
c_exp_prev_const =
    exponential_interpolation "exp-prev-const" (TrackLang.real (-0.1)) "" $
        \_ -> return . default_real

c_exp_next :: Derive.PitchCall
c_exp_next = exponential_interpolation "exp-next" Nothing
        "If not given default to the start of the next event." $
    \args maybe_time -> return $ maybe (next_dur args) default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_exp_next_const :: Derive.PitchCall
c_exp_next_const =
    exponential_interpolation "exp-next-const" (TrackLang.real 0.1) "" $
        \_ -> return . default_real

pitch_arg :: Sig.Parser Transpose
pitch_arg = required "pitch"
    "Destination pitch, or a transposition from the previous one."

-- * misc

c_neighbor :: Derive.PitchCall
c_neighbor = Derive.generator1 "neighbor" mempty
    ("Emit a slide from a neighboring pitch to the given one."
    ) $ Sig.call ((,,)
    <$> required "pitch" "Destination pitch."
    <*> defaulted "neighbor" (Pitch.Chromatic 1) "Neighobr interval."
    <*> defaulted "time" (TrackLang.real 0.1)
        "Time to get to destination pitch."
    ) $ \(pitch, neighbor, TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        let pitch1 = Pitches.transpose neighbor pitch
        make_interpolator id True start pitch1 end pitch

c_approach :: Derive.PitchCall
c_approach = Derive.generator1 "approach" Tags.next
    "Slide to the next pitch." $ Sig.call
    ( defaulted "time" (TrackLang.real 0.2) "Time to get to destination pitch."
    ) $ \(TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        approach args start end

approach :: Derive.PassedArgs PitchSignal.Signal -> RealTime -> RealTime
    -> Derive.Deriver PitchSignal.Signal
approach args start end = do
    maybe_next <- next_pitch args
    case (Args.prev_val args, maybe_next) of
        (Just (_, prev), Just next) ->
            make_interpolator id True start prev end next
        _ -> Util.pitch_signal []

next_pitch :: Derive.PassedArgs d -> Derive.Deriver (Maybe PitchSignal.Pitch)
next_pitch = maybe (return Nothing) eval_pitch . Seq.head . Args.next_events

eval_pitch :: Event.Event -> Derive.Deriver (Maybe PitchSignal.Pitch)
eval_pitch event =
    justm (either (const Nothing) Just <$> Call.eval_event event) $ \strm -> do
    start <- Derive.real (Event.start event)
    return $ PitchSignal.at start $ mconcat $ LEvent.events_of strm

c_up :: Derive.PitchCall
c_up = Derive.generator1 "up" Tags.prev
    "Ascend at the given speed until the next event." $ slope "Ascend" 1

c_down :: Derive.PitchCall
c_down = Derive.generator1 "down" Tags.prev
    "Descend at the given speed until the next event." $ slope "Descend" (-1)

slope :: String -> Double -> Derive.WithArgDoc
    (Derive.PassedArgs PitchSignal.Signal -> Derive.Deriver PitchSignal.Signal)
slope word sign =
    Sig.call (defaulted "speed" (Pitch.Chromatic 1)
        (word <> " this many steps per second.")) $
    \speed args -> case Args.prev_val args of
        Nothing -> Util.pitch_signal []
        Just (_, prev_pitch) -> do
            start <- Args.real_start args
            next <- Derive.real (Args.next args)
            let diff = RealTime.to_seconds (next - start) * speed_val * sign
                (speed_val, typ) = Util.split_transpose speed
                dest = Pitches.transpose (Util.join_transpose diff typ)
                    prev_pitch
            make_interpolator id True start prev_pitch next dest

-- * high level calls

-- Thees calls emit pitch but also modify other controls, mostly 'dyn'.
-- The convention is that low level calls have single-letter or letter+symbol
-- names, while high level calls have words or abbreviated words.

c_drop :: Derive.PitchCall
c_drop = Derive.generator1 "drop" Tags.cmod "Drop pitch and `dyn`." $
    Sig.call ((,)
    <$> defaulted "interval" (Pitch.Chromatic 7) "Drop interval."
    <*> defaulted "time" (TrackLang.real 0.25)
        "Time to drop the given interval and fade to nothing."
    ) $ \(interval, TrackLang.DefaultReal time) args ->
        case Args.prev_val args of
            Nothing -> Util.pitch_signal []
            Just (_, prev_pitch) -> do
                (start, end) <- Util.duration_from_start args time
                drop_call start end prev_pitch interval

drop_call :: RealTime -> RealTime -> PitchSignal.Pitch -> Pitch.Transpose
    -> Derive.Deriver PitchSignal.Signal
drop_call start end prev_pitch interval = do
    let dest = Pitches.transpose
            (Pitch.modify_transpose negate interval) prev_pitch
    Control.multiply_dyn id start 1 end 0
    make_interpolator id False start prev_pitch end dest

c_approach_dyn :: Derive.PitchCall
c_approach_dyn = Derive.generator1 "approach-dyn" (Tags.cmod <> Tags.next)
    "Like `approach`, slide to the next pitch, but also drop the `dyn`."
    $ Sig.call ((,)
    <$> defaulted "time" (TrackLang.real 0.2)
        "Time to get to destination pitch and dyn."
    <*> defaulted "dyn" 0.25 "Drop `dyn` by this factor."
    ) $ \(TrackLang.DefaultReal time, dyn) args -> do
        (start, end) <- Util.duration_from_start args time
        Control.multiply_dyn id start 1 end dyn
        approach args start end

-- * util

default_real :: TrackLang.DefaultReal -> TrackLang.RealOrScore
default_real (TrackLang.DefaultReal t) = t

type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -- ^ start -> starty -> end -> endy
    -> PitchSignal.Signal

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate :: (Double -> Double) -> Derive.PassedArgs PitchSignal.Signal
    -> Transpose -> TrackLang.RealOrScore
    -> Derive.Deriver PitchSignal.Signal
interpolate f args pitch_transpose dur = do
    (start, end) <- Util.duration_from_start args dur
    case Args.prev_val args of
        Nothing -> case pitch_transpose of
            Left pitch -> Util.pitch_signal [(start, pitch)]
            Right _ -> Util.pitch_signal []
        Just (_, prev) -> do
            make_interpolator f False (min start end) prev (max start end) $
                either id (flip Pitches.transpose prev) pitch_transpose

make_interpolator :: (Double -> Double)
    -> Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> Derive.Deriver PitchSignal.Signal
make_interpolator f include_initial x1 note1 x2 note2 = do
    scale <- Util.get_scale
    srate <- Util.get_srate
    return $ interpolator scale srate f include_initial x1 note1 x2 note2

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
interpolator :: Scale.Scale -> RealTime -> (Double -> Double)
    -> Interpolator
interpolator scale srate f include_initial x1 note1 x2 note2 =
    Util.signal scale $ (if include_initial then id else drop 1)
        [(x, pitch_of x) | x <- Seq.range_end x1 x2 srate]
    where
    pitch_of = Pitches.interpolated note1 note2
        . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds
