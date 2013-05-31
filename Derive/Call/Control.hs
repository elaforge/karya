-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic calls for control tracks.
module Derive.Call.Control where
import qualified Data.Set as Set

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (required, defaulted)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- | This is a special lookup for control tracks that lets you directly type
-- a number, and have that be interpreted as setting the control to that value.
-- In addition, it allows a special hex syntax
--
-- Formerly, control tracks used a slightly different parser to enable the same
-- thing, but that turned out to be awkward when I wanted to implement
-- 'Call.eval_event'.
lookup_number :: Derive.LookupCall Derive.ControlCall
lookup_number = Derive.pattern_lookup "numbers and hex" doc $
    \(TrackLang.Symbol sym) -> return $! case ParseBs.parse_num sym of
        Left _ -> Nothing
        Right val -> Just $ set val
    where
    set :: Signal.Y -> Derive.ControlCall
    set val = Derive.generator1 "self-eval" Tags.prelude
        "Emit a sample with no interpolation. This accepts either decimal\
        \ numbers or hex numbers that look like `\\`0x\\`xx`.  The hex\
        \ is divided by 255, so they represent a number between 0 and 1.\n\
        \ Setting a control called `<controlname>-rnd` will cause the set value\
        \ to be randomized by the given number." $
        Sig.call0 $ \args -> do
            pos <- Args.real_start args
            maybe_cname <- Derive.lookup_val Environ.control
            rnd <- case maybe_cname of
                Nothing -> return 0
                Just cname -> do
                    rnd_max <- fromMaybe 0 <$> Derive.untyped_control_at
                        (Score.Control $ cname <> "-rnd") pos
                    Util.random_in 0 rnd_max
            return $! Signal.signal [(pos, val + rnd)]
    doc = Derive.extract_doc (set 0)

-- * call map

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("=", Util.c_equal)
    -- Fallback call will take val-call output.
    , ("", c_set)
    , ("set", c_set)
    , ("set-prev", c_set_prev)
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
    , ("d", c_down)
    , ("u", c_up)

    -- not sure which one I'll like better
    , ("`ped`", c_pedal)
    , ("h", c_pedal)
    ]

-- | This should contain the calls that require the previous value.  It's used
-- by a hack in 'Derive.Slice.slice'.
require_previous :: Set.Set Text
require_previous = Set.fromList
    ["'", "i>", "i>>", "i<<", "e>", "e>>", "e<<", "u", "d"]

c_set :: Derive.ControlCall
c_set = Derive.generator1 "set" Tags.prelude
    "Emit a sample with no interpolation." $
    Sig.call (required "val" "Destination value.") $ \val args -> do
        pos <- Args.real_start args
        return $! Signal.signal [(pos, val)]

-- | Re-set the previous val.  This can be used to extend a breakpoint, and is
-- also automatically set by the control track deriver for the hack described
-- in 'Perform.Signal.integrate'.
c_set_prev :: Derive.ControlCall
c_set_prev = Derive.generator "set-prev" Tags.prelude
    ("Re-set the previous value.  This can be used to extend a breakpoint,\
    \ and is also automatically set by the control track deriver for\
    \ the hack described in 'Perform.Signal.integrate'."
    ) $ Sig.call0 $ \args -> case Args.prev_val args of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Args.real_start args
            return $ if pos > prev_x
                then [Signal.signal [(pos, prev_y)]]
                else []

-- * linear

linear_interpolation :: (TrackLang.Typecheck time) =>
    Text -> time -> Text
    -> (Derive.ControlArgs -> time -> Derive.Deriver TrackLang.RealOrScore)
    -> Derive.ControlCall
linear_interpolation name time_default time_default_doc get_time =
    Derive.generator1 name (Tags.prelude <> Tags.prev) doc $ Sig.call ((,)
    <$> required "val" "Destination value."
    <*> defaulted "time" time_default time_doc
    ) $ \(val, time) args ->
        interpolate id args val =<< get_time args time
    where
    doc = "Interpolate from the previous sample to the given one in a straight\
        \ line."
    time_doc = "Time to reach destination. " <> time_default_doc

c_linear_prev :: Derive.ControlCall
c_linear_prev = linear_interpolation "linear" Nothing
    "If not given, start from the previous sample." default_prev

default_prev :: Derive.PassedArgs d -> Maybe TrackLang.DefaultReal
    -> Derive.Deriver TrackLang.RealOrScore
default_prev args Nothing = TrackLang.Real <$> case Args.prev_val args of
    Nothing -> Args.real_start args
    Just (prev, _) -> do
        start <- Args.real_start args
        return $ prev - start
default_prev _ (Just (TrackLang.DefaultReal t)) = return t

c_linear_prev_const :: Derive.ControlCall
c_linear_prev_const =
    linear_interpolation "linear-prev-const" (TrackLang.real (-0.1)) "" $
        \_ -> return . default_real

c_linear_next :: Derive.ControlCall
c_linear_next =
    linear_interpolation "linear-next" Nothing
        "If not given, default to the start of the next event." $
    \args maybe_time -> return $ maybe (next_dur args) default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_linear_next_const :: Derive.ControlCall
c_linear_next_const =
    linear_interpolation "linear-next-const" (TrackLang.real 0.1) "" $
        \_ -> return . default_real


-- * exponential

-- | Exponential interpolation, with different start times.
exponential_interpolation :: (TrackLang.Typecheck time) =>
    Text -> time -> Text
    -> (Derive.ControlArgs -> time -> Derive.Deriver TrackLang.RealOrScore)
    -> Derive.ControlCall
exponential_interpolation name time_default time_default_doc get_time =
    Derive.generator1 name (Tags.prelude <> Tags.prev) doc $ Sig.call ((,,)
    <$> required "val" "Destination value."
    <*> defaulted "exp" 2 exp_doc
    <*> defaulted "time" time_default time_doc
    ) $ \(pitch, exp, time) args ->
        interpolate (expon exp) args pitch =<< get_time args time
    where
    doc = "Interpolate from the previous pitch to the given one in a curve."
    time_doc = "Time to reach destination. " <> time_default_doc

exp_doc :: Text
exp_doc = "Slope of an exponential curve. Positive `n` is taken as `x^n`\
    \ and will generate a slowly departing and rapidly approaching\
    \ curve. Negative `-n` is taken as `x^1/n`, which will generate a\
    \ rapidly departing and slowly approaching curve."

c_exp_prev :: Derive.ControlCall
c_exp_prev = exponential_interpolation "exp-prev" Nothing
    "If not given, start from the previous sample." default_prev

c_exp_prev_const :: Derive.ControlCall
c_exp_prev_const =
    exponential_interpolation "exp-prev-const" (TrackLang.real (-0.1)) "" $
        \_ -> return . default_real

c_exp_next :: Derive.ControlCall
c_exp_next = exponential_interpolation "exp-next" Nothing
        "If not given default to the start of the next event." $
    \args maybe_time -> return $ maybe (next_dur args) default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_exp_next_const :: Derive.ControlCall
c_exp_next_const =
    exponential_interpolation "exp-next-const" (TrackLang.real 0.1) "" $
        \_ -> return . default_real


-- * misc

c_neighbor :: Derive.ControlCall
c_neighbor = Derive.generator1 "neighbor" Tags.prelude
    ("Emit a slide from a value to 0 in absolute time. This is the control\
    \ equivalent of the neighbor pitch call."
    ) $ Sig.call ((,)
    <$> defaulted "neighbor" 1 "Start at this value."
    <*> defaulted "time" (TrackLang.real 0.1) "Time taken to get to 0."
    ) $ \(neighbor, TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        srate <- Util.get_srate
        return $! interpolator srate id True start neighbor end 0

c_down :: Derive.ControlCall
c_down = Derive.generator1 "down" (Tags.prelude <> Tags.prev)
    ("Descend at the given speed until the value reaches 0 or the next event."
    ) $
    Sig.call (defaulted "speed" 1 "Descend this amount per second.") $
    \speed args -> slope args $ \start next prev_y ->
        let diff = RealTime.to_seconds (next - start) * speed
            end = min next
                (start + RealTime.seconds prev_y / RealTime.seconds speed)
        in (end, max 0 (prev_y - diff))

c_up :: Derive.ControlCall
c_up = Derive.generator1 "up" (Tags.prelude <> Tags.prev)
    ("Ascend at the given speed until the value reaches 1 or the next event."
    ) $
    Sig.call (defaulted "speed" 1 "Ascend this amount per second.") $
    \speed args -> slope args $ \start next prev_y ->
        let diff = RealTime.to_seconds (next - start) * speed
            end = min next
                (start + RealTime.seconds (1-prev_y) / RealTime.seconds speed)
        in (end, min 1 (prev_y + diff))

slope :: Derive.ControlArgs
    -> (RealTime -> RealTime -> Signal.Y -> (RealTime, Signal.Y))
    -> Derive.Deriver Signal.Control
slope args f = case Args.prev_val args of
    Nothing -> return Signal.empty
    Just (_, prev_y) -> do
        start <- Args.real_start args
        next <- Derive.real (Args.next args)
        srate <- Util.get_srate
        let (end, dest) = f start next prev_y
        return $ interpolator srate id True start prev_y end dest

c_pedal :: Derive.ControlCall
c_pedal = Derive.generator1 "pedal" mempty
    ("Unlike most control events, this uses a duration. Set the control to\
    \ the given value for the event's duration, and reset to the old\
    \ value afterwards."
    ) $ Sig.call (defaulted "val" 1 "Set to this value.") $
    \val args -> do
        (start, end) <- Args.real_range args
        let prev = maybe 0 snd (Args.prev_val args)
        return $ Signal.signal [(start, val), (end, prev)]

-- * util

default_real :: TrackLang.DefaultReal -> TrackLang.RealOrScore
default_real (TrackLang.DefaultReal t) = t

type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -- ^ start -> starty -> end -> endy
    -> Signal.Control

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate :: (Double -> Double) -> Derive.ControlArgs
    -> Signal.Y -> TrackLang.RealOrScore
    -> Derive.Deriver Signal.Control
interpolate f args val dur = do
    (start, end) <- Util.duration_from_start args dur
    srate <- Util.get_srate
    return $ case Args.prev_val args of
        -- This can happen a lot when the control track is sliced, and is
        -- nothing to worry about.
        Nothing -> Signal.signal [(start, val)]
        Just (_, prev_val) -> interpolator srate f False
            (min start end) prev_val (max start end) val

interpolator :: RealTime -> (Double -> Double) -> Interpolator
interpolator srate f include_initial x1 y1 x2 y2 =
    Signal.signal $ (if include_initial then id else drop 1) sig
    where
    sig = [(x, y_of x) | x <- Seq.range_end x1 x2 srate]
    y_of = Num.scale y1 y2 . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds

-- | Negative exponents produce a curve that jumps from the "starting point"
-- which doesn't seem too useful, so so hijack the negatives as an easier way
-- to write 1/n.  That way n is smoothly departing, while -n is smoothly
-- approaching.
expon :: Double -> Double -> Double
expon n x = x**exp
    where exp = if n >= 0 then n else 1 / abs n

-- ** control modification

multiply_dyn :: (Double -> Double) -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver ()
multiply_dyn = multiply_control Score.c_dynamic

-- | Emit a multiplying modify control.
multiply_control :: Score.Control -> (Double -> Double)
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y -> Derive.Deriver ()
multiply_control control f x1 y1 x2 y2 = do
    sig <- make_signal f x1 y1 x2 y2
    -- Since signals are impliitly 0 before the first sample, the modification
    -- will zero out the control before 'x1'.  That's usually not what I want,
    -- so assume it's 'y1' before that.
    Derive.modify_control Derive.op_mul control (Signal.signal [(0, y1)] <> sig)

add_control :: Score.Control -> (Double -> Double)
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y -> Derive.Deriver ()
add_control control f x1 y1 x2 y2 = do
    sig <- make_signal f x1 y1 x2 y2
    Derive.modify_control Derive.op_add control sig

make_signal :: (Double -> Double) -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
make_signal f x1 y1 x2 y2 = do
    srate <- Util.get_srate
    return $ interpolator srate f True x1 y1 x2 y2
