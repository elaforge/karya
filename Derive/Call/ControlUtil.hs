-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE Rank2Types #-}
-- | Utilities that emit 'Signal.Control's and 'Derive.ControlMod's.
module Derive.Call.ControlUtil where
import qualified Data.Monoid as Monoid

import qualified Util.ApproxEq as ApproxEq
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


-- | Sampling rate.
type SRate = RealTime

-- | Interpolation function.  This maps 0--1 to the desired curve, which is
-- also normalized to 0--1.
type Curve = Double -> Double

-- * interpolator call

-- | Left for an explicit time arg.  Right is for an implicit time, inferred
-- from the args, along with an extra bit of documentation to describe it.
type InterpolatorTime a =
    Either (Sig.Parser TrackLang.Duration) (GetTime a, Text)
type GetTime a = Derive.PassedArgs a -> Derive.Deriver TrackLang.Duration

interpolator_call :: Text
    -> (Sig.Parser arg, arg -> Curve)
    -- ^ get args for the function and the interpolating function
    -> InterpolatorTime Derive.Control -> Derive.Generator Derive.Control
interpolator_call name (get_arg, curve) interpolator_time =
    Derive.generator1 Module.prelude name Tags.prev doc
    $ Sig.call ((,,,)
    <$> Sig.required "to" "Destination value."
    <*> either id (const $ pure $ TrackLang.Real 0) interpolator_time
    <*> get_arg <*> from_env
    ) $ \(to, time, curve_arg, from) args -> do
        time <- if Args.duration args == 0
            then case interpolator_time of
                Left _ -> return time
                Right (get_time, _) -> get_time args
            else TrackLang.Real <$> Args.real_duration args
        interpolate_from_start (curve curve_arg) args
            (prev_val from args) time to
    where
    doc = "Interpolate from the previous value to the given one."
        <> either (const "") ((" "<>) . snd) interpolator_time

-- | Use this for calls that start from the previous value, to give a way
-- to override that behaviour.
from_env :: Sig.Parser (Maybe Signal.Y)
from_env = Sig.environ "from" Sig.Both Nothing
    "Start from this value. If unset, use the previous value."

prev_val :: Maybe Signal.Y -> Derive.ControlArgs -> Maybe Signal.Y
prev_val from args = from <|> (snd <$> Args.prev_control args)

-- | For calls whose curve can be configured.
curve_env :: Sig.Parser Curve
curve_env = cf_to_curve <$>
    Sig.environ "curve" Sig.Both cf_linear "Curve function."

curve_time_env :: Sig.Parser (Curve, RealTime)
curve_time_env = (,) <$> curve_env <*> time
    where time = Sig.environ "curve-time" Sig.Both 0 "Curve transition time."

-- | Create the standard set of interpolator calls.  Generic so it can
-- be used by PitchUtil as well.
interpolator_variations_ :: Derive.Taggable a =>
    (Text -> get_arg -> InterpolatorTime a -> call)
    -> Text -> Text -> get_arg -> [(TrackLang.CallId, call)]
interpolator_variations_ make c name get_arg =
    [ (sym c, make name get_arg prev)
    , (sym $ c <> "<<", make (name <> "-prev-const") get_arg
        (Left prev_time_arg))
    , (sym $ c <> ">", make (name <> "-next") get_arg next)
    , (sym $ c <> ">>", make (name <> "-next-const") get_arg
        (Left next_time_arg))
    ]
    where
    sym = TrackLang.Symbol
    next_time_arg = TrackLang.default_real <$>
        Sig.defaulted "time" default_interpolation_time
            "Time to reach destination."
    prev_time_arg = invert . TrackLang.default_real <$>
        Sig.defaulted "time" default_interpolation_time
            "Time to reach destination, starting before the event."
    invert (TrackLang.Real t) = TrackLang.Real (-t)
    invert (TrackLang.Score t) = TrackLang.Score (-t)

    next = Right (next, "If the event's duration is 0, interpolate from this\
            \ event to the next.")
        where
        next args = return $ TrackLang.Score $ Args.next args - Args.start args
    prev = Right (get_prev_val,
        "If the event's duration is 0, interpolate from the\
        \ previous event to this one.")

default_interpolation_time :: TrackLang.DefaultReal
default_interpolation_time = TrackLang.real 0.1

get_prev_val :: Derive.Taggable a => Derive.PassedArgs a
    -> Derive.Deriver TrackLang.Duration
get_prev_val args = do
    start <- Args.real_start args
    return $ TrackLang.Real $ case Args.prev_val_end args of
        -- It's likely the callee won't use the duration if there's no
        -- prev val.
        Nothing -> 0
        Just prev -> prev - start

interpolator_variations :: Text -> Text -> (Sig.Parser arg, arg -> Curve)
    -> [(TrackLang.CallId, Derive.Generator Derive.Control)]
interpolator_variations = interpolator_variations_ interpolator_call

standard_interpolators ::
    (forall arg. Text -> Text -> (Sig.Parser arg, arg -> Curve)
        -> [(TrackLang.CallId, Derive.Generator result)])
    -> Derive.CallMaps result
standard_interpolators make = Derive.generator_call_map $ concat
    [ make "i" "linear" (pure (), const id)
    , make "e" "exp" exponential_curve
    , make "s" "sigmoid" sigmoid_curve
    ]

exponential_curve :: (Sig.Parser Double, Double -> Curve)
exponential_curve = (args, expon)
    where args = Sig.defaulted "exp" 2 exp_doc

sigmoid_curve :: (Sig.Parser (Double, Double), (Double, Double) -> Curve)
sigmoid_curve = (args, f)
    where
    f (w1, w2) = guess_x $ sigmoid w1 w2
    args = (,)
        <$> Sig.defaulted "w1" 0.5 "Start weight."
        <*> Sig.defaulted "w2" 0.5 "End weight."

-- * control functions

-- | Stuff a curve function into a ControlFunction.
cf_interpolater :: Text -> Curve -> TrackLang.ControlFunction
cf_interpolater name f = TrackLang.ControlFunction name $
    \_ _ -> Score.untyped . f . RealTime.to_seconds

-- | Convert a ControlFunction back into a curve function.
cf_to_curve :: TrackLang.ControlFunction -> Curve
cf_to_curve cf =
    Score.typed_val
    . TrackLang.call_control_function cf Controls.null TrackLang.empty_dynamic
    . RealTime.seconds

cf_linear :: TrackLang.ControlFunction
cf_linear = cf_interpolater "cf-linear" id

-- * interpolate

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate_from_start :: Curve -> Derive.ControlArgs
    -> Maybe Signal.Y -> TrackLang.Duration -> Signal.Y
    -> Derive.Deriver Signal.Control
interpolate_from_start f args from dur to = do
    (start, end) <- Call.duration_from_start args dur
    srate <- Call.get_srate
    return $ case from of
        Nothing -> Signal.signal [(start, to)]
        -- I always set include_initial.  It might be redundant, but if the
        -- previous call was sliced off, it won't be.
        Just from -> segment srate True True f
            (min start end) from (max start end) to

make_segment :: Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
make_segment = make_segment_ True True

make_segment_ :: Bool -> Bool -> Curve -> RealTime -> Signal.Y
    -> RealTime -> Signal.Y -> Derive.Deriver Signal.Control
make_segment_ include_initial include_end f x1 y1 x2 y2 = do
    srate <- Call.get_srate
    return $ segment srate include_initial include_end f x1 y1 x2 y2

-- | Interpolate between the given points.
segment :: SRate -> Bool -- ^ include the initial sample
    -> Bool -- ^ add a sample at end time if one doesn't naturally land there
    -> Curve -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> Signal.Control
segment srate include_initial include_end f x1 y1 x2 y2 =
    Signal.unfoldr go $
        (if include_initial then id else drop 1) (Seq.range_ x1 srate)
    where
    go [] = Nothing
    go (x:xs)
        | x >= x2 = if include_end then Just ((x2, y2), []) else Nothing
        | otherwise = Just ((x, y_of x), xs)
    y_of = Num.scale y1 y2 . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds

-- * exponential

exp_doc :: Text
exp_doc = "Slope of an exponential curve. Positive `n` is taken as `x^n`\
    \ and will generate a slowly departing and rapidly approaching\
    \ curve. Negative `-n` is taken as `x^1/n`, which will generate a\
    \ rapidly departing and slowly approaching curve."

-- | Negative exponents produce a curve that jumps from the \"starting point\"
-- which doesn't seem too useful, so so hijack the negatives as an easier way
-- to write 1/n.  That way n is smoothly departing, while -n is smoothly
-- approaching.
expon :: Double -> Curve
expon n x = x**exp
    where exp = if n >= 0 then n else 1 / abs n

-- | I could probably make a nicer curve of this general shape if I knew more
-- math.
expon2 :: Double -> Double -> Curve
expon2 a b x
    | x >= 1 = 1
    | x < 0.5 = expon a (x * 2) / 2
    | otherwise = expon (-b) ((x-0.5) * 2) / 2 + 0.5

-- * bezier

type Point = (Double, Double)

-- | As far as I can tell, there's no direct way to know what value to give to
-- the bezier function in order to get a specific @x@.  So I guess with binary
-- search.
guess_x :: (Double -> (Double, Double)) -> Curve
guess_x f x1 = go 0 1
    where
    go low high = case ApproxEq.compare threshold x x1 of
        EQ -> y
        LT -> go mid high
        GT -> go low mid
        where
        mid = (low + high) / 2
        (x, y) = f mid
    threshold = 0.00015

-- | Generate a sigmoid curve.  The first weight is the flatness at the start,
-- and the second is the flatness at the end.  Both should range from 0--1.
sigmoid :: Double -> Double -> Double -> Point
sigmoid w1 w2 = bezier3 (0, 0) (w1, 0) (1-w2, 1) (1, 1)

-- | Cubic bezier curve.
bezier3 :: Point -> Point -> Point -> Point -> (Double -> Point)
bezier3 (x1, y1) (x2, y2) (x3, y3) (x4, y4) t =
    (f x1 x2 x3 x4 t, f y1 y2 y3 y4 t)
    where
    f p1 p2 p3 p4 t =
        (1-t)^3 * p1 + 3*(1-t)^2*t * p2 + 3*(1-t)*t^2 * p3 + t^3 * p4

-- * breakpoints

-- | Create line segments between the given breakpoints.
breakpoints :: SRate -> Curve -> [(RealTime, Signal.Y)] -> Signal.Control
breakpoints srate f =
    signal_breakpoints Signal.signal (segment srate True False f)

signal_breakpoints :: Monoid.Monoid sig => ([(RealTime, y)] -> sig)
    -> (RealTime -> y -> RealTime -> y -> sig) -> [(RealTime, y)] -> sig
signal_breakpoints make_signal make_segment = mconcatMap line . Seq.zip_next
    where
    line ((x1, y1), Just (x2, y2)) = make_segment x1 y1 x2 y2
    line ((x1, y2), Nothing) = make_signal [(x1, y2)]

-- | Distribute the values evenly over the given time range.
distribute :: RealTime -> RealTime -> [a] -> [(RealTime, a)]
distribute start end vals = case vals of
    [] -> []
    [x] -> [(start, x)]
    _ -> [(Num.scale start end (n / (len - 1)), x)
        | (n, x) <- zip (Seq.range_ 0 1) vals]
    where len = fromIntegral (length vals)

-- * control mod

multiply_dyn :: RealTime -> Signal.Control -> Derive.Deriver ()
multiply_dyn = multiply_signal Controls.dynamic

-- | Emit a multiplying modify control.
multiply_signal :: Score.Control -> RealTime
    -- ^ End time, after which the signal becomes 1.  This should be set to the
    -- next event, otherwise, all subsequent events will be zeroed.
    -> Signal.Control -> Derive.Deriver ()
multiply_signal control end sig =
    -- Since signals are implicitly 0 before the first sample, the modification
    -- will zero out the control before 'x1'.  That's usually not what I want,
    -- so assume it's 'y1' before that.
    Derive.modify_control (Derive.Merge Derive.op_mul) control $
        initial <> sig <> Signal.signal [(end, 1)]
    where
    initial = case Signal.head sig of
        Nothing -> mempty
        Just (_, y) -> Signal.signal [(0, y)]

add_control :: Score.Control -> (Double -> Double)
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y -> Derive.Deriver ()
add_control control f x1 y1 x2 y2 = do
    sig <- make_segment f x1 y1 x2 y2
    Derive.modify_control (Derive.Merge Derive.op_add) control sig
