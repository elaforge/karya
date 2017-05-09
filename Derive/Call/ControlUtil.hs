-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE Rank2Types #-}
-- | Utilities that emit 'Signal.Control's and 'Derive.ControlMod's.
module Derive.Call.ControlUtil where
import qualified Util.ApproxEq as ApproxEq
import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

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
    Either (Sig.Parser BaseTypes.Duration) (GetTime a, Text)
type GetTime a = Derive.PassedArgs a -> Derive.Deriver BaseTypes.Duration

interpolator_call :: Derive.CallName
    -> (Sig.Parser arg, arg -> Curve)
    -- ^ get args for the function and the interpolating function
    -> InterpolatorTime Derive.Control -> Derive.Generator Derive.Control
interpolator_call name (get_arg, curve) interpolator_time =
    Derive.generator1 Module.prelude name Tags.prev doc
    $ Sig.call ((,,,)
    <$> Sig.required "to" "Destination value."
    <*> either id (const $ pure $ BaseTypes.RealDuration 0) interpolator_time
    <*> get_arg <*> from_env
    ) $ \(to, time, curve_arg, from) args -> do
        time <- if Args.duration args == 0
            then case interpolator_time of
                Left _ -> return time
                Right (get_time, _) -> get_time args
            else BaseTypes.RealDuration <$> Args.real_duration args
        (start, end) <- Call.duration_from_start args time
        make_segment_from (curve curve_arg)
            (min start end) (prev_val from args) (max start end) to
    where
    doc = Doc.Doc $ "Interpolate from the previous value to the given one."
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
    (Derive.CallName -> get_arg -> InterpolatorTime a -> call)
    -> BaseTypes.CallId -> Derive.CallName -> get_arg
    -> [(BaseTypes.CallId, call)]
interpolator_variations_ make (Expr.CallId sym) (Derive.CallName name)
        get_arg =
    [ (mksym sym, make (Derive.CallName name) get_arg prev)
    , (mksym $ sym <> "<<",
        make (Derive.CallName (name <> "-prev-const")) get_arg
            (Left prev_time_arg))
    , (mksym $ sym <> ">",
        make (Derive.CallName (name <> "-next")) get_arg next)
    , (mksym $ sym <> ">>",
        make (Derive.CallName (name <> "-next-const")) get_arg
            (Left next_time_arg))
    ]
    where
    mksym = Expr.CallId
    next_time_arg = Typecheck._real <$>
        Sig.defaulted "time" default_interpolation_time
            "Time to reach destination."
    prev_time_arg = invert . Typecheck._real <$>
        Sig.defaulted "time" default_interpolation_time
            "Time to reach destination, starting before the event."
    invert (BaseTypes.RealDuration t) = BaseTypes.RealDuration (-t)
    invert (BaseTypes.ScoreDuration t) = BaseTypes.ScoreDuration (-t)

    next = Right (next, "If the event's duration is 0, interpolate from this\
            \ event to the next.")
        where
        next args = return $ BaseTypes.ScoreDuration $
            Args.next args - Args.start args
    prev = Right (get_prev_val,
        "If the event's duration is 0, interpolate from the\
        \ previous event to this one.")

default_interpolation_time :: Typecheck.DefaultReal
default_interpolation_time = Typecheck.real 0.1

get_prev_val :: Derive.Taggable a => Derive.PassedArgs a
    -> Derive.Deriver BaseTypes.Duration
get_prev_val args = do
    start <- Args.real_start args
    return $ BaseTypes.RealDuration $ case Args.prev_val_end args of
        -- It's likely the callee won't use the duration if there's no
        -- prev val.
        Nothing -> 0
        Just prev -> prev - start

interpolator_variations :: BaseTypes.CallId -> Derive.CallName
    -> (Sig.Parser arg, arg -> Curve)
    -> [(BaseTypes.CallId, Derive.Generator Derive.Control)]
interpolator_variations = interpolator_variations_ interpolator_call

standard_interpolators ::
    (forall arg. BaseTypes.CallId -> Derive.CallName
        -> (Sig.Parser arg, arg -> Curve)
        -> [(BaseTypes.CallId, Derive.Generator result)])
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
sigmoid_curve = (args, curve)
    where
    curve (w1, w2) = guess_x $ sigmoid w1 w2
    args = (,)
        <$> Sig.defaulted "w1" 0.5 "Start weight."
        <*> Sig.defaulted "w2" 0.5 "End weight."

-- * control functions

-- | Stuff a curve function into a ControlFunction.
cf_interpolater :: Text -> Curve -> BaseTypes.ControlFunction
cf_interpolater name curve = BaseTypes.ControlFunction name $
    \_ _ -> Score.untyped . curve . RealTime.to_seconds

-- | Convert a ControlFunction back into a curve function.
cf_to_curve :: BaseTypes.ControlFunction -> Curve
cf_to_curve cf =
    Score.typed_val
    . BaseTypes.call_control_function cf Controls.null BaseTypes.empty_dynamic
    . RealTime.seconds

cf_linear :: BaseTypes.ControlFunction
cf_linear = cf_interpolater "cf-linear" id

-- * interpolate

-- | Given a placement, start, and duration, return the range thus implied.
place_range :: Typecheck.Normalized -> ScoreTime -> BaseTypes.Duration
    -> Derive.Deriver (RealTime, RealTime)
place_range (Typecheck.Normalized place) start dur = do
    start <- Derive.real start
    dur <- Call.real_duration start dur
    -- 0 is before, 1 is after.
    let offset = dur * RealTime.seconds (1 - place)
    return (start - offset, start + dur - offset)

make_segment_from :: Curve -> RealTime -> Maybe Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
make_segment_from curve start maybe_from end to = case maybe_from of
    Nothing -> return $ Signal.signal [(start, to)]
    Just from -> make_segment curve start from end to

make_segment :: Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
make_segment = make_segment_ True True
    -- I always set include_initial.  It might be redundant, but if the
    -- previous call was sliced off, it won't be.

make_segment_ :: Bool -> Bool -> Curve -> RealTime -> Signal.Y
    -> RealTime -> Signal.Y -> Derive.Deriver Signal.Control
make_segment_ include_initial include_end curve x1 y1 x2 y2 = do
    srate <- Call.get_srate
    return $ segment srate include_initial include_end curve x1 y1 x2 y2

-- | Interpolate between the given points.
segment :: SRate -> Bool -- ^ include the initial sample
    -> Bool -- ^ add a sample at end time if one doesn't naturally land there
    -> Curve -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> Signal.Control
segment srate include_initial include_end curve x1 y1 x2 y2 =
    Signal.unfoldr go $
        (if include_initial then id else drop 1) (Seq.range_ x1 srate)
    where
    go [] = Nothing
    go (x:xs)
        | x >= x2 = if include_end then Just ((x2, y2), []) else Nothing
        | otherwise = Just ((x, y_at x), xs)
    y_at = make_function curve x1 y1 x2 y2

make_function :: Curve -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> (RealTime -> Signal.Y)
make_function curve x1 y1 x2 y2 =
    Num.scale y1 y2 . curve . Num.normalize (secs x1) (secs x2) . secs
    where secs = RealTime.to_seconds

-- * exponential

exp_doc :: Doc.Doc
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
breakpoints srate curve =
    signal_breakpoints Signal.signal (segment srate True False curve)

signal_breakpoints :: Monoid sig => ([(RealTime, y)] -> sig)
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

modify :: Score.Control -> RealTime -> Signal.Control -> Derive.Deriver ()
modify = modify_with Derive.DefaultMerge

modify_with :: Derive.Merge Signal.Control -> Score.Control -> RealTime
    -> Signal.Control -> Derive.Deriver ()
modify_with merge control end sig = do
    merger@(Derive.Merger _ _ identity) <- Derive.resolve_merge merge control
    Derive.modify_control merger control $
        mconcat [initial identity, sig, id_signal identity end]
    where
    id_signal identity x = case Signal.head identity of
        Just (_, y) | y /= 0 -> Signal.signal [(x, y)]
        _ -> mempty
    initial identity = case Signal.head sig of
        Just (x, _) | x > 0 -> id_signal identity 0
        _ -> mempty

multiply_dyn :: RealTime -> Signal.Control -> Derive.Deriver ()
multiply_dyn = modify_with (Derive.Merge Derive.merge_mul) Controls.dynamic
