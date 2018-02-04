-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ExistentialQuantification #-}
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

-- | Package up a curve name along with arguments.
data CurveD = forall arg. CurveD !Text !(Sig.Parser arg) !(arg -> Curve)

curve_name :: CurveD -> Text
curve_name (CurveD name _ _) = name

data Curve = Function !(Double -> Double)
    -- | Signals can represent linear segments directly, so if I keep track of
    -- them, I can use the efficient direct representation.
    | Linear

-- | Interpolation function.  This maps 0--1 to the desired curve, which is
-- also normalized to 0--1.
type CurveF = Double -> Double

standard_curves :: [(Expr.Symbol, CurveD)]
standard_curves =
    [ ("i", CurveD "linear" (pure ()) (\() -> Linear))
    , ("e", exponential_curve)
    , ("s", sigmoid_curve)
    ]

-- * interpolator call

-- | Left for an explicit time arg.  Right is for an implicit time, inferred
-- from the args, along with an extra bit of documentation to describe it.
type InterpolatorTime a =
    Either (Sig.Parser BaseTypes.Duration) (GetTime a, Text)
type GetTime a = Derive.PassedArgs a -> Derive.Deriver BaseTypes.Duration

interpolator_call :: Text -> CurveD
    -> InterpolatorTime Derive.Control -> Derive.Generator Derive.Control
interpolator_call name_suffix (CurveD name get_arg curve) interpolator_time =
    Derive.generator1 Module.prelude (Derive.CallName (name <> name_suffix))
        Tags.prev doc
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

-- | Create the standard set of interpolator calls.  Generic so it can
-- be used by PitchUtil as well.
interpolator_variations_ :: Derive.Taggable a =>
    (Text -> CurveD -> InterpolatorTime a -> call)
    -> Expr.Symbol -> CurveD -> [(Expr.Symbol, call)]
interpolator_variations_ make (Expr.Symbol sym) curve =
    [ (mksym sym, make "" curve prev)
    , (mksym $ sym <> "<<", make "-prev-const" curve (Left prev_time_arg))
    , (mksym $ sym <> ">", make "-next" curve next)
    , (mksym $ sym <> ">>", make "-next-const" curve (Left next_time_arg))
    ]
    where
    mksym = Expr.Symbol
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

interpolator_variations :: [(Expr.Symbol, Derive.Generator Derive.Control)]
interpolator_variations = concat
    [ interpolator_variations_ interpolator_call sym curve
    | (sym, curve) <- standard_curves
    ]

-- * curve as argument

-- | For calls whose curve can be configured.
curve_env :: Sig.Parser Curve
curve_env = cf_to_curve <$>
    Sig.environ "curve" Sig.Both cf_linear "Curve function."

cf_linear :: BaseTypes.ControlFunction
cf_linear = curve_to_cf "" Linear

-- | A ControlFunction is a generic function, so it can't retain the
-- distinction between Function and Linear.  So I use a grody hack and keep
-- the distinction in a special name.
cf_linear_name :: Text
cf_linear_name = "cf-linear"

curve_time_env :: Sig.Parser (Curve, RealTime)
curve_time_env = (,) <$> curve_env <*> time
    where time = Sig.environ "curve-time" Sig.Both 0 "Curve transition time."

make_curve_call :: Maybe Doc.Doc -> CurveD -> Derive.ValCall
make_curve_call doc (CurveD name get_arg curve) =
    Derive.val_call Module.prelude (Derive.CallName ("cf-" <> name)) Tags.curve
    (fromMaybe ("Interpolation function: " <> Doc.Doc name) doc)
    $ Sig.call get_arg $ \arg _args ->
        return $ curve_to_cf name (curve arg)

-- | Stuff a curve function into a ControlFunction.
curve_to_cf :: Text -> Curve -> BaseTypes.ControlFunction
curve_to_cf name (Function curvef) = BaseTypes.ControlFunction name $
    \_ _ -> Score.untyped . curvef . RealTime.to_seconds
curve_to_cf _ Linear = BaseTypes.ControlFunction cf_linear_name $
    \_ _ -> Score.untyped . RealTime.to_seconds

-- | Convert a ControlFunction back into a curve function.
cf_to_curve :: BaseTypes.ControlFunction -> Curve
cf_to_curve cf@(BaseTypes.ControlFunction name _)
    | name == cf_linear_name = Linear
    | otherwise = Function $ Score.typed_val
        . BaseTypes.call_control_function cf Controls.null
            BaseTypes.empty_dynamic
        . RealTime.seconds

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
    Nothing -> return $ Signal.from_sample start to
    Just from -> make_segment curve start from end to

make_segment :: Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
make_segment curve x1 y1 x2 y2 = do
    srate <- Call.get_srate
    return $ segment srate curve x1 y1 x2 y2

-- | Interpolate between the given points.
segment :: SRate -> Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Signal.Control
segment srate curve x1 y1 x2 y2
    | x1 == x2 && y1 == y2 = mempty
    -- If x1 == x2 I still need to make a vertical segment
    | y1 == y2 = Signal.from_pairs [(x1, y1), (x2, y2)]
    | otherwise = case curve of
        Linear -> Signal.from_pairs [(x1, y1), (x2, y2)]
        Function curvef -> Signal.from_pairs $ map (make curvef) $
            Seq.range_end x1 x2 (1/srate)
    where
    make curvef x
        -- Otherwise if x1==x2 then I get y1.
        | x >= x2 = (x2, y2)
        | otherwise = (x, make_function curvef x1 y1 x2 y2 x)

make_function :: CurveF -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> (RealTime -> Signal.Y)
make_function curvef x1 y1 x2 y2 =
    Num.scale y1 y2 . curvef . Num.normalize (secs x1) (secs x2) . secs
    where secs = RealTime.to_seconds

-- * slope

-- | Make a line with a certain slope, with optional lower and upper limits.
-- TODO I could support Curve but it would make the intercept more complicated.
slope_to_limit :: Maybe Signal.Y -> Maybe Signal.Y
    -> Signal.Y -> Double -> RealTime -> RealTime -> Signal.Control
slope_to_limit low high from slope start end
    | slope == 0 = Signal.from_sample start from
    | Just limit <- if slope < 0 then low else high =
        let intercept = start + max 0 (RealTime.seconds ((limit-from) / slope))
        in if intercept < end then make intercept limit else make end end_y
    | otherwise = make end end_y
    where
    make = segment srate Linear start from
    srate = 1 -- not used for Linear
    end_y = from + RealTime.to_seconds (end - start) * slope

-- * exponential

exponential_curve :: CurveD
exponential_curve = CurveD "expon" args (Function . expon)
    where
    args = Sig.defaulted "expon" 2 exponential_doc

exponential_doc :: Doc.Doc
exponential_doc =
    "Slope of an exponential curve. Positive `n` is taken as `x^n`\
    \ and will generate a slowly departing and rapidly approaching\
    \ curve. Negative `-n` is taken as `x^1/n`, which will generate a\
    \ rapidly departing and slowly approaching curve."

-- | Negative exponents produce a curve that jumps from the \"starting point\"
-- which doesn't seem too useful, so so hijack the negatives as an easier way
-- to write 1/n.  That way n is smoothly departing, while -n is smoothly
-- approaching.
expon :: Double -> CurveF
expon n x = x**exp
    where exp = if n >= 0 then n else 1 / abs n

-- | I could probably make a nicer curve of this general shape if I knew more
-- math.
expon2 :: Double -> Double -> CurveF
expon2 a b x
    | x >= 1 = 1
    | x < 0.5 = expon a (x * 2) / 2
    | otherwise = expon (-b) ((x-0.5) * 2) / 2 + 0.5

-- * bezier

sigmoid_curve :: CurveD
sigmoid_curve = CurveD "sigmoid" args curve
    where
    curve (w1, w2) = Function $ sigmoid w1 w2
    args = (,)
        <$> Sig.defaulted "w1" 0.5 "Start weight."
        <*> Sig.defaulted "w2" 0.5 "End weight."

type Point = (Double, Double)

-- | As far as I can tell, there's no direct way to know what value to give to
-- the bezier function in order to get a specific @x@.  So I guess with binary
-- search.
guess_x :: (Double -> (Double, Double)) -> CurveF
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
sigmoid :: Double -> Double -> CurveF
sigmoid w1 w2 = guess_x $ bezier3 (0, 0) (w1, 0) (1-w2, 1) (1, 1)

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
    signal_breakpoints Signal.from_sample (segment srate curve)
    -- TODO I think if curve==Linear, I can just do Signal.from_samples

signal_breakpoints :: Monoid sig => (RealTime -> y -> sig)
    -> (RealTime -> y -> RealTime -> y -> sig) -> [(RealTime, y)] -> sig
signal_breakpoints make_signal make_segment = mconcatMap line . Seq.zip_next
    where
    line ((x1, y1), Just (x2, y2)) = make_segment x1 y1 x2 y2
    line ((x1, y2), Nothing) = make_signal x1 y2

-- | Distribute the values evenly over the given time range.
distribute :: RealTime -> RealTime -> [a] -> [(RealTime, a)]
distribute start end vals = case vals of
    [] -> []
    [x] -> [(start, x)]
    _ -> [(Num.scale start end (n / (len - 1)), x)
        | (n, x) <- zip (Seq.range_ 0 1) vals]
    where len = fromIntegral (length vals)

-- * smooth

-- | Use the function to create a segment between each point in the signal.
--
-- > 0 1 2 3 4 5 6 7 8
-- > 0-------1-------0
-- > 0-----0=1-----1=0      time = -1
-- > 0-------0=1-----1=0    time = 1
--
-- TODO rename to smooth_absolute
smooth :: Curve -> RealTime -> RealTime
    -- ^ If negative, each segment is from this much before the original sample
    -- until the sample.  If positive, it starts on the sample.  If samples are
    -- too close, the segments are shortened correspondingly.
    -> [(RealTime, Signal.Y)] -> Signal.Control
smooth curve srate time = breakpoints srate curve . split_samples_absolute time

-- | Split apart samples to make a flat segment.
--
-- TODO if y=Pitch there's no Eq, so breakpoints winds up sampling flat
-- segments.  I could emit Maybe y where Nothing means same as previous.
split_samples_absolute :: RealTime -> [(RealTime, y)] -> [(RealTime, y)]
split_samples_absolute time
    | time >= 0 = concatMap split_prev . Seq.zip_neighbors
    | otherwise = concatMap split_next . Seq.zip_next
    where
    split_prev (Nothing, (x1, y1), _) = [(x1, y1)]
    split_prev (Just (_, y0), (x1, y1), next) =
        (x1, y0) : if is_room then [(x1 + time, y1)] else []
        where is_room = maybe True ((x1 + time <) . fst) next
    split_next ((x1, y1), Nothing) = [(x1, y1)]
    split_next ((x1, y1), Just (x2, _)) =
        -- (x1, y1) : if x1 - time < x2 then [(x1 - time, y1)] else []
        (x1, y1) : if x2 + time > x1 then [(x2 + time, y1)] else []

-- | Like 'smooth', but the transition time is a 0--1 proportion of the
-- available time, rather than an absolute time.
--
-- TODO support negative time and make it consistent with
-- 'split_samples_absolute'.
--
-- > 0 1 2 3 4 5 6 7 8
-- > 0-------1-------0
-- > 0-----0=1-----1=0 map time_at [0, 4] = [0.25, 0.25]
smooth_relative :: Curve -> RealTime -> Typecheck.Function
    -> [(RealTime, Signal.Y)] -> Signal.Control
smooth_relative curve srate time_at =
    breakpoints srate curve . split_samples_relative time_at

split_samples_relative :: Typecheck.Function -> [(RealTime, y)]
    -> [(RealTime, y)]
split_samples_relative time_at = concatMap split . Seq.zip_next
    where
    split ((x1, y1), Nothing) = [(x1, y1)]
    split ((x1, y1), Just (x2, _)) =
        (x1, y1) : if offset == 0 then [] else [(x1 + offset, y1)]
        where
        offset = (x2 - x1) * (1 - time)
        time = RealTime.seconds (Num.clamp 0 1 (time_at x1))

-- * control mod

modify :: Score.Control -> RealTime -> Signal.Control -> Derive.Deriver ()
modify = modify_with Derive.DefaultMerge

-- | Modify the signal only in the given range.
modify_with :: Derive.Merge -> Score.Control -> RealTime -> Signal.Control
    -> Derive.Deriver ()
modify_with merge control end sig = do
    merger@(Derive.Merger _ _ identity) <- Derive.resolve_merge merge control
    -- Since signals are implicitly 0 before the first sample, I prepend
    -- a segment with the identity value, in case the identity isn't 0.
    Derive.modify_control merger control $ mconcat
        [ if identity == 0 then mempty else Signal.constant identity
        , sig
        , Signal.from_sample end identity
        ]

multiply_dyn :: RealTime -> Signal.Control -> Derive.Deriver ()
multiply_dyn = modify_with (Derive.Merge Derive.merge_mul) Controls.dynamic
