-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ExistentialQuantification #-}
-- | Utilities that emit 'Signal.Control's and 'Derive.ControlMod's.
module Derive.Call.ControlUtil where
import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Test.ApproxEq as ApproxEq

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


-- | Sampling rate.
type SRate = RealTime

-- | Package up a curve name along with arguments.
data CurveD = forall arg. CurveD !Text !(Sig.Parser arg) !(arg -> Curve)

curveName :: CurveD -> Text
curveName (CurveD name _ _) = name

data Curve = Function !(Double -> Double)
    -- | Signals can represent linear segments directly, so if I keep track of
    -- them, I can use the efficient direct representation.
    | Linear

-- | Interpolation function.  This maps 0--1 to the desired curve, which is
-- also normalized to 0--1.
type CurveF = Double -> Double

standardCurves :: [(Expr.Symbol, CurveD)]
standardCurves =
    [ ("i", CurveD "linear" (pure ()) (\() -> Linear))
    , ("e", exponentialCurve)
    , ("s", sigmoidCurve)
    ]

-- * interpolator call

-- | Left for an explicit time arg.  Right is for an implicit time, inferred
-- from the args, along with an extra bit of documentation to describe it.
type InterpolatorTime a =
    Either (Sig.Parser DeriveT.Duration) (GetTime a, Text)
type GetTime a = Derive.PassedArgs a -> Derive.Deriver DeriveT.Duration

interpolatorCall :: Text -> CurveD
    -> InterpolatorTime Derive.Control -> Derive.Generator Derive.Control
interpolatorCall nameSuffix (CurveD name getArg curve) interpolatorTime =
    Derive.generator1 Module.prelude (Derive.CallName (name <> nameSuffix))
        Tags.prev doc
    $ Sig.call ((,,,)
    <$> Sig.required "to" "Destination value."
    <*> either id (const $ pure $ DeriveT.RealDuration 0) interpolatorTime
    <*> getArg <*> fromEnv
    ) $ \(to, time, curveArg, from) args -> do
        time <- if Args.duration args == 0
            then case interpolatorTime of
                Left _ -> pure time
                Right (getTime, _) -> getTime args
            else DeriveT.RealDuration <$> Args.realDuration args
        (start, end) <- Call.durationFromStart args time
        makeSegmentFrom (curve curveArg)
            (min start end) (prevVal from args) (max start end) to
    where
    doc = Doc.Doc $ "Interpolate from the previous value to the given one."
        <> either (const "") ((" "<>) . snd) interpolatorTime

-- | Use this for calls that start from the previous value, to give a way
-- to override that behaviour.
fromEnv :: Sig.Parser (Maybe Signal.Y)
fromEnv = Sig.environ "from" Sig.Both (Nothing :: Maybe Sig.Dummy)
    "Start from this value. If unset, use the previous value."

prevVal :: Maybe Signal.Y -> Derive.ControlArgs -> Maybe Signal.Y
prevVal from args = from <|> (snd <$> Args.prevControl args)

-- | Create the standard set of interpolator calls.  Generic so it can
-- be used by PitchUtil as well.
interpolatorVariations_ :: Derive.Taggable a =>
    (Text -> CurveD -> InterpolatorTime a -> call)
    -> Expr.Symbol -> CurveD -> [(Expr.Symbol, call)]
interpolatorVariations_ make (Expr.Symbol sym) curve =
    [ (mksym sym, make "" curve prev)
    , (mksym $ sym <> "<<", make "-prev-const" curve (Left prevTimeArg))
    , (mksym $ sym <> ">", make "-next" curve next)
    , (mksym $ sym <> ">>", make "-next-const" curve (Left nextTimeArg))
    ]
    where
    mksym = Expr.Symbol
    nextTimeArg = Typecheck._real <$>
        Sig.defaulted "time" defaultInterpolationTime
            "Time to reach destination."
    prevTimeArg = invert . Typecheck._real <$>
        Sig.defaulted "time" defaultInterpolationTime
            "Time to reach destination, starting before the event."
    invert (DeriveT.RealDuration t) = DeriveT.RealDuration (-t)
    invert (DeriveT.ScoreDuration t) = DeriveT.ScoreDuration (-t)

    next = Right (next, "If the event's duration is 0, interpolate from this\
            \ event to the next.")
        where
        next args = pure $ DeriveT.ScoreDuration $
            Args.next args - Args.start args
    prev = Right (getPrevVal,
        "If the event's duration is 0, interpolate from the\
        \ previous event to this one.")

defaultInterpolationTime :: Typecheck.DefaultReal
defaultInterpolationTime = Typecheck.real 0.1

getPrevVal :: Derive.Taggable a => Derive.PassedArgs a
    -> Derive.Deriver DeriveT.Duration
getPrevVal args = do
    start <- Args.realStart args
    pure $ DeriveT.RealDuration $ case Args.prevValEnd args of
        -- It's likely the callee won't use the duration if there's no
        -- prev val.
        Nothing -> 0
        Just prev -> prev - start

interpolatorVariations :: [(Expr.Symbol, Derive.Generator Derive.Control)]
interpolatorVariations = concat
    [ interpolatorVariations_ interpolatorCall sym curve
    | (sym, curve) <- standardCurves
    ]

-- * curve as argument

-- | For calls whose curve can be configured.
curveEnv :: Sig.Parser Curve
curveEnv = pfToCurve <$>
    Sig.environ "curve" Sig.Both pfLinear "Curve function."

curveArg :: Sig.Parser Curve
curveArg = pfToCurve <$>
    Sig.defaulted "curve" pfLinear "Curve function."

pfLinear :: DeriveT.PFunction
pfLinear = curveToPf "" Linear

-- | A PFunction is a generic function, so it can't retain the
-- distinction between Function and Linear.  So I use a grody hack and keep
-- the distinction in a special name.
pfLinearName :: Text
pfLinearName = "curve-linear"

curveTimeEnv :: Sig.Parser (Curve, RealTime)
curveTimeEnv = (,) <$> curveEnv <*> time
    where
    time = Sig.environ "curve-time" Sig.Both (0 :: Int) "Curve transition time."

makeCurveCall :: Maybe Doc.Doc -> CurveD -> Derive.ValCall
makeCurveCall doc (CurveD name getArg curve) =
    Derive.valCall Module.prelude (Derive.CallName ("curve-" <> name))
        Tags.curve
    (fromMaybe ("Interpolation function: " <> Doc.Doc name) doc)
    $ Sig.call getArg $ \arg _args ->
        pure $ curveToPf name (curve arg)

-- | Stuff a curve function into a PFunction.
curveToPf :: Text -> Curve -> DeriveT.PFunction
curveToPf name = \case
    Function curvef -> DeriveT.PFunction
        { pfName = name
        , pfFunction = ScoreT.untyped (curvef . RealTime.toSeconds)
        }
    Linear -> DeriveT.PFunction
        { pfName = pfLinearName
        , pfFunction = ScoreT.untyped RealTime.toSeconds
        }

-- | Convert a PFunction back into a curve function.
pfToCurve :: DeriveT.PFunction -> Curve
pfToCurve (DeriveT.PFunction name pf)
    | name == pfLinearName = Linear
    | otherwise = Function (ScoreT.valOf pf . RealTime.seconds)

-- * interpolate

-- | Given a placement, start, and duration, return the range thus implied.
placeRange :: Typecheck.Normalized -> ScoreTime -> DeriveT.Duration
    -> Derive.Deriver (RealTime, RealTime)
placeRange (Typecheck.Normalized place) start dur = do
    start <- Derive.real start
    dur <- Call.realDuration start dur
    -- 0 is before, 1 is after.
    let offset = dur * RealTime.seconds (1 - place)
    pure (start - offset, start + dur - offset)

-- | Make a curve segment from the previous value, if there was one.
makeSegmentFrom :: Curve -> RealTime -> Maybe Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
makeSegmentFrom curve start maybeFrom end to = case maybeFrom of
    Nothing -> pure $ Signal.fromSample start to
    Just from -> makeSegment curve start from end to

makeSegment :: Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
makeSegment curve x1 y1 x2 y2 = do
    srate <- Call.getSrate
    pure $ segment srate curve x1 y1 x2 y2

-- | Interpolate between the given points.
segment :: SRate -> Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Signal.Control
segment srate curve x1 y1 x2 y2
    | x1 == x2 && y1 == y2 = mempty
    -- If x1 == x2 I still need to make a vertical segment
    | y1 == y2 = Signal.fromPairs [(x1, y1), (x2, y2)]
    | otherwise = case curve of
        Linear -> Signal.fromPairs [(x1, y1), (x2, y2)]
        Function curvef -> Signal.fromPairs $ map (make curvef) $
            Lists.rangeEnd x1 x2 (1/srate)
    where
    make curvef x
        -- Otherwise if x1==x2 then I get y1.
        | x >= x2 = (x2, y2)
        | otherwise = (x, makeFunction curvef x1 y1 x2 y2 x)

makeFunction :: CurveF -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> (RealTime -> Signal.Y)
makeFunction curvef x1 y1 x2 y2 =
    Num.scale y1 y2 . curvef . Num.normalize (secs x1) (secs x2) . secs
    where secs = RealTime.toSeconds

-- * slope

-- | Make a line with a certain slope, with optional lower and upper limits.
-- TODO I could support Curve but it would make the intercept more complicated.
slopeToLimit :: Maybe Signal.Y -> Maybe Signal.Y
    -> Signal.Y -> Double -> RealTime -> RealTime -> Signal.Control
slopeToLimit low high from slope start end
    | slope == 0 = Signal.fromSample start from
    | Just limit <- if slope < 0 then low else high =
        let intercept = start + max 0 (RealTime.seconds ((limit-from) / slope))
        in if intercept < end then make intercept limit else make end endY
    | otherwise = make end endY
    where
    make = segment srate Linear start from
    srate = 1 -- not used for Linear
    endY = from + RealTime.toSeconds (end - start) * slope

-- * exponential

exponentialCurve :: CurveD
exponentialCurve = CurveD "expon" args (Function . expon)
    where
    args = Sig.defaulted "expon" (2 :: Double) exponentialDoc

exponentialDoc :: Doc.Doc
exponentialDoc =
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

sigmoidCurve :: CurveD
sigmoidCurve = CurveD "sigmoid" args curve
    where
    curve (w1, w2) = Function $ sigmoid w1 w2
    args = (,)
        <$> Sig.defaulted "w1" (0.5 :: Double) "Start weight."
        <*> Sig.defaulted "w2" (0.5 :: Double) "End weight."

type Point = (Double, Double)

-- | As far as I can tell, there's no direct way to know what value to give to
-- the bezier function in order to get a specific @x@.  So I guess with binary
-- search.
-- https://youtu.be/aVwxzDHniEw?t=1119
guessX :: (Double -> (Double, Double)) -> CurveF
guessX f x1 = go 0 1
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
sigmoid w1 w2 = guessX $ bezier3 (0, 0) (w1, 0) (1-w2, 1) (1, 1)

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
breakpoints _srate Linear = Signal.fromPairs
breakpoints srate curve =
    signalBreakpoints Signal.fromSample (segment srate curve)

signalBreakpoints :: Monoid sig => (RealTime -> y -> sig)
    -> (RealTime -> y -> RealTime -> y -> sig) -> [(RealTime, y)] -> sig
signalBreakpoints makeSignal makeSegment = mconcatMap line . Lists.zipNext
    where
    line ((x1, y1), Just (x2, y2)) = makeSegment x1 y1 x2 y2
    line ((x1, y2), Nothing) = makeSignal x1 y2

-- | Distribute the values evenly over the given time range.
distribute :: RealTime -> RealTime -> [a] -> [(RealTime, a)]
distribute start end vals = case vals of
    [] -> []
    [x] -> [(start, x)]
    _ -> [(Num.scale start end (n / (len - 1)), x)
        | (n, x) <- zip (Lists.range_ 0 1) vals]
    where len = fromIntegral (length vals)

-- * smooth

-- | Use the function to create a segment between each point in the signal.
-- Smooth with 'split_samples_absolute'.
smoothAbsolute :: Curve -> RealTime -> RealTime
    -- ^ If negative, each segment is from this much before the original sample
    -- until the sample.  If positive, it starts on the sample.  If samples are
    -- too close, the segments are shortened correspondingly.
    -> [(RealTime, Signal.Y)] -> Signal.Control
smoothAbsolute curve srate time =
    breakpoints srate curve . splitSamplesAbsolute time

-- | Smooth with 'split_samples_relative'.
smoothRelative :: Curve -> RealTime -> ScoreT.Function
    -> [(RealTime, Signal.Y)] -> Signal.Control
smoothRelative curve srate timeAt =
    breakpoints srate curve . splitSamplesRelative timeAt

-- | Split apart samples to make a flat segment.
--
-- TODO if y=Pitch there's no Eq, so breakpoints winds up sampling flat
-- segments.  I could emit Maybe y where Nothing means same as previous.
--
-- > 0 1 2 3 4 5 6 7 8
-- > 0-------1-------0
-- > 0-----0=1-----1=0      time = -1
-- > 0-------0=1-----1=0    time = 1
splitSamplesAbsolute :: RealTime -> [(RealTime, y)] -> [(RealTime, y)]
splitSamplesAbsolute time
    | time >= 0 = concatMap splitPrev . Lists.zipNeighbors
    | otherwise = concatMap splitNext . Lists.zipNext
    where
    splitPrev (Nothing, (x1, y1), _) = [(x1, y1)]
    splitPrev (Just (_, y0), (x1, y1), next) =
        (x1, y0) : if isRoom then [(x1 + time, y1)] else []
        where isRoom = maybe True ((x1 + time <) . fst) next
    splitNext ((x1, y1), Nothing) = [(x1, y1)]
    splitNext ((x1, y1), Just (x2, _)) =
        (x1, y1) : if x2 + time > x1 then [(x2 + time, y1)] else []

-- | Like 'smooth_absolute', but the transition time is a 0--1 proportion of the
-- available time, rather than an absolute time.  Also, the transition is
-- always before the destination sample, unlike absolute, where it's only
-- before for a negative transition time.  This is because I can't transition
-- after the sample, because the last sample has no next sample to take
-- a proportional time from!
--
-- > 0 1 2 3 4 5 6 7 8
-- > 0-------1-------0
-- > 0-----0=1-----1=0 time_at = const 0.25
splitSamplesRelative :: ScoreT.Function -> [(RealTime, y)]
    -> [(RealTime, y)]
splitSamplesRelative timeAt = concatMap split . Lists.zipNext
    where
    split ((x1, y1), Nothing) = [(x1, y1)]
    split ((x1, y1), Just (x2, _)) =
        (x1, y1) : if offset == 0 then [] else [(x1 + offset, y1)]
        where
        offset = (x2 - x1) * (1 - time)
        time = RealTime.seconds (Num.clamp 0 1 (timeAt x1))

-- * control mod

-- | Modify the signal only in the given range.
modifyWith :: Derive.Merge -> ScoreT.Control -> RealTime
    -- ^ Where the modification should end.  I don't need a start time since
    -- signals already have an implicit start time.
    -> Signal.Control -> Derive.Deriver ()
modifyWith merge control end sig = do
    merger <- Derive.resolveMerge merge control
    -- Since signals are implicitly 0 before the first sample, I prepend
    -- a segment with the identity value, in case the identity isn't 0.
    Derive.modifyControl merger control =<< case merger of
        Derive.Merger _ _ identity -> pure $ mconcat
            [ if identity == 0 then mempty else Signal.constant identity
            , sig
            , Signal.fromSample end identity
            ]
        Derive.Set -> do
            -- There's no identity for Set, so I have to slice the signal
            -- myself.
            maybeOld <- Derive.lookupSignal control
            pure $ case ScoreT.valOf <$> maybeOld of
                Nothing -> sig
                Just old -> old <> sig <> Signal.clipBefore end old
        Derive.Unset -> pure sig

multiplyDyn :: RealTime -> Signal.Control -> Derive.Deriver ()
multiplyDyn = modifyWith (Derive.Merge Derive.mergeMul) Controls.dynamic
