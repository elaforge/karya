-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE EmptyDataDecls #-}
{- | Sample values are doubles, which means each point in the signal is 8*2
    bytes.  The double resolution is overkill for the value, but float would
    be too small for time given the time stretching.

    TODO split this into Float and Double versions since only Warp really
    needs Double.  Or does Warp really need Double?
-}
module Perform.Signal (
    -- * types
    Signal, Sample(..)
    , X, Y, xToY, yToX, yToScore, yToNn, nnToY
    , Tempo, Warp, Control, NoteNumber, Display

    -- * construct / destruct
    , fromSample, fromPairs, fromSegments
    , toSamples, toPairs, toPairsDesc
    , toSegments, toVector
    , constant, constantVal, constantValFrom
    , zeroOrBelow
    , beginning
    , prepend
    , unfoldr
    , coerce
    , toPiecewiseConstant
    , withPtr

    -- * query
    , null
    , at, atMaybe, segmentAt
    , head, last
    , minimum, maximum
    , find

    -- * transform
    , dropAfter, dropBefore
    , clipAfter, clipBefore, clipBeforeSegments, clipBeforePairs
    , clipAfterKeepLast
    , shift

    , invert, sigAdd, sigSubtract, sigMultiply, sigScale
    , scale, scaleInvert

    -- ** hacks
    , dropDiscontinuityAt

    -- ** scalar transformation
    , scalarMax
    , scalarAdd, scalarSubtract, scalarMultiply, scalarDivide
    , scalarScale
    , mapX, mapY, mapYLinear, mapErr

    -- * special functions
    , integrateInverse, integrate, tempoSrate
) where
import           Prelude hiding (head, last, maximum, minimum, null, drop)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Vector.Storable as Vector
import qualified Foreign

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Segment as Segment
import           Util.Segment (Sample(..), X)
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- * types

-- | A Signal is a 'Segment.Signal' of 'Y' values, which are just Doubles.  It
-- takes a phantom type parameter to make the signal's intended uses a little
-- clearer.  There are type aliases for the various flavors of signal below,
-- but it really is just documentation and anyone who wants to operate on
-- a generic signal can take a @Signal kind@.
newtype Signal kind = Signal Segment.NumSignal
    deriving (Show, Eq, DeepSeq.NFData, Serialize.Serialize)

instance Pretty (Signal kind) where
    format sig = case constantVal sig of
        Just y -> "Signal.constant" Pretty.<+> Pretty.format y
        Nothing -> Pretty.format (_signal sig)

_signal :: Signal kind -> Segment.NumSignal
_signal (Signal sig) = sig

modify :: (Segment.NumSignal -> Segment.NumSignal) -> Signal kind -> Signal kind
modify f = Signal . f . _signal

type Y = Double

instance Semigroup (Signal kind) where
    s1 <> s2
        | null s1 = s2
        | null s2 = s1
        | otherwise = mconcat [s1, s2]

instance Monoid (Signal kind) where
    mempty = Signal Segment.empty
    mappend = (<>)
    mconcat = Signal . Segment.concat (Just (==)) Segment.numInterpolate
        . map _signal . filter (not . null)

-- | This is the type of performer-interpreted controls that go into the
-- event's control map.
type Control = Signal ControlSig
data ControlSig

-- | A tempo is a normal Control signal, except that instead of going into the
-- control map, it gets turned into a Warp and goes into the warp map.
type Tempo = Signal TempoSig
data TempoSig

-- | A tempo warp maps score time to real time.  Of course the type is still
-- (ScoreTime, Y), so functions that process Warps have to convert.
type Warp = Signal WarpSig
data WarpSig

-- | This is the type of pitch signals used by the performer, after the scale
-- has been factored out.
type NoteNumber = Signal NoteNumberSig
data NoteNumberSig

-- | This is the type of signals which are sent to the UI for display.
type Display = Signal DisplaySig
data DisplaySig

xToY :: X -> Y
xToY = RealTime.toSeconds

yToX :: Y -> X
yToX = RealTime.seconds

-- | Some control signals may be interpreted as score time.
yToScore :: Y -> ScoreTime
yToScore = ScoreTime.fromDouble

yToNn :: Y -> Pitch.NoteNumber
yToNn = Pitch.NoteNumber

nnToY :: Pitch.NoteNumber -> Y
nnToY (Pitch.NoteNumber nn) = nn

-- * construct / destruct

fromSample :: X -> Y -> Signal kind
fromSample x y = fromPairs [(x, y)]

fromPairs :: [(X, Y)] -> Signal kind
fromPairs = Signal . Segment.fromPairs

fromSegments :: [Segment.Segment Y] -> Signal kind
fromSegments = Signal . Segment.fromSegments

toSamples :: Signal kind -> [Sample Y]
toSamples = Segment.toSamples . _signal

toPairs :: Signal kind -> [(X, Y)]
toPairs = Lists.dropDups id . Segment.toPairs . _signal
    -- Since Segment functions don't have Eq y, they can emit duplicate
    -- samples.  They should be harmless but they clutter tests.

toPairsDesc :: Signal kind -> [(X, Y)]
toPairsDesc = Lists.dropDups id . Segment.toPairsDesc . _signal

toSegments :: Signal kind -> [Segment.Segment Y]
toSegments = Segment.toSegments . _signal

toVector :: Signal kind -> Vector.Vector (Sample Y)
toVector = Segment.toVector . _signal

constant :: Y -> Signal kind
constant = Signal . Segment.constant

-- | Just if the signal is constant.
constantVal :: Signal kind -> Maybe Y
constantVal (Signal sig) = case Segment.constantVal sig of
    Just y -> Just y
    Nothing
        -- Segment.NumSignal is 0 where not given, so mempty should be the same
        -- as constant 0.  They're not equal, but that's generally true since
        -- I don't normalize away redundant samples.
        | Segment.null sig -> Just 0
        | otherwise -> Nothing

constantValFrom :: X -> Signal kind -> Maybe Y
constantValFrom x = Segment.constantValFrom x . _signal

-- | True if the signal becomes <=0 at any point.  This assumes the signal
-- starts at X==0, which is true of signals from control tracks only.
zeroOrBelow :: Signal kind -> Bool
zeroOrBelow signal = case head signal of
    Nothing -> True
    Just (x, _)
        | x > 0 -> True
        | otherwise -> not $ Segment.allY (>0) (_signal signal)

beginning :: RealTime
beginning = Segment.beginning

prepend :: Signal kind -> Signal kind -> Signal kind
prepend sig1 sig2 = Signal $
    Segment.prepend (Just (==)) Segment.numInterpolate
        (_signal sig1) (_signal sig2)

unfoldr :: (state -> Maybe ((X, Y), state)) -> state -> Signal kind
unfoldr gen state = Signal $ Segment.unfoldr gen state

-- | Sometimes signal types need to be converted.
coerce :: Signal kind1 -> Signal kind2
coerce (Signal vec) = Signal vec

toPiecewiseConstant :: X -> Signal kind -> TimeVector.Unboxed
toPiecewiseConstant srate = Segment.toPiecewiseConstant srate . _signal

-- | 'Segment.with_ptr'.
withPtr :: Display -> (X -> Foreign.Ptr (Sample Y) -> Int -> IO a) -> IO a
withPtr sig = Segment.withPtr (_signal sig)

-- * query

null :: Signal kind -> Bool
null = Segment.null . _signal

at :: Signal kind -> X -> Y
at sig = fromMaybe 0 . atMaybe sig

atMaybe :: Signal kind -> X -> Maybe Y
atMaybe = Segment.at Segment.numInterpolate . _signal

segmentAt :: Signal kind -> X -> Maybe (Segment.Segment Y)
segmentAt = Segment.segmentAt . _signal

head, last :: Signal kind -> Maybe (X, Y)
head = Segment.head . _signal
last = Segment.last . _signal

-- * transform

dropAfter, dropBefore :: X -> Signal kind -> Signal kind
dropAfter x = modify $ Segment.dropAfter x
dropBefore x = modify $ Segment.dropBefore x

clipAfter, clipBefore :: X -> Signal kind -> Signal kind
clipAfter x = modify $ Segment.numClipAfter False x
clipBefore x = modify $ Segment.clipBefore Segment.numInterpolate x

-- TODO if Segment.clip_before_samples is the same as Segment.clip_before,
-- then this is the same as 'clip_before'.
clipBeforeSegments :: X -> Signal kind -> [Segment.Segment Y]
clipBeforeSegments x = Segment.samplesToSegments
    . Segment.clipBeforeSamples Segment.numInterpolate x
    . _signal

clipBeforePairs :: X -> Signal kind -> [(X, Y)]
clipBeforePairs x =
    map TimeVector.toPair
    . Segment.clipBeforeSamples Segment.numInterpolate x
    . _signal

-- | Like 'clip_after', but always put a sample at the end time, even if it's
-- flat.  This is not necessary if you keep this as a Signal since (<>) will
-- extend the final sample, but might be if you go to breakpoints via
-- 'to_pairs'.
clipAfterKeepLast :: X -> Signal kind -> Signal kind
clipAfterKeepLast x = modify $ Segment.numClipAfter True x

shift :: X -> Signal kind -> Signal kind
shift x = modify (Segment.shift x)

invert :: Signal kind -> Signal kind
invert = modify Segment.invert

sigAdd, sigMultiply :: Control -> Control -> Control
sigAdd = linearOperator (Just 0) (+)
sigMultiply = linearOperator (Just 1) (*)

sigSubtract :: Control -> Control -> Control
sigSubtract sig1 sig2
    | Just v <- constantVal sig2, v == 0 = sig1
    | otherwise = linearOperator Nothing (-) sig1 sig2

-- TODO I think this is linear?  Because Num.scale is just add and multiply?
sigScale :: Control -> Control -> Control
sigScale = linearOperator (Just 1) scale

scale :: Y -> Y -> Y
scale x v
    | v >= 0 = Num.scale x 1 v
    | otherwise = Num.scale 0 x (v + 1)

scaleInvert :: Y -> Y -> Y
scaleInvert old new
    | new >= old = Num.normalize old 1 new
    | otherwise = Num.normalize 0 old new - 1

linearOperator :: Maybe Y -- ^ If an identity value is given, I can avoid
    -- copying the whole signal if the other one is a constant identity.
    -> (Y -> Y -> Y) -> Signal kind -> Signal kind -> Signal kind
linearOperator (Just identity) _ sig1 sig2
    | Just v <- constantVal sig1, v == identity = sig2
    | Just v <- constantVal sig2, v == identity = sig1
linearOperator _ op sig1 sig2 =
    Signal $ Segment.linearOperator op (_signal sig1) (_signal sig2)

-- ** hacks

dropDiscontinuityAt :: X -> Control -> Control
dropDiscontinuityAt x = modify $ Segment.dropDiscontinuityAt x

-- ** scalar transformation

scalarAdd, scalarSubtract, scalarMultiply, scalarDivide ::
    Y -> Signal kind -> Signal kind
scalarAdd n = mapYLinear (+n)
scalarSubtract n = mapYLinear (subtract n)
scalarMultiply n = mapYLinear (*n)
scalarDivide n = mapYLinear (/n)

scalarScale :: Y -> Signal kind -> Signal kind
scalarScale n = mapYLinear (scale n)

-- | Clip signal to never go below the given value.
--
-- This is way more complicated than the piecewise constant version.
scalarMax :: Y -> Signal kind -> Signal kind
scalarMax val sig
    | minimum sig >= val = sig
    | otherwise = modify (Segment.transformSamples go) sig
    where
    go [] = []
    go [Sample x y] = [Sample x (max val y)]
    go (s1@(Sample x1 y1) : s2s@(Sample x2 y2 : sn))
        | y1 < val && y2 < val = Sample x1 val : below (s1 : s2s)
        | y1 >= val && y2 >= val = s1 : go s2s
        | otherwise = case TimeVector.xAt x1 y1 x2 y2 val of
            Nothing
                | y1 < val -> Sample x1 val : go (Sample x2 val : sn)
                | otherwise -> s1 : go s2s
            Just xVal
                | y1 < val -> Sample x1 val : go (Sample xVal val : s2s)
                | otherwise -> s1 : Sample xVal val : below s2s
    -- The first sample is below val, discard until it comes back up again.
    below (Sample x1 y1 : s2s@(Sample x2 y2 : _))
        | y2 < val = below s2s
        | y2 == val = go s2s
        | otherwise = case TimeVector.xAt x1 y1 x2 y2 val of
            -- y1 and y2 are both below, should have been caught above.
            Nothing -> below s2s
            Just xVal -> go (Sample xVal val : s2s)
    below [_] = []
    below [] = []

minimum, maximum :: Signal kind -> Y
minimum = fromMaybe 0 . Segment.minimum . _signal
maximum = fromMaybe 0 . Segment.maximum . _signal

find :: (X -> Y -> Bool) -> Signal kind -> Maybe (X, Y)
find f = Segment.find f . _signal

-- | Map Xs.  The slopes will definitely change unless the function is adding
-- a constant, but presumably that's what you want.
{-# SCC mapX #-}
mapX :: (X -> X) -> Signal kind -> Signal kind
mapX = modify . Segment.mapX

-- | Map Ys.  This resamples the signal, so it's valid for a nonlinear
-- function.
{-# SCC mapY #-}
mapY :: X -> (Y -> Y) -> Signal kind -> Signal kind
mapY srate = modify . Segment.mapY srate

-- | If the function is linear, there's no need to resample.
{-# SCC mapYLinear #-}
mapYLinear :: (Y -> Y) -> Signal kind -> Signal kind
mapYLinear = modify . Segment.mapYLinear

mapErr :: (Sample Y -> Either err (Sample Y)) -> Signal kind
    -> (Signal kind, [err])
mapErr f = first Signal . Segment.mapErr f . _signal

-- * special functions

integrateInverse :: Tempo -> Warp
integrateInverse = integrate . mapY tempoSrate (1/)

-- | Integrate the signal.
--
-- Since the output will have more samples than the input, this needs
-- a sampling rate.  The sampling rate determines the resolution of the tempo
-- track.  So it can probably be fairly low resolution before having
-- a noticeable impact.
--
-- TODO this is only called after map_y at srate, so it's already been
-- resampled.  Maybe it would be more efficient to remove srate from
-- Segment.integrate.
integrate :: Tempo -> Warp
integrate = Signal . Segment.integrate tempoSrate . _signal

tempoSrate :: X
tempoSrate = RealTime.seconds 10

-- | Total duration of horizontal segments in the warp signal.  These are
-- the places where 'Warp.compose_hybrid' will emit a 1\/1 line.
_flatDuration :: Warp -> ScoreTime
_flatDuration =
    RealTime.toScore . fst . Vector.foldl' go (0, Segment.Sample 0 0)
        . Segment.toVector . _signal
    where
    go (!acc, Segment.Sample x0 y0) sample@(Segment.Sample x y)
        | y == y0 = (acc + (x - x0), sample)
        | otherwise = (acc, sample)
