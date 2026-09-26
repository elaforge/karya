-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
-- | The 'Signal' type and functions.
module Util.Segment (
    Signal, Interpolate
    , NumSignal
    , Segment(..)
    , X, Sample(..)
    -- * construct / destruct
    , empty
    , constant
    , constantVal, constantValFrom
    , allY
    , beginning
    , fromVector, toVector
    , toSamples
    , fromPairs, toPairs, toPairsDesc
    , fromSegments, toSegments, samplesToSegments
    , simplify
    , unfoldr
    , withPtr

    -- * query
    , null
    , at, atNegative, segmentAt
    , head, last
    , maximum, minimum
    , find

    -- * concat
    , concat, prepend
    -- * slice
    , dropAfter, clipAfter, numClipAfter
    , dropBefore, clipBefore, clipBeforeSamples
    -- * transform
    , shift
    , mapY, mapYLinear, mapX
    , transformSamples, mapErr

    -- ** hacks
    , dropDiscontinuityAt

    -- * NumSignal
    , numInterpolate, numInterpolateS
    , invert
    , integrate
    -- * resample
    , linearOperator
    , resampleNum, resampleMaybe
    , sampleXs, addZeroTransition
    -- * piecewise constant
    , toPiecewiseConstant
#ifdef TESTING
    , module Util.Segment
#endif
) where
import           Prelude hiding (concat, head, last, maximum, minimum, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as Storable

import qualified Foreign

import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector
import           Util.TimeVector (Sample(..), X)
import qualified Util.Vector

import qualified Perform.RealTime as RealTime
import qualified Ui.Types as Types

import           Global


{- | A signal modeled as segments.  Presumably the segments are linear, but
    since you pass your own 'Interpolate', nothing in this module enforces
    that, though there are some transformations that are only valid for linear
    segments.

    A signal has no value before its first sample, and maintains a constant
    value of the last sample forever.  There is an implicit discontinuity
    to the first sample, so if @x@ is the first sample, then @[(x, y), ..]@ is
    implicitly @[(x, Nothing), (x, y), ...]@.  'NumSignal' uses 0 for not set,
    so unless the first @y@ is also 0 it becomes @[(x, 0), (x, y), ..]@.

    This comes with a built-in X offset, so translation is cheap, via 'shift'.

    Each X should be >= the previous X, and there shouldn't be more than two
    equal Xs in a row.  The first ensures that binary search works, and the
    second insures that I don't try to interpolate a zero length segment.
    Construction via 'from_samples' should establish them, and transformations
    should maintain them.

    However, a few functions in here can break them, e.g. 'map_x' and 'invert',
    and I think trying to fix them would be expensive.  So be careful with
    those.  Functions should be robust against zero length segments, but if you
    break ordering you're out of luck.

    If the @y@ value doesn't have an Eq instance, there's no way to filter
    out redundant segments like @[(0, 1), (1, 1), (2, 1)]@.  Functions
    specialized to 'NumSignal' may make some effort to do that, but only if it
    seems worth it.
-}
data Signal v y = Signal X (v (Sample y))
    -- | Explicitly represent a constant signal.  Previously I used
    -- [(beginning, y)], but a single Constant constructor should take quite a
    -- bit less memory, which is good because every numeric literal is
    -- represented as a constant signal.
    | Constant y

deriving instance (Show (v (Sample y)), Show y) => Show (Signal v y)
deriving instance (Eq (v (Sample y)), Eq y) => Eq (Signal v y)

type NumSignal = Signal Storable.Vector Y -- Same as TimeVector.Unboxed
type Y = Double -- same as TimeVector.UnboxedY
type Interpolate y = Sample y -> Sample y -> X -> y

signalOffset :: Signal v y -> X
signalOffset = \case
    Signal offset _ -> offset
    Constant _ -> 0

modifyVector :: (v (Sample y) -> v (Sample y)) -> Signal v y -> Signal v y
modifyVector modify = \case
    Signal o v -> Signal o (modify v)
    Constant y -> Constant y

instance (Pretty (v (Sample y)), Pretty y) => Pretty (Signal v y) where
    format (Signal offset vector) = "Signal" Pretty.<+> Pretty.text offsetS
        Pretty.<+> Pretty.format vector
        where
        offsetS
            | offset > 0 = "+" <> pretty offset
            | otherwise = pretty offset
    format (Constant y) = "Constant" Pretty.<+> Pretty.format y

instance Serialize.Serialize NumSignal where
    put = \case
        Signal offset vec -> Serialize.put offset >> Serialize.put vec
        Constant y ->
            Serialize.put (0 :: X) >> Serialize.put (constantVector y)
    get = do
        offset <- Serialize.get
        vec <- Serialize.get
        pure $ case TimeVector.uncons vec of
            Just (Sample x y, rest)
                | V.null rest && x <= beginning -> Constant y
            _ -> Signal offset vec

instance DeepSeq.NFData (v (Sample y)) => DeepSeq.NFData (Signal v y) where
    rnf (Signal offset vec) = DeepSeq.rnf offset `seq` DeepSeq.rnf vec `seq` ()
    rnf (Constant _) = ()

data Segment y = Segment
    { _x1 :: X, _y1 :: y
    , _x2 :: X, _y2 :: y
    } deriving (Eq, Show)

instance Pretty y => Pretty (Segment y) where
    pretty (Segment x1 y1 x2 y2) = "(" <> pretty x1 <> ", " <> pretty y1
        <> ")--(" <> pretty x2 <> ", " <> pretty y2 <> ")"

-- * construct / destruct

empty :: V.Vector v (Sample y) => Signal v y
empty = Signal 0 V.empty

constant :: y -> Signal v y
constant = Constant

constantVal :: Signal v y -> Maybe y
constantVal = \case
    Constant y -> Just y
    Signal {} -> Nothing

-- | Like 'constant_val', but also can notice if it's constant after a given X.
-- Also NumSignals are implicitly 0 before the first sample.
constantValFrom :: X -> NumSignal -> Maybe Y
constantValFrom from = \case
    Constant y -> Just y
    Signal offset v -> case TimeVector.uncons v of
        Nothing -> Just 0
        -- I compare multiple samples because a track might have redundant
        -- values, but I still want to detect if it's constant.
        Just (Sample x y, rest)
            | x <= from - offset && V.all ((==y) . sy) rest -> Just y
            | V.all ((==0) . sy) v -> Just 0
            | otherwise -> Nothing

allY :: (Y -> Bool) -> NumSignal -> Bool
allY f = \case
    Constant y -> f y
    Signal _ v -> V.all (f . sy) v

-- | Use this as the stand-in for "since the beginning of time."
beginning :: RealTime.RealTime
beginning = -RealTime.larger

fromVector :: v (Sample y) -> Signal v y
fromVector = Signal 0

signal :: V.Vector v (Sample y) => X -> v (Sample y) -> Signal v y
signal offset v
    | V.null v = empty
    | otherwise = Signal offset v

toVector :: V.Vector v (Sample y) => Signal v y -> v (Sample y)
toVector = \case
    Constant y -> V.singleton $ Sample beginning y
    Signal offset v
        | offset == 0 -> v
        | otherwise -> TimeVector.mapX (+offset) v

-- | The final sample extends for "all time".  However, there's no value before
-- the first sample.  The reason is that I'd have to have a zero value for y,
-- and there isn't really an appropriate one for pitch.
--
-- TODO I could simplify straight lines, but then I'd need Eq on y.  Maybe do
-- that separately for NumSignal.
fromSamples :: V.Vector v (Sample y) => [Sample y] -> Signal v y
fromSamples =
    Signal 0 . V.fromList . dropCoincident . dropInitialDup
        . Maybe.catMaybes . snd . List.mapAccumL inOrder Nothing
    where
    -- Since the first sample comes from 0, I can drop leading dups.
    dropInitialDup (s1 : ss@(s2 : _)) | sx s1 == sx s2 = dropInitialDup ss
    dropInitialDup s = s
    -- Drop out-of-order samples.
    inOrder Nothing cur = (Just cur, Just cur)
    inOrder (Just prev) cur
        | sx cur < sx prev = (Just prev, Nothing)
        | otherwise = (Just cur, Just cur)
    -- Abbreviate coincident samples.
    dropCoincident (Sample x1 y1 : _ : sn@(Sample x2 _ : _)) | x1 == x2 =
        dropCoincident $ Sample x1 y1 : sn
    dropCoincident (s1:sn) = s1 : dropCoincident sn
    dropCoincident [] = []

fromSamplesRaw :: V.Vector v (Sample y) => [Sample y] -> Signal v y
fromSamplesRaw = Signal 0 . V.fromList

toSamples :: V.Vector v (Sample y) => Signal v y -> [Sample y]
toSamples = \case
    Constant y -> [Sample beginning y]
    Signal 0 v -> V.toList v
    Signal offset v -> map (plus offset) $ V.toList v
    where plus n (Sample x y) = Sample (n+x) y
-- TODO verify that TimeVector.map_x fuses with V.toList so there is no extra
-- vector, and I can use the simpler version
-- to_samples = V.toList . to_vector

toSamplesDesc :: V.Vector v (Sample y) => Signal v y -> [Sample y]
toSamplesDesc = \case
    Constant y -> [Sample beginning y]
    Signal 0 v -> Util.Vector.toReverseList v
    Signal offset v -> map (plus offset) $ Util.Vector.toReverseList v
    where plus n (Sample x y) = Sample (n+x) y

fromPairs :: V.Vector v (Sample y) => [(X, y)] -> Signal v y
fromPairs = fromSamples . map (uncurry Sample)

-- | from_pairs without the sanity checking.
fromPairsRaw :: V.Vector v (Sample y) => [(X, y)] -> Signal v y
fromPairsRaw = fromSamplesRaw . map (uncurry Sample)

toPairs :: V.Vector v (Sample y) => Signal v y -> [(X, y)]
toPairs = map TimeVector.toPair . toSamples

toPairsDesc :: V.Vector v (Sample y) => Signal v y -> [(X, y)]
toPairsDesc = map TimeVector.toPair . toSamplesDesc

fromSegments :: V.Vector v (Sample y) => [Segment y] -> Signal v y
fromSegments = fromSamples . toList
    where
    toList (Segment x1 y1 x2 y2 : segments) =
        Sample x1 y1 : Sample x2 y2 : toList segments
    toList [] = []

toSegments :: V.Vector v (Sample y) => Signal v y -> [Segment y]
toSegments = samplesToSegments . toSamples

samplesToSegments :: [Sample y] -> [Segment y]
samplesToSegments = go
    where
    go [] = []
    go [Sample x y]
        | x < RealTime.large = [Segment x y RealTime.large y]
        | otherwise = []
    go (Sample x1 y1 : xs@(Sample x2 y2 : _))
        | x1 == x2 = go xs
        | otherwise = Segment x1 y1 x2 y2 : go xs

-- | Simplify away redundant samples.
simplify :: (Eq x, Eq y) => [(x, y)] -> [(x, y)]
simplify ((x1, _) : xys@((x2, _) : _)) | x1 == x2 = simplify xys
simplify xys = go xys
    where
    -- Drop samples in the middle of horizontal and vertical lines.
    go ((x1, y1) : (x2, y2) : xys@((x3, y3) : _))
        | y1 == y2 && y2 == y3 = go ((x1, y1) : xys)
        | x1 == x2 && x2 == x3 = go ((x1, y1) : xys)
    -- Identical samples are always redundant.
    go ((x1, y1) : xys@((x2, y2) : _))
        | x1 == x2 && y1 == y2 = go xys
    go xys = xys
{-# INLINEABLE simplify #-}
{-# SPECIALIZE simplify :: [(X, Y)] -> [(X, Y)] #-}

unfoldr :: V.Vector v (Sample y) => (state -> Maybe ((X, y), state)) -> state
    -> Signal v y
unfoldr gen state = Signal 0 $ TimeVector.unfoldr gen state

-- | Get a Ptr to the vector.  This is 'Vector.Storable.unsafeWith'.
withPtr :: NumSignal -> (X -> Foreign.Ptr (Sample Y) -> Int-> IO b) -> IO b
withPtr sig action = case sig of
    Signal offset v -> TimeVector.withPtr v (action offset)
    Constant y -> TimeVector.withPtr (constantVector y) (action 0)

constantVector :: Y -> Storable.Vector (Sample Y)
constantVector y = Storable.singleton (Sample beginning y)

-- * query

null :: V.Vector v (Sample y) => Signal v y -> Bool
null = \case
    Signal _ v -> V.null v
    Constant {} -> False

-- | The arguments may seem backwards, but I've always done it this way, and it
-- seems to be more convenient in practice.
at :: V.Vector v (Sample y) => Interpolate y -> Signal v y -> X -> Maybe y
at interpolate (Signal offset vec) x_
    | i < 0 = Nothing
    | i + 1 == V.length vec = Just (sy (V.unsafeIndex vec i))
    | otherwise =
        Just $ interpolate (V.unsafeIndex vec i) (V.unsafeIndex vec (i+1)) x
    where
    i = TimeVector.highestIndex x vec
    x = x_ - offset
at _ (Constant y) _ = Just y

-- | Like 'at', but if the x matches a discontinuity, take the value before
-- instead of after.
atNegative :: V.Vector v (Sample y) => Interpolate y -> Signal v y -> X
    -> Maybe y
atNegative interpolate signal x = do
    Segment x1 y1 x2 y2 <- segmentAtOrientation Types.Negative signal x
    pure $ interpolate (Sample x1 y1) (Sample x2 y2) x

segmentAt :: V.Vector v (Sample y) => Signal v y -> X -> Maybe (Segment y)
segmentAt = segmentAtOrientation Types.Positive

segmentAtOrientation :: V.Vector v (Sample y) => Types.Orientation
    -> Signal v y -> X -> Maybe (Segment y)
segmentAtOrientation orient (Signal offset vec) x =
    bump . snd <$> segmentAtV orient (x - offset) vec
    where bump (Segment x1 y1 x2 y2) = Segment (x1+offset) y1 (x2+offset) y2
segmentAtOrientation _ (Constant y) _ =
    Just $ Segment beginning y RealTime.larger y

segmentAtV :: V.Vector v (Sample y) => Types.Orientation -> X -> v (Sample y)
    -> Maybe (Int, Segment y)
segmentAtV orient x vec
    | i < 0 = Nothing
    | otherwise =
        let Sample x1 y1 = V.unsafeIndex vec i
            Sample x2 y2 = if i + 1 >= V.length vec
                then Sample RealTime.large y1
                else V.unsafeIndex vec (i+1)
        in Just (i, Segment x1 y1 x2 y2)
    where
    i = get x vec
    get = case orient of
        Types.Negative -> TimeVector.indexBelow
        Types.Positive -> TimeVector.highestIndex

head :: V.Vector v (Sample y) => Signal v y -> Maybe (X, y)
head = \case
    Constant y -> Just (beginning, y)
    Signal offset v -> case TimeVector.head v of
        Nothing -> Nothing
        Just (Sample x y) -> Just (offset + x, y)

last :: V.Vector v (Sample y) => Signal v y -> Maybe (X, y)
last = \case
    Constant y -> Just (beginning, y)
    Signal offset v -> case TimeVector.last v of
        Nothing -> Nothing
        Just (Sample x y) -> Just (offset + x, y)

minimum, maximum :: (V.Vector v (Sample a), Ord a) => Signal v a -> Maybe a
minimum = \case
    Constant y -> Just y
    Signal _ v
        | V.null v -> Nothing
        | otherwise ->
            Just $ sy $ V.minimumBy (\a b -> compare (sy a) (sy b)) v
maximum = \case
    Constant y -> Just y
    Signal _ v
        | V.null v -> Nothing
        | otherwise ->
            Just $ sy $ V.maximumBy (\a b -> compare (sy a) (sy b)) v

find :: V.Vector v (Sample y) => (X -> y -> Bool) -> Signal v y -> Maybe (X, y)
find f (Signal offset v) = first (+ offset) . TimeVector.toPair <$>
    V.find (\(Sample x y) -> f (x + offset) y) v
find f (Constant y)
    | f beginning y = Just (beginning, y)
    | otherwise = Nothing

-- * concat

-- | Concatenate signals, where signals to the right replace the ones to the
-- left where they overlap.
concat :: V.Vector v (Sample y) => Maybe (y -> y -> Bool)
    -- ^ signals with Eq y can drop some redundant samples
    -> Interpolate y -> [Signal v y] -> Signal v y
concat _ _ [] = empty
concat _ _ [sig] = sig
concat maybeEq interpolate sigs =
    Signal 0 . V.concat . tryStripDuplicates . reverse . chunks . reverse
        . map toVector $ sigs
    where
    chunks [] = []
    chunks [v] = [v]
    -- head of v1 cuts of tail of v2
    -- v1:     |--->        |--->
    -- v2:   |--->        |->
    -- vs: |--->     => |->
    chunks (v1:v2:vs) = case sx <$> TimeVector.head v1 of
        Nothing -> chunks (v2:vs)
        Just x1 -> case TimeVector.last clipped of
            Nothing -> chunks (v1:vs)
            Just end
                | sx end < x1 -> v1 : extension end : chunks (clipped:vs)
                | otherwise -> v1 : chunks (clipped:vs)
            where
            clipped = clipAfterV interpolate x1 v2
            extension end = V.singleton (Sample x1 (sy end))
    tryStripDuplicates = case maybeEq of
        Nothing -> id
        Just eq -> stripDuplicates eq
    -- If I have Eq, I can strip redundant Y values.
    stripDuplicates eq (v1 : v2 : vs)
        | Just (Sample x1 y1) <- TimeVector.last v1
        , Just (Sample x2 y2) <- TimeVector.head v2
        , x1 == x2 && eq y1 y2 =
            v1 : stripDuplicates eq (V.drop 1 v2 : vs)
    stripDuplicates eq (v1 : vs) = v1 : stripDuplicates eq vs
    stripDuplicates _ [] = []

-- | With 'concat', each signal start clips the signal to its left.  This is
-- the other way around, the final sample in the first signal is taken as its
-- end, and it replaces the start of the second signal.
prepend :: V.Vector v (Sample y) => Maybe (y -> y -> Bool) -> Interpolate y
    -> Signal v y -> Signal v y -> Signal v y
prepend eq interpolate sig1 sig2 = case last sig1 of
    Nothing -> sig2
    Just (x, _) -> concat eq interpolate [sig1, clipBefore interpolate x sig2]

-- * slice

-- | Drop the segments after the given time.  The last segment may overlap it.
dropAfter :: V.Vector v (Sample y) => X -> Signal v y -> Signal v y
dropAfter x = \case
    Signal offset v -> signal offset $ dropAfterV (x - offset) v
    sig@(Constant _) -> sig

dropAfterV :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
dropAfterV x vec = case vec V.!? i of
    Nothing -> V.empty
    Just (Sample x1 _) -> V.take (if x1 >= x then i+1 else i+2) vec
    where i = TimeVector.indexBelow x vec

-- | This is like 'drop_after', but meant to clip the signal directly on x,
-- rather than at the first sample >=x.  This means I might have to insert a
-- new sample, which means copying the signal.  This is intended to be a "drop
-- at and after", but since signals extend infinitely to the right, I can only
-- go up to x.  TODO maybe signals should go to Nothing >= the last sample?
--
-- If the signal has only a point exactly at x, then return the empty signal.
-- This is because the first sample is effectively a transition from Nothing,
-- or 0.
clipAfter :: V.Vector v (Sample y) => Interpolate y -> X -> Signal v y
    -> Signal v y
clipAfter interpolate x = \case
    Signal offset v -> signal offset $ clipAfterV interpolate (x - offset) v
    Constant y -> fromPairsRaw [(beginning, y), (x, y)]

clipAfterV :: V.Vector v (Sample y) => Interpolate y -> X
    -> v (Sample y) -> v (Sample y)
clipAfterV interpolate x vec
    | [Sample x0 _] <- V.toList clipped, x0 == x = V.empty
    | otherwise = case TimeVector.last clipped of
        Nothing -> V.empty
        Just (Sample x2 _)
            | x < x2, Just y <- at interpolate (Signal 0 vec) x ->
                V.snoc (V.take (V.length clipped - 1) clipped) (Sample x y)
            | otherwise -> clipped
    where clipped = dropAfterV x vec

numClipAfter :: Bool -> X -> NumSignal -> NumSignal
numClipAfter keepLast x = \case
    Signal offset v
        | V.null clipped -> empty
        | [Sample x0 _] <- V.toList clipped, x0 == x -> empty
        | otherwise -> Signal offset clipped
        where clipped = numClipAfterV keepLast (x - offset) v
    sig@(Constant y)
        | keepLast -> fromPairsRaw [(beginning, y), (x, y)]
        | otherwise -> sig

-- | 'clip_after' specialized for 'Y'.  Since it has Eq, it can do an
-- additional optimization.
numClipAfterV :: Bool -- ^ if False, inhibit the optimization that omits
    -- the end sample if it's a flat line.  If you are going to splice
    -- another signal afterwards, you need the final sample.  If you are
    -- not, you can save a reallocation by omitting the final sample.
    -> X -> TimeVector.Unboxed -> TimeVector.Unboxed
numClipAfterV keepLast x vec = case segmentAtV Types.Negative x vec of
    Nothing -> vec
    Just (i, Segment x1 y1 x2 y2)
        | not keepLast && y1 == y2 -> prefix
        | otherwise -> V.snoc prefix (Sample x (TimeVector.yAt x1 y1 x2 y2 x))
        where prefix = V.take (i+1) vec

-- | Drop the segments before the given time.  The first segment will start at
-- or before the given time.
dropBefore :: V.Vector v (Sample y) => X -> Signal v y -> Signal v y
dropBefore x = \case
    Signal offset v
        | V.null clipped -> empty
        | otherwise -> Signal offset clipped
        where clipped = TimeVector.dropBefore (x - offset) v
    sig@(Constant _) -> sig

-- | Like 'drop_before', but ensure that the signal starts exactly at the given
-- time by splitting a segment that crosses it.
clipBefore :: V.Vector v (Sample y) => Interpolate y -> X -> Signal v y
    -> Signal v y
clipBefore interpolate x sig = case head clipped of
    Nothing -> empty
    Just (x1, _)
        | x1 < x, Just y <- at interpolate sig x ->
            Signal 0 $ V.cons (Sample x y) (V.drop 1 (toVector clipped))
        | otherwise -> clipped
    where clipped = dropBefore x sig

-- TODO is this the same as 'to_samples . clip_before'?
clipBeforeSamples :: V.Vector v (Sample y) => Interpolate y -> X
    -> Signal v y -> [Sample y]
clipBeforeSamples interpolate x sig = case head clipped of
    Nothing -> []
    Just (x1, _)
        | x1 < x, Just y <- at interpolate sig x ->
            Sample x y : toSamples (modifyVector (V.drop 1) clipped)
        | otherwise -> toSamples clipped
    where clipped = dropBefore x sig

-- * transform

-- | Shift the signal in time.
shift :: X -> Signal v y -> Signal v y
shift offset = \case
    Signal o v -> Signal (o + offset) v
    sig@(Constant _) -> sig

-- | Apply the _offset, and set it to 0.  Just for tests.
_flattenShift :: V.Vector v (Sample y) => Signal v y -> Signal v y
_flattenShift = fromVector . toVector

-- | Map Ys.  This resamples the signal, so it's valid for a nonlinear
-- function.
mapY :: X -> (Y -> Y) -> NumSignal -> NumSignal
mapY srate f =
    fromVector . TimeVector.mapY f . toVector . resampleRate srate

-- | Map Ys.  Only valid if the function is linear.
mapYLinear :: V.Vector v (Sample y) => (y -> y) -> Signal v y -> Signal v y
mapYLinear f = \case
    Constant y -> Constant (f y)
    Signal offset v -> Signal offset (TimeVector.mapY f v)

-- | Map Xs.  The slopes will definitely change unless the function is adding
-- a constant, but presumably that's what you want.
--
-- TODO this can break 'Signal' invariants.
mapX :: V.Vector v (Sample y) => (X -> X) -> Signal v y -> Signal v y
mapX f = modifyVector $ TimeVector.mapX f

transformSamples :: V.Vector v (Sample y) => ([Sample y] -> [Sample y])
    -> Signal v y -> Signal v y
transformSamples f = fromSamples . f . toSamples

mapErr :: V.Vector v (Sample y) => (Sample y -> Either err (Sample y))
    -> Signal v y -> (Signal v y, [err])
mapErr f = first fromVector . TimeVector.mapErr f . toVector

-- ** hacks

-- | Drop a x1==x2 discontinuity at the given time, if there is one.
-- Used for Block.trim_controls, which is a terrible hack that I'm trying to
-- get rid of.
dropDiscontinuityAt :: V.Vector v (Sample y) => X -> Signal v y
    -> Signal v y
dropDiscontinuityAt x sig = case V.toList clipped of
    Sample x1 _ : Sample x2 _ : _ | x == x1 && x1 == x2 ->
        fromVector $ V.concat
            [ pre
            -- Insert an extra sample to avoid changing the slope.
            , case (TimeVector.last pre, TimeVector.head post) of
                (Just (Sample _ y), Just (Sample x _)) ->
                    V.singleton (Sample x y)
                _ -> V.empty
            , drop1 post
            ]
            where
            pre = TimeVector.dropAtAfter x vector
            post = TimeVector.dropBeforeAt x vector
    _ -> sig
    where
    vector = toVector sig
    clipped = TimeVector.dropBeforeStrict (x - signalOffset sig) vector
    -- Drop an extra x to avoid >2 samples in the same spot.
    drop1 v = case V.toList v of
        Sample x1 _ : Sample x2 _ : _ | x1 == x2 -> V.drop 1 v
        _ -> v


-- * NumSignal

numInterpolate :: Interpolate Y
numInterpolate (Sample x1 y1) (Sample x2 y2) = TimeVector.yAt x1 y1 x2 y2

numInterpolateS :: Segment Y -> X -> Y
numInterpolateS (Segment x1 y1 x2 y2) = TimeVector.yAt x1 y1 x2 y2

-- | Swap X and Y.  Y must be non-decreasing or this will break 'Signal'
-- invariants.
invert :: NumSignal -> NumSignal
invert = \case
    Signal offset v -> Signal 0 (V.map (swap (toY offset)) v)
    -- A vertical line is not very useful, but I might as well be consistent.
    Constant y -> fromSegments
        [ Segment (toX y) (toY (-RealTime.larger))
            (toX y) (toY RealTime.larger)
        ]
    where
    swap offset (Sample x y) = Sample (toX y) (offset + toY x)
    toX = RealTime.seconds
    toY = RealTime.toSeconds
    -- TODO simpler version, if map_x for offset will fuse
    -- invert sig = Signal 0 (V.map swap (to_vector sig))

-- | Integrate the signal.
--
-- Since the output will have more samples than the input, this needs
-- a sampling rate.  The sampling rate determines the resolution of the tempo
-- track.  So it can probably be fairly low resolution before having
-- a noticeable impact.
integrate :: X -> NumSignal -> NumSignal
integrate srateX =
    fromSamples . List.concat . snd
        . List.mapAccumL segment 0 . map toDouble . toSegments
    where
    -- Integral of nx + k = nx^2 / 2 + kx
    toDouble (Segment x1 y1 x2 y2) = (s x1, y1, s x2, y2)
        where s = RealTime.toSeconds
    toSample x y = Sample (RealTime.seconds x) y
    segment accum (x1, y1, x2, y2) =
        ( f (x2 - x1)
        , if y1 == y2
            then [toSample x1 (f 0), toSample x2 (f (x2 - x1))]
            else [toSample x (f (x - x1)) | x <- Lists.range' x1 x2 (1/srate)]
        )
        where
        f x = n * x^2 / 2 + y1*x + accum
        n = (y2 - y1) / (x2 - x1)
    srate = RealTime.toSeconds srateX

-- * resample

-- | Combine two vectors with the given function.  The signals are resampled
-- to have coincident samples, assuming linear interpolation.  This only works
-- for linear functions, so the result can also be represented with linear
-- segments.
linearOperator :: (Y -> Y -> Y) -> NumSignal -> NumSignal -> NumSignal
linearOperator merge (Constant a) (Constant b) = Constant (merge a b)
linearOperator merge asig bsig =
    fromSamples $ zipWith3 make (getXs ()) as2 bs2
    where
    make x ay by = Sample x (merge ay by)
    as2 = resampleNum (getXs ()) as
    bs2 = resampleNum (getXs ()) bs
    (as, bs) = toSamples2 0 asig bsig
    -- The () is to prevent memoization, which should hopefully allow the
    -- intermediate list to fuse away.  Or maybe I should try to use vectors
    -- instead of lists?
    -- TODO profile
    getXs () = sampleXs2 (map sx as) (map sx bs)

-- | Like 'to_samples', except the signal that starts later gets an extra
-- sample to transition from zero.
toSamples2 :: V.Vector v (Sample y) => y -> Signal v y -> Signal v y
    -> ([Sample y], [Sample y])
toSamples2 zero asig bsig = case (toSamples asig, toSamples bsig) of
    (as@(Sample ax _ : _), bs@(Sample bx _ : _))
        | ax < bx -> (as, Sample bx zero : bs)
        | bx < ax -> (Sample ax zero : as, bs)
    (as, bs) -> (as, bs)

-- | The output has the union of the Xs in the inputs, except where they match
-- exactly.  Discontinuities should get two Xs.
sampleXs2 :: [X] -> [X] -> [X]
sampleXs2 = go
    where
    go [] bs = bs
    go as [] = as
    go (a:as) (b:bs)
        | a == b = a : go as bs
        | a < b = a : go as (b:bs)
        | otherwise = b : go (a:as) bs

-- ** polymorphic implementation

-- | This should be the same as 'linear_operator', except using the
-- variable length functions.  I could replace linear_operator with this, but
-- I worry that it's less efficient.
_linearOperator2 :: ([Y] -> Y) -> NumSignal -> NumSignal -> NumSignal
_linearOperator2 merge asig bsig =
    -- Lists.rotate zips up the samples from each signal.
    fromSamples $ zipWith make xs $ Lists.rotate $
        map (resampleNum xs) samples
    where
    make x ys = Sample x (merge ys)
    xs = sampleXs $ map (map sx) samples
    samples = map (addZeroTransition 0 . toSamples) [asig, bsig]

resample :: y -> Interpolate y -> [X] -> [Sample y] -> [y]
resample = resample_ id

resampleNum :: [X] -> [Sample Y] -> [Y]
resampleNum = resample 0 numInterpolate

-- | This is the same as 'resample', only for ys without a zero.
resampleMaybe :: Interpolate y -> [X] -> [Sample y] -> [Maybe y]
resampleMaybe = resample_ Just Nothing

{-# INLINE resample_ #-}
resample_ :: (y1 -> y2) -> y2 -> Interpolate y1 -> [X] -> [Sample y1] -> [y2]
resample_ present absent interpolate xs samples =
    snd $ List.mapAccumL get samples xs
    where
    get ss@(Sample x1 y1 : s2s@(Sample x2 y2 : _)) x
        -- If it's a discontinuity, I want to consume the sample, or I won't
        -- see the after Y.  Each discontinuity should have 2 Xs, one for
        -- before and one for after.  This is brittle and depends on
        -- 'sample_xs2' emitting two Xs for a discontinuity and 'to_samples2'
        -- adding a "from zero" discontinuity to the first sample.  But
        -- otherwise I'd have to recognize a discontinuity here and emit one,
        -- which means this would have to be concatMap, which seems
        -- inefficient.  Of course maybe the whole thing is already so
        -- inefficient it doesn't matter.
        | x == x1 = if x1 == x2 then (s2s, present y1) else (ss, present y1)
        | x >= x2 = get s2s x
        | x > x1 = (ss, present $ interpolate (Sample x1 y1) (Sample x2 y2) x)
        | otherwise = (ss, absent)
    get ss@[Sample x1 y1] x
        | x >= x1 = (ss, present y1)
        | otherwise = (ss, absent)
    get [] _ = ([], absent)

addZeroTransition :: y -> [Sample y] -> [Sample y]
addZeroTransition zero ss@(Sample x _ : _) = Sample x zero : ss
addZeroTransition _ [] = []

-- | The output has the union of the Xs in the inputs, except where they match
-- exactly.  Discontinuities should get two Xs.  This is the list version of
-- 'sample_xs2'.
sampleXs :: [[X]] -> [X]
sampleXs = go
    where
    go [] = []
    go xss_ = case Lists.minimum xs of
        Nothing -> go (map tail xss)
        Just x -> x : go (map (drop1 (==x)) xss)
        where
        xs = map List.head xss
        xss = filter (not . List.null) xss_
    drop1 f (x:xs) | f x = xs
    drop1 _ xs = xs


-- ** constant rate resamples

-- | This is like 'to_piecewise_constant', except it retains discontinuities,
-- which is important since it's used for 'map_y', which is still operating on
-- linear segments.  Or it's like 'resample', except it uses a constant rate
-- instead of [X].
resampleRate :: X -> NumSignal -> NumSignal
resampleRate srate =
    fromSamples . concatMap resample . Lists.zipNext . toSamples
    where
    resample (Sample x1 y1, Nothing) = [Sample x1 y1]
    resample (Sample x1 y1, Just (Sample x2 y2))
        | y1 == y2 || x1 == x2 = [Sample x1 y1]
        | otherwise =
            [ Sample x (TimeVector.yAt x1 y1 x2 y2 x)
            | x <- Lists.range' x1 x2 (1/srate)
            ]

-- TODO possible vector implementation that might fuse.  But this
-- requires Storable (a, b).
-- resample_rate :: X -> NumSignal -> NumSignal
-- resample_rate srate =
--     from_vector . V.concatMap resample . zip_next . to_vector
--     where
--     zip_next xs = V.zip xs (V.drop 1 xs)
--     resample (Sample x1 y1, Sample x2 y2) = V.fromList
--         [Sample x1 y1, Sample x2 y2]

toPiecewiseConstant :: X -> NumSignal -> TimeVector.Unboxed
toPiecewiseConstant srate =
    V.fromList . Lists.dropDups sy . Lists.dropInitialDups sx . List.concat
        . List.unfoldr make . toSamples
    where
    make [] = Nothing
    make [Sample x y] = Just ([Sample x y], [])
    make (Sample x1 y1 : s2s@(Sample x2 y2 : _))
        | y1 == y2 = Just ([Sample x1 y1], s2s)
        | x1 >= x2 = make s2s
        | otherwise = Just (segment x1 y1 x2 y2, s2s)
    segment x1 y1 x2 y2 =
        [ Sample x (TimeVector.yAt x1 y1 x2 y2 x)
        | x <- Lists.range' x1 x2 (1/srate)
        ]
