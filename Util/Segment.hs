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
    , constant_val, constant_val_from
    , all_y
    , beginning
    , from_vector, to_vector
    , to_samples
    , from_pairs, to_pairs, to_pairs_desc
    , from_segments, to_segments, samples_to_segments
    , simplify
    , unfoldr
    , with_ptr

    -- * query
    , null
    , at, at_negative, segment_at
    , head, last
    , maximum, minimum
    , find

    -- * concat
    , concat, prepend
    -- * slice
    , drop_after, clip_after, num_clip_after
    , drop_before, clip_before, clip_before_samples
    -- * transform
    , shift
    , map_y, map_y_linear, map_x
    , transform_samples, map_err

    -- ** hacks
    , drop_discontinuity_at

    -- * NumSignal
    , num_interpolate, num_interpolate_s
    , invert
    , integrate
    -- * resample
    , linear_operator
    , resample_num, resample_maybe
    , sample_xs, add_zero_transition
    -- * piecewise constant
    , to_piecewise_constant
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

signal_offset :: Signal v y -> X
signal_offset = \case
    Signal offset _ -> offset
    Constant _ -> 0

modify_vector :: (v (Sample y) -> v (Sample y)) -> Signal v y -> Signal v y
modify_vector modify = \case
    Signal o v -> Signal o (modify v)
    Constant y -> Constant y

instance (Pretty (v (Sample y)), Pretty y) => Pretty (Signal v y) where
    format (Signal offset vector) = "Signal" Pretty.<+> Pretty.text offset_s
        Pretty.<+> Pretty.format vector
        where
        offset_s
            | offset > 0 = "+" <> pretty offset
            | otherwise = pretty offset
    format (Constant y) = "Constant" Pretty.<+> Pretty.format y

instance Serialize.Serialize NumSignal where
    put = \case
        Signal offset vec -> Serialize.put offset >> Serialize.put vec
        Constant y ->
            Serialize.put (0 :: X) >> Serialize.put (constant_vector y)
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

data Segment y = Segment {
    _x1 :: X, _y1 :: y
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

constant_val :: Signal v y -> Maybe y
constant_val = \case
    Constant y -> Just y
    Signal {} -> Nothing

-- | Like 'constant_val', but also can notice if it's constant after a given X.
-- Also NumSignals are implicitly 0 before the first sample.
constant_val_from :: X -> NumSignal -> Maybe Y
constant_val_from from = \case
    Constant y -> Just y
    Signal offset v -> case TimeVector.uncons v of
        Nothing -> Just 0
        -- I compare multiple samples because a track might have redundant
        -- values, but I still want to detect if it's constant.
        Just (Sample x y, rest)
            | x <= from - offset && V.all ((==y) . sy) rest -> Just y
            | V.all ((==0) . sy) v -> Just 0
            | otherwise -> Nothing

all_y :: (Y -> Bool) -> NumSignal -> Bool
all_y f = \case
    Constant y -> f y
    Signal _ v -> V.all (f . sy) v

-- | Use this as the stand-in for "since the beginning of time."
beginning :: RealTime.RealTime
beginning = -RealTime.larger

from_vector :: v (Sample y) -> Signal v y
from_vector = Signal 0

singleton :: V.Vector v (Sample y) => X -> y -> Signal v y
singleton x y = Signal 0 (V.singleton (Sample x y))

signal :: V.Vector v (Sample y) => X -> v (Sample y) -> Signal v y
signal offset v
    | V.null v = empty
    | otherwise = Signal offset v

to_vector :: V.Vector v (Sample y) => Signal v y -> v (Sample y)
to_vector = \case
    Constant y -> V.singleton $ Sample beginning y
    Signal offset v
        | offset == 0 -> v
        | otherwise -> TimeVector.map_x (+offset) v

-- | The final sample extends for "all time".  However, there's no value before
-- the first sample.  The reason is that I'd have to have a zero value for y,
-- and there isn't really an appropriate one for pitch.
--
-- TODO I could simplify straight lines, but then I'd need Eq on y.  Maybe do
-- that separately for NumSignal.
from_samples :: V.Vector v (Sample y) => [Sample y] -> Signal v y
from_samples =
    Signal 0 . V.fromList . drop_coincident . drop_initial_dup
        . Maybe.catMaybes . snd . List.mapAccumL in_order Nothing
    where
    -- Since the first sample comes from 0, I can drop leading dups.
    drop_initial_dup (s1 : ss@(s2 : _)) | sx s1 == sx s2 = drop_initial_dup ss
    drop_initial_dup s = s
    -- Drop out-of-order samples.
    in_order Nothing cur = (Just cur, Just cur)
    in_order (Just prev) cur
        | sx cur < sx prev = (Just prev, Nothing)
        | otherwise = (Just cur, Just cur)
    -- Abbreviate coincident samples.
    drop_coincident (Sample x1 y1 : _ : sn@(Sample x2 _ : _)) | x1 == x2 =
        drop_coincident $ Sample x1 y1 : sn
    drop_coincident (s1:sn) = s1 : drop_coincident sn
    drop_coincident [] = []

to_samples :: V.Vector v (Sample y) => Signal v y -> [Sample y]
to_samples = \case
    Constant y -> [Sample beginning y]
    Signal 0 v -> V.toList v
    Signal offset v -> map (plus offset) $ V.toList v
    where plus n (Sample x y) = Sample (n+x) y
-- TODO verify that TimeVector.map_x fuses with V.toList so there is no extra
-- vector, and I can use the simpler version
-- to_samples = V.toList . to_vector

to_samples_desc :: V.Vector v (Sample y) => Signal v y -> [Sample y]
to_samples_desc = \case
    Constant y -> [Sample beginning y]
    Signal 0 v -> Util.Vector.to_reverse_list v
    Signal offset v -> map (plus offset) $ Util.Vector.to_reverse_list v
    where plus n (Sample x y) = Sample (n+x) y

from_pairs :: V.Vector v (Sample y) => [(X, y)] -> Signal v y
from_pairs = from_samples . map (uncurry Sample)

to_pairs :: V.Vector v (Sample y) => Signal v y -> [(X, y)]
to_pairs = map TimeVector.to_pair . to_samples

to_pairs_desc :: V.Vector v (Sample y) => Signal v y -> [(X, y)]
to_pairs_desc = map TimeVector.to_pair . to_samples_desc

from_segments :: V.Vector v (Sample y) => [Segment y] -> Signal v y
from_segments = from_samples . to_list
    where
    to_list (Segment x1 y1 x2 y2 : segments) =
        Sample x1 y1 : Sample x2 y2 : to_list segments
    to_list [] = []

to_segments :: V.Vector v (Sample y) => Signal v y -> [Segment y]
to_segments = samples_to_segments . to_samples

samples_to_segments :: [Sample y] -> [Segment y]
samples_to_segments = go
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
with_ptr :: NumSignal -> (X -> Foreign.Ptr (Sample Y) -> Int-> IO b) -> IO b
with_ptr sig action = case sig of
    Signal offset v -> TimeVector.with_ptr v (action offset)
    Constant y -> TimeVector.with_ptr (constant_vector y) (action 0)

constant_vector :: Y -> Storable.Vector (Sample Y)
constant_vector y = Storable.singleton (Sample beginning y)

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
    i = TimeVector.highest_index x vec
    x = x_ - offset
at _ (Constant y) _ = Just y

-- | Like 'at', but if the x matches a discontinuity, take the value before
-- instead of after.
at_negative :: V.Vector v (Sample y) => Interpolate y -> Signal v y -> X
    -> Maybe y
at_negative interpolate signal x = do
    Segment x1 y1 x2 y2 <- segment_at_orientation Types.Negative signal x
    return $ interpolate (Sample x1 y1) (Sample x2 y2) x

segment_at :: V.Vector v (Sample y) => Signal v y -> X -> Maybe (Segment y)
segment_at = segment_at_orientation Types.Positive

segment_at_orientation :: V.Vector v (Sample y) => Types.Orientation
    -> Signal v y -> X -> Maybe (Segment y)
segment_at_orientation orient (Signal offset vec) x =
    bump . snd <$> segment_at_v orient (x - offset) vec
    where bump (Segment x1 y1 x2 y2) = Segment (x1+offset) y1 (x2+offset) y2
segment_at_orientation _ (Constant y) _ =
    Just $ Segment beginning y RealTime.larger y

segment_at_v :: V.Vector v (Sample y) => Types.Orientation -> X -> v (Sample y)
    -> Maybe (Int, Segment y)
segment_at_v orient x vec
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
        Types.Negative -> TimeVector.index_below
        Types.Positive -> TimeVector.highest_index

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
find f (Signal offset v) = first (+ offset) . TimeVector.to_pair <$>
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
concat maybe_eq interpolate sigs =
    Signal 0 . V.concat . try_strip_duplicates . reverse . chunks . reverse
        . map to_vector $ sigs
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
            clipped = clip_after_v interpolate x1 v2
            extension end = V.singleton (Sample x1 (sy end))
    try_strip_duplicates = case maybe_eq of
        Nothing -> id
        Just eq -> strip_duplicates eq
    -- If I have Eq, I can strip redundant Y values.
    strip_duplicates eq (v1 : v2 : vs)
        | Just (Sample x1 y1) <- TimeVector.last v1
        , Just (Sample x2 y2) <- TimeVector.head v2
        , x1 == x2 && eq y1 y2 =
            v1 : strip_duplicates eq (V.drop 1 v2 : vs)
    strip_duplicates eq (v1 : vs) = v1 : strip_duplicates eq vs
    strip_duplicates _ [] = []

-- | With 'concat', each signal start clips the signal to its left.  This is
-- the other way around, the final sample in the first signal is taken as its
-- end, and it replaces the start of the second signal.
prepend :: V.Vector v (Sample y) => Maybe (y -> y -> Bool) -> Interpolate y
    -> Signal v y -> Signal v y -> Signal v y
prepend eq interpolate sig1 sig2 = case last sig1 of
    Nothing -> sig2
    Just (x, _) -> concat eq interpolate [sig1, clip_before interpolate x sig2]

-- * slice

-- | Drop the segments after the given time.  The last segment may overlap it.
drop_after :: V.Vector v (Sample y) => X -> Signal v y -> Signal v y
drop_after x = \case
    Signal offset v -> signal offset $ drop_after_v (x - offset) v
    sig@(Constant _) -> sig

drop_after_v :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_after_v x vec = case vec V.!? i of
    Nothing -> V.empty
    Just (Sample x1 _) -> V.take (if x1 >= x then i+1 else i+2) vec
    where i = TimeVector.index_below x vec

-- | This is like 'drop_after', but meant to clip the signal directly on x,
-- rather than at the first sample >=x.  This means I might have to insert a
-- new sample, which means copying the signal.  This is intended to be a "drop
-- at and after", but since signals extend infinitely to the right, I can only
-- go up to x.  TODO maybe signals should go to Nothing >= the last sample?
--
-- If the signal has only a point exactly at x, then return the empty signal.
-- This is because the first sample is effectively a transition from Nothing,
-- or 0.
clip_after :: V.Vector v (Sample y) => Interpolate y -> X -> Signal v y
    -> Signal v y
clip_after interpolate x = \case
    Signal offset v -> signal offset $ clip_after_v interpolate (x - offset) v
    Constant y -> singleton x y

clip_after_v :: V.Vector v (Sample y) => Interpolate y -> X
    -> v (Sample y) -> v (Sample y)
clip_after_v interpolate x vec
    | [Sample x0 _] <- V.toList clipped, x0 == x = V.empty
    | otherwise = case TimeVector.last clipped of
        Nothing -> V.empty
        Just (Sample x2 _)
            | x < x2, Just y <- at interpolate (Signal 0 vec) x ->
                V.snoc (V.take (V.length clipped - 1) clipped) (Sample x y)
            | otherwise -> clipped
    where clipped = drop_after_v x vec

num_clip_after :: Bool -> X -> NumSignal -> NumSignal
num_clip_after keep_last x = \case
    Signal offset v
        | V.null clipped -> empty
        | [Sample x0 _] <- V.toList clipped, x0 == x -> empty
        | otherwise -> Signal offset clipped
        where clipped = num_clip_after_v keep_last (x - offset) v
    Constant y -> singleton x y

-- | 'clip_after' specialized for 'Y'.  Since it has Eq, it can do an
-- additional optimization.
num_clip_after_v :: Bool -- ^ if False, inhibit the optimization that omits
    -- the end sample if it's a flat line
    -> X -> TimeVector.Unboxed -> TimeVector.Unboxed
num_clip_after_v keep_last x vec = case segment_at_v Types.Negative x vec of
    Nothing -> vec
    Just (i, Segment x1 y1 x2 y2)
        | not keep_last && y1 == y2 -> prefix
        | otherwise -> V.snoc prefix (Sample x (TimeVector.y_at x1 y1 x2 y2 x))
        where prefix = V.take (i+1) vec

-- | Drop the segments before the given time.  The first segment will start at
-- or before the given time.
drop_before :: V.Vector v (Sample y) => X -> Signal v y -> Signal v y
drop_before x = \case
    Signal offset v
        | V.null clipped -> empty
        | otherwise -> Signal offset clipped
        where clipped = TimeVector.drop_before (x - offset) v
    sig@(Constant _) -> sig

-- | Like 'drop_before', but ensure that the signal starts exactly at the given
-- time by splitting a segment that crosses it.
clip_before :: V.Vector v (Sample y) => Interpolate y -> X -> Signal v y
    -> Signal v y
clip_before interpolate x sig = case head clipped of
    Nothing -> empty
    Just (x1, _)
        | x1 < x, Just y <- at interpolate sig x ->
            Signal 0 $ V.cons (Sample x y) (V.drop 1 (to_vector clipped))
        | otherwise -> clipped
    where clipped = drop_before x sig

-- TODO is this the same as 'to_samples . clip_before'?
clip_before_samples :: V.Vector v (Sample y) => Interpolate y -> X
    -> Signal v y -> [Sample y]
clip_before_samples interpolate x sig = case head clipped of
    Nothing -> []
    Just (x1, _)
        | x1 < x, Just y <- at interpolate sig x ->
            Sample x y : to_samples (modify_vector (V.drop 1) clipped)
        | otherwise -> to_samples clipped
    where clipped = drop_before x sig

-- * transform

-- | Shift the signal in time.
shift :: X -> Signal v y -> Signal v y
shift offset = \case
    Signal o v -> Signal (o + offset) v
    sig@(Constant _) -> sig

-- | Apply the _offset, and set it to 0.  Just for tests.
_flatten_shift :: V.Vector v (Sample y) => Signal v y -> Signal v y
_flatten_shift = from_vector . to_vector

-- | Map Ys.  This resamples the signal, so it's valid for a nonlinear
-- function.
map_y :: X -> (Y -> Y) -> NumSignal -> NumSignal
map_y srate f =
    from_vector . TimeVector.map_y f . to_vector . resample_rate srate

-- | Map Ys.  Only valid if the function is linear.
map_y_linear :: V.Vector v (Sample y) => (y -> y) -> Signal v y -> Signal v y
map_y_linear f = \case
    Constant y -> Constant (f y)
    Signal offset v -> Signal offset (TimeVector.map_y f v)

-- | Map Xs.  The slopes will definitely change unless the function is adding
-- a constant, but presumably that's what you want.
--
-- TODO this can break 'Signal' invariants.
map_x :: V.Vector v (Sample y) => (X -> X) -> Signal v y -> Signal v y
map_x f = modify_vector $ TimeVector.map_x f

transform_samples :: V.Vector v (Sample y) => ([Sample y] -> [Sample y])
    -> Signal v y -> Signal v y
transform_samples f = from_samples . f . to_samples

map_err :: V.Vector v (Sample y) => (Sample y -> Either err (Sample y))
    -> Signal v y -> (Signal v y, [err])
map_err f = first from_vector . TimeVector.map_err f . to_vector

-- ** hacks

-- | Drop a x1==x2 discontinuity at the given time, if there is one.
-- Used for Block.trim_controls, which is a terrible hack that I'm trying to
-- get rid of.
drop_discontinuity_at :: V.Vector v (Sample y) => X -> Signal v y
    -> Signal v y
drop_discontinuity_at x sig = case V.toList clipped of
    Sample x1 _ : Sample x2 _ : _ | x == x1 && x1 == x2 ->
        from_vector $ V.concat
            [ pre
            -- Insert an extra sample to avoid changing the slope.
            , case (TimeVector.last pre, TimeVector.head post) of
                (Just (Sample _ y), Just (Sample x _)) ->
                    V.singleton (Sample x y)
                _ -> V.empty
            , drop1 post
            ]
            where
            pre = TimeVector.drop_at_after x vector
            post = TimeVector.drop_before_at x vector
    _ -> sig
    where
    vector = to_vector sig
    clipped = TimeVector.drop_before_strict (x - signal_offset sig) vector
    -- Drop an extra x to avoid >2 samples in the same spot.
    drop1 v = case V.toList v of
        Sample x1 _ : Sample x2 _ : _ | x1 == x2 -> V.drop 1 v
        _ -> v


-- * NumSignal

num_interpolate :: Interpolate Y
num_interpolate (Sample x1 y1) (Sample x2 y2) = TimeVector.y_at x1 y1 x2 y2

num_interpolate_s :: Segment Y -> X -> Y
num_interpolate_s (Segment x1 y1 x2 y2) = TimeVector.y_at x1 y1 x2 y2

-- | Swap X and Y.  Y must be non-decreasing or this will break 'Signal'
-- invariants.
invert :: NumSignal -> NumSignal
invert = \case
    Signal offset v -> Signal 0 (V.map (swap (to_y offset)) v)
    -- A vertical line is not very useful, but I might as well be consistent.
    Constant y -> from_segments
        [ Segment (to_x y) (to_y (-RealTime.larger))
            (to_x y) (to_y RealTime.larger)
        ]
    where
    swap offset (Sample x y) = Sample (to_x y) (offset + to_y x)
    to_x = RealTime.seconds
    to_y = RealTime.to_seconds
    -- TODO simpler version, if map_x for offset will fuse
    -- invert sig = Signal 0 (V.map swap (to_vector sig))

-- | Integrate the signal.
--
-- Since the output will have more samples than the input, this needs
-- a sampling rate.  The sampling rate determines the resolution of the tempo
-- track.  So it can probably be fairly low resolution before having
-- a noticeable impact.
integrate :: X -> NumSignal -> NumSignal
integrate srate_x =
    from_samples . List.concat . snd
        . List.mapAccumL segment 0 . map to_double . to_segments
    where
    -- Integral of nx + k = nx^2 / 2 + kx
    to_double (Segment x1 y1 x2 y2) = (s x1, y1, s x2, y2)
        where s = RealTime.to_seconds
    to_sample x y = Sample (RealTime.seconds x) y
    segment accum (x1, y1, x2, y2) =
        ( f (x2 - x1)
        , if y1 == y2
            then [to_sample x1 (f 0), to_sample x2 (f (x2 - x1))]
            else [to_sample x (f (x - x1)) | x <- Lists.range' x1 x2 (1/srate)]
        )
        where
        f x = n * x^2 / 2 + y1*x + accum
        n = (y2 - y1) / (x2 - x1)
    srate = RealTime.to_seconds srate_x

-- * resample

-- | Combine two vectors with the given function.  The signals are resampled
-- to have coincident samples, assuming linear interpolation.  This only works
-- for linear functions, so the result can also be represented with linear
-- segments.
linear_operator :: (Y -> Y -> Y) -> NumSignal -> NumSignal -> NumSignal
linear_operator merge (Constant a) (Constant b) = Constant (merge a b)
linear_operator merge asig bsig =
    from_samples $ zipWith3 make (get_xs ()) as2 bs2
    where
    make x ay by = Sample x (merge ay by)
    as2 = resample_num (get_xs ()) as
    bs2 = resample_num (get_xs ()) bs
    (as, bs) = to_samples2 0 asig bsig
    -- The () is to prevent memoization, which should hopefully allow the
    -- intermediate list to fuse away.  Or maybe I should try to use vectors
    -- instead of lists?
    -- TODO profile
    get_xs () = sample_xs2 (map sx as) (map sx bs)

-- | Like 'to_samples', except the signal that starts later gets an extra
-- sample to transition from zero.
to_samples2 :: V.Vector v (Sample y) => y -> Signal v y -> Signal v y
    -> ([Sample y], [Sample y])
to_samples2 zero asig bsig = case (to_samples asig, to_samples bsig) of
    (as@(Sample ax _ : _), bs@(Sample bx _ : _))
        | ax < bx -> (as, Sample bx zero : bs)
        | bx < ax -> (Sample ax zero : as, bs)
    (as, bs) -> (as, bs)

-- | The output has the union of the Xs in the inputs, except where they match
-- exactly.  Discontinuities should get two Xs.
sample_xs2 :: [X] -> [X] -> [X]
sample_xs2 = go
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
_linear_operator2 :: ([Y] -> Y) -> NumSignal -> NumSignal -> NumSignal
_linear_operator2 merge asig bsig =
    -- Lists.rotate zips up the samples from each signal.
    from_samples $ zipWith make xs $ Lists.rotate $
        map (resample_num xs) samples
    where
    make x ys = Sample x (merge ys)
    xs = sample_xs $ map (map sx) samples
    samples = map (add_zero_transition 0 . to_samples) [asig, bsig]

resample :: y -> Interpolate y -> [X] -> [Sample y] -> [y]
resample = resample_ id

resample_num :: [X] -> [Sample Y] -> [Y]
resample_num = resample 0 num_interpolate

-- | This is the same as 'resample', only for ys without a zero.
resample_maybe :: Interpolate y -> [X] -> [Sample y] -> [Maybe y]
resample_maybe = resample_ Just Nothing

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

add_zero_transition :: y -> [Sample y] -> [Sample y]
add_zero_transition zero ss@(Sample x _ : _) = Sample x zero : ss
add_zero_transition _ [] = []

-- | The output has the union of the Xs in the inputs, except where they match
-- exactly.  Discontinuities should get two Xs.  This is the list version of
-- 'sample_xs2'.
sample_xs :: [[X]] -> [X]
sample_xs = go
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
resample_rate :: X -> NumSignal -> NumSignal
resample_rate srate =
    from_samples . concatMap resample . Lists.zipNext . to_samples
    where
    resample (Sample x1 y1, Nothing) = [Sample x1 y1]
    resample (Sample x1 y1, Just (Sample x2 y2))
        | y1 == y2 || x1 == x2 = [Sample x1 y1]
        | otherwise =
            [ Sample x (TimeVector.y_at x1 y1 x2 y2 x)
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

to_piecewise_constant :: X -> NumSignal -> TimeVector.Unboxed
to_piecewise_constant srate =
    V.fromList . Lists.dropDups sy . Lists.dropInitialDups sx . List.concat
        . List.unfoldr make . to_samples
    where
    make [] = Nothing
    make [Sample x y] = Just ([Sample x y], [])
    make (Sample x1 y1 : s2s@(Sample x2 y2 : _))
        | y1 == y2 = Just ([Sample x1 y1], s2s)
        | x1 >= x2 = make s2s
        | otherwise = Just (segment x1 y1 x2 y2, s2s)
    segment x1 y1 x2 y2 =
        [ Sample x (TimeVector.y_at x1 y1 x2 y2 x)
        | x <- Lists.range' x1 x2 (1/srate)
        ]
