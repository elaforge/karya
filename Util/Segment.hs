-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'Signal' type and functions.
module Util.Segment (
    Signal, Segment(..)
    , X, Sample(..)
    -- * construct / destrect
    , empty
    , constant, constant_val
    , from_vector, to_vector
    , from_samples, to_samples
    , from_pairs, to_pairs
    , from_segments, to_segments
    , unfoldr
    , with_ptr

    -- * query
    , null
    , at_interpolate
    , maximum, minimum
    -- , segment_at

    -- * concat
    , concat
    -- * slice
    , before, after
    -- * transform
    , shift
    , linear_map_y, linear_map_x
    -- , map_segments
    , transform_samples, map_err
    -- ** resampling transform
    , linear_operator

    -- * Boxed
    , Boxed
    -- * NumSignal
    , NumSignal
    , invert
    , integrate
) where
import Prelude hiding (concat, null, maximum, minimum)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as Vector.Storable

import qualified Foreign

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector
import Util.TimeVector (X, Sample(..))

import qualified Perform.RealTime as RealTime
import Global


-- | A signal modeled as segments.  Presumably the segments are linear.
-- Nothing in this module enforces that, though there are some transformations
-- that are only valid for linear segments.
--
-- This comes with a built-in X offset, so translation is cheap.
data Signal v = Signal {
    _offset :: !X
    , _vector :: !v
    } deriving (Eq, Show)

data Segment y = Segment {
    _x1 :: !X, _y1 :: !y
    , _x2 :: !X, _y2 :: !y
    } deriving (Eq, Show)

type SignalS v y = Signal (v (Sample y))

instance Pretty v => Pretty (Signal v) where
    format (Signal offset vector) = "Signal" Pretty.<+> Pretty.format offset
        Pretty.<+> Pretty.format vector

instance Serialize.Serialize v => Serialize.Serialize (Signal v) where
    put (Signal offset vec) = Serialize.put offset >> Serialize.put vec
    get = Signal <$> Serialize.get <*> Serialize.get

instance DeepSeq.NFData v => DeepSeq.NFData (Signal v) where
    rnf (Signal offset vec) = DeepSeq.rnf offset `seq` DeepSeq.rnf vec `seq` ()

modify_vector :: (a -> b) -> Signal a -> Signal b
modify_vector modify sig = sig { _vector = modify (_vector sig) }

-- * construct / destruct

empty :: V.Vector v a => Signal (v a)
empty = Signal 0 V.empty

constant :: V.Vector v (Sample y) => y -> SignalS v y
constant a = from_vector $ V.fromList [Sample (-RealTime.large) a]

constant_val :: V.Vector v (Sample a) => SignalS v a -> Maybe a
constant_val sig = case TimeVector.uncons (_vector sig) of
    -- This will naturally disregard 'shift's, which is as it should be for
    -- so-called constant signals.
    Just (Sample x1 y1, rest) | x1 <= -RealTime.large && V.null rest ->
        Just y1
    _ -> Nothing

from_vector :: v -> Signal v
from_vector = Signal 0

to_vector :: V.Vector v (Sample y) => SignalS v y -> v (Sample y)
to_vector sig
    | offset == 0 = _vector sig
    | otherwise = TimeVector.map_x (+offset) (_vector sig)
    where
    offset = _offset sig

-- | The final sample extends for "all time".  However, there's no value before
-- the first sample.  The reason is that I'd have to have a zero value for y,
-- and there isn't really an appropriate one for pitch.
--
-- TODO I could simplify straight lines, but then I'd need Eq on y.  Maybe do
-- that separately for NumSignal.
from_samples :: V.Vector v (Sample y) => [Sample y] -> SignalS v y
from_samples = from_vector . V.fromList . strip
    where
    -- Ignore the out-of-order sample.
    strip (s1@(Sample x1 _) : Sample x2 _ : sn) | x2 < x1 = strip (s1 : sn)
    -- Abbreviate coincident samples.
    strip (Sample x1 y1 : _ : sn@(Sample x2 _ : _)) | x1 == x2 =
        strip $ Sample x1 y1 : sn
    strip (s1:sn) = s1 : strip sn
    strip [] = []

to_samples :: V.Vector v (Sample y) => SignalS v y -> [Sample y]
to_samples = V.toList . to_vector

from_pairs :: V.Vector v (Sample y) => [(X, y)] -> SignalS v y
from_pairs = from_samples . map (uncurry Sample)

to_pairs :: V.Vector v (Sample y) => SignalS v y -> [(X, y)]
to_pairs = map TimeVector.to_pair . to_samples

from_segments :: V.Vector v (Sample y) => [Segment y] -> SignalS v y
from_segments = from_samples . to_list
    where
    to_list (Segment x1 y1 x2 y2 : segments) =
        Sample x1 y1 : Sample x2 y2 : to_list segments
    to_list [] = []

to_segments :: V.Vector v (Sample y) => SignalS v y -> [Segment y]
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

unfoldr :: V.Vector v (Sample y) => (state -> Maybe ((X, y), state)) -> state
    -> SignalS v y
unfoldr gen state = from_vector $ TimeVector.unfoldr gen state

-- | Get a Ptr to the vector.  This is 'Vector.Storable.unsafeWith'.
with_ptr :: Foreign.Storable a =>
    Signal (Vector.Storable.Vector a) -> (X -> Foreign.Ptr a -> Int-> IO b)
    -> IO b
with_ptr (Signal offset vec) f = TimeVector.with_ptr vec $
    \ptr -> f offset ptr (V.length vec)

-- * query

null :: V.Vector v (Sample y) => SignalS v y -> Bool
null = V.null . _vector

-- | The arguments may seem backwards, but I've always done it this way, and it
-- seems to be more convenient in practice.
at_interpolate :: V.Vector v (Sample y) => (X -> y -> X -> y -> X -> y) -> X
    -> SignalS v y -> Maybe y
at_interpolate interpolate x (Signal offset vec)
    | V.null vec = Nothing
    | i + 1 >= V.length vec = Just y0
    | i == -1 = Nothing
    | otherwise = Just $ interpolate x0 y0 x1 y1 (x + offset)
    where
    i = TimeVector.highest_index (x + offset) vec
    TimeVector.Sample x0 y0 = V.unsafeIndex vec i
    TimeVector.Sample x1 y1 = V.unsafeIndex vec (i+1)

minimum, maximum :: (V.Vector v (Sample a), Ord a) => SignalS v a -> Maybe a
minimum sig
    | null sig = Nothing
    | otherwise = Just $ sy $ V.minimumBy (\a b -> compare (sy a) (sy b)) $
        _vector sig
maximum sig
    | null sig = Nothing
    | otherwise = Just $ sy $ V.maximumBy (\a b -> compare (sy a) (sy b)) $
        _vector sig


-- segment_at :: V.Vector v (Sample y) => X -> SignalS v y -> Maybe (Segment y)
-- segment_at x (Signal offset vec)
--     | i < 0 = Nothing
--     | i + 1 >= TimeVector.length vec = Nothing
--     | otherwise =
--         let Sample x1 y1 = V.unsafeIndex vec i
--             Sample x2 y2 = V.unsafeIndex vec (i+1)
--         in Just $ Segment x1 y1 x2 y2
--     where
--     i = TimeVector.highest_index (x + offset) vec


-- * concat

-- | Concatenate signals, where signals to the right replace the ones to the
-- left where they overlap.
concat :: V.Vector v (Sample y) => [SignalS v y] -> SignalS v y
concat [] = empty
concat [x] = x
concat (x : xs) | _offset x /= 0 && all (== _offset x) (map _offset xs) =
    (concat [sig { _offset = 0 } | sig <- x : xs]) { _offset = _offset x }
concat xs =
    from_vector . V.concat . reverse . chunks . reverse . map to_vector $ xs
    where
    chunks [] = []
    chunks [v] = [v]
    -- head of v1 cuts of tail of v2
    -- v1:     |--->        |--->
    -- v2:   |--->        |->
    -- vs: |--->     => |->
    chunks (v1:v2:vs) = case TimeVector.head v1 of
        Nothing -> chunks (v2:vs)
        Just start -> case TimeVector.last clipped of
            Nothing -> chunks (v1:vs)
            Just end
                | sx end < sx start -> v1 : extension end : chunks (clipped:vs)
                | otherwise -> v1 : chunks (clipped:vs)
            where
            clipped = V.take (TimeVector.lowest_index_1 (sx start) v2) v2
            extension end = V.singleton (Sample (sx start) (sy end))

-- * slice

before :: V.Vector v (Sample y) => X -> SignalS v y -> SignalS v y
before x sig = Signal
    { _offset = _offset sig
    , _vector = TimeVector.drop_after ( x + _offset sig) (_vector sig)
    }

after :: V.Vector v (Sample y) => X -> SignalS v y -> SignalS v y
after x sig = Signal
    { _offset = _offset sig
    , _vector = TimeVector.drop_before (x + _offset sig) (_vector sig)
    }

-- * transform

-- | Shift the signal in time.
shift :: X -> Signal v -> Signal v
shift offset sig = sig { _offset = _offset sig + offset }

-- | Map across Ys.  Only valid if the function is linear.
linear_map_y :: V.Vector v (Sample y) => (y -> y) -> SignalS v y -> SignalS v y
linear_map_y f = modify_vector $ TimeVector.map_y f

linear_map_x :: V.Vector v (Sample y) => (X -> X) -> SignalS v y -> SignalS v y
linear_map_x f = modify_vector $ TimeVector.map_x f

-- -- | Transform Segments.  Only valid if the function is linear.
-- map_segments :: V.Vector v (Sample y) => (Segment y -> [Segment y])
--     -> SignalS v y -> SignalS v y
-- map_segments f sig =
--     (from_segments $ concatMap f $ samples_to_segments $ V.toList $ _vector sig)
--         { _offset = _offset sig }

transform_samples :: V.Vector v (Sample y) => ([Sample y] -> [Sample y])
    -> SignalS v y -> SignalS v y
transform_samples f = modify_vector $ _vector . from_samples . f . V.toList

map_err :: V.Vector v (Sample y) => (Sample y -> Either err (Sample y))
    -> SignalS v y -> (SignalS v y, [err])
map_err f = first from_vector . TimeVector.map_err f . to_vector

-- ** resampling tranform

-- | Combine two vectors with the given function, via 'TimeVector.sig_op'.
-- Since the samples are directly transformed, this only works for linear
-- transformations.
linear_operator :: V.Vector v (Sample y) =>
    y -- ^ The implicit y value of a vector before its first sample.  It should
    -- probably be the identity for the operator.
    -> (y -> y -> y) -> SignalS v y -> SignalS v y -> SignalS v y
linear_operator initial combine sig1 sig2 = from_vector $
    TimeVector.sig_op initial combine (to_vector sig1) (to_vector sig2)

-- * Boxed

type Boxed y = Signal (TimeVector.Boxed y)

-- * NumSignal

type NumSignal = Signal TimeVector.Unboxed

invert :: NumSignal -> NumSignal
invert sig = sig { _vector = V.map swap (_vector sig) }
    where
    swap (Sample x y) = Sample (RealTime.seconds y) (RealTime.to_seconds x)

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
    -- Integral of nx + y -> nx^2 / 2 + yx
    to_double (Segment x1 y1 x2 y2) = (s x1, y1, s x2, y2)
        where s = RealTime.to_seconds
    to_sample x y = Sample (RealTime.seconds x) y
    segment accum (x1, y1, x2, y2) = -- Debug.trace_ret "seg" ((x1, y1), (x2, y2))
        ( f (x2 - x1)
        , if y1 == y2
            then [to_sample x1 (f 0), to_sample x2 (f (x2 - x1))]
            else [to_sample x (f (x - x1)) | x <- Seq.range' x1 x2 srate]
        )
        where
        f x = n * x^2 / 2 + y1*x + accum
        n = (y2 - y1) / (x2 - x1)
    srate = RealTime.to_seconds srate_x
