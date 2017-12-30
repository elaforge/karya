-- | A signal modeled as linear segments.
module Util.Linear (
    NumSignal
    , Signal, Segment(..)
    , X
    -- * construct
    , empty, from_vector, from_samples, from_pairs, from_segments
    -- * destruct
    , to_pairs, to_vector, to_segments
    -- * access
    , at_interpolate, segment_at
    -- * concat
    , concat
) where
import Prelude hiding (concat)
import qualified Data.Vector.Generic as V

import qualified Util.Pretty as Pretty
import qualified Util.TimeVector as TimeVector
import Util.TimeVector (X, Sample(..))

import qualified Perform.RealTime as RealTime
import Global


{-
    The in-memory representation should be like TimeVector: (x, y), where two
    concurrent xs denote a discontinuity.

    But the high-level API shouldn't give access to samples, and I should be
    able to slice without being error prone.

    Linear transformations keep segments.
    Integration should keep them too if possible.

    Can composition keep them?


    Problems I'm trying to solve:

    - Need to remember to use Signal.set, and provide previous y.  Signals
    should be implicitly flat in both directions, and merging them should
    respect that.

    - Transpose signal resampling should "just work", without needing special
    'at_before' stuff.  Why does it require that now?

    - Awkward rules where the sample at 0 sets values before.  Or at least the
    part where it leads to errors when asking for the value at <0.

    - Include shift so I can get rid of Score.Event 'untransformed' nonsense.
    I think the problem before is having to use the shift on all signal
    operations even when it's 0 for everyone except Event seemed over complex.
    Also that I just want a per-event offset, not one for each signal inside.

    Though if I extend this to stretch as well, then I don't need a separate
    Warp type.  I don't think anyone except warp would use it though.

    How much would I solve just be changing (<>)?
-}

type NumSignal = Signal TimeVector.Unboxed

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

-- * construct

empty :: V.Vector v a => Signal (v a)
empty = Signal 0 V.empty

from_vector :: v -> Signal v
from_vector = Signal 0

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

from_pairs :: V.Vector v (Sample y) => [(X, y)] -> SignalS v y
from_pairs = from_samples . map (uncurry Sample)

from_segments :: V.Vector v (Sample y) => [Segment y] -> SignalS v y
from_segments = from_samples . to_list
    where
    to_list (Segment x1 y1 x2 y2 : segments) =
        Sample x1 y1 : Sample x2 y2 : to_list segments
    to_list [] = []

-- * destruct

to_pairs :: V.Vector v (Sample y) => SignalS v y -> [(X, y)]
to_pairs = map TimeVector.to_pair . V.toList . to_vector

to_vector :: V.Vector v (Sample y) => SignalS v y -> v (Sample y)
to_vector sig
    | offset == 0 = _vector sig
    | otherwise = TimeVector.map_x (+offset) (_vector sig)
    where
    offset = _offset sig

to_segments :: V.Vector v (Sample y) => SignalS v y -> [Segment y]
to_segments = go . V.toList . to_vector
    where
    go [] = []
    go [Sample x y] = [Segment x y RealTime.large y]
    go (Sample x1 y1 : xs@(Sample x2 y2 : _))
        | x1 == x2 = go xs
        | otherwise = Segment x1 y1 x2 y2 : go xs

-- * access

-- | The arguments may seem backwards, but I've always done it this way, and it
-- seems to be more convenient in practice.
at_interpolate :: V.Vector v (Sample y) => (X -> y -> X -> y -> X -> y) -> X
    -> SignalS v y -> Maybe y
at_interpolate interpolate x (Signal offset vec)
    | V.null vec = Nothing
    | i + 1 >= V.length vec = Just y0
    | i == -1 = Just y1
    | otherwise = Just $ interpolate x0 y0 x1 y1 (x + offset)
    where
    i = TimeVector.highest_index (x + offset) vec
    TimeVector.Sample x0 y0 = V.unsafeIndex vec i
    TimeVector.Sample x1 y1 = V.unsafeIndex vec (i+1)

segment_at :: V.Vector v (Sample y) => X -> SignalS v y -> Maybe (Segment y)
segment_at x (Signal offset vec)
    | i < 0 = Nothing
    | i + 1 >= TimeVector.length vec = Nothing
    | otherwise =
        let Sample x1 y1 = V.unsafeIndex vec i
            Sample x2 y2 = V.unsafeIndex vec (i+1)
        in Just $ Segment x1 y1 x2 y2
    where
    i = TimeVector.highest_index (x + offset) vec


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
