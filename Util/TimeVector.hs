{- | Generic functions over vectors of Samples which have a RealTime
    attribute.

    TODO lots of duplication with "Perform.Signal".  Unfortunately
    Perform.Signal is unboxed storablevector which makes it hard to share the
    code.  I can port Signal to unboxed data-vector but even then I'll need
    a typeclass or accessor for the unboxed data.
-}
module Util.TimeVector where
import qualified Data.Vector as V

import qualified Util.Pretty as Pretty
import qualified Perform.RealTime as RealTime


type X = RealTime.RealTime

data Sample y = Sample {
    sx :: {-# UNPACK #-} !X
    , sy :: !y
    } deriving (Read, Show)

type Vector y = V.Vector (Sample y)

instance (Pretty.Pretty y) => Pretty.Pretty (Sample y) where
    format (Sample x y) = Pretty.format (x, y)

make :: [(X, y)] -> Vector y
make = V.fromList . map (\(x, y) -> Sample x y)

-- | Merge a sorted list of vectors.  Samples are not interspersed, and if
-- the vectors overlap the later one wins.
merge :: [Vector y] -> Vector y
merge vecs = V.unfoldrN len go vecs
    where
    -- This will be too big if there's lots of overlap.
    len = sum (map V.length vecs) + 1
    go [] = Nothing
    go [vec] = case viewL vec of
        Nothing -> Nothing
        Just (x, rest) -> Just (x, [rest])
    go (cur : vecs@(next : rest)) = case viewL cur of
        Nothing -> go vecs
        Just (Sample x y, cur_tl) -> case viewL next of
            Nothing -> go (cur : rest)
            Just (Sample next_x next_y, next_tl)
                | next_x <= x -> Just (Sample next_x next_y, next_tl : rest)
                | otherwise -> Just (Sample x y, cur_tl : vecs)

viewL :: V.Vector a -> Maybe (a, V.Vector a)
viewL v
    | V.null v = Nothing
    | otherwise = Just (V.unsafeHead v, V.unsafeTail v)

at :: X -> Vector y -> Maybe y
at x vec
    | i >= 0 = Just $ sy (V.unsafeIndex vec i)
    -- Before the first sample is implicitly 0.
    | otherwise = Nothing
    where i = highest_index x vec


-- | Shift the signal in time.
shift :: X -> Vector y -> Vector y
shift offset vec
    | offset == 0 = vec
    | otherwise = map_x (+offset) vec

-- | Truncate a signal.  It's just a view of the old signal, so it
-- doesn't allocate a new signal.
truncate :: X -> Vector y -> Vector y
truncate x vec = fst $ V.splitAt (bsearch vec x) vec

-- | The dual of 'truncate'.  Trim a signal's head up until, but not including,
-- the given X.  If there is no sample at @x@, keep one sample before it to
-- preserve the value at @x@.
--
-- As with 'truncate', this doesn't do any copying.
drop_before :: X -> Vector y -> Vector y
drop_before x vec
    | i < V.length vec && sx (V.unsafeIndex vec i) == x =
        snd $ V.splitAt i vec
    | otherwise = snd $ V.splitAt (i-1) vec
    where i = bsearch vec x

map_x :: (X -> X) -> Vector y -> Vector y
map_x f = V.map $ \(Sample x y) -> Sample (f x) y

-- * util

-- | A version of 'bsearch_on' specialized to search X.  Profiling says
-- this gets called a lot and apparently the specialization makes a difference.
bsearch :: Vector y -> X -> Int
bsearch vec v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v <= sx (V.unsafeIndex vec mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- | Return the highest index of the given X.  So the next value is
-- guaranteed to have a higher x, if it exists.  Return -1 if @x@ is before
-- the first element.
highest_index :: X -> Vector y -> Int
highest_index x vec
    | V.null vec = -1
    | otherwise = i - 1
    where i = bsearch_above vec x

-- | This gets the index of the value *after* @x@.
bsearch_above :: Vector y -> X -> Int
bsearch_above vec x = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | x >= sx (V.unsafeIndex vec mid) = go vec (mid+1) high
        | otherwise = go vec low mid
        where mid = (low + high) `div` 2
