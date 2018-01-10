-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- | Generic functions over vectors of 'Sample's.

    The samples should be sorted, though this is currently not enforced by
    the constructors.

    The meaning of the samples is defined by the 'at' implementation.  Each
    sample starts at its time and extends up to, but not including, the next
    sample.  Therefore if there are multiple samples at the same time, only the
    last one is visible.

    There is a special rule that says a sample at <=0 is considered to extend
    backwards indefinitely.  So @at (-1) [(1, 1)]@ is 0, but @at (-1) [(0, 1)]@
    is 1.  This is weird and I've gone back and forth a couple times on the
    design so I should document how this came up.

    The idea is that some calls will move events back in time (e.g. grace
    notes).  If the call starts at 0, the events will start at negative time.
    Controls start at 0, so these events will have no pitch and no controls.
    Even if the pitch is set explicitly via 'constant', it still starts at 0.
    I could solve the problem with 'constant' by starting it at -bignum, but
    that doesn't help for control tracks, which still start at 0.

    Since tracks provide no way to say what the value should be before they
    start, I have to do something implicit (well, technically they could, but
    it seems awkward).  So the idea is that a sample at 0 gives the value for
    negative times as well.  Actually the implementation is <=0, since it seems
    weird that translating a signal back from 0 would change its meaning.  This
    still leaves a bit of weirdness where translating a signal back from
    positive to will change its definition when it crosses 0.  Hopefully this
    won't be a problem in practice.

    This solves the problem for 'constant' and for tracks in the same way, and
    doesn't result in any ugly arbitrary -bignum values suddenly showing up in
    signals.

    NOTE [signal-discontinuity] TODO
-}
module Util.TimeVector (
    module Util.TimeVector
    , module Util.TimeVectorStorable
    , module Data.Vector.Generic
) where
import Prelude hiding (head, last, take)
import qualified Control.Monad.State.Strict as State
import qualified Data.DList as DList
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as V
import Data.Vector.Generic
       (all, drop, foldl', length, null, take, toList, unsafeIndex)
import qualified Data.Vector.Storable as Storable

import qualified Foreign

import qualified Util.CallStack as CallStack
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.TimeVectorStorable (X, Sample(..))

import qualified Perform.RealTime as RealTime
import Global


x_to_double :: X -> Double
x_to_double = RealTime.to_seconds

double_to_x :: Double -> X
double_to_x = RealTime.seconds

type Boxed y = Vector.Vector (Sample y)

-- * unboxed

-- A number of functions in here are SPECIALIZEd on Unboxed.  This improves
-- performance significantly since the functions are heavily used and the
-- specialization likely enables some unboxing in inner loops.

-- (Vector a) already has a monoid instance, so I can't make my own.
-- I tried making newtypes for Boxed and Unboxed, but couldn't then figure
-- out how to get the generic functions to apply to them.  So clients have to
-- implement Monoid themselves, using 'merge'.

type Unboxed = Storable.Vector (Sample UnboxedY)
type UnboxedY = Double

to_foreign_ptr :: Storable.Storable a =>
    Storable.Vector a -> (Foreign.ForeignPtr a, Int)
to_foreign_ptr = Storable.unsafeToForeignPtr0

with_ptr :: Storable.Storable a =>
    Storable.Vector a -> (Foreign.Ptr a -> IO b) -> IO b
with_ptr = Storable.unsafeWith

-- * implementation

index :: V.Vector v a => v a -> Int -> a
index = (V.!)

head, last :: V.Vector v a => v a -> Maybe a
head v
    | V.null v = Nothing
    | otherwise = Just $ V.unsafeIndex v 0
last v
    | V.null v = Nothing
    | otherwise = Just $ V.last v

uncons :: V.Vector v a => v a -> Maybe (a, v a)
uncons v
    | V.null v = Nothing
    | otherwise = Just (V.unsafeHead v, V.unsafeTail v)

-- ** TimeVector specific

-- | Construct a TimeVector from a list.
{-# SPECIALIZE signal :: [(X, UnboxedY)] -> Unboxed #-}
{-# INLINEABLE signal #-}
signal :: V.Vector v (Sample y) => [(X, y)] -> v (Sample y)
signal = V.fromList . map (uncurry Sample)

unsignal :: V.Vector v (Sample y) => v (Sample y) -> [(X, y)]
unsignal = map to_pair . V.toList

-- | Like 'unsignal', but filter out explicit discontinuities, so each
-- X is unique.  This is for tests, where they're just clutter if I'm not
-- explicitly testing them.  NOTE [signal-discontinuity]
unsignal_unique :: V.Vector v (Sample y) => v (Sample y) -> [(X, y)]
unsignal_unique = Seq.drop_initial_dups fst . unsignal

-- | Set the signal value, with a discontinuity.  See
-- NOTE [signal-discontinuity].
set :: V.Vector v (Sample y) => Maybe y -> X -> y -> v (Sample y)
set prev_y x y = signal $ maybe id ((:) . (x,)) prev_y [(x, y)]

{-# SPECIALIZE constant :: UnboxedY -> Unboxed #-}
{-# INLINEABLE constant #-}
constant :: V.Vector v (Sample y) => y -> v (Sample y)
constant y = V.singleton (Sample (-RealTime.larger) y)

constant_val :: Unboxed -> Maybe UnboxedY
constant_val vec = case uncons vec of
    Nothing -> Just 0
    Just (Sample x0 y0, rest)
        -- I compare multiple samples because a track might have redundant
        -- values, but I still want to detect if it's constant.
        | x0 <= -RealTime.large && V.all ((==y0) . sy) rest -> Just y0
        | V.all ((==0) . sy) vec -> Just 0
        | otherwise -> Nothing

to_pair :: Sample y -> (X, y)
to_pair (Sample x y) = (x, y)

instance Pretty y => Pretty (Sample y) where
    format (Sample x y) = Pretty.format x <> Pretty.char ':' <> Pretty.format y

-- | TimeVectors should be sorted by the X value.  Return warnings for where
-- that's not true.
{-# SPECIALIZE check :: Unboxed -> [String] #-}
{-# INLINEABLE check #-}
check :: V.Vector v (Sample y) => v (Sample y) -> [String]
check = reverse . fst . V.foldl' check ([], (0, 0))
    where
    check (warns, (i, prev_x)) (Sample x _)
        | x < prev_x = first (msg:) next
        | otherwise = next
        where
        msg = "index " <> show i <> ": x decreased: " <> show x <> " < "
            <> show prev_x
        next = (warns, (i + 1, x))

{-# SPECIALIZE merge :: [Unboxed] -> Unboxed #-}
{-# INLINEABLE merge #-}
merge :: V.Vector v (Sample y) => [v (Sample y)] -> v (Sample y)
merge = merge_right

-- | This is a merge where the vectors to the right will win in the case of
-- overlap.
{-# SPECIALIZE merge_right :: [Unboxed] -> Unboxed #-}
{-# INLINEABLE merge_right #-}
merge_right :: V.Vector v (Sample y) => [v (Sample y)] -> v (Sample y)
merge_right [v] = v
merge_right vs = case next_start (reverse vs) of
    Nothing -> V.empty
    Just (v, vs, x) -> V.concat $ reverse $ v : trim x vs
    where
    -- I don't really like the double reverse, but it's easiest this way.
    trim prev_start (v : vs) =
        clipped : trim (maybe prev_start sx (head clipped)) vs
        where clipped = V.take (bsearch_below prev_start v) v
    trim _ [] = []
    next_start [] = Nothing
    next_start (v:vs) = maybe (next_start vs) (\s -> (Just (v, vs, sx s)))
        (head v)

-- | This is a merge where the vectors to the left will win in the case of
-- overlap.
{-# SPECIALIZE merge_left :: [Unboxed] -> Unboxed #-}
{-# INLINEABLE merge_left #-}
merge_left :: V.Vector v (Sample y) => [v (Sample y)] -> v (Sample y)
merge_left [v] = v
merge_left vs = case next_end vs of
    Nothing -> V.empty
    Just (v, vs, x) -> V.concat $ v : trim x vs
    where
    trim prev_end (v : vs) =
        clipped : trim (maybe prev_end sx (last clipped)) vs
        where clipped = V.dropWhile ((<=prev_end) . sx) v
    trim _ [] = []
    next_end [] = Nothing
    next_end (v:vs) = maybe (next_end vs) (\s -> (Just (v, vs, sx s))) (last v)
    -- |--->        => |--->
    --   |--->             |->
    --     |--->             |->

-- | Merge two vectors, interleaving their samples.  Analogous to
-- 'Data.Map.union', if two samples coincide, the one from the first vector
-- wins.
-- TODO this doesn't work with signal-discontinuity and is sketchy anyway
{-# SPECIALIZE interleave :: Unboxed -> Unboxed -> Unboxed #-}
{-# INLINEABLE interleave #-}
interleave :: V.Vector v (Sample y) => v (Sample y) -> v (Sample y)
    -> v (Sample y)
interleave vec1 vec2 = V.unfoldrN (len1 + len2) go (0, 0)
    where
    len1 = V.length vec1
    len2 = V.length vec2
    go (i1, i2)
        | i1 >= len1 && i2 >= len2 = Nothing
        | i1 >= len1 = Just (s2, (i1, i2+1))
        | i2 >= len2 = Just (s1, (i1+1, i2))
        | otherwise = Just $ case compare (sx s1) (sx s2) of
            EQ -> (s1, (i1+1, i2+1))
            LT -> (s1, (i1+1, i2))
            GT -> (s2, (i1, i2+1))
        where
        s1 = V.unsafeIndex vec1 i1
        s2 = V.unsafeIndex vec2 i2

-- | When signals are 'merge'd, the later one overrides the first one.  This
-- is the other way: the first one will override the second.
{-# SPECIALIZE prepend :: Unboxed -> Unboxed -> Unboxed #-}
{-# INLINEABLE prepend #-}
prepend :: V.Vector v (Sample y) => v (Sample y) -> v (Sample y)
    -> v (Sample y)
prepend vec1 vec2 = case last vec1 of
    Nothing -> vec2
    Just v ->
        vec1 V.++ V.dropWhile ((<= sx v) . sx) (drop_before_strict (sx v) vec2)

-- | Same as 'sample_at', except don't return the X.
{-# SPECIALIZE at :: X -> Unboxed -> Maybe UnboxedY #-}
{-# INLINEABLE at #-}
at :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe y
at x = fmap snd . sample_at x

-- | Find the sample at or before X.  Nothing if the X is before the first
-- sample.
{-# SPECIALIZE sample_at :: X -> Unboxed -> Maybe (X, UnboxedY) #-}
{-# INLINEABLE sample_at #-}
sample_at :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe (X, y)
sample_at x vec
    | i >= 0 = Just $ to_pair $ V.unsafeIndex vec i
    | otherwise = Nothing
    where i = highest_index x vec

-- | Find the sample before the given X.
{-# SPECIALIZE before :: X -> Unboxed -> Maybe (Sample UnboxedY) #-}
{-# INLINEABLE before #-}
before :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe (Sample y)
before x vec
    | i > 0 = Just $ V.unsafeIndex vec (i-1)
    | otherwise = Nothing
    where i = bsearch_below x vec

-- | Samples at and above the given time.
ascending :: V.Vector v (Sample y) => X -> v (Sample y) -> [Sample y]
ascending x vec =
    [ V.unsafeIndex vec i
    | i <- Seq.range' (bsearch_below x vec) (V.length vec) 1
    ]

-- | Descending samples, starting below the time.
descending :: V.Vector v (Sample y) => X -> v (Sample y) -> [Sample y]
descending x vec =
    [V.unsafeIndex vec i | i <- Seq.range (bsearch_below x vec - 1) 0 (-1)]

-- * transform

-- | Shift the signal in time.
shift :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
shift offset vec
    | offset == 0 = vec
    | otherwise = map_x (+offset) vec

-- | Truncate a signal so it doesn't include the given X - RealTime.eta.  It's
-- just a view of the old signal, so it doesn't allocate a new signal.
--
-- If the x<=0 the signal will still contain up to and including 0.  That's
-- because, as per the module haddock, a sample <=0 stands in for all values
-- <=0.
{-# SPECIALIZE drop_at_after :: X -> Unboxed -> Unboxed #-}
{-# INLINEABLE drop_at_after #-}
drop_at_after :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_at_after x vec
    -- TODO remove magic
    | x <= 0 = V.takeWhile ((<=0) . sx) vec
    | otherwise = V.take (bsearch_below (x - RealTime.eta) vec) vec

drop_after :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_after x = drop_at_after (x + RealTime.eta + RealTime.eta)

-- | Like 'drop_before_strict', except if there is no sample at @x@, keep one
-- sample before it to preserve the value at @x@.  If there are multiple
-- samples at @x@, drop all but the last one.  This is because they indicate
-- a discontinuity, but if you don't care about the previous value, then you
-- don't need the discontinuity.
{-# SPECIALIZE drop_before :: X -> Unboxed -> Unboxed #-}
{-# INLINEABLE drop_before #-}
drop_before :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_before x vec
    | i == -1 = vec
    | i < V.length vec = V.drop i vec
    | otherwise = V.drop (V.length vec - 1) vec
    where i = highest_index x vec

-- | The reverse of 'drop_at_after': trim a signal's head up until, but not
-- including, the given X.
drop_before_strict :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_before_strict x vec = V.drop (bsearch_below x vec) vec

-- | Like 'drop_before_strict', but also drop samples at the X.
drop_before_at :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_before_at x = V.dropWhile ((<=x) . sx) . drop_before_strict x

-- | Return samples to set the value at start and until end.  This means
-- samples start <= t < end, along with one < start if necessary to set
-- the initial value, and the end sample if start == end.
within :: V.Vector v (Sample y) => X -> X -> v (Sample y) -> v (Sample y)
within start end = (if start == end then drop_after end else drop_at_after end)
    . drop_before start

map_x :: V.Vector v (Sample y) => (X -> X) -> v (Sample y) -> v (Sample y)
map_x f = V.map $ \(Sample x y) -> Sample (f x) y

map_y :: V.Vector v (Sample y) => (y -> y) -> v (Sample y) -> v (Sample y)
map_y f = V.map $ \(Sample x y) -> Sample x (f y)

{-# SPECIALIZE map_err :: (Sample UnboxedY -> Either err (Sample UnboxedY))
    -> Unboxed -> (Unboxed, [err]) #-}
{-# INLINEABLE map_err #-}
-- | A map that can return error msgs.
map_err :: V.Vector v a => (a -> Either err a) -> v a -> (v a, [err])
map_err f vec = second reverse $ State.runState (V.mapM go vec) []
    where
    go sample =
        either (\err -> State.modify (err:) >> return sample) return (f sample)

{-# SPECIALIZE sig_op :: UnboxedY -> (UnboxedY -> UnboxedY -> UnboxedY)
    -> Unboxed -> Unboxed -> Unboxed #-}
{-# INLINEABLE sig_op #-}
-- | Combine two vectors with the given function.  They will be resampled so
-- they have samples at the same time.
sig_op :: V.Vector v (Sample y) =>
    y -- ^ The implicit y value of a vector before its first sample.  It should
    -- probably be the identity for the operator.
    -> (y -> y -> y) -> v (Sample y) -> v (Sample y) -> v (Sample y)
sig_op initial combine vec1 vec2 = V.unfoldr go (initial, initial, 0, 0)
    where
    go (prev_ay, prev_by, i1, i2) =
        case resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> Nothing
            Just (x, ay, by, i1, i2) ->
                Just (Sample x (combine ay by), (ay, by, i1, i2))
    len1 = V.length vec1
    len2 = V.length vec2

-- | Polymorphic variant of 'sig_op'.
--
-- The signature is specialized to Boxed since you might as well use 'sig_op'
-- for Unboxed vectors.
sig_op_poly :: y1 -> y2 -> (y1 -> y2 -> y3) -> Boxed y1 -> Boxed y2 -> Boxed y3
sig_op_poly initial1 initial2 combine vec1 vec2 =
    V.unfoldr go (initial1, initial2, 0, 0)
    where
    -- Yeah I could probably make 'sig_op' a specialization of this, but can't
    -- be bothered at the moment.
    go (prev_ay, prev_by, i1, i2) =
        case resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> Nothing
            Just (x, ay, by, i1, i2) ->
                Just (Sample x (combine ay by), (ay, by, i1, i2))
    len1 = V.length vec1
    len2 = V.length vec2

{-# INLINE resample1 #-}
resample1 :: (V.Vector v1 (Sample y1), V.Vector v2 (Sample y2)) => y1 -> y2
    -> Int -> Int -> Int -> Int
    -> v1 (Sample y1) -> v2 (Sample y2) -> Maybe (X, y1, y2, Int, Int)
resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2
    | i1 >= len1 && i2 >= len2 = Nothing
    | i1 >= len1 = Just (bx, prev_ay, by, i1, i2+1)
    | i2 >= len2 = Just (ax, ay, prev_by, i1+1, i2)
    | ax == bx = Just (ax, ay, by, i1+1, i2+1)
    | ax < bx = Just (ax, ay, prev_by, i1+1, i2)
    | otherwise = Just (bx, prev_ay, by, i1, i2+1)
    where
    Sample ax ay = V.unsafeIndex vec1 i1
    Sample bx by = V.unsafeIndex vec2 i2

-- * util

{-# SPECIALIZE find_nonascending :: Unboxed -> [(X, UnboxedY)] #-}
{-# INLINEABLE find_nonascending #-}
-- | Find samples whose 'sx' is <= the previous X.
find_nonascending :: V.Vector v (Sample y) => v (Sample y) -> [(X, y)]
find_nonascending vec = case uncons vec of
    Nothing -> []
    Just (x, xs) -> map to_pair $ reverse $ snd $ V.foldl' go (sx x, []) xs
    where
    go (prev, acc) s
        | sx s <= prev = (sx s, s : acc)
        | otherwise = (sx s, acc)

{-# SPECIALIZE unfoldr :: (state -> Maybe ((X, UnboxedY), state)) -> state
    -> Unboxed #-}
{-# INLINEABLE unfoldr #-}
unfoldr :: V.Vector v (Sample y) => (state -> Maybe ((X, y), state)) -> state
    -> v (Sample y)
unfoldr f = V.unfoldr $ \st -> case f st of
    Nothing -> Nothing
    Just ((x, y), next) -> Just (Sample x y, next)

-- | Given a line defined by the two points, find the y at the given x.
-- Crashes if called on a vertical line (y0==y1).  Yeah, it's inconsistent
-- with 'x_at'.
y_at :: CallStack.Stack => X -> Double -> X -> Double -> X -> Double
y_at x0 y0 x1 y1 x
    | x0 == x1 = errorStack $ "y_at on vertical line: "
        <> showt ((x0, y0), (x1, y1), x)
    | otherwise = (y1 - y0) / x_to_double (x1 - x0) * x_to_double (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
x_at :: X -> Double -> X -> Double -> Double -> Maybe X
x_at x0 y0 x1 y1 y
    | y0 == y1 = Nothing -- line is horizontal
    | otherwise = Just $
        double_to_x (y - y0) / (double_to_x (y1 - y0) / (x1 - x0)) + x0

-- | Binary search for the highest index of the given X.  So the next value is
-- guaranteed to be >X, if it exists.  Return -1 if @x@ is before
-- the first element.  'RealTime.eta' is added to @x@, so a sample that's
-- almost the same will still be considered a match.
{-# SPECIALIZE highest_index :: X -> Unboxed -> Int #-}
{-# SPECIALIZE highest_index :: X -> Boxed y -> Int #-}
{-# INLINEABLE highest_index #-}
highest_index :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
highest_index x vec
    | V.null vec = -1
    | otherwise = i - 1
    where i = bsearch_above (x + RealTime.eta) vec

-- | Binary search for the index of the first element that is >x, or one past
-- the end of the vector.
{-# SPECIALIZE bsearch_above :: X -> Unboxed -> Int #-}
{-# SPECIALIZE bsearch_above :: X -> Boxed y -> Int #-}
{-# INLINEABLE bsearch_above #-}
bsearch_above :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
bsearch_above x vec = go 0 (V.length vec)
    where
    go low high
        | low == high = low
        | x >= sx (V.unsafeIndex vec mid) = go (mid+1) high
        | otherwise = go low mid
        where mid = (low + high) `div` 2

-- | 'bsearch_below', but if you use it with take, it includes the first
-- element ==x.
{-# SPECIALIZE bsearch_below_1 :: X -> Unboxed -> Int #-}
{-# INLINEABLE bsearch_below_1 #-}
bsearch_below_1 :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
bsearch_below_1 x vec = case vec V.!? i of
    Just vi | sx vi == x -> i + 1
    _ -> i
    where i = bsearch_below x vec

-- | Binary search for the index of the first element ==x, or the last one <x.
-- So it will be <=x, or one past the end of the vector.  If you ues it with
-- take, it's everything <x.
{-# SPECIALIZE bsearch_below :: X -> Unboxed -> Int #-}
{-# SPECIALIZE bsearch_below :: X -> Boxed y -> Int #-}
{-# INLINEABLE bsearch_below #-}
bsearch_below :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
bsearch_below x vec = go 0 (V.length vec)
    where
    go low high
        | low == high = low
        | x <= sx (V.unsafeIndex vec mid) = go low mid
        | otherwise = go (mid+1) high
        where mid = (low + high) `div` 2

concat_map_accum :: UnboxedY
    -> (accum -> X -> UnboxedY -> X -> UnboxedY -> (accum, [Sample UnboxedY]))
    -- ^ Take the previous accum, previous x and y, and current x and y.
    -> (accum -> Sample UnboxedY -> [Sample UnboxedY])
    -- ^ Given the final @(accum, Sample)@, produce samples to append.
    -> accum -> Unboxed -> Unboxed
concat_map_accum zero f final accum vec = V.fromList (DList.toList result)
    where
    (last_accum, _, dlist) =
        V.foldl' go (accum, Sample 0 zero, DList.empty) vec
    end = if V.null vec then [] else final last_accum (V.last vec)
    result = dlist `DList.append` DList.fromList end
    go (accum, Sample x0 y0, lst) (Sample x1 y1) =
        (accum2, Sample x1 y1, lst `DList.append` DList.fromList samples)
        where (accum2, samples) = f accum x0 y0 x1 y1


-- * signal-discontinuity

-- Functions with ad-hoc signal-discontinuity support.

merge_segments :: [(X, Unboxed)] -> Unboxed
merge_segments segments =
    merge_right_extend [clip_to start v | (start, v) <- segments]

-- | This is like 'at', but if there is a discontinuity, this is the value
-- before the discontinuity.  So if you have (1, 0), (1, 1) and ask for 1,
-- this will be 0, not 1.
{-# SPECIALIZE at_before :: X -> Unboxed -> Maybe UnboxedY #-}
{-# INLINEABLE at_before #-}
at_before :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe y
at_before x = fmap snd . sample_at_before x

{-# SPECIALIZE sample_at_before :: X -> Unboxed -> Maybe (X, UnboxedY) #-}
{-# INLINEABLE sample_at_before #-}
sample_at_before :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe (X, y)
sample_at_before x vec
    | i >= 0 = Just $ to_pair $ V.unsafeIndex vec i
    | otherwise = case uncons vec of
        Just (Sample x y, _) | x <= 0 -> Just (x, y)
        _ -> Nothing
    where i = bsearch_below x vec

clip_to :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
clip_to x vec = case head clipped of
    Nothing -> clipped
    Just s
        | sx s < x -> V.cons (s { sx = x}) (V.drop 1 clipped)
        | otherwise -> clipped
    where clipped = drop_before x vec

append_extend :: V.Vector v (Sample y) => v (Sample y) -> v (Sample y)
    -> v (Sample y)
append_extend v1 v2 = merge_right_extend [v1, v2]

-- | This is like 'merge_right' except assuming NOTE [signal-discontinuity].
-- If the first signal is cut off by the second, its last sample will be
-- extended up to the cut-off point.
--
-- TODO this should probably replace 'merge_right'.  But I should really have
-- an organized design around line segments, and not this ad-hoc thing where
-- everyone has to manually remember to treat Samples right.
{-# SPECIALIZE merge_right_extend :: [Unboxed] -> Unboxed #-}
{-# INLINEABLE merge_right_extend #-}
merge_right_extend :: V.Vector v (Sample y) => [v (Sample y)] -> v (Sample y)
merge_right_extend = V.concat . reverse . chunks . reverse
    where
    chunks [] = []
    chunks [v] = [v]
    -- head of v1 cuts of tail of v2
    -- v1:     |--->        |--->
    -- v2:   |--->        |->
    -- vs: |--->     => |->
    chunks (v1:v2:vs) = case head v1 of
        Nothing -> chunks (v2:vs)
        Just start -> case last clipped of
            Nothing -> chunks (v1:vs)
            Just end
                | sx end < sx start -> v1 : extension end : chunks (clipped:vs)
                | otherwise -> v1 : chunks (clipped:vs)
            where
            clipped = V.take (bsearch_below_1 (sx start) v2) v2
            extension end = V.singleton (Sample (sx start) (sy end))

-- | Strip out samples that don't have any effect.
--
-- TODO verify that the intermediate list is eliminated
{-# SPECIALIZE strip :: Unboxed -> Unboxed #-}
{-# INLINEABLE strip #-}
strip :: Eq y => V.Vector v (Sample y) => v (Sample y) -> v (Sample y)
strip = V.fromList . go . V.toList
    where
    go [] = []
    go (s1:s2:s3:ss) | sx s1 == sx s2 && sx s2 == sx s3 = go (s1:s3:ss)
    go (s1:s2:ss) | s1 == s2 = go (s2:ss)
    go (s1:ss) = s1 : go ss
