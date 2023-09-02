-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to operate on lists.
{-# LANGUAGE BangPatterns #-}
module Util.Lists (
    NonNull
    , head, tail
    , last
    , unsnoc
    -- * indexing
    , at
    , insertAt
    , removeAt
    , takeAt
    , modifyAt
    , findModify
    , updateAt
    , move
    -- * enumeration
    , range, range', rangeEnd, range_
    -- * transformation
    , keyOn
    , keyOnSnd
    , keyOnJust
    , firstLast
    , mapMaybeFst, mapMaybeSnd
    , mapHead, mapTail, mapHeadTail
    , mapInit, mapLast
    , scanlOn
    -- * min / max
    , minOn, maxOn
    , minimumOn, maximumOn
    , minimum, maximum
    -- * ordered lists
    , insertOn
    , sortOn
    , reverseSortOn
    , merge, mergeBy, mergeOn
    , mergeLists, mergeAscLists

    -- * grouping
    -- ** adjacent
    , groupAdjacent
    , keyedGroupAdjacent
    , groupAdjacentFst
    , groupAdjacentSnd
    -- ** sort
    , keyedGroupSort
    , groupFst, groupSnd
    , groupSort
    -- ** stable
    , groupStableWith
    , groupStable
    , keyedGroupStable

    -- * zipping
    , zipNext, zipNexts
    , zipPrev
    , zipNeighbors
    , zipRemainder
    , zipper

    -- * Paired
    , Paired(..)
    , pairedSecond, pairedFirst
    , partitionPaired
    , zipPadded, zipPaddedFst, zipPaddedSnd
    , diff
    , diffEither
    , diffIndex
    , diffIndexOn
    , pairSorted
    , pairSortedOn, pairSortedOn1
    , pairOn1
    -- * partition
    , partition2
    , partitionOn
    -- * sublists
    , chunked
    , rotate
    , rotate2
    -- * prefix / suffix
    , dropBefore
    , dropPrefix
    , dropSuffix

    -- * permutations
    , cartesian
    -- * enumeration
    , enumerate
    -- * sublists
    , takeEnd
    , dropEnd
    , takeWhileEnd
    -- * split / join
    , splitWith
    , breakWith
    -- * transform
    , mapAccumLM
    -- ** split and join
    , split
    , join
    , splitBefore, splitBetween
    -- * span and break
    , spanWhile
    -- * duplicates
    , dropDups
    , dropWith
    , partitionDups
    , findDups
    , dropInitialDups
    , unique
    , uniqueOn
    , uniqueSort
    -- * replace
    , replace, replace1
    -- * search
    , count
) where
import           Prelude hiding (head, last, tail, minimum, maximum)
import qualified Data.Algorithm.Diff as Diff
import           Data.Bifunctor (Bifunctor(bimap), first, second)
import qualified Data.Either as Either
import qualified Data.Function as Function
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.Ordered as Ordered
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set


-- | This is just a list, but is documentation that a return value will never
-- be null, or an argument should never be null.  This is for cases where
-- 'NonEmpty' is too annoying to work with.
type NonNull a = [a]

-- * extract

-- | Total variants of unsafe list operations.
head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

last :: [a] -> Maybe a
last [] = Nothing
last xs = Just (List.last xs)

-- | List initial and final element, if any.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x:xs) = Just $ go x xs
    where
    go x0 [] = ([], x0)
    go x0 (x:xs) = let (pre, post) = go x xs in (x0:pre, post)

-- * indexing

-- | Get @xs !! n@, but return Nothing if the index is out of range.
at :: [a] -> Int -> Maybe a
at xs n
    | n < 0 = Nothing
    | otherwise = go xs n
    where
    go [] _ = Nothing
    go (x:_) 0 = Just x
    go (_:xs) n = go xs (n-1)

-- | Insert @x@ into @xs@ at index @i@.  If @i@ is out of range, insert at the
-- beginning or end of the list.
insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = let (pre, post) = splitAt i xs in pre ++ (x : post)

-- | Remove the element at the given index.  Do nothing if the index is out
-- of range.
removeAt :: Int -> [a] -> [a]
removeAt i xs = let (pre, post) = splitAt i xs in pre ++ drop 1 post

-- | Like 'removeAt' but return the removed element as well.
takeAt :: Int -> [a] -> Maybe (a, [a])
takeAt i xs = case post of
        v : vs -> Just (v, pre ++ vs)
        [] -> Nothing
    where (pre, post) = splitAt i xs

-- | Modify element at an index by applying a function to it.  If the index is
-- out of range, nothing happens.
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = case post of
    [] -> pre
    elt : rest -> pre ++ f elt : rest
    where (pre, post) = splitAt i xs

-- | Find an element, then change it.  Return Nothing if the element wasn't
-- found.
findModify :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
findModify match modify = go
    where
    go (x:xs)
        | match x = Just $ modify x : xs
        | otherwise = (x:) <$> go xs
    go [] = Nothing

-- | Similar to 'modifyAt', but will insert an element for an out of range
-- positive index.  The list will be extended with @deflt@, and the modify
-- function passed a Nothing.
updateAt :: a -> Int -> (Maybe a -> a) -> [a] -> [a]
updateAt deflt i f xs
    | i < 0 = error $ "updateAt: negative index " ++ show i
    | otherwise = go i xs
    where
    go 0 [] = [f Nothing]
    go 0 (x:xs) = f (Just x) : xs
    go i [] = deflt : go (i-1) []
    go i (x:xs) = x : go (i-1) xs

-- | Move an element from one index to another, or Nothing if the @from@
-- index was out of range.
move :: Int -> Int -> [a] -> Maybe [a]
move from to xs = do
    (x, dropped) <- takeAt from xs
    return $ insertAt to x dropped

-- * enumeration

-- | Enumerate an inclusive range.  Uses multiplication instead of successive
-- addition to avoid loss of precision.
--
-- Also it doesn't require an Enum instance.
range :: (Num a, Ord a) => a -> a -> a -> [a]
range start end step = go 0
    where
    go i
        | step >= 0 && val > end = []
        | step < 0 && val < end = []
        | otherwise = val : go (i+1)
        where val = start + (i*step)
{-# INLINEABLE range #-}
{-# SPECIALIZE range :: Int -> Int -> Int -> [Int] #-}

-- | Enumerate a half-open range.
range' :: (Num a, Ord a) => a -> a -> a -> [a]
range' start end step = go 0
    where
    go i
        | step >= 0 && val >= end = []
        | step < 0 && val <= end = []
        | otherwise = val : go (i+1)
        where val = start + (i*step)
{-# INLINEABLE range' #-}
{-# SPECIALIZE range' :: Int -> Int -> Int -> [Int] #-}

-- | Like 'range', but always includes the end, even if it doesn't line up on
-- a step.
rangeEnd :: (Num a, Ord a) => a -> a -> a -> [a]
rangeEnd start end step = go 0
    where
    go i
        | step >= 0 && val >= end = [end]
        | step < 0 && val <= end = [end]
        | otherwise = val : go (i+1)
        where val = start + (i*step)
{-# INLINEABLE rangeEnd #-}
{-# SPECIALIZE rangeEnd :: Int -> Int -> Int -> [Int] #-}

-- | Infinite range.
range_ :: Num a => a -> a -> [a]
range_ start step = go 0
    where go i = start + (i*step) : go (i+1)
{-# INLINEABLE range_ #-}
{-# SPECIALIZE range_ :: Int -> Int -> [Int] #-}

-- * transformation

keyOn :: (a -> k) -> [a] -> [(k, a)]
keyOn f xs = zip (map f xs) xs

keyOnSnd :: (a -> k) -> [a] -> [(a, k)]
keyOnSnd f xs = zip xs (map f xs)

keyOnJust :: (a -> Maybe k) -> [a] -> [(k, a)]
keyOnJust f xs = [(k, a) | (Just k, a) <- keyOn f xs]

-- | Apply a function to the first and last elements.  Middle elements are
-- unchanged.  A null or singleton list is also unchanged.
firstLast :: (a -> a) -> (a -> a) -> [a] -> [a]
firstLast start end xs = case xs of
    [] -> []
    [x] -> [x]
    x : xs -> start x : go xs
    where
    go [] = []
    go [x] = [end x]
    go (x:xs) = x : go xs

-- | Filter on the fst values returning Just.
mapMaybeFst :: (a -> Maybe a2) -> [(a, b)] -> [(a2, b)]
mapMaybeFst f xs = [(a, b) | (Just a, b) <- map (first f) xs]

-- | Filter on the snd values returning Just.
mapMaybeSnd :: (b -> Maybe b2) -> [(a, b)] -> [(a, b2)]
mapMaybeSnd f xs = [(a, b) | (a, Just b) <- map (second f) xs]

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

mapTail :: (a -> a) -> [a] -> [a]
mapTail f (x:xs) = x : map f xs
mapTail _ [] = []

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail f g (x : xs) = f x : map g xs
mapHeadTail _ _ [] = []

mapInit :: (a -> a) -> [a] -> [a]
mapInit _ [] = []
mapInit _ [x] = [x]
mapInit f (x:xs) = f x : mapInit f xs

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f [x] = [f x]
mapLast f (x:xs) = x : mapLast f xs

-- | This is like 'scanl', but you can scan with a key function.  E.g. to
-- accumulate line lengths: @scanlOn length (+) 0@.
scanlOn :: (accum -> key -> accum) -> (a -> key) -> accum -> [a]
    -> [(accum, a)]
scanlOn f key z xs = zip (scanl (\t -> f t . key) z xs) xs

-- * min / max

minOn :: Ord k => (a -> k) -> a -> a -> a
minOn key x y = if key x <= key y then x else y

maxOn :: Ord k => (a -> k) -> a -> a -> a
maxOn key x y = if key x >= key y then x else y

minimumOn :: Ord k => (a -> k) -> [a] -> Maybe a
minimumOn _ [] = Nothing
minimumOn key xs = Just (List.foldl1' f xs)
    where f low x = if key x < key low then x else low

maximumOn :: Ord k => (a -> k) -> [a] -> Maybe a
maximumOn _ [] = Nothing
maximumOn key xs = Just (List.foldl1' f xs)
    where f high x = if key x > key high then x else high

minimum :: Ord a => [a] -> Maybe a
minimum [] = Nothing
minimum xs = Just (List.minimum xs)

maximum :: Ord a => [a] -> Maybe a
maximum [] = Nothing
maximum xs = Just (List.maximum xs)

-- * ordered lists

insertOn :: Ord k => (a -> k) -> a -> [a] -> [a]
insertOn key = List.insertBy (\a b -> compare (key a) (key b))

-- | Stable sort on a cheap key function.  Different from 'List.sortOn', which
-- is for an expensive key function.
sortOn :: Ord k => (a -> k) -> [a] -> [a]
sortOn = Ordered.sortOn'

-- | Like 'sortOn', but sort highest-to-lowest.
reverseSortOn :: Ord b => (a -> b) -> [a] -> [a]
reverseSortOn f = List.sortBy $ \a b -> Ord.comparing f b a

-- | Merge sorted lists.  If two elements compare equal, the one from the left
-- list comes first.
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeOn id

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy = Ordered.mergeBy

mergeOn :: Ord k => (a -> k) -> [a] -> [a] -> [a]
mergeOn key = Ordered.mergeBy (Ord.comparing key)

mergeLists :: Ord k => (a -> k) -> [[a]] -> [a]
mergeLists key = foldr (mergeOn key) []

-- | If the heads of the sublists are also sorted I can be lazy in the list of
-- sublists too.  This version is optimized for minimal overlap.
mergeAscLists :: Ord k => (a -> k) -> [[a]] -> [a]
mergeAscLists key = foldr go []
    where
    go [] ys = ys
    go (x:xs) ys = x : mergeOn key xs ys

-- * grouping

-- ** adjacent

-- Adjacent groups only group adjacent elements, just like 'List.group'.

-- | This is just 'List.groupBy' except with a key function.
groupAdjacent :: Eq key => (a -> key) -> [a] -> [NonNull a]
groupAdjacent key = List.groupBy ((==) `Function.on` key)

-- | Like 'groupAdjacent', but include the key.
keyedGroupAdjacent :: Eq key => (a -> key) -> [a] -> [(key, NonNull a)]
keyedGroupAdjacent key =
    keyOn (key . List.head) . List.groupBy ((==) `Function.on` key)

groupAdjacentFst :: Eq a => [(a, b)] -> [(a, NonNull b)]
groupAdjacentFst xs =
    [(key, map snd group) | (key, group) <- keyedGroupAdjacent fst xs]

groupAdjacentSnd :: Eq b => [(a, b)] -> [(NonNull a, b)]
groupAdjacentSnd xs =
    [(map fst group, key) | (key, group) <- keyedGroupAdjacent snd xs]

-- ** sort

-- Sort groups sort the input by the group key as a side-effect of grouping.

-- | Group the unsorted list into @(key x, xs)@ where all @xs@ compare equal
-- after @key@ is applied to them.
keyedGroupSort :: Ord key => (a -> key) -> [a] -> [(key, NonNull a)]
    -- ^ Sorted by key. The NonNull group is in the same order as the input.
keyedGroupSort key = Map.toAscList . foldr go Map.empty
    where go x = Map.alter (Just . maybe [x] (x:)) (key x)

-- | Similar to 'keyedGroupSort', but key on the fst element, and strip the
-- key out of the groups.
groupFst :: Ord a => [(a, b)] -> [(a, NonNull b)]
groupFst xs = [(key, map snd group) | (key, group) <- keyedGroupSort fst xs]

-- | Like 'groupFst', but group on the snd element.
groupSnd :: Ord b => [(a, b)] -> [(NonNull a, b)]
groupSnd xs = [(map fst group, key) | (key, group) <- keyedGroupSort snd xs]

-- | Like 'List.groupBy', but the list doesn't need to be sorted, and use a key
-- function instead of equality.  The list is sorted by the key, and the groups
-- appear in their original order in the input list.
groupSort :: Ord key => (a -> key) -> [a] -> [NonNull a]
groupSort key = map snd . keyedGroupSort key

-- ** stable

-- Stable groups preserve the original input order, in both the heads of the
-- groups, and within the groups themselves.

-- | Group each element with all the other elements that compare equal to it.
-- The heads of the groups appear in their original order.
groupStableWith :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupStableWith is_equal = go
    where
    go [] = []
    go (x:xs) = (x :| equal) : go inequal
        where (equal, inequal) = List.partition (is_equal x) xs

-- | 'groupStableWith' but with a key function.
groupStable :: Eq key => (a -> key) -> [a] -> [NonEmpty a]
groupStable key = groupStableWith (\x y -> key x == key y)

keyedGroupStable :: Eq key => (a -> key) -> [a] -> [(key, [a])]
keyedGroupStable key = map (\(g :| gs) -> (key g, g:gs)) . groupStable key

-- * zipping

-- | Pair each element with the following element.  The last element is paired
-- with Nothing.  Like @zip xs (drop 1 xs ++ [Nothing])@ but only traverses
-- @xs@ once.
zipNext :: [a] -> [(a, Maybe a)]
zipNext [] = []
zipNext [x] = [(x, Nothing)]
zipNext (x : xs@(y:_)) = (x, Just y) : zipNext xs

zipNexts :: [a] -> [(a, [a])]
zipNexts xs = zip xs (drop 1 (List.tails xs))

zipPrev :: [a] -> [(Maybe a, a)]
zipPrev xs = zip (Nothing : map Just xs) xs

-- | Like 'zipNext' but with both preceding and following elements.
zipNeighbors :: [a] -> [(Maybe a, a, Maybe a)]
zipNeighbors [] = []
zipNeighbors (x:xs) = (Nothing, x, head xs) : go x xs
    where
    go _ [] = []
    go prev [x] = [(Just prev, x, Nothing)]
    go prev (x : xs@(y:_)) = (Just prev, x, Just y) : go x xs

-- | This is like 'zip', but it returns the remainder of the longer argument
-- instead of discarding it.
zipRemainder :: [a] -> [b] -> ([(a, b)], Either [a] [b])
zipRemainder (x:xs) (y:ys) = first ((x, y) :) (zipRemainder xs ys)
zipRemainder [] ys = ([], Right ys)
zipRemainder xs [] = ([], Left xs)

-- | Return the reversed inits paired with the tails.  This is like a zipper
-- moving focus along the input list.
zipper :: [a] -> [a] -> [([a], [a])]
zipper prev [] = [(prev, [])]
zipper prev lst@(x:xs) = (prev, lst) : zipper (x:prev) xs

-- * Paired

data Paired a b = First !a | Second !b | Both !a !b
    deriving (Show, Eq)

instance Bifunctor Paired where
    bimap f g paired = case paired of
        First a -> First (f a)
        Second b -> Second (g b)
        Both a b -> Both (f a) (g b)

pairedSecond :: Paired a b -> Maybe b
pairedSecond (First _) = Nothing
pairedSecond (Second b) = Just b
pairedSecond (Both _ b) = Just b

pairedFirst :: Paired a b -> Maybe a
pairedFirst (First a) = Just a
pairedFirst (Second _) = Nothing
pairedFirst (Both a _) = Just a

partitionPaired :: [Paired a b] -> ([a], [b])
partitionPaired (pair : pairs) = case pair of
    Both a b -> (a : as, b : bs)
    First a -> (a : as, bs)
    Second b -> (as, b : bs)
    where (as, bs) = partitionPaired pairs
partitionPaired [] = ([], [])

-- | Like 'zip', but emit 'First's or 'Second's if the list lengths are
-- unequal.
zipPadded :: [a] -> [b] -> [Paired a b]
zipPadded [] [] = []
zipPadded [] bs = map Second bs
zipPadded as [] = map First as
zipPadded (a:as) (b:bs) = Both a b : zipPadded as bs

-- | Like 'zip', but the first list is padded with Nothings.
zipPaddedFst :: [a] -> [b] -> [(Maybe a, b)]
zipPaddedFst _ [] = []
zipPaddedFst (a:as) (b:bs) = (Just a, b) : zipPaddedFst as bs
zipPaddedFst [] (b:bs) = map ((,) Nothing) (b:bs)

-- | Like 'zip', but the second list is padded with Nothings.
zipPaddedSnd :: [a] -> [b] -> [(a, Maybe b)]
zipPaddedSnd [] _ = []
zipPaddedSnd (a:as) (b:bs) = (a, Just b) : zipPaddedSnd as bs
zipPaddedSnd (a:as) [] = [(a, Nothing) | a <- a : as]

-- | Perform a Meyes diff.
diff :: (a -> b -> Bool) -> [a] -> [b] -> [Paired a b]
diff eq as bs = map convert $ Diff.getDiffBy eq as bs
    where
    convert a = case a of
        Diff.First a -> First a
        Diff.Second b -> Second b
        Diff.Both a b -> Both a b

-- | Left if the val was in the left list but not the right, Right for the
-- converse.
diffEither :: (a -> b -> Bool) -> [a] -> [b] -> [Either a b]
diffEither eq as bs = Maybe.mapMaybe to_either $ diff eq as bs
    where
    to_either (First a) = Just (Left a)
    to_either (Second a) = Just (Right a)
    to_either _ = Nothing

-- | This is like 'diff', except that the index of each pair in the
-- /right/ list is included.  So the index is where you should delete or
-- add the element to turn as into bs:
--
-- * @(i, Second b)@, @i@ is the position of @b@ in @bs@.
--
-- * @(i, First a)@, @i@ is where @a@ was deleted from @bs@.
diffIndex :: (a -> b -> Bool) -> [a] -> [b] -> [(Int, Paired a b)]
diffIndex eq as bs = zip (indexed pairs) pairs
    where
    pairs = diff eq as bs
    indexed = scanl f 0
        where
        f i (First _) = i
        f i _ = i+1

diffIndexOn :: Eq k => (a -> k) -> [a] -> [a] -> [(Int, Paired a a)]
diffIndexOn key = diffIndex (\a b -> key a == key b)

-- | Pair up two lists of sorted pairs by their first element.
pairSorted :: Ord k => [(k, a)] -> [(k, b)] -> [(k, Paired a b)]
pairSorted xs [] = [(k, First v) | (k, v) <- xs]
pairSorted [] ys = [(k, Second v) | (k, v) <- ys]
pairSorted x@((k0, v0) : xs) y@((k1, v1) : ys)
    | k0 == k1 = (k0, Both v0 v1) : pairSorted xs ys
    | k0 < k1 = (k0, First v0) : pairSorted xs y
    | otherwise = (k1, Second v1) : pairSorted x ys

-- | Like 'pairSorted', but use a key function, and omit the extracted key.
pairSortedOn1 :: Ord k => (a -> k) -> [a] -> [a] -> [Paired a a]
pairSortedOn1 key = pairSortedOn key key

-- | Like 'pairSorted', but use a key function, and omit the extracted key.
pairSortedOn :: Ord k => (a -> k) -> (b -> k) -> [a] -> [b] -> [Paired a b]
pairSortedOn key1 key2 xs ys =
    map snd $ pairSorted (keyOn key1 xs) (keyOn key2 ys)

-- | Sort the lists on with the key functions, then pair them up.
pairOn :: Ord k => (a -> k) -> (b -> k) -> [a] -> [b] -> [Paired a b]
pairOn k1 k2 xs ys = map snd $
    pairSorted (sortOn fst (keyOn k1 xs)) (sortOn fst (keyOn k2 ys))

-- | Like 'pairOn', but when the lists have the same type.
pairOn1 :: Ord k => (a -> k) -> [a] -> [a] -> [Paired a a]
pairOn1 k = pairOn k k

-- * partition

-- | Like 'List.partition', but partition by two functions consecutively.
partition2 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
partition2 f1 f2 xs = (as, bs, xs3)
    where
    (as, xs2) = List.partition f1 xs
    (bs, xs3) = List.partition f2 xs2

-- | Partition and transform at the same time.
partitionOn :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionOn f = go
    where
    go [] = ([], [])
    go (x:xs) = case f x of
        Just b -> (b:bs, as)
        Nothing -> (bs, x:as)
        where (bs, as) = go xs

-- * sublists

-- | Split into groups of a certain size.
chunked :: Int -> [a] -> [[a]]
chunked n xs = case splitAt n xs of
    ([], []) -> []
    (pre, []) -> [pre]
    (pre, post) -> pre : chunked n post


-- | Take a list of rows to a list of columns.  This is like a zip except
-- for variable-length lists.  Similar to zip, the result is trimmed to the
-- length of the shortest row.
--
-- 'List.transpose' is similar, but it skips missing elements, instead of
-- truncating all to the shortest.  Skipping means you lose what column the
-- element came from.
rotate :: [[a]] -> [[a]]
rotate [] = []
rotate xs = maybe [] (: rotate (map List.tail xs)) (mapM head xs)

-- | Similar to 'rotate', except that the result is the length of the longest
-- row and missing columns are Nothing.  Analogous to 'zip_padded'.
rotate2 :: [[a]] -> [[Maybe a]]
rotate2 xs
    | all Maybe.isNothing heads = []
    | otherwise = heads : rotate2 (map (drop 1) xs)
    where heads = map head xs

-- * prefix / suffix

-- | Drop until the last element before or equal to the given element.
dropBefore :: Ord key => (a -> key) -> key -> [a] -> [a]
dropBefore key p = go
    where
    go [] = []
    go (x0 : xs)
        | p < key x0 = x0 : xs
        | otherwise = case xs of
            x1 : _ | p >= key x1 -> go xs
            _ -> x0 : xs

-- | If the list doesn't have the given prefix, return the original list and
-- False.  Otherwise, strip it off and return True.  'List.stripPrefix' is an
-- alternate version.
dropPrefix :: Eq a => [a] -> [a] -> ([a], Bool)
dropPrefix pref list = go pref list
    where
    go [] xs = (xs, True)
    go _ [] = (list, False)
    go (p:ps) (x:xs)
        | p == x = go ps xs
        | otherwise = (list, False)

dropSuffix :: Eq a => [a] -> [a] -> ([a], Bool)
dropSuffix suffix list
    | post == suffix = (pre, True)
    | otherwise = (list, False)
    where (pre, post) = splitAt (length list - length suffix) list

-- * permutations

-- | The cartesian product of a list of lists.  E.g.
-- @[[1, 2], [3, 4]]@ -> @[[1, 3], [1, 4], [2, 3], [2, 4]]@.
cartesian :: [[a]] -> [[a]]
cartesian [] = []
cartesian [xs] = [[x] | x <- xs]
cartesian (xs:rest) = [x:ps | x <- xs, ps <- cartesian rest]

-- * enumeration

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- * sublists

takeEnd :: Int -> [a] -> [a]
takeEnd n = snd . foldr go (n, [])
    where
    go x (n, xs)
        | n <= 0 = (0, xs)
        | otherwise = (n - 1, x : xs)

dropEnd :: Int -> [a] -> [a]
dropEnd n = either id (const []) . foldr f (Right n)
    where
    f x (Right n)
        | n <= 0 = Left [x]
        | otherwise = Right (n-1)
    f x (Left xs) = Left (x:xs)

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f = either id (const []) . foldr go (Right [])
    where
    -- Left means I'm done taking.
    go _ (Left xs) = Left xs
    go x (Right xs)
        | f x = Right (x:xs)
        | otherwise = Left xs

-- * split / join

splitWith :: (a -> Maybe b) -> [a] -> ([a], [(b, [a])])
splitWith match = go1
    where
    go1 as = case breakWith match as of
        (pre, Nothing) -> (pre, [])
        (pre, Just (b, post)) -> (pre, go2 b post)
    go2 b0 as = case breakWith match as of
        (pre, Nothing) -> [(b0, pre)]
        (pre, Just (b1, post)) -> (b0, pre) : go2 b1 post

breakWith :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakWith f = go
    where
    go (a : as) = case f a of
        Just b -> ([], Just (b, as))
        Nothing -> first (a:) (go as)
    go [] = ([], Nothing)


-- * transform

-- | Like 'List.mapAccumL', but monadic.  Strict in the accumulator.
mapAccumLM :: Monad m => (state -> x -> m (state, y)) -> state -> [x]
    -> m (state, [y])
mapAccumLM f = go
    where
    go !state [] = return (state, [])
    go !state (x:xs) = do
        (state, y) <- f state x
        (state, ys) <- go state xs
        return (state, y : ys)

-- ** split and join

-- | Split before places where the function matches.
--
-- > > splitBefore (==1) [1, 2, 1]
-- > [[], [1, 2], [1]]
splitBefore :: (a -> Bool) -> [a] -> [[a]]
splitBefore f = go
    where
    go [] = []
    go xs0 = pre : case post of
        x : xs -> cons1 x (go xs)
        [] -> []
        where (pre, post) = break f xs0
    cons1 x [] = [[x]]
    cons1 x (g:gs) = (x:g) : gs

-- | Split @xs@ on @sep@, dropping @sep@ from the result.
split :: Eq a => NonNull a -> [a] -> NonNull [a]
split [] = error "Util.Lists.split: empty separator"
split sep = go
    where
    go xs
        | null post = [pre]
        | otherwise = pre : go (drop (length sep) post)
        where (pre, post) = breakTails (sep `List.isPrefixOf`) xs

-- | Like 'split', but split on a single element.
split1 :: Eq a => a -> [a] -> [[a]]
split1 sep = go
    where
    go xs
        | null post = [pre]
        | otherwise = pre : go (drop 1 post)
        where (pre, post) = break (==sep) xs

-- | Interspense a separator and concat.
join :: Monoid a => a -> [a] -> a
join sep = mconcat . List.intersperse sep

-- | Split the list on the points where the given function returns true.
--
-- This is similar to 'groupBy', except this is defined to compare adjacent
-- elements.  'groupBy' actually compares to the first element of each group.
-- E.g. you can't group numeric runs with @groupBy (\a b -> b > a+1)@.
splitBetween :: (a -> a -> Bool) -> [a] -> [[a]]
splitBetween _ [] = []
splitBetween f xs = pre : splitBetween f post
    where (pre, post) = breakBetween f xs

breakBetween :: (a -> a -> Bool) -> [a] -> ([a], [a])
breakBetween f (x1 : xs@(x2:_))
    | f x1 x2 = ([x1], xs)
    | otherwise = let (pre, post) = breakBetween f xs in (x1 : pre, post)
breakBetween _ xs = (xs, [])

-- * span and break

-- | Like 'break', but the called function has access to the entire tail.
breakTails :: ([a] -> Bool) -> [a] -> ([a], [a])
breakTails _ [] = ([], [])
breakTails f lst@(x:xs)
    | f lst = ([], lst)
    | otherwise = let (pre, post) = breakTails f xs in (x:pre, post)

-- | Like 'span', but it can transform the spanned sublist.
spanWhile :: (a -> Maybe b) -> [a] -> ([b], [a])
spanWhile f = go
    where
    go [] = ([], [])
    go (a:as) = case f a of
        Just b -> first (b:) (go as)
        Nothing -> ([], a : as)

-- * duplicates

-- | Drop adjacent elts if they are equal after applying the key function.
-- The first elt is kept.
dropDups :: Eq k => (a -> k) -> [a] -> [a]
dropDups _ [] = []
dropDups key (x:xs) = x : map snd (filter (not . equal) (zip (x:xs) xs))
    where equal (a, b) = key a == key b

-- | Filter out elts when the predicate is true for adjacent elts.  The first
-- elt is kept, and the later ones are dropped.  This is like 'dropDups'
-- except it can compare two elements.  E.g. @dropWith (>=)@ will ensure the
-- sequence is increasing.
dropWith :: (a -> a -> Bool) -> [a] -> [a]
dropWith _ [] = []
dropWith _ [x] = [x]
dropWith f (x:y:xs)
    | f x y = dropWith f (x:xs)
    | otherwise = x : dropWith f (y:xs)

-- | Sort the input by the key, extract unique values, and also return the
-- duplicates.
partitionDups :: Ord k => (a -> k) -> [a] -> ([a], [(a, NonNull a)])
    -- ^ ([unique], [(used_for_unique, [dups])])
partitionDups key xs =
    Either.partitionEithers $ concatMap extract (groupSort key xs)
    where
    extract [] = []
    extract [x] = [Left x]
    extract (x:xs) = [Left x, Right (x, xs)]

-- | Find duplicate values.  There are always at least 2 of each output.
findDups :: Ord k => (a -> k) -> [a] -> [(a, NonEmpty a)]
findDups key = Maybe.mapMaybe extract . groupSort key
    where
    extract [] = Nothing
    extract [_] = Nothing
    extract (x1:x2:xs) = Just (x1, x2 :| xs)

-- | Like 'dropDups', but keep the last adjacent equal elt instead of the
-- first.
dropInitialDups :: Eq k => (a -> k) -> [a] -> [a]
dropInitialDups _ [] = []
dropInitialDups _ [x] = [x]
dropInitialDups key (x:xs@(next:_))
    | key x == key next = rest
    | otherwise = x:rest
    where rest = dropInitialDups key xs

unique :: Ord a => [a] -> [a]
unique = uniqueOn id

-- | This is like 'dropDups', except that it's not limited to just adjacent
-- elts.  The output list is in the same order as the input.
uniqueOn :: Ord k => (a -> k) -> [a] -> [a]
uniqueOn f xs = go Set.empty xs
    where
    go _set [] = []
    go set (x:xs)
        | k `Set.member` set = go set xs
        | otherwise = x : go (Set.insert k set) xs
        where k = f x

-- | Like 'unique', but sort the list, and should be more efficient.
uniqueSort :: Ord a => [a] -> [a]
uniqueSort = Set.toList . Set.fromList


-- * replace

-- | Replace sublist @from@ with @to@ in the given list.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to = go
    where
    len = length from
    go [] = []
    go lst@(x:xs)
        | from `List.isPrefixOf` lst = to ++ go (drop len lst)
        | otherwise = x : go xs

-- | Replace occurrances of an element with zero or more other elements.
replace1 :: Eq a => a -> [a] -> [a] -> [a]
replace1 from to = concatMap (\v -> if v == from then to else [v])


-- * search

count :: Foldable t => (a -> Bool) -> t a -> Int
count f = List.foldl' (\n c -> if f c then n + 1 else n) 0
