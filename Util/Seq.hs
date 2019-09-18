-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE BangPatterns #-}
module Util.Seq where
import           Prelude hiding (head, last, tail)
import           Data.Bifunctor (Bifunctor(bimap), first, second)
import qualified Data.Char as Char
import qualified Data.Either as Either
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.Ordered as Ordered
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Monoid ((<>))
import qualified Data.Ord as Ord
import qualified Data.Set as Set

import qualified Util.PolyDiff as PolyDiff
import qualified Util.Then as Then


-- | This is just a list, but is documentation that a return value will never
-- be null, or an argument should never be null.  This is for cases where
-- 'NonEmpty' is too annoying to work with.
type NonNull a = [a]

-- * enumeration

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

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
range_end :: (Num a, Ord a) => a -> a -> a -> [a]
range_end start end step = go 0
    where
    go i
        | step >= 0 && val >= end = [end]
        | step < 0 && val <= end = [end]
        | otherwise = val : go (i+1)
        where val = start + (i*step)
{-# INLINEABLE range_end #-}
{-# SPECIALIZE range_end :: Int -> Int -> Int -> [Int] #-}

-- | Infinite range.
range_ :: Num a => a -> a -> [a]
range_ start step = go 0
    where go i = start + (i*step) : go (i+1)
{-# INLINEABLE range_ #-}
{-# SPECIALIZE range_ :: Int -> Int -> [Int] #-}

-- * transformation

key_on :: (a -> k) -> [a] -> [(k, a)]
key_on f xs = zip (map f xs) xs

key_on_snd :: (a -> k) -> [a] -> [(a, k)]
key_on_snd f xs = zip xs (map f xs)

key_on_just :: (a -> Maybe k) -> [a] -> [(k, a)]
key_on_just f xs = [(k, a) | (Just k, a) <- key_on f xs]

-- | Apply a function to the first and last elements.  Middle elements are
-- unchanged.  A null or singleton list is also unchanged.
first_last :: (a -> a) -> (a -> a) -> [a] -> [a]
first_last start end xs = case xs of
    [] -> []
    [x] -> [x]
    x : xs -> start x : go xs
    where
    go [] = []
    go [x] = [end x]
    go (x:xs) = x : go xs

-- | Filter on the fst values returning Just.
map_maybe_fst :: (a -> Maybe a2) -> [(a, b)] -> [(a2, b)]
map_maybe_fst f xs = [(a, b) | (Just a, b) <- map (first f) xs]

-- | Filter on the snd values returning Just.
map_maybe_snd :: (b -> Maybe b2) -> [(a, b)] -> [(a, b2)]
map_maybe_snd f xs = [(a, b) | (a, Just b) <- map (second f) xs]

map_head :: (a -> a) -> [a] -> [a]
map_head _ [] = []
map_head f (x:xs) = f x : xs

map_tail :: (a -> a) -> [a] -> [a]
map_tail f (x:xs) = x : map f xs
map_tail _ [] = []

map_head_tail :: (a -> b) -> (a -> b) -> [a] -> [b]
map_head_tail f g (x : xs) = f x : map g xs
map_head_tail _ _ [] = []

map_init :: (a -> a) -> [a] -> [a]
map_init _ [] = []
map_init _ [x] = [x]
map_init f (x:xs) = f x : map_init f xs

map_last :: (a -> a) -> [a] -> [a]
map_last _ [] = []
map_last f [x] = [f x]
map_last f (x:xs) = x : map_last f xs

-- | This is like 'scanl', but you can scan with a key function.  E.g. to
-- accumulate line lengths: @scanl_on length (+) 0@.
scanl_on :: (accum -> key -> accum) -> (a -> key) -> accum -> [a]
    -> [(accum, a)]
scanl_on f key z xs = zip (scanl (\t -> f t . key) z xs) xs

-- * permutations

-- | The cartesian product of a list of lists.  E.g.
-- @[[1, 2], [3, 4]]@ -> @[[1, 3], [1, 4], [2, 3], [2, 4]]@.
cartesian :: [[a]] -> [[a]]
cartesian [] = []
cartesian [xs] = [[x] | x <- xs]
cartesian (xs:rest) = [x:ps | x <- xs, ps <- cartesian rest]

-- * indexing lists

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
insert_at :: Int -> a -> [a] -> [a]
insert_at i x xs = let (pre, post) = splitAt i xs in pre ++ (x : post)

-- | Remove the element at the given index.  Do nothing if the index is out
-- of range.
remove_at :: Int -> [a] -> [a]
remove_at i xs = let (pre, post) = splitAt i xs in pre ++ drop 1 post

-- | Like 'remove_at' but return the removed element as well.
take_at :: Int -> [a] -> Maybe (a, [a])
take_at i xs = case post of
        v : vs -> Just (v, pre ++ vs)
        [] -> Nothing
    where (pre, post) = splitAt i xs

-- | Modify element at an index by applying a function to it.  If the index is
-- out of range, nothing happens.
modify_at :: Int -> (a -> a) -> [a] -> [a]
modify_at i f xs = case post of
    [] -> pre
    elt : rest -> pre ++ f elt : rest
    where (pre, post) = splitAt i xs

-- | Find an element, then change it.  Return Nothing if the element wasn't
-- found.
find_modify :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
find_modify match modify = go
    where
    go (x:xs)
        | match x = Just $ modify x : xs
        | otherwise = (x:) <$> go xs
    go [] = Nothing

-- | Similar to 'modify_at', but will insert an element for an out of range
-- positive index.  The list will be extended with 'deflt', and the modify
-- function passed a Nothing.
update_at :: a -> Int -> (Maybe a -> a) -> [a] -> [a]
update_at deflt i f xs
    | i < 0 = error $ "Seq.update_at: negative index " ++ show i
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
    (x, dropped) <- take_at from xs
    return $ insert_at to x dropped


-- * min / max

min_on :: Ord k => (a -> k) -> a -> a -> a
min_on key x y = if key x <= key y then x else y

max_on :: Ord k => (a -> k) -> a -> a -> a
max_on key x y = if key x >= key y then x else y

minimum_on :: Ord k => (a -> k) -> [a] -> Maybe a
minimum_on _ [] = Nothing
minimum_on key xs = Just (List.foldl1' f xs)
    where f low x = if key x < key low then x else low

maximum_on :: Ord k => (a -> k) -> [a] -> Maybe a
maximum_on _ [] = Nothing
maximum_on key xs = Just (List.foldl1' f xs)
    where f high x = if key x > key high then x else high

minimum :: Ord a => [a] -> Maybe a
minimum [] = Nothing
minimum xs = Just (List.minimum xs)

maximum :: Ord a => [a] -> Maybe a
maximum [] = Nothing
maximum xs = Just (List.maximum xs)

ne_minimum :: Ord a => NonEmpty a -> a
ne_minimum (x :| xs) = List.minimum (x : xs)

ne_maximum :: Ord a => NonEmpty a -> a
ne_maximum (x :| xs) = List.maximum (x : xs)

-- * ordered lists

insert_on :: Ord k => (a -> k) -> a -> [a] -> [a]
insert_on key = List.insertBy (\a b -> compare (key a) (key b))

-- | Stable sort on a cheap key function.  Different from 'List.sortOn', which
-- is for an expensive key function.
sort_on :: Ord k => (a -> k) -> [a] -> [a]
sort_on = Ordered.sortOn'

-- | Like 'sort_on', but sort highest-to-lowest.
reverse_sort_on :: Ord b => (a -> b) -> [a] -> [a]
reverse_sort_on f = List.sortBy $ \a b -> Ord.comparing f b a

-- | Merge sorted lists.  If two elements compare equal, the one from the left
-- list comes first.
merge :: Ord a => [a] -> [a] -> [a]
merge = merge_on id

merge_by :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge_by = Ordered.mergeBy

merge_on :: Ord k => (a -> k) -> [a] -> [a] -> [a]
merge_on key = Ordered.mergeBy (Ord.comparing key)

merge_lists :: Ord k => (a -> k) -> [[a]] -> [a]
merge_lists key = foldr (merge_on key) []

-- | If the heads of the sublists are also sorted I can be lazy in the list of
-- sublists too.  This version is optimized for minimal overlap.
merge_asc_lists :: Ord k => (a -> k) -> [[a]] -> [a]
merge_asc_lists key = foldr go []
    where
    go [] ys = ys
    go (x:xs) ys = x : merge_on key xs ys

-- * grouping

-- ** adjacent

-- Adjacent groups only group adjacent elements, just like 'List.group'.

-- | This is just 'List.groupBy' except with a key function.
group_adjacent :: Eq key => (a -> key) -> [a] -> [NonNull a]
group_adjacent key = List.groupBy ((==) `on` key)

-- | Like 'group_adjacent', but include the key.
keyed_group_adjacent :: Eq key => (a -> key) -> [a] -> [(key, NonNull a)]
keyed_group_adjacent key =
    key_on (key . List.head) . List.groupBy ((==) `on` key)

-- ** sort

-- Sort groups sort the input by the group key as a side-effect of grouping.

-- | Group the unsorted list into @(key x, xs)@ where all @xs@ compare equal
-- after @key@ is applied to them.
keyed_group_sort :: Ord key => (a -> key) -> [a] -> [(key, NonNull a)]
    -- ^ Sorted by key. The NonNull group is in the same order as the input.
keyed_group_sort key = Map.toAscList . foldr go Map.empty
    where go x = Map.alter (Just . maybe [x] (x:)) (key x)

-- | Similar to 'keyed_group_sort', but key on the fst element, and strip the
-- key out of the groups.
group_fst :: Ord a => [(a, b)] -> [(a, NonNull b)]
group_fst xs = [(key, map snd group) | (key, group) <- keyed_group_sort fst xs]

-- | Like 'group_fst', but group on the snd element.
group_snd :: Ord b => [(a, b)] -> [(NonNull a, b)]
group_snd xs = [(map fst group, key) | (key, group) <- keyed_group_sort snd xs]

-- | Like 'groupBy', but the list doesn't need to be sorted, and use a key
-- function instead of equality.  The list is sorted by the key, and the groups
-- appear in their original order in the input list.
group_sort :: Ord key => (a -> key) -> [a] -> [NonNull a]
group_sort key = map snd . keyed_group_sort key

-- ** stable

-- Stable groups preserve the original input order, in both the heads of the
-- groups, and within the groups themselves.

-- | Group each element with all the other elements that compare equal to it.
-- The heads of the groups appear in their original order.
group_stable_with :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
group_stable_with is_equal = go
    where
    go [] = []
    go (x:xs) = (x :| equal) : go inequal
        where (equal, inequal) = List.partition (is_equal x) xs

-- | 'group_stable_with' but with a key function.
group_stable :: Eq key => (a -> key) -> [a] -> [NonEmpty a]
group_stable key = group_stable_with (\x y -> key x == key y)

keyed_group_stable :: Eq key => (a -> key) -> [a] -> [(key, [a])]
keyed_group_stable key = map (\(g :| gs) -> (key g, g:gs)) . group_stable key

-- * zipping

-- | Pair each element with the following element.  The last element is paired
-- with Nothing.  Like @zip xs (drop 1 xs ++ f (last xs))@ but only traverses
-- @xs@ once.
zip_next :: [a] -> [(a, Maybe a)]
zip_next [] = []
zip_next [x] = [(x, Nothing)]
zip_next (x : xs@(y:_)) = (x, Just y) : zip_next xs

zip_nexts :: [a] -> [(a, [a])]
zip_nexts xs = zip xs (drop 1 (List.tails xs))

zip_prev :: [a] -> [(Maybe a, a)]
zip_prev xs = zip (Nothing : map Just xs) xs

-- | Like 'zip_next' but with both preceding and following elements.
zip_neighbors :: [a] -> [(Maybe a, a, Maybe a)]
zip_neighbors [] = []
zip_neighbors (x:xs) = (Nothing, x, head xs) : go x xs
    where
    go _ [] = []
    go prev [x] = [(Just prev, x, Nothing)]
    go prev (x : xs@(y:_)) = (Just prev, x, Just y) : go x xs

-- | This is like 'zip', but it returns the remainder of the longer argument
-- instead of discarding it.
zip_remainder :: [a] -> [b] -> ([(a, b)], Either [a] [b])
zip_remainder (x:xs) (y:ys) = first ((x, y) :) (zip_remainder xs ys)
zip_remainder [] ys = ([], Right ys)
zip_remainder xs [] = ([], Left xs)

data Paired a b = First !a | Second !b | Both !a !b
    deriving (Show, Eq)

instance Bifunctor Paired where
    bimap f g paired = case paired of
        First a -> First (f a)
        Second b -> Second (g b)
        Both a b -> Both (f a) (g b)

paired_second :: Paired a b -> Maybe b
paired_second (First _) = Nothing
paired_second (Second b) = Just b
paired_second (Both _ b) = Just b

paired_first :: Paired a b -> Maybe a
paired_first (First a) = Just a
paired_first (Second _) = Nothing
paired_first (Both a _) = Just a

partition_paired :: [Paired a b] -> ([a], [b])
partition_paired (pair : pairs) = case pair of
    Both a b -> (a : as, b : bs)
    First a -> (a : as, bs)
    Second b -> (as, b : bs)
    where (as, bs) = partition_paired pairs
partition_paired [] = ([], [])

-- | Like 'zip', but emit 'First's or 'Second's if the list lengths are
-- unequal.
zip_padded :: [a] -> [b] -> [Paired a b]
zip_padded [] [] = []
zip_padded [] bs = map Second bs
zip_padded as [] = map First as
zip_padded (a:as) (b:bs) = Both a b : zip_padded as bs

-- | Like 'zip', but the first list is padded with Nothings.
zip_padded_fst :: [a] -> [b] -> [(Maybe a, b)]
zip_padded_fst _ [] = []
zip_padded_fst (a:as) (b:bs) = (Just a, b) : zip_padded_fst as bs
zip_padded_fst [] (b:bs) = map ((,) Nothing) (b:bs)

-- | Like 'zip', but the second list is padded with Nothings.
zip_padded_snd :: [a] -> [b] -> [(a, Maybe b)]
zip_padded_snd [] _ = []
zip_padded_snd (a:as) (b:bs) = (a, Just b) : zip_padded_snd as bs
zip_padded_snd (a:as) [] = [(a, Nothing) | a <- a : as]

-- | Return the reversed inits paired with the tails.  This is like a zipper
-- moving focus along the input list.
zipper :: [a] -> [a] -> [([a], [a])]
zipper prev [] = [(prev, [])]
zipper prev lst@(x:xs) = (prev, lst) : zipper (x:prev) xs

-- | Perform a Meyes diff.
diff :: (a -> b -> Bool) -> [a] -> [b] -> [Paired a b]
diff eq as bs = map convert $ PolyDiff.getDiffBy eq as bs
    where
    convert a = case a of
        PolyDiff.First a -> First a
        PolyDiff.Second b -> Second b
        PolyDiff.Both a b -> Both a b

-- | Left if the val was in the left list but not the right, Right for the
-- converse.
diff_either :: (a -> b -> Bool) -> [a] -> [b] -> [Either a b]
diff_either eq as bs = Maybe.mapMaybe to_either $ diff eq as bs
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
diff_index :: (a -> b -> Bool) -> [a] -> [b] -> [(Int, Paired a b)]
diff_index eq as bs = zip (indexed pairs) pairs
    where
    pairs = diff eq as bs
    indexed = scanl f 0
        where
        f i (First _) = i
        f i _ = i+1

diff_index_on :: Eq k => (a -> k) -> [a] -> [a] -> [(Int, Paired a a)]
diff_index_on key = diff_index (\a b -> key a == key b)

-- | Pair up two lists of sorted pairs by their first element.
pair_sorted :: Ord k => [(k, a)] -> [(k, b)] -> [(k, Paired a b)]
pair_sorted xs [] = [(k, First v) | (k, v) <- xs]
pair_sorted [] ys = [(k, Second v) | (k, v) <- ys]
pair_sorted x@((k0, v0) : xs) y@((k1, v1) : ys)
    | k0 == k1 = (k0, Both v0 v1) : pair_sorted xs ys
    | k0 < k1 = (k0, First v0) : pair_sorted xs y
    | otherwise = (k1, Second v1) : pair_sorted x ys

-- | Like 'pair_sorted', but use a key function, and omit the extracted key.
pair_sorted_on :: Ord k => (a -> k) -> [a] -> [a] -> [Paired a a]
pair_sorted_on key xs ys =
    map snd $ pair_sorted (key_on key xs) (key_on key ys)

-- | Sort the lists on with the key functions, then pair them up.
pair_on :: Ord k => (a -> k) -> (b -> k) -> [a] -> [b] -> [Paired a b]
pair_on k1 k2 xs ys = map snd $
    pair_sorted (sort_on fst (key_on k1 xs)) (sort_on fst (key_on k2 ys))

-- | Like 'pair_on', but when the lists have the same type.
pair_on1 :: Ord k => (a -> k) -> [a] -> [a] -> [Paired a a]
pair_on1 k = pair_on k k

-- * partition

-- | Like 'List.partition', but partition by two functions consecutively.
partition2 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
partition2 f1 f2 xs = (as, bs, xs3)
    where
    (as, xs2) = List.partition f1 xs
    (bs, xs3) = List.partition f2 xs2

partition_on :: (a -> Maybe b) -> [a] -> ([b], [a])
partition_on f = go
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
    | otherwise = heads : rotate2 (map tl xs)
    where
    heads = map head xs
    tl [] = []
    tl (_:xs) = xs


-- ** extracting sublists

-- | Total variants of unsafe list operations.
head, last :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x
last [] = Nothing
last xs = Just (List.last xs)

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

-- | Drop until the last element before or equal to the given element.
drop_before :: Ord key => (a -> key) -> key -> [a] -> [a]
drop_before key p = go
    where
    go [] = []
    go (x0 : xs)
        | p < key x0 = x0 : xs
        | otherwise = case xs of
            x1 : _ | p >= key x1 -> go xs
            _ -> x0 : xs


-- ** duplicates

-- | Drop adjacent elts if they are equal after applying the key function.
-- The first elt is kept.
drop_dups :: Eq k => (a -> k) -> [a] -> [a]
drop_dups _ [] = []
drop_dups key (x:xs) = x : map snd (filter (not . equal) (zip (x:xs) xs))
    where equal (a, b) = key a == key b

-- | Filter out elts when the predicate is true for adjacent elts.  The first
-- elt is kept, and the later ones are dropped.  This is like 'drop_dups'
-- except it can compare two elements.  E.g. @drop_with (>=)@ will ensure the
-- sequence is increasing.
drop_with :: (a -> a -> Bool) -> [a] -> [a]
drop_with _ [] = []
drop_with _ [x] = [x]
drop_with f (x:y:xs)
    | f x y = drop_with f (x:xs)
    | otherwise = x : drop_with f (y:xs)

-- | Sort the input by the key, extract unique values, and also return the
-- duplicates.
partition_dups :: Ord k => (a -> k) -> [a] -> ([a], [(a, NonNull a)])
    -- ^ ([unique], [(used_for_unique, [dups])])
partition_dups key xs =
    Either.partitionEithers $ concatMap extract (group_sort key xs)
    where
    extract [] = []
    extract [x] = [Left x]
    extract (x:xs) = [Left x, Right (x, xs)]

-- | Find duplicate values.  There are always at least 2 of each output.
find_dups :: Ord k => (a -> k) -> [a] -> [(a, NonEmpty a)]
find_dups key = Maybe.mapMaybe extract . group_sort key
    where
    extract [] = Nothing
    extract [_] = Nothing
    extract (x1:x2:xs) = Just (x1, x2 :| xs)

-- | Like 'drop_dups', but keep the last adjacent equal elt instead of the
-- first.
drop_initial_dups :: Eq k => (a -> k) -> [a] -> [a]
drop_initial_dups _ [] = []
drop_initial_dups _ [x] = [x]
drop_initial_dups key (x:xs@(next:_))
    | key x == key next = rest
    | otherwise = x:rest
    where rest = drop_initial_dups key xs

unique :: Ord a => [a] -> [a]
unique = unique_on id

-- | This is like 'drop_dups', except that it's not limited to just adjacent
-- elts.  The output list is in the same order as the input.
unique_on :: Ord k => (a -> k) -> [a] -> [a]
unique_on f xs = go Set.empty xs
    where
    go _set [] = []
    go set (x:xs)
        | k `Set.member` set = go set xs
        | otherwise = x : go (Set.insert k set) xs
        where k = f x

-- | Like 'unique', but sort the list, and should be more efficient.
unique_sort :: Ord a => [a] -> [a]
unique_sort = Set.toList . Set.fromList

-- ** right variants

rtake :: Int -> [a] -> [a]
rtake n = snd . foldr go (n, [])
    where
    go x (n, xs)
        | n <= 0 = (0, xs)
        | otherwise = (n - 1, x : xs)

rtake_while :: (a -> Bool) -> [a] -> [a]
rtake_while f = either id (const []) . foldr go (Right [])
    where
    -- Left means I'm done taking.
    go _ (Left xs) = Left xs
    go x (Right xs)
        | f x = Right (x:xs)
        | otherwise = Left xs

rdrop :: Int -> [a] -> [a]
rdrop n = either id (const []) . foldr f (Right n)
    where
    f x (Right n)
        | n <= 0 = Left [x]
        | otherwise = Right (n-1)
    f x (Left xs) = Left (x:xs)

-- | The same as 'List.dropWhileEnd` except I also have all the other from-end
-- variants.
rdrop_while :: (a -> Bool) -> [a] -> [a]
rdrop_while = List.dropWhileEnd

lstrip, rstrip, strip :: String -> String
lstrip = dropWhile Char.isSpace
rstrip = rdrop_while Char.isSpace
strip = rstrip . lstrip

-- | If the list doesn't have the given prefix, return the original list and
-- False.  Otherwise, strip it off and return True.  'List.stripPrefix' is an
-- alternate version.
drop_prefix :: Eq a => [a] -> [a] -> ([a], Bool)
drop_prefix pref list = go pref list
    where
    go [] xs = (xs, True)
    go _ [] = (list, False)
    go (p:ps) (x:xs)
        | p == x = go ps xs
        | otherwise = (list, False)

drop_suffix :: Eq a => [a] -> [a] -> ([a], Bool)
drop_suffix suffix list
    | post == suffix = (pre, True)
    | otherwise = (list, False)
    where (pre, post) = splitAt (length list - length suffix) list

-- ** span and break

-- | Like 'break', but the called function has access to the entire tail.
break_tails :: ([a] -> Bool) -> [a] -> ([a], [a])
break_tails _ [] = ([], [])
break_tails f lst@(x:xs)
    | f lst = ([], lst)
    | otherwise = let (pre, post) = break_tails f xs in (x:pre, post)

-- | 'List.span' from the end of the list.
span_end :: (a -> Bool) -> [a] -> ([a], [a])
span_end f xs = (reverse post, reverse pre)
    where (pre, post) = span f (reverse xs)

-- | Like 'span', but it can transform the spanned sublist.
span_while :: (a -> Maybe b) -> [a] -> ([b], [a])
span_while f = go
    where
    go [] = ([], [])
    go (a:as) = case f a of
        Just b -> first (b:) (go as)
        Nothing -> ([], a : as)

-- | 'span_while' from the end of the list.
span_end_while :: (a -> Maybe b) -> [a] -> ([a], [b])
span_end_while f xs = (reverse post, reverse pre)
    where (pre, post) = span_while f (reverse xs)

-- | List initial and final element, if any.
viewr :: [a] -> Maybe ([a], a)
viewr [] = Nothing
viewr (x:xs) = Just $ go x xs
    where
    go x0 [] = ([], x0)
    go x0 (x:xs) = let (pre, post) = go x xs in (x0:pre, post)

ne_viewr :: NonEmpty a -> ([a], a)
ne_viewr (x :| xs) =
    Maybe.fromMaybe (error "ne_viewr: not reached") (viewr (x : xs))

-- ** split and join

-- | Split before places where the function matches.
--
-- > > split_before (==1) [1, 2, 1]
-- > [[], [1, 2], [1]]
split_before :: (a -> Bool) -> [a] -> [[a]]
split_before f = go
    where
    go [] = []
    go xs0 = pre : case post of
        x : xs -> cons1 x (go xs)
        [] -> []
        where (pre, post) = break f xs0
    cons1 x [] = [[x]]
    cons1 x (g:gs) = (x:g) : gs

-- | Like 'split_before', but express the NonEmpty parts in the type.
--
-- > > split_before_ne (==1) [1, 2, 1]
-- > ([], [1 :| [2], 1 :| []])
split_before_ne :: (a -> Bool) -> [a] -> ([a], [NonEmpty a])
split_before_ne f = second go . break f
    where
    go [] = []
    go (x : xs) = (x :| pre) : go post
        where (pre, post) = break f xs

-- | Split after places where the function matches.
split_after :: (a -> Bool) -> [a] -> [[a]]
split_after f = go
    where
    go [] = []
    go xs = pre : go post
        where (pre, post) = Then.break1 f xs

-- | Split 'xs' on 'sep', dropping 'sep' from the result.
split :: Eq a => NonNull a -> [a] -> NonNull [a]
split [] = error "Util.Seq.split: empty separator"
split sep = go
    where
    go xs
        | null post = [pre]
        | otherwise = pre : go (drop (length sep) post)
        where (pre, post) = break_tails (sep `List.isPrefixOf`) xs

-- | Like 'split', but it returns [] if the input was null.
split_null :: Eq a => NonNull a -> [a] -> [[a]]
split_null _ [] = []
split_null sep xs = split sep xs

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

-- | Binary join, but the separator is only used if both joinees are non-empty.
join2 :: (Monoid a, Eq a) => a -> a -> a -> a
join2 sep x y
    | y == mempty = x
    | x == mempty = y
    | otherwise = x <> sep <> y

-- | Split the list on the points where the given function returns true.
--
-- This is similar to 'groupBy', except this is defined to compare adjacent
-- elements.  'groupBy' actually compares to the first element of each group.
-- E.g. you can't group numeric runs with @groupBy (\a b -> b > a+1)@.
split_between :: (a -> a -> Bool) -> [a] -> [[a]]
split_between _ [] = []
split_between f xs = pre : split_between f post
    where (pre, post) = break_between f xs

break_between :: (a -> a -> Bool) -> [a] -> ([a], [a])
break_between f (x1 : xs@(x2:_))
    | f x1 x2 = ([x1], xs)
    | otherwise = let (pre, post) = break_between f xs in (x1 : pre, post)
break_between _ xs = (xs, [])


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


-- * monadic

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
