-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Seq where
import Prelude hiding (head, tail, last)
import Control.Applicative ((<$>))
import qualified Data.Char as Char
import Data.Function
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.Ordered as Ordered
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set


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

-- | Enumerate a half-open range.
range' :: (Num a, Ord a) => a -> a -> a -> [a]
range' start end step = go 0
    where
    go i
        | step >= 0 && val >= end = []
        | step < 0 && val <= end = []
        | otherwise = val : go (i+1)
        where val = start + (i*step)

-- | Like 'range', but always includes the end, even if it doesn't line up on
-- a step.
range_end :: (Num a, Ord a) => a -> a -> a -> [a]
range_end start end step = go 0
    where
    go i
        | step >= 0 && val >= end = [end]
        | step < 0 && val < end = [end]
        | otherwise = val : go (i+1)
        where val = start + (i*step)

-- | Infinite range.
range_ :: (Num a) => a -> a -> [a]
range_ start step = go 0
    where go i = start + (i*step) : go (i+1)

-- * transformation

key_on :: (a -> k) -> [a] -> [(k, a)]
key_on f xs = zip (map f xs) xs

key_on_maybe :: (a -> Maybe k) -> [a] -> [(k, a)]
key_on_maybe f xs = [(k, a) | (Just k, a) <- key_on f xs]

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

-- | Like mapMaybe, but keep the original value.
map_filter :: (a -> Maybe b) -> [a] -> [(b, a)]
map_filter f = go
    where
    go [] = []
    go (a:as) = case f a of
        Just b -> (b, a) : go as
        Nothing -> go as

map_head :: (a -> a) -> [a] -> [a]
map_head _ [] = []
map_head f (x:xs) = f x : xs

map_tail :: (a -> a) -> [a] -> [a]
map_tail f (x:xs) = x : map f xs
map_tail _ [] = []

map_init :: (a -> a) -> [a] -> [a]
map_init _ [] = []
map_init _ [x] = [x]
map_init f (x:xs) = f x : map_init f xs

map_last :: (a -> a) -> [a] -> [a]
map_last _ [] = []
map_last f [x] = [f x]
map_last f (x:xs) = x : map_last f xs


-- * permutations

-- | The cartesian product of a list of lists.  E.g.
-- @[[1, 2], [3, 4]]@ -> @[[1, 3], [1, 4], [2, 3], [2, 4]]@.
cartesian :: [[a]] -> [[a]]
cartesian [] = []
cartesian [xs] = [[x] | x <- xs]
cartesian (xs:rest) = [x:ps | x <- xs, ps <- cartesian rest]

-- * indexing lists

-- | Get @xs !! n@, but return Nothing if the index is out of range.
{-# SPECIALIZE at :: [a] -> Int -> Maybe a #-}
at :: (Num i, Ord i) => [a] -> i -> Maybe a
at xs n
    | n < 0 = Nothing
    | otherwise = _at xs n
    where
    _at [] _ = Nothing
    _at (x:_) 0 = Just x
    _at (_:xs) n = at xs (n-1)

at_err :: String -> [a] -> Int -> a
at_err msg xs n = Maybe.fromMaybe
    (error $ "Seq.at_err: " ++ msg ++ "; index " ++ show n ++ " out of range "
        ++ show (length xs))
    (at xs n)

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
        (elt:rest) -> pre ++ f elt : rest
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

min_on :: (Ord k) => (a -> k) -> a -> a -> a
min_on key x y = if key x <= key y then x else y

max_on :: (Ord k) => (a -> k) -> a -> a -> a
max_on key x y = if key x >= key y then x else y

minimum_on :: (Ord ord) => (a -> ord) -> [a] -> Maybe a
minimum_on _ [] = Nothing
minimum_on key xs = Just (List.foldl1' f xs)
    where f low x = if key x < key low then x else low

maximum_on :: (Ord ord) => (a -> ord) -> [a] -> Maybe a
maximum_on _ [] = Nothing
maximum_on key xs = Just (List.foldl1' f xs)
    where f high x = if key x > key high then x else high

minimum :: (Ord a) => [a] -> Maybe a
minimum [] = Nothing
minimum xs = Just (List.minimum xs)

maximum :: (Ord a) => [a] -> Maybe a
maximum [] = Nothing
maximum xs = Just (List.maximum xs)

-- * ordered lists

insert_on :: (Ord k) => (a -> k) -> a -> [a] -> [a]
insert_on key = List.insertBy (\a b -> compare (key a) (key b))

-- | Stable sort on a cheap key function.
sort_on :: (Ord k) => (a -> k) -> [a] -> [a]
sort_on = Ordered.sortOn'

-- | Like 'sort_on', but sort highest-to-lowest.
reverse_sort_on :: (Ord b) => (a -> b) -> [a] -> [a]
reverse_sort_on f = List.sortBy $ \a b -> Ord.comparing f b a

-- | Merge sorted lists.  If two elements compare equal, the one from the left
-- list comes first.
merge :: Ord a => [a] -> [a] -> [a]
merge = merge_on id

merge_by :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge_by = Ordered.mergeBy

merge_on :: (Ord k) => (a -> k) -> [a] -> [a] -> [a]
merge_on key = Ordered.mergeBy (Ord.comparing key)

merge_lists :: (Ord k) => (a -> k) -> [[a]] -> [a]
merge_lists key = foldr (merge_on key) []

-- | If the heads of the sublists are also sorted I can be lazy in the list of
-- sublists too.  This version is optimized for minimal overlap.
merge_asc_lists :: (Ord k) => (a -> k) -> [[a]] -> [a]
merge_asc_lists key = foldr go []
    where
    go [] ys = ys
    go (x:xs) ys = x : merge_on key xs ys

-- * grouping

-- | Group the unsorted list into @(key x, xs)@ where all @xs@ compare equal
-- after @key@ is applied to them.  List is returned in sorted order.
keyed_group_on :: (Ord key) => (a -> key) -> [a] -> [(key, NonNull a)]
keyed_group_on key = map (\gs -> (key (List.head gs), gs)) . group_on key

-- | Like 'groupBy', but the list doesn't need to be sorted, and use a key
-- function instead of equality.  The list is returned in sorted order.
group_on :: (Ord key) => (a -> key) -> [a] -> [NonNull a]
group_on key = group key . sort_on key -- TODO faster to use a map?

-- | Group each element with all the other elements that compare equal to it.
-- The heads of the groups appear in their original order.
group_eq :: (a -> a -> Bool) -> [a] -> [NonNull a]
group_eq is_equal = go
    where
    go [] = []
    go (x:xs) = (x : equal) : go inequal
        where (equal, inequal) = List.partition (is_equal x) xs

-- | 'group_eq' but with a key function.
group_eq_on :: (Eq key) => (a -> key) -> [a] -> [NonNull a]
group_eq_on key = group_eq (\x y -> key x == key y)

-- | This is just 'List.groupBy' except with a key function.  It only groups
-- adjacent elements.
group :: (Eq key) => (a -> key) -> [a] -> [[a]]
group key = List.groupBy ((==) `on` key)

-- | Pair each element with the following element.  The last element is paired
-- with Nothing.  Like @zip xs (drop 1 xs ++ f (last xs))@ but only traverses
-- @xs@ once.
zip_next :: [a] -> [(a, Maybe a)]
zip_next [] = []
zip_next [x] = [(x, Nothing)]
zip_next (x : xs@(y:_)) = (x, Just y) : zip_next xs

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

data Paired a b = First !a | Second !b | Both !a !b
    deriving (Show, Eq)

paired_second :: Paired a b -> Maybe b
paired_second (First _) = Nothing
paired_second (Second b) = Just b
paired_second (Both _ b) = Just b

paired_first :: Paired a b -> Maybe a
paired_first (First a) = Just a
paired_first (Second _) = Nothing
paired_first (Both a _) = Just a

-- | Like 'zip', but emit 'First's or 'Second's if the list lengths are
-- unequal.
padded_zip :: [a] -> [b] -> [Paired a b]
padded_zip [] [] = []
padded_zip [] bs = map Second bs
padded_zip as [] = map First as
padded_zip (a:as) (b:bs) = Both a b : padded_zip as bs

-- | Like 'zip', but the second list is padded with Nothings.
zip_padded2 :: [a] -> [b] -> [(a, Maybe b)]
zip_padded2 [] _ = []
zip_padded2 (x:xs) (y:ys) = (x, Just y) : zip_padded2 xs ys
zip_padded2 (x:xs) [] = [(x, Nothing) | x <- x : xs]

-- | Return the reversed inits paired with the tails.  This is like a zipper
-- moving focus along the input list.
zipper :: [a] -> [a] -> [([a], [a])]
zipper prev [] = [(prev, [])]
zipper prev lst@(x:xs) = (prev, lst) : zipper (x:prev) xs

-- | Pair @a@ elements up with @b@ elements.  If they are equal according to
-- @eq@, they'll both be Just in the result.  If an @a@ is deleted going from
-- @a@ to @b@, it will be Nothing, and vice versa for @b@.  You should never
-- get @(Nothing, Nothing)@.
--
-- Kind of like an edit distance, or a diff.
equal_pairs :: (a -> b -> Bool) -> [a] -> [b] -> [Paired a b]
equal_pairs _ [] ys = map Second ys
equal_pairs _ xs [] = map First xs
equal_pairs eq (x:xs) (y:ys)
    | x `eq` y = Both x y : equal_pairs eq xs ys
    | any (eq x) ys = Second y : equal_pairs eq (x:xs) ys
    | otherwise = First x : equal_pairs eq xs (y:ys)

-- | This is like 'equal_pairs', except that the index of each pair in the
-- /right/ list is included.  In other words, given @(i, Nothing, Just y)@,
-- @i@ is the position of @y@ in the @b@ list.  Given @(i, Just x, Nothing)@,
-- @i@ is where @x@ was deleted from the @b@ list.
indexed_pairs :: (a -> b -> Bool) -> [a] -> [b] -> [(Int, Paired a b)]
indexed_pairs eq xs ys = zip (indexed pairs) pairs
    where
    pairs = equal_pairs eq xs ys
    indexed = scanl f 0
        where
        f i (First _) = i
        f i _ = i+1

indexed_pairs_on :: (Eq eq) => (a -> eq) -> [a] -> [a] -> [(Int, Paired a a)]
indexed_pairs_on key = indexed_pairs (\a b -> key a == key b)

-- | Pair up two lists of sorted pairs by their first element.
pair_sorted :: (Ord k) => [(k, a)] -> [(k, b)] -> [(k, Paired a b)]
pair_sorted xs [] = [(k, First v) | (k, v) <- xs]
pair_sorted [] ys = [(k, Second v) | (k, v) <- ys]
pair_sorted x@((k0, v0) : xs) y@((k1, v1) : ys)
    | k0 == k1 = (k0, Both v0 v1) : pair_sorted xs ys
    | k0 < k1 = (k0, First v0) : pair_sorted xs y
    | otherwise = (k1, Second v1) : pair_sorted x ys

-- | Like 'pair_sorted', but use a key function, and omit the extracted key.
pair_sorted_on :: (Ord k) => (a -> k) -> [a] -> [a] -> [Paired a a]
pair_sorted_on key xs ys =
    map snd $ pair_sorted (key_on key xs) (key_on key ys)

-- | Sort the lists on with the key functions, then pair them up.
pair_on :: (Ord k) => (a -> k) -> (b -> k) -> [a] -> [b] -> [Paired a b]
pair_on k1 k2 xs ys = map snd $
    pair_sorted (sort_on fst (key_on k1 xs)) (sort_on fst (key_on k2 ys))

-- | Like 'pair_on', but when the lists have the same type.
pair_on1 :: (Ord k) => (a -> k) -> [a] -> [a] -> [Paired a a]
pair_on1 k = pair_on k k

-- | Left if the val was in the left list but not the right, Right for the
-- converse.
diff :: (a -> b -> Bool) -> [a] -> [b] -> [Either a b]
diff eq xs ys = Maybe.mapMaybe f (equal_pairs eq xs ys)
    where
    f (First a) = Just (Left a)
    f (Second a) = Just (Right a)
    f _ = Nothing

-- * partition

-- | Partition a list of Eithers into a pair.  Lazy enough to handle an
-- infinite input list.
partition_either :: [Either a b] -> ([a], [b])
partition_either [] = ([], [])
partition_either (x:xs) =
    let (ls, rs) = partition_either xs
    in case x of
        Left l -> (l:ls, rs)
        Right r -> (ls, r:rs)

partition_with :: (a -> Maybe b) -> [a] -> ([b], [a])
partition_with f = go
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

-- | Take a list of rows to a list of columns.  Similar to zip, the result is
-- trimmed to the length of the shortest row.
rotate :: [[a]] -> [[a]]
rotate [] = []
rotate xs = maybe [] (: rotate (map List.tail xs)) (mapM head xs)

-- | Similar to 'rotate', except that the result is the length of the longest
-- row and missing columns are Nothing.
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

-- | Drop adjacent elts if they are equal after applying the key function.
-- The first elt is kept.
drop_dups :: (Eq k) => (a -> k) -> [a] -> [a]
drop_dups _ [] = []
drop_dups key (x:xs) = x : map snd (filter (not . equal) (zip (x:xs) xs))
    where equal (x, y) = key x == key y

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

-- | Like 'drop_dups', but return the dropped values.
partition_dups :: (Ord k) => (a -> k) -> [a] -> ([a], [(a, [a])])
    -- ^ ([unique], [(used_for_unique, [dups])])
partition_dups key xs = partition_either $ concatMap extract (group_on key xs)
    where
    extract [] = []
    extract [x] = [Left x]
    extract (x:xs) = [Left x, Right (x, xs)]

-- | Like 'drop_dups', but keep the last adjacent equal elt instead of the
-- first.
drop_initial_dups :: (Eq k) => (a -> k) -> [a] -> [a]
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

rdrop :: Int -> [a] -> [a]
rdrop n = either (const []) id . foldr f (Left n)
    where
    f x (Left left)
        | left <= 0 = Right [x]
        | otherwise = Left (left-1)
    f x (Right xs) = Right (x:xs)

rdrop_while :: (a -> Bool) -> [a] -> [a]
rdrop_while f = foldr (\x xs -> if null xs && f x then [] else x:xs) []

lstrip, rstrip, strip :: String -> String
lstrip = dropWhile Char.isSpace
rstrip = rdrop_while Char.isSpace
strip = rstrip . lstrip

-- | If the list doesn't have the given prefix, return the original list and
-- False.  Otherwise, strip it off and return True.
drop_prefix :: (Eq a) => [a] -> [a] -> ([a], Bool)
drop_prefix pref list = go pref list
    where
    go [] xs = (xs, True)
    go _ [] = (list, False)
    go (p:ps) (x:xs)
        | p == x = go ps xs
        | otherwise = (list, False)

drop_suffix :: (Eq a) => [a] -> [a] -> ([a], Bool)
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
    go (x:xs) = case f x of
        Nothing -> ([], x : xs)
        Just y -> let (ys, rest) = go xs in (y:ys, rest)

-- | 'span_while' from the end of the list.
span_end_while :: (a -> Maybe b) -> [a] -> ([a], [b])
span_end_while f xs = (reverse post, reverse pre)
    where (pre, post) = span_while f (reverse xs)

-- | List initial and final element, if any.
viewr :: [a] -> Maybe ([a], a)
viewr [] = Nothing
viewr (x:xs) = Just $ go x xs
    where
    go x [] = ([], x)
    go x (x':xs) = let (pre, post) = go x' xs in (x:pre, post)

ne_viewr :: NonEmpty a -> ([a], a)
ne_viewr (x :| xs) =
    Maybe.fromMaybe (error "ne_viewr: not reached") (viewr (x : xs))

-- ** split and join

-- | Split @xs@ before places where @f@ matches.
--
-- > split_with (==1) [1,2,1]
-- > --> [[1, 2], [1]]
split_with :: (a -> Bool) -> [a] -> [[a]]
split_with f xs = map reverse (go f xs [])
    where
    go _ [] collect = [collect]
    go f (x:xs) collect
        | f x = collect : go f xs [x]
        | otherwise = go f xs (x:collect)

-- | Split 'xs' on 'sep', dropping 'sep' from the result.
split :: (Eq a) => NonNull a -> [a] -> NonNull [a]
split [] _ = error "Util.Seq.split: empty separator"
split sep xs = go sep xs
    where
    go sep xs
        | null post = [pre]
        | otherwise = pre : split sep (drop (length sep) post)
        where (pre, post) = break_tails (sep `List.isPrefixOf`) xs

-- | Like 'split', but it returns [] if the input was null.
split_null :: (Eq a) => NonNull a -> [a] -> [[a]]
split_null _ [] = []
split_null sep xs = split sep xs

-- | 'split' never returns null, so sometimes it's more convenient to express
-- that in the type.
split_t :: (Eq a) => NonNull a -> [a] -> ([a], [[a]])
split_t sep xs = case split sep xs of
    (g:gs) -> (g, gs)
    _ -> error "split_t: unreached"

-- | Like 'split', but only split once.
split1 :: (Eq a) => NonNull a -> [a] -> ([a], [a])
split1 [] _ = error "Util.Seq.split1: empty seperator"
split1 sep xs = (pre, drop (length sep) post)
    where (pre, post) = break_tails (sep `List.isPrefixOf`) xs

-- | Concat a list with 'sep' in between.
join :: [a] -> [[a]] -> [a]
join = List.intercalate

-- | Binary join, but drops null values.
join2 :: [a] -> [a] -> [a] -> [a]
join2 _ a [] = a
join2 _ [] b = b
join2 sep a b = a ++ sep ++ b

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
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace from to = go
    where
    len = length from
    go [] = []
    go lst@(x:xs)
        | from `List.isPrefixOf` lst = to ++ go (drop len lst)
        | otherwise = x : go xs

-- | Replace occurrances of an element with zero or more other elements.
replace1 :: (Eq a) => a -> [a] -> [a] -> [a]
replace1 from to = concatMap (\v -> if v == from then to else [v])


-- * search

count :: (Eq a) => a -> [a] -> Int
count x = List.foldl' (\n c -> if c == x then n + 1 else n) 0


-- * monadic

-- | Like 'List.mapAccumL', but monadic.
mapAccumLM :: (Monad m) => (state -> x -> m (state, y)) -> state -> [x]
    -> m (state, [y])
mapAccumLM f state xs = go state xs
    where
    go state [] = return (state, [])
    go state (x:xs) = do
        (state, y) <- f state x
        (state, ys) <- go state xs
        return (state, y : ys)
