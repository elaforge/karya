module Util.Seq where
import Prelude hiding (head, tail, last)
import qualified Data.Char as Char
import Data.Function
import qualified Data.List as List
import qualified Data.List.Ordered as Ordered
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set


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

-- | Sort on a cheap key function.
sort_on :: (Ord b) => (a -> b) -> [a] -> [a]
sort_on = Ordered.sortOn'

-- | Like 'sort_on', but sort highest-to-lowest.
reverse_sort_on :: (Ord b) => (a -> b) -> [a] -> [a]
reverse_sort_on f = List.sortBy $ \a b -> compare (f b) (f a)

-- | Merge sorted lists.  If two elements compare equal, the one from the left
-- list comes first.
merge :: Ord a => [a] -> [a] -> [a]
merge = merge_on id

merge_by :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge_by = Ordered.mergeBy

merge_on :: (Ord k) => (a -> k) -> [a] -> [a] -> [a]
merge_on key = Ordered.mergeBy (compare `on` key)

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
--
-- The sublists are never null.
keyed_group_on :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
keyed_group_on key = map (\gs -> (key (List.head gs), gs)) . group_on key

-- | Like 'groupBy', but the list doesn't need to be sorted, and use a key
-- function instead of equality.  List is returned in sorted order.
--
-- The sublists are never null.
group_on :: (Ord b) => (a -> b) -> [a] -> [[a]]
group_on key = List.groupBy ((==) `on` key) . sort_on key

group :: (Ord b) => (a -> b) -> [a] -> [[a]]
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

-- | Like 'zip', but the shorter list is padded with Nothings.
padded_zip :: [a] -> [b] -> [Paired a b]
padded_zip [] [] = []
padded_zip [] bs = map Second bs
padded_zip as [] = map First as
padded_zip (a:as) (b:bs) = Both a b : padded_zip as bs

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
    indexed pairs = scanl f 0 pairs
        where
        f i (First _) = i
        f i _ = i+1

indexed_pairs_on :: (Eq eq) => (a -> eq) -> [a] -> [a] -> [(Int, Paired a a)]
indexed_pairs_on key xs ys = indexed_pairs (\a b -> key a == key b) xs ys

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

-- | Left if the val was in the left list but not the right, Right for the
-- converse.
diff :: (a -> b -> Bool) -> [a] -> [b] -> [Either a b]
diff eq xs ys = Maybe.mapMaybe f (equal_pairs eq xs ys)
    where
    f (First a) = Just (Left a)
    f (Second a) = Just (Right a)
    f _ = Nothing

-- * sublists

-- | Partition a list of Eithers into a pair.  Lazy enough to handle an
-- infinite input list.
partition_either :: [Either a b] -> ([a], [b])
partition_either [] = ([], [])
partition_either (x:xs) =
    let (ls, rs) = partition_either xs
    in case x of
        Left l -> (l:ls, rs)
        Right r -> (ls, r:rs)

-- | Take a list of rows to a list of columns.  Similar to zip, the result is
-- trimmed to the length of the shortest row.
rotate :: [[a]] -> [[a]]
rotate xs = maybe [] (: rotate (map List.tail xs)) (mapM head xs)

-- | Similar to 'rotate', except that the result is the length of the longest
-- row and missing columns is Nothing.
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

-- ** splitting and joining

break_tails :: ([a] -> Bool) -> [a] -> ([a], [a])
break_tails _ [] = ([], [])
break_tails f lst@(x:xs)
    | f lst = ([], lst)
    | otherwise = let (pre, post) = break_tails f xs in (x:pre, post)

-- | List initial and final element, if any.
viewr :: [a] -> ([a], Maybe a)
viewr [] = ([], Nothing)
viewr [x] = ([], Just x)
viewr (x:xs) = let (first, last) = viewr xs in (x:first, last)

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
split :: (Eq a) => [a] -> [a] -> [[a]]
split [] _ = error "Util.Seq.split: empty separator"
split sep xs = go sep xs
    where
    go sep xs
        | null post = [pre]
        | otherwise = pre : split sep (drop (length sep) post)
        where (pre, post) = break_tails (sep `List.isPrefixOf`) xs

-- | 'split' never returns nil, so sometimes it's more convenient to express
-- that in the type.
split_t :: (Eq a) => [a] -> [a] -> ([a], [[a]])
split_t sep xs = case split sep xs of
    (g:gs) -> (g, gs)
    _ -> error "split_t: unreached"

-- | Like 'split', but only split once.
split1 :: (Eq a) => [a] -> [a] -> ([a], [a])
split1 [] _ = error "Util.Seq.split1: empty seperator"
split1 sep xs = (pre, drop (length sep) post)
    where (pre, post) = break_tails (sep `List.isPrefixOf`) xs

-- | Concat a list with 'sep' in between.
join :: [a] -> [[a]] -> [a]
join sep = concat . List.intersperse sep

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
replace1 from to xs = concatMap (\v -> if v == from then to else [v]) xs


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
