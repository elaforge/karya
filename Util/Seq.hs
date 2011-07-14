module Util.Seq where
import Prelude hiding (head, last)
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Function
import qualified Data.List as List
import qualified Data.List.Ordered as Ordered


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
        | val > end = []
        | otherwise = val : go (i+1)
        where val = start + (i*step)

-- | Like 'range', but includes the end.
rangeEnd :: (Num a, Ord a) => a -> a -> a -> [a]
rangeEnd start end step = go 0
    where
    go i
        | val >= end = [end]
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
insert_at :: [a] -> Int -> a -> [a]
insert_at xs i x = let (pre, post) = splitAt i xs in pre ++ (x : post)

-- | Remove the element at the given index.  Do nothing if the index is out
-- of range.
remove_at :: [a] -> Int -> [a]
remove_at xs i = let (pre, post) = splitAt i xs in pre ++ drop 1 post

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

-- * min max

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

sort_on :: (Ord b) => (a -> b) -> [a] -> [a]
sort_on = Ordered.sortOn'

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

-- | Handy to merge or sort a descending list.
reverse_compare :: (Ord a) => a -> a -> Ordering
reverse_compare a b = case compare a b of
    LT -> GT
    EQ -> EQ
    GT -> LT

-- * grouping

-- | Group the unsorted list into @(key x, xs)@ where all @xs@ compare equal
-- after @key@ is applied to them.  List is returned in sorted order.
keyed_group_on :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
keyed_group_on key = map (\gs -> (key (List.head gs), gs))
    . group_on key . sort_on key

-- | Like 'groupBy', but the list doesn't need to be sorted, and use a key
-- function instead of equality.  List is returned in sorted order.
group_on :: (Ord b) => (a -> b) -> [a] -> [[a]]
group_on key = List.groupBy ((==) `on` key) . sort_on key

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

-- | Like 'zip', but the shorter list is padded with Nothings.
padded_zip :: [a] -> [b] -> [(Maybe a, Maybe b)]
padded_zip [] [] = []
padded_zip [] bs = zip (repeat Nothing) (map Just bs)
padded_zip as [] = zip (map Just as) (repeat Nothing)
padded_zip (a:as) (b:bs) = (Just a, Just b) : padded_zip as bs

-- | Return the reversed inits paired with the tails.  This is like a zipper
-- moving focus along the input list.
zipper :: [a] -> [a] -> [([a], [a])]
zipper prev [] = [(prev, [])]
zipper prev lst@(x:xs) = (prev, lst) : zipper (x:prev) xs

{-
pairs :: [a] -> [(a, a)]
pairs (x0 : x1 : xs) = (x0, x1) : pairs xs
pairs _ = []

unpairs :: [(a, a)] -> [a]
unpairs ((x0, x1) : xs) = x0 : x1 : unpairs xs
unpairs [] = []

partition2 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
partition2 _ _ [] = ([], [], [])
partition2 f g (x:xs)
    | f x = (x : fs, gs, rest)
    | g x = (fs, x : gs, rest)
    | otherwise = (fs, gs, x : rest)
    where (fs, gs, rest) = partition2 f g xs
-}

-- | Pair @a@ elements up with @b@ elements.  If they are equal according to
-- @eq@, they'll both be Just in the result.  If an @a@ is deleted going from
-- @a@ to @b@, it will be Nothing, and vice versa for @b@.  You should never
-- get @(Nothing, Nothing)@.
--
-- Kind of like an edit distance, or a diff.
equal_pairs :: (a -> b -> Bool) -> [a] -> [b] -> [(Maybe a, Maybe b)]
equal_pairs _ [] ys = [(Nothing, Just y) | y <- ys]
equal_pairs _ xs [] = [(Just x, Nothing) | x <- xs]
equal_pairs eq (x:xs) (y:ys)
    | x `eq` y = (Just x, Just y) : equal_pairs eq xs ys
    | any (eq x) ys = (Nothing, Just y) : equal_pairs eq (x:xs) ys
    | otherwise = (Just x, Nothing) : equal_pairs eq xs (y:ys)

-- | This is like 'equal_pairs', except that the index of each pair in the
-- /right/ list is included.  In other words, given @(i, Nothing, Just y)@,
-- @i@ is the position of @y@ in the @b@ list.  Given @(i, Just x, Nothing)@,
-- @i@ is where @x@ was deleted from the @b@ list.
indexed_pairs :: (a -> b -> Bool) -> [a] -> [b] -> [(Int, Maybe a, Maybe b)]
indexed_pairs eq xs ys = zip3 (indexed pairs) (map fst pairs) (map snd pairs)
    where
    pairs = equal_pairs eq xs ys
    indexed pairs = scanl f 0 pairs
        where
        f i (_, Nothing) = i
        f i _ = i+1

indexed_pairs_on :: (Eq eq) => (a -> eq) -> [a] -> [a]
    -> [(Int, Maybe a, Maybe a)]
indexed_pairs_on key xs ys = indexed_pairs (\a b -> key a == key b) xs ys

-- | Left if the val was in the left list but not the right, Right for the
-- converse.
diff :: (a -> b -> Bool) -> [a] -> [b] -> [Either a b]
diff eq xs ys = Maybe.mapMaybe f (equal_pairs eq xs ys)
    where
    f (Just a, Nothing) = Just (Left a)
    f (Nothing, Just a) = Just (Right a)
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

-- ** extracting sublists

-- | Total variants of unsafe list operations.
head, last :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x
last [] = Nothing
last xs = Just (List.last xs)

-- | Drop adjacent elts if they are equal after applying the key function.  The
-- first elt is kept.
drop_dups :: (Eq k) => (a -> k) -> [a] -> [a]
drop_dups _ [] = []
drop_dups key (x:xs) = x : map snd (filter (not . equal) (zip (x:xs) xs))
    where equal (x, y) = key x == key y

-- | Like 'drop_dups', but keep the last adjacent equal elt instead of the
-- first.
drop_initial_dups :: (Eq k) => (a -> k) -> [a] -> [a]
drop_initial_dups _ [] = []
drop_initial_dups _ [x] = [x]
drop_initial_dups key (x:xs@(next:_))
    | key x == key next = rest
    | otherwise = x:rest
    where rest = drop_initial_dups key xs

-- | A specialized drop where the predicate can return Nothing for "don't
-- know".  Unknowns will not be dropped if the first known after them is not
-- dropped (f x == Just False).  If there are only unknowns, they are all kept.
drop_unknown :: (a -> Maybe Bool) -> [a] -> [a]
drop_unknown f xs = case rest of
    [] -> xs
    y : ys
        | f y == Just False -> xs
        | otherwise -> drop_unknown f ys
    where rest = dropWhile ((==Nothing) . f) xs

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

lstrip = dropWhile Char.isSpace
rstrip = rdrop_while Char.isSpace
strip = rstrip . lstrip

-- ** splitting and joining

break_tails :: ([a] -> Bool) -> [a] -> ([a], [a])
break_tails _ [] = ([], [])
break_tails f lst@(x:xs)
    | f lst = ([], lst)
    | otherwise = let (pre, post) = break_tails f xs in (x:pre, post)

break_last :: [a] -> ([a], Maybe a)
break_last [] = ([], Nothing)
break_last [x] = ([], Just x)
break_last (x:xs) = let (first, last) = break_last xs in (x:first, last)

break_then :: (a -> Bool) -> ([a] -> ([a], rest)) -> [a] -> ([a], rest)
break_then f cont (x:xs)
    | f x = let (pre, post) = cont (x:xs) in (pre, post)
    | otherwise = let (pre, post) = break_then f cont xs in (x:pre, post)
break_then _ cont [] = cont []

-- | Break right after the function returns True.
break1 :: (a -> Bool) -> [a] -> ([a], [a])
break1 f = break_then f (splitAt 1)

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

-- | Split on commas and strip whitespace.
split_commas :: String -> [String]
split_commas = map strip . split ","

-- | Concat a list with 'sep' in between.
join :: [a] -> [[a]] -> [a]
join sep = concat . List.intersperse sep

replace1 :: (Eq a) => a -> [a] -> [a] -> [a]
replace1 from to xs = concatMap (\v -> if v == from then to else [v]) xs

-- | Replace sublists in 'xs'.  'repl' is given the tails of 'xs' and can
-- return (replacement, rest_of_xs) or Nothing.
replaceWith :: ([a] -> Maybe ([a], [a])) -> [a] -> [a]
replaceWith _ [] = []
replaceWith repl xs = case repl xs of
    Just (insert, rest) -> insert ++ replaceWith repl rest
    Nothing -> List.head xs : replaceWith repl (tail xs)

-- | Replace sublist 'val' with 'repl' in the given list.
replace val repl = replaceWith (replaceVal val repl)

-- | Helper for replaceWith to replace a constant sublist 'val' with 'repl'.
replaceVal val repl xs
    | val `List.isPrefixOf` xs = Just (repl, drop (length val) xs)
    | otherwise = Nothing

split_between :: (a -> a -> Bool) -> [a] -> [[a]]
split_between _ [] = []
split_between f xs = pre : split_between f post
    where (pre, post) = break_between f xs

break_between :: (a -> a -> Bool) -> [a] -> ([a], [a])
break_between f (x1 : xs@(x2:_))
    | f x1 x2 = ([x1], xs)
    | otherwise = let (pre, post) = break_between f xs in (x1 : pre, post)
break_between _ xs = (xs, [])
