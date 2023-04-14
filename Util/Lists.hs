-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to operate on lists.
--
-- TODO port over Util.Seq
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
    -- ** span and break
    , spanWhile
    -- * replace
    , replace, replace1
    -- * search
    , count
) where
import           Prelude hiding (head, last, tail)
import           Data.Bifunctor (first)
import qualified Data.List as List


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

-- ** span and break

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
