-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | List functions with continuations.  This allows you to chain them and
-- easily express things like 'take until f then take one more'.
module Util.Then where
import Prelude hiding (break, span, take, takeWhile, map, mapM)
import qualified Data.List as List


takeWhile :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
takeWhile _ cont [] = cont []
takeWhile f cont (x:xs)
    | f x = x : takeWhile f cont xs
    | otherwise = cont (x:xs)

-- | takeWhile plus one extra
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f = takeWhile f (List.take 1)

take :: Int -> ([a] -> [a]) -> [a] -> [a]
take _ cont [] = cont []
take n cont (x:xs)
    | n <= 0 = cont (x:xs)
    | otherwise = x : take (n-1) cont xs

filter :: (a -> Bool) -> (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
filter f done cont = go
    where
    go [] = []
    go (x:xs)
        | done x = cont (x:xs)
        | f x = x : go xs
        | otherwise = go xs

map :: (a -> b) -> [b] -> [a] -> [b]
map f bs = go
    where
    go [] = bs
    go (a:as) = f a : go as

-- | Like 'List.mapAccumL', except that you can do something with the final
-- state and append that to the list.
mapAccumL :: (acc -> x -> (acc, y)) -> acc -> (acc -> [y]) -> [x] -> [y]
mapAccumL f acc cont = go acc
    where
    go acc [] = cont acc
    go acc (x:xs) = y : go acc2 xs
        where (acc2, y) = f acc x

mapM :: Monad m => (a -> m b) -> m [b] -> [a] -> m [b]
mapM f cont = go
    where
    go [] = cont
    go (a:as) = do
        b <- f a
        (b:) <$> go as

break :: (a -> Bool) -> ([a] -> ([a], rest))
    -- ^ Given the list after the break, return (pre, post), where pre will
    -- be appended to the end of the first list.
    -> [a] -> ([a], rest)
break f cont (x:xs)
    | f x = cont (x:xs)
    | otherwise = let (pre, post) = break f cont xs in (x:pre, post)
break _ cont [] = cont []

span :: (a -> Bool) -> ([a] -> ([a], rest)) -> [a] -> ([a], rest)
span f = break (not . f)

-- | Break right after the function returns True.
break1 :: (a -> Bool) -> [a] -> ([a], [a])
break1 f = break f (splitAt 1)
