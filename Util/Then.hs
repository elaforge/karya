-- | List functions with continuations.  This allows you to chain them and
-- easily express things like 'take until f then take one more'.
module Util.Then where
import Prelude hiding (take, takeWhile)
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

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> (acc -> [y]) -> [x] -> [y]
mapAccumL f acc cont = go acc
    where
    go acc [] = cont acc
    go acc (x:xs) = y : go acc2 xs
        where (acc2, y) = f acc x
