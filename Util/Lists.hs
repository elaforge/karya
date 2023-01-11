-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to operate on lists.
--
-- TODO port over Util.Seq
module Util.Lists (
    head, tail
    , last
    , unsnoc
    -- * split / join
    , splitWith
    , breakWith
    -- * transform
    , mapAccumLM
) where
import           Prelude hiding (head, last, tail)
import           Data.Bifunctor (first)
import qualified Data.List as List


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

-- * sublists

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
