-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Also known as a conc-list or a merge-list.  In the ghc source it's called
-- OrdList.
module Util.AppendList (
    AppendList, empty, singleton, cons, snoc, append
    , from_list, to_list
    , length, view, head, last
) where
import Prelude hiding (length, head, last)
import Control.Monad
import qualified Data.List as List
import qualified Data.Monoid as Monoid


data AppendList a =
    Nil | Single a | Pair (AppendList a) (AppendList a) | Many [a]
    deriving (Eq, Show)

instance Functor AppendList where
    fmap _ Nil = Nil
    fmap f (Single x) = Single (f x)
    fmap f (Pair xs ys) = Pair (fmap f xs) (fmap f ys)
    fmap f (Many xs) = Many (fmap f xs)

instance Semigroup (AppendList a) where
    (<>) = append

instance Monoid (AppendList a) where
    mempty = empty

empty :: AppendList a
empty = Nil

singleton :: a -> AppendList a
singleton = Single

cons :: a -> AppendList a -> AppendList a
cons x Nil = Single x
cons x xs = Pair (Single x) xs

snoc :: AppendList a -> a -> AppendList a
snoc Nil x = Single x
snoc xs x = Pair xs (Single x)

append :: AppendList a -> AppendList a -> AppendList a
append Nil ys = ys
append xs Nil = xs
append xs ys = Pair xs ys

from_list :: [a] -> AppendList a
from_list [] = Nil
from_list [a] = Single a
from_list xs@(_:_) = Many xs

to_list :: AppendList a -> [a]
to_list alist = go alist []
    where
    go Nil xs = xs
    go (Single x) xs = x : xs
    go (Pair xs1 xs2) ys = go xs1 (go xs2 ys)
    go (Many xs) ys = xs ++ ys

length :: AppendList a -> Int
length Nil = 0
length (Single _) = 1
length (Pair xs ys) = length xs + length ys
-- possibly more efficient if the list is built with 'snoc'?
-- length (Pair xs ys) = let leny = length ys in leny `seq` leny + length xs
length (Many xs) = List.length xs

view :: AppendList a -> Maybe (a, AppendList a)
view Nil = Nothing
view (Single a) = Just (a, Nil)
view (Pair xs ys) = case view xs of
    Nothing -> view ys
    Just (v, Nil) -> Just (v, ys)
    Just (v, Many []) -> Just (v, ys)
    Just (v, rest) -> Just (v, Pair rest ys)
view (Many []) = Nothing
view (Many (x:xs)) = Just (x, Many xs)

head :: AppendList a -> Maybe a
head Nil = Nothing
head (Single a) = Just a
head (Pair xs ys) = head xs `mplus` head ys
head (Many []) = Nothing
head (Many (x:_)) = Just x

last :: AppendList a -> Maybe a
last Nil = Nothing
last (Single a) = Just a
last (Pair xs ys) = last ys `mplus` last xs
last (Many []) = Nothing
last (Many xs) = Just (List.last xs)
