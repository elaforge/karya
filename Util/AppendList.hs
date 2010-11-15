-- | Also known as a conc-list or a merge-list.  In the ghc source it's called
-- OrdList.
module Util.AppendList where
import qualified Data.Monoid as Monoid

data AppendList a = Nil
    | Single a | Pair (AppendList a) (AppendList a) | Many [a]
    deriving (Eq, Show)

instance Functor AppendList where
    fmap _ Nil = Nil
    fmap f (Single x) = Single (f x)
    fmap f (Pair xs ys) = Pair (fmap f xs) (fmap f ys)
    fmap f (Many xs) = Many (fmap f xs)

instance Monoid.Monoid (AppendList a) where
    mempty = Nil
    mappend = append

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
