-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | 'UntilFail' list.
module Util.UF where
import Prelude hiding (map, concatMap)
import Control.Arrow (first)
import Util.Pretty (Pretty)
import qualified Util.Pretty as Pretty


-- | This is a list, optionally terminated with an error.
data UntilFail err a = a :+ UntilFail err a | Done | Fail err
    deriving (Eq, Show, Functor)
infixr :+

instance (Pretty err, Pretty a) => Pretty (UntilFail err a) where
    format = Pretty.delimitedList False '[' ']' . go
        where
        go (x :+ xs) = Pretty.format x : go xs
        go Done = []
        go (Fail err) = [Pretty.format err]

fromListFail :: [a] -> err -> UntilFail err a
fromListFail as err = foldr (:+) (Fail err) as

fromList :: [a] -> UntilFail err a
fromList = foldr (:+) Done

singleton :: a -> UntilFail err a
singleton x = x :+ Done

toList :: UntilFail err a -> ([a], Maybe err)
toList (x :+ xs) = first (x:) (toList xs)
toList Done = ([], Nothing)
toList (Fail err) = ([], Just err)

map :: (a -> Either err b) -> UntilFail err a -> UntilFail err b
map f = go
    where
    go Done = Done
    go (Fail err) = Fail err
    go (a :+ as) = case f a of
        Left err -> Fail err
        Right b -> b :+ go as

concatMap :: (a -> UntilFail err b) -> UntilFail err a -> UntilFail err b
concatMap f = go
    where
    go Done = Done
    go (Fail err) = Fail err
    go (a :+ as) = append as (f a)
    append _ (Fail err) = Fail err
    append as Done = go as
    append as (b :+ bs) = b :+ append as bs

-- | Like 'concatMap', but consume and produce a variable number of
-- results.
--
-- A more precise type would end with @Done [a]@.
process :: (a -> [a] -> (UntilFail err b, [a])) -> [a] -> UntilFail err b
process f = go
    where
    go (a : as) = let (bs, remain) = f a as in append remain bs
    go [] = Done
    append as (b :+ bs) = b :+ append as bs
    append as Done = go as
    append _ (Fail err) = Fail err
