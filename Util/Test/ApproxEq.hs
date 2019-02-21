-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | ApproxEq class for comparing floating point numbers.
module Util.Test.ApproxEq (ApproxEq(eq), neq, compare) where
import Prelude hiding (compare)
import qualified Data.Text as Text

import qualified Util.Num as Num


compare :: (ApproxEq a, Ord a) => Double -> a -> a -> Ordering
compare eta a b
    | eq eta a b = EQ
    | a > b = GT
    | otherwise = LT

class ApproxEq a where
    eq :: Double -> a -> a -> Bool

neq :: ApproxEq a => Double -> a -> a -> Bool
neq eta x y = not $ eq eta x y

instance ApproxEq Float where
    eq eta x y = abs (x - y) <= Num.d2f eta
instance ApproxEq Double where
    eq eta x y = abs (x - y) <= eta

-- * prelude types

instance ApproxEq Char where eq _ = (==)
instance ApproxEq Int where eq _ = (==)
instance ApproxEq Integer where eq _ = (==)
instance ApproxEq Bool where eq _ = (==)

instance ApproxEq a => ApproxEq [a] where
    eq eta = go
        where
        go [] [] = True
        go [] _ = False
        go _ [] = False
        go (x:xs) (y:ys) = eq eta x y && go xs ys

instance ApproxEq a => ApproxEq (Maybe a) where
    eq eta (Just x) (Just y) = eq eta x y
    eq _ _ _ = False

instance (ApproxEq a, ApproxEq b) => ApproxEq (Either a b) where
    eq eta (Right x) (Right y) = eq eta x y
    eq eta (Left x) (Left y) = eq eta x y
    eq _ _ _ = False

instance (ApproxEq a, ApproxEq b) => ApproxEq (a, b) where
    eq eta (a, b) (a', b') = eq eta a a' && eq eta b b'
instance (ApproxEq a, ApproxEq b, ApproxEq c) => ApproxEq (a, b, c) where
    eq eta (a, b, c) (a', b', c') = eq eta a a' && eq eta b b' && eq eta c c'
instance (ApproxEq a, ApproxEq b, ApproxEq c, ApproxEq d) =>
        ApproxEq (a, b, c, d) where
    eq eta (a, b, c, d) (a', b', c', d') =
        eq eta a a' && eq eta b b' && eq eta c c' && eq eta d d'

-- * hackage types

instance ApproxEq Text.Text where eq _ = (==)
