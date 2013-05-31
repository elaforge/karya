-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
module Util.Lens (
    Lens, lens
    , (#)
    -- * operators
    , (#$), (#=), (%=)
    , (<#>)

    -- * data
    , map, set, list
    , mapf
) where
import Prelude hiding ((.), map)
import Control.Category ((.))
import Data.Label hiding (Lens, set)
import qualified Data.Label.Maybe as Maybe
import qualified Data.Label.Pure as Pure
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Seq as Seq


-- * lens operators

type Lens a b = a :-> b

-- | Compose lenses.
(#) :: Lens a b -> Lens b c -> Lens a c
(#) = flip (.)
infixr 9 #

-- | Get: @bval = a#b $# record@
(#$) :: Lens f a -> f -> a
(#$) = get
infixr 1 #$

-- | Set: @a#b #= 42 record@
(#=) :: Lens f a -> a -> f -> f
(#=) = Pure.set
infix 1 #=

-- | Modify: @a#b %= (+1) record@
(%=) :: Lens f a -> (a -> a) -> f -> f
(%=) = modify
infix 1 %=

-- | Use like @a#b <#> State.get@.
(<#>) :: (Functor f) => Lens a b -> f a -> f b
(<#>) = fmap . get
infixl 4 <#> -- same as <$>

-- * data

map :: (Ord k) => k -> Lens (Map.Map k a) (Maybe a)
map k = lens (Map.lookup k) (maybe (Map.delete k) (Map.insert k))

set :: (Ord k) => k -> Lens (Set.Set k) Bool
set k = lens (Set.member k) (\a -> if a then Set.insert k else Set.delete k)

-- | A negative index counts from the end of the list, and a too-high index
-- will be ignored.
list :: Int -> Lens [a] (Maybe a)
list i
    | i < 0 = lens (\xs -> Seq.head (drop (length xs + i) xs))
        (\v xs -> modify (length xs + i) v xs)
    | otherwise = lens (Seq.head . drop i) (modify i)
    where
    modify i Nothing = Seq.remove_at i
    modify i (Just a) = Seq.modify_at i (const a)

mapf :: (Ord k) => k -> Map.Map k a Maybe.:~> a
mapf k = Maybe.lens (Map.lookup k) (\a -> Just . Map.insert k a)
