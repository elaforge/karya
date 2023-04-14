-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
-- | Re-export a minimal version of the fclabels API.
--
-- I'd like to be able to write @(a #= x) . (b #= y)@ without parentheses,
-- but since (.) already binds the strongest at 9, I can't make (#=) stronger.
-- Besides, I already want (#=) to bind loosely so I can write @x#y #= 1+2@.
-- I would need a version of (.) at 0, but that's too much trouble.
module Util.Lens (
    Lens, lens
    , (#)
    -- * operators
    , (#$), (#=), (%=)
    , (<#>)

    -- * data
    , map, set, list
) where
import           Prelude hiding ((.), map)
import           Control.Category ((.))
import qualified Data.Label as Label
import           Data.Label (get, lens, modify, (:->))
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Lists as Lists


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
(#=) = Label.set
infix 1 #=

-- | Modify: @a#b %= (+1) record@
(%=) :: Lens f a -> (a -> a) -> f -> f
(%=) = modify
infix 1 %=

-- | Use like @a#b <#> State.get@.
(<#>) :: Functor f => Lens a b -> f a -> f b
(<#>) = fmap . get
infixl 4 <#> -- same as <$>

-- * data

map :: Ord k => k -> Lens (Map.Map k a) (Maybe a)
map k = lens (Map.lookup k) (\modify -> Map.alter modify k)

set :: Ord k => k -> Lens (Set.Set k) Bool
set k = lens (Set.member k) $ \modify s ->
    if modify (Set.member k s) then Set.insert k s else Set.delete k s

-- | A negative index counts from the end of the list, and a too-high index
-- will be ignored.
list :: Int -> Lens [a] (Maybe a)
list i
    | i < 0 = lens (\xs -> Lists.head (drop (length xs + i) xs))
        (\modify xs -> at (length xs + i) modify xs)
    | otherwise = lens (Lists.head . drop i) (at i)
    where
    at i modify xs = case modify (Lists.at xs i) of
        Nothing -> Lists.removeAt i xs
        Just x -> Lists.modifyAt i (const x) xs
