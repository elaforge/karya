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

    -- * derive
    , derive_with, derive, derive_except, derive_avoid, mklabel
) where
import Prelude hiding ((.), map)
import Control.Category ((.))
import Data.Label hiding (Lens, set)
import qualified Data.Label.Maybe as Maybe
import qualified Data.Label.Pure as Pure
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Language.Haskell.TH.Syntax as Syntax

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

-- | Set: @a#b =# 42 record@
(#=) :: Lens f a -> a -> f -> f
(#=) = Pure.set
infix 1 #=

-- | Modify: @a#b =% (+1) record@
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


-- * derivation

type Deriver = [Syntax.Name] -> Syntax.Q [Syntax.Dec]

derive_with :: (String -> String) -> Deriver
derive_with = mkLabelsWith

derive :: Deriver
derive = derive_with mklabel

derive_except :: [String] -> Deriver
derive_except avoid = mkLabelsWith $ \name ->
    if name `elem` avoid then name ++ "_" else mklabel name

derive_avoid :: [String] -> Deriver
derive_avoid avoid = derive_with $ \name ->
    let label = mklabel name
    in if label `elem` avoid then label ++ "_" else label

mklabel :: String -> String
mklabel name
    | label `elem` keywords = label ++ "_"
    | otherwise = label
    where
    label = drop 1 $ dropWhile (/='_') name
    keywords =
        ["module", "infix", "infixl", "infixr"
        , "class", "data", "deriving", "instance", "default", "where"
        , "type", "newtype" , "do", "case", "of", "let", "in"
        , "if", "then", "else"
        ]
