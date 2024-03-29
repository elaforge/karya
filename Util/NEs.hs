-- | Functions for NonEmpty lists.
module Util.NEs where
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Maybe as Maybe

import qualified Util.Lists as Lists


unsnoc :: NonEmpty a -> ([a], a)
unsnoc (x :| xs) =
    Maybe.fromMaybe (error "unsnoc: not reached") (Lists.unsnoc (x : xs))

minimum :: Ord a => NonEmpty a -> a
minimum (x :| xs) = List.minimum (x : xs)

maximum :: Ord a => NonEmpty a -> a
maximum (x :| xs) = List.maximum (x : xs)
