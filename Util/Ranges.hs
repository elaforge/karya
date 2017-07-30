-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ranges are half-open.
module Util.Ranges (
    Ranges, fmap, extract
    , ranges, sorted_ranges, merge_sorted, range, point, everything, nothing
    , overlapping, overlapping_closed, intersection, invert
) where
import Prelude hiding (fmap)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Monoid as Monoid

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq


data Ranges n = Ranges [(n, n)] | Everything
    deriving (Eq, Show)

instance Ord n => Monoid.Monoid (Ranges n) where
    mempty = Ranges []
    mappend Everything _ = Everything
    mappend _ Everything = Everything
    mappend (Ranges r1) (Ranges r2) = Ranges (merge r1 r2)

instance Pretty.Pretty n => Pretty.Pretty (Ranges n) where
    format (Ranges rs) = Pretty.textList (map f rs)
        where f (s, e) = Pretty.pretty s <> "--" <> Pretty.pretty e
    format Everything = Pretty.text "[*--*]"

instance DeepSeq.NFData n => DeepSeq.NFData (Ranges n) where
    rnf Everything = ()
    rnf (Ranges xs) = DeepSeq.rnf xs

-- | It has a different type from the real fmap, but it wants to be an fmap.
fmap :: Ord b => ((a, a) -> (b, b)) -> Ranges a -> Ranges b
fmap f r = case extract r of
    Nothing -> everything
    Just pairs -> sorted_ranges (map f pairs)

-- | Nothing means an everything range.
extract :: Ranges n -> Maybe [(n, n)]
extract (Ranges pairs) = Just pairs
extract Everything = Nothing

-- * constructors

-- | This doesn't ensure that, given @(s, e)@, s <= e.
ranges :: Ord n => [(n, n)] -> Ranges n
ranges = sorted_ranges . List.sort

sorted_ranges :: Ord n => [(n, n)] -> Ranges n
sorted_ranges = Ranges . merge_sorted

merge_sorted :: Ord n => [(n, n)] -> [(n, n)]
merge_sorted [] = []
merge_sorted [x] = [x]
merge_sorted ((s1, e1) : (s2, e2) : rest)
    | e1 >= e2 = merge_sorted ((s1, e1) : rest)
    | e1 >= s2 = merge_sorted ((s1, e2) : rest)
    | otherwise = (s1, e1) : merge_sorted ((s2, e2) : rest)

range :: n -> n -> Ranges n
range s e = Ranges [(s, e)]

point :: n -> Ranges n
point p = range p p

everything :: Ranges n
everything = Everything

nothing :: Ranges n
nothing = Ranges []

-- * functions

overlapping :: Ord n => Ranges n -> Ranges n -> Bool
overlapping = check_overlapping False

-- | This is like 'overlapping', except the ranges are closed instead of
-- half-open.
overlapping_closed :: Ord n => Ranges n -> Ranges n -> Bool
overlapping_closed = check_overlapping True

check_overlapping :: Ord n => Bool -> Ranges n -> Ranges n -> Bool
check_overlapping _ Everything r2 = r2 /= nothing
check_overlapping _ r1 Everything = r1 /= nothing
check_overlapping closed (Ranges r1) (Ranges r2) = go r1 r2
    where
    go [] _ = False
    go _ [] = False
    go r1@((s1, e1) : rest1) r2@((s2, e2) : rest2)
        -- It's important that zero width ranges can still overlap, otherwise
        -- zero width track damage won't invalidate any caches.
        | s1 == s2 = True
        | e1 `lt` s2 = go rest1 r2
        | e2 `lt` s1 = go r1 rest2
        | otherwise = True
        where lt = if closed then (<) else (<=)

intersection :: Ord n => Ranges n -> Ranges n -> Ranges n
intersection Everything r2 = r2
intersection r1 Everything = r1
intersection (Ranges r1) (Ranges r2) = Ranges (go r1 r2)
    where
    go [] _ = []
    go _ [] = []
    go r1@((s1, e1) : rest1) r2@((s2, e2) : rest2)
        | s1 == s2 = (s1, min e1 e2) : rest
        | e1 <= s2 = go rest1 r2
        | e2 <= s1 = go r1 rest2
        | otherwise = (max s1 s2, min e1 e2) : rest
        where rest = if e1 < e2 then go rest1 r2 else go r1 rest2

merge :: Ord n => [(n, n)] -> [(n, n)] -> [(n, n)]
merge r1 r2 = merge_sorted (Seq.merge_on fst r1 r2)

-- | Given a complete range, invert the ranges.
invert :: Ord n => (n, n) -> Ranges n -> Ranges n
invert _ Everything = Monoid.mempty
invert (start, end) (Ranges pairs) = Ranges $ go start pairs
    where
    go p ((s, e) : rs)
        | s > p = (p, s) : go e rs
        | otherwise = go e rs
    go p [] = if p < end then [(p, end)] else []
