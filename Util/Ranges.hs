module Util.Ranges (
    Ranges, extract, ranges, sorted_ranges, range, point, everything, nothing
    , overlapping
) where
import qualified Data.List as List
import qualified Data.Monoid as Monoid

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq


data Ranges n = Ranges [(n, n)] | Everything
    deriving (Eq, Show)

-- | Nothing means an everything range.
extract :: Ranges n -> Maybe [(n, n)]
extract (Ranges pairs) = Just pairs
extract Everything = Nothing

-- * constructors

-- | This doesn't ensure that, given @(s, e)@, s <= e.
ranges :: (Ord n) => [(n, n)] -> Ranges n
ranges = sorted_ranges . List.sort

sorted_ranges :: (Ord n) => [(n, n)] -> Ranges n
sorted_ranges = Ranges . merge_sorted_pairs

merge_sorted_pairs :: (Ord n) => [(n, n)] -> [(n, n)]
merge_sorted_pairs [] = []
merge_sorted_pairs [x] = [x]
merge_sorted_pairs ((s1, e1) : (s2, e2) : rest)
    | e1 >= e2 = merge_sorted_pairs ((s1, e1) : rest)
    | e1 >= s2 = merge_sorted_pairs ((s1, e2) : rest)
    | otherwise = (s1, e1) : merge_sorted_pairs ((s2, e2) : rest)

range :: (Ord n) => n -> n -> Ranges n
range s e = Ranges [(s, e)]

point :: (Ord n) => n -> Ranges n
point p = range p p

everything :: Ranges n
everything = Everything

nothing :: Ranges n
nothing = Ranges []

-- * functions

overlapping :: (Ord n) => Ranges n -> Ranges n -> Bool
overlapping Everything _ = True
overlapping _ Everything = True
overlapping (Ranges r1) (Ranges r2) = go r1 r2
    where
    go [] _ = False
    go _ [] = False
    go r1@((s1, e1) : rest1) r2@((s2, e2) : rest2)
        -- It's important that zero width ranges can still overlap, overwise
        -- zero width track damage won't invalidate any caches.
        | s1 == s2 = True
        | e1 <= s2 = go rest1 r2
        | e2 <= s1 = go r1 rest2
        | otherwise = True

merge :: (Ord n) => [(n, n)] -> [(n, n)] -> [(n, n)]
merge [] r2 = r2
merge r1 [] = r1
merge r1@((s1, e1) : rest1) r2@((s2, e2) : rest2)
    | e1 < s2 = (s1, e1) : merge rest1 r2
    | e2 < s1 = (s2, e2) : merge r1 rest2
    | s1 >= s2 && e1 <= e2 = merge rest1 r2
    | s2 >= s1 && e2 <= e1 = merge r1 rest2
    | otherwise = (min s1 s2, max e1 e2) : merge rest1 rest2

instance (Ord n) => Monoid.Monoid (Ranges n) where
    mempty = Ranges []
    mappend Everything _ = Everything
    mappend _ Everything = Everything
    mappend (Ranges r1) (Ranges r2) = Ranges (merge r1 r2)

instance (Pretty.Pretty n) => Pretty.Pretty (Ranges n) where
    pretty (Ranges rs) = "[" ++ Seq.join ", " (map f rs) ++ "]"
        where f (s, e) = Pretty.pretty s ++ "--" ++ Pretty.pretty e
    pretty Everything = "[*--*]"
