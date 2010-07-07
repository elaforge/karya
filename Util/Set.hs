-- | Extra utils for "Data.Set".
module Util.Set where
import qualified Data.Set as Set


-- | Find a key at or below the given one.
lookup_below :: (Ord k) => k -> Set.Set k -> Maybe k
lookup_below k set = case Set.splitMember k set of
    (_, True, _) -> Just k
    (below, False, _) -> find_max below

-- | Safe versions of findMin and findMax.
find_min :: Set.Set k -> Maybe k
find_min set
    | Set.null set = Nothing
    | otherwise = Just (Set.findMin set)

find_max :: Set.Set k -> Maybe k
find_max set
    | Set.null set = Nothing
    | otherwise = Just (Set.findMax set)
