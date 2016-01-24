module Seq where
import qualified Data.List as List


minimumOn :: Ord k => (a -> k) -> [a] -> Maybe a
minimumOn _ [] = Nothing
minimumOn key xs = Just (List.foldl1' f xs)
    where f low x = if key x < key low then x else low
