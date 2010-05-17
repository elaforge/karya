-- | Some more utilities for "Data.Tree".
module Util.Tree where
import Control.Monad
import qualified Data.List as List
import Data.Tree


-- | The edges of a forest, as (parent, child).
edges :: Forest a -> [(a, a)]
edges = concatMap $ \(Node val subs) ->
    [(val, sub_val) | Node sub_val _ <- subs] ++ edges subs

-- | Return every element along with its parents and children.  Parents are
-- returned closest first, children are in depth first search order.
paths :: Forest a -> [(a, [a], [a])]
paths trees = concatMap (go []) trees
    where
    go parents (Node val subs) =
        (val, parents, children) : concatMap (go (val:parents)) subs
        where children = concatMap flatten subs

-- | Given a predicate, return the first depthwise matching element and
-- its children.
find :: (a -> Bool) -> Forest a -> Maybe (Tree a)
find p trees = case List.find (p . rootLabel) trees of
    Nothing -> msum $ map (find p . subForest) trees
    Just tree -> Just tree
