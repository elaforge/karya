-- | Some more utilities for "Data.Tree".
module Util.Tree where
import Control.Monad
import qualified Data.List as List
import qualified Data.Tree as Tree
import Data.Tree (Tree(..), Forest)


-- | The edges of a forest, as (parent, child).
edges :: Forest a -> [(a, a)]
edges = concatMap $ \(Node val subs) ->
    [(val, sub_val) | Node sub_val _ <- subs] ++ edges subs

-- | Get every element along with its parents and children.
paths :: Forest a -> [(a, [a], [a])]
    -- ^ (element, parents, children).  Parents are closest first up to the
    -- root, children are in depth first order.
paths trees = concatMap (go []) trees
    where
    go parents (Node val subs) =
        (val, parents, children) : concatMap (go (val:parents)) subs
        where children = concatMap Tree.flatten subs

-- | Given a predicate, return the first depthwise matching element and
-- its children.
find :: (a -> Bool) -> Forest a -> Maybe (Tree a)
find p trees = case List.find (p . rootLabel) trees of
    Nothing -> msum $ map (find p . subForest) trees
    Just tree -> Just tree

-- | Find the first matching depthwise matching element and the path to reach
-- it.  The parents list is ordered immediate to distant.
find_with_parents :: (a -> Bool) -> Forest a -> Maybe (Tree a, [Tree a])
find_with_parents f trees = msum (map (go []) trees)
    where
    go parents tree@(Node val subs)
        | f val = Just (tree, parents)
        | otherwise = msum (map (go (tree : parents)) subs)

-- | Find the first leaf, always taking the leftmost branch.
first_leaf :: Tree a -> a
first_leaf (Node a []) = a
first_leaf (Node _ (child : _)) = first_leaf child

leaves :: Forest a -> [Tree a]
leaves trees = do
    tree <- trees
    if null (subForest tree) then [tree] else leaves (subForest tree)
