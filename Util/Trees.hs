-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Some more utilities for "Data.Tree".
module Util.Trees where
import Prelude hiding (filter)
import Control.Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Tree as Tree
import Data.Tree (Tree(..))


-- | The edges of a forest, as (parent, child).
edges :: [Tree a] -> [(a, a)]
edges = concatMap $ \(Node val subs) ->
    [(val, sub_val) | Node sub_val _ <- subs] ++ edges subs

-- | Get every element along with its parents.  The parents are closest first
-- root last.
paths :: [Tree a] -> [(Tree a, [Tree a])]
paths = concatMap (go [])
    where
    go parents tree@(Node _ subs) =
        (tree, parents) : concatMap (go (tree:parents)) subs

-- | Like 'paths' but the parents and children have been flattened.
flatPaths :: [Tree a] -> [(a, [a], [a])]
    -- ^ (element, parents, children).  Parents are closest first root last,
    -- children are in depth first order.
flatPaths = map flatten . paths
    where
    flatten (Node val subs, parents) =
        (val, map Tree.rootLabel parents, concatMap Tree.flatten subs)

-- | Given a predicate, return the first depthwise matching element and
-- its children.
find :: (a -> Bool) -> [Tree a] -> Maybe (Tree a)
find p trees = case List.find (p . rootLabel) trees of
    Nothing -> msum $ map (find p . subForest) trees
    Just tree -> Just tree

-- | Like 'paths', but return only the element that matches the predicate.
findWithParents :: (a -> Bool) -> [Tree a] -> Maybe (Tree a, [Tree a])
findWithParents f = List.find (f . Tree.rootLabel . fst) . paths

-- | Get all leaves.  The list will never be null.
leaves :: Tree a -> [a]
leaves = Foldable.toList

-- | Return the first subtrees that match the predicate.
filter :: (a -> Bool) -> [Tree a] -> [Tree a]
filter f = concatMap node
    where
    node (Node a trees)
        | f a = [Node a trees]
        | otherwise = concatMap node trees
