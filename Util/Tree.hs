-- | Some more utilities for "Data.Tree".
module Util.Tree where
import Data.Tree


-- | The edges of a forest, as (parent, child).
edges :: Forest a -> [(a, a)]
edges = concatMap $ \(Node val subs) ->
    [(val, sub_val) | Node sub_val _ <- subs] ++ edges subs

-- | Return every element along with its parents and children, closest first.
paths :: Forest a -> [(a, [a], [a])]
paths trees = concatMap (go []) trees
    where
    go parents (Node val subs) =
        (val, parents, children) : concatMap (go (val:parents)) subs
        where children = concatMap flatten subs

t0 = [Node 0 [Node 1 [Node 2 [], Node 3 []]], Node 4 [Node 5 []]]
