-- | Some functions missing from "Data.Graph".
module Util.Graph where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ( (!), (//) )
import Data.Graph
import qualified Data.Tree as Tree

import qualified Util.Array as Array
import qualified Util.Seq as Seq

build :: [Edge] -> Graph
build edges = buildG (0, maximum (-1 : (map (\(x, y) -> max x y) edges))) edges


-- | Roots are all vertices with no parents.
roots_of :: Graph -> [Vertex]
roots_of graph =
    Set.toList $ List.foldl' (flip Set.delete) (Set.fromList [lo..hi])
        (concat (IArray.elems graph))
    where (lo, hi) = IArray.bounds graph

-- | This is like 'dfs', except it allows duplicated vertices.  So don't
-- use it one a graph with cycles.
to_forest :: Graph -> Tree.Forest Vertex
to_forest graph = map (generate graph) (roots_of graph)
    where
    generate graph vertex = Node vertex (map (generate graph) (graph!vertex))

draw :: Graph -> String
draw = Tree.drawForest . map (fmap show) . to_forest

-- | Remove the edge if it already exists, create a new one of it doesn't.
-- Return Nothing if adding an edge would create a cycle.
toggle_edge :: Edge -> Graph -> Maybe Graph
toggle_edge edge graph
    | has_edge edge graph = Just (remove_edges [edge] graph)
    | would_make_cycle edge graph = Nothing
    | otherwise = Just $ add_edges [edge] graph

-- | Splice @from@ into the graph above @to@.  That means both @from@ and @to@
-- are detached from their parents, @from@ is relinked to @to@'s old parents,
-- and @from@ is linked to @to@.
--
-- This operation should be idempotent.
splice :: Edge -> Graph -> Graph
splice (from, to) graph =
    -- If I don't filter p/=from, a duplicate splice will cause a vertex to
    -- loop back to itself.
    add_edges ((from, to) : [(p, from) | p <- parents, p /= from]) $
        remove_edges [(p, to) | p <- parents] graph
    where
    parents = [p | (p, cs) <- IArray.assocs graph, to `elem` cs]

would_make_cycle :: Edge -> Graph -> Bool
would_make_cycle (from, to) graph =
    Array.in_bounds from graph && path graph to from

has_edge :: Edge -> Graph -> Bool
has_edge (from, to) graph = Array.in_bounds from graph && to `elem` graph!from

-- | A lonely vertex has no edges.
lonely_vertex :: Graph -> Vertex -> Bool
lonely_vertex graph vertex =
    not (Array.in_bounds vertex graph) || null (graph!vertex)

add_edges :: [Edge] -> Graph -> Graph
add_edges edges graph =
    IArray.accumArray add [] new_bounds
        (edges ++ [(p, c) | (p, cs) <- IArray.assocs graph, c <- cs])
    where
    (low, high) = IArray.bounds graph
    flattened = [v | (p, c) <- edges, v <- [p, c]]
    new_bounds = (minimum (low : flattened), maximum (high : flattened))
    add cs c = if c `elem` cs then cs else c:cs

remove_edges :: [Edge] -> Graph -> Graph
remove_edges edges graph =
    graph // [(from, filter (`notElem` map snd groups) (graph!from))
        | (from, groups) <- grouped]
    where
    in_bounds = filter ((\p -> Array.in_bounds p graph) . fst) edges
    grouped = Seq.keyed_group_with fst in_bounds

-- | Increment all vertices at and above, insert new empty vertex.
insert_vertex :: Int -> Graph -> Graph
insert_vertex vertex graph = map_vertices incr graph
    where incr v = if v < vertex then v else v+1

-- | Remove a vertex.  All vertices pointing to the removed vertex instead
-- point to what pointed to it.
remove_vertex :: Int -> Graph -> Graph
remove_vertex vertex graph
    | vertex `Array.in_bounds` graph =
        map_vertices decr (unlink_vertex vertex graph)
    | otherwise = graph
    where decr v = if v > vertex then v-1 else v

-- | All vertices pointing to the removed vertex instead point to what pointed
-- to it.  It will be removed from the list of roots.
unlink_vertex :: Int -> Graph -> Graph
unlink_vertex vertex graph =
    IArray.amap (Seq.replace1 v (graph IArray.! v)) graph // [(v, [])]
    where v = Array.assert_in_bounds "unlink_vertex" vertex graph


-- | Transform all the vertices by the given function.  If multiple vertices
-- are transformed to the same value, the one with the originally highest
-- vertex wins.
map_vertices :: (Vertex -> Vertex) -> Graph -> Graph
map_vertices f graph = IArray.listArray (0, length vals - 1) vals
    where
    vals = strip_indices [] $ Seq.drop_initial_dups fst $
        map (\(p, cs) -> (f p, map f cs)) (IArray.assocs graph)

strip_indices :: a -> [(Int, a)] -> [a]
strip_indices def = go 0
    where
    go _ [] = []
    go prev lst@((i, v):xs)
        | prev < i = def : go (prev+1) lst
        | otherwise = v : go (prev+1) xs
