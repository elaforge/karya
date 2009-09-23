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
    | has_edge edge graph = Just (remove_edge edge graph)
    | would_make_cycle edge graph = Nothing
    | otherwise = Just $ add_edge edge graph

would_make_cycle :: Edge -> Graph -> Bool
would_make_cycle (from, to) graph =
    Array.in_bounds graph from && path graph to from

has_edge :: Edge -> Graph -> Bool
has_edge (from, to) graph = Array.in_bounds graph from && to `elem` graph!from

-- | A lonely vertex has no edges.
lonely_vertex :: Graph -> Vertex -> Bool
lonely_vertex graph vertex =
    not (Array.in_bounds graph vertex) || null (graph!vertex)

add_edge :: Edge -> Graph -> Graph
add_edge (from, to) graph
    | Array.in_bounds graph from = IArray.accum (flip (:)) graph [(from, to)]
    | otherwise = IArray.accumArray (flip const) [] new_bounds
        ((from, [to]) : IArray.assocs graph)
    where
    (lo, hi) = IArray.bounds graph
    new_bounds = (min from (min to lo), max from (max to hi))

remove_edge :: Edge -> Graph -> Graph
remove_edge (from, to) graph
    | Array.in_bounds graph from =
        graph // [(from, List.delete to (graph!from))]
    | otherwise = graph

-- | Increment all vertices at and above, insert new empty vertex.
insert_vertex :: Int -> Graph -> Graph
insert_vertex vertex graph = map_vertices incr graph
    where incr v = if v < vertex then v else v+1

-- | Remove a vertex.  All vertices pointing to the removed vertex instead
-- point to what pointed to it.
remove_vertex :: Int -> Graph -> Graph
remove_vertex vertex graph = map_vertices decr (unlink_vertex vertex graph)
    where decr v = if v > vertex then v-1 else v

-- | All vertices pointing to the removed vertex instead point to what pointed
-- to it.  It will be removed from the list of roots.
unlink_vertex :: Int -> Graph -> Graph
unlink_vertex vertex graph =
    IArray.amap (Seq.replace1 v (graph IArray.! v)) graph // [(v, [])]
    where v = Array.assert_in_bounds "unlink_vertex" graph vertex


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
