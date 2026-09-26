-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Some functions missing from "Data.Graph".
module Util.Graphs where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ( (!), (//) )
import Data.Graph
import qualified Data.Tree as Tree

import qualified Util.Array as Array
import qualified Util.Lists as Lists


build :: [Edge] -> Graph
build edges = buildG (0, upper) unique
    where
    unique = Lists.unique edges
    upper = maximum (-1 : map (\(x, y) -> max x y) unique)

-- | Roots are all vertices with no parents.
rootsOf :: Graph -> [Vertex]
rootsOf graph =
    Set.toList $ List.foldl' (flip Set.delete) (Set.fromList [lo..hi])
        (concat (IArray.elems graph))
    where (lo, hi) = IArray.bounds graph

-- | This is like 'dfs', except it allows duplicated vertices.  So don't
-- use it one a graph with cycles.
toForest :: Graph -> Tree.Forest Vertex
toForest graph = map (generate graph) (rootsOf graph)
    where
    generate graph vertex = Node vertex (map (generate graph) (graph!vertex))

draw :: Graph -> String
draw = Tree.drawForest . map (fmap show) . toForest

-- | Remove the edge if it already exists, create a new one of it doesn't.
-- Return Nothing if adding an edge would create a cycle.
toggleEdge :: Edge -> Graph -> Maybe Graph
toggleEdge edge graph
    | hasEdge graph edge = Just (removeEdges [edge] graph)
    | wouldMakeCycle edge graph = Nothing
    | otherwise = Just $ addEdges [edge] graph

-- | Splice @new@ into the graph above @to@.  The parents of @to@ are detached
-- from it and re-attached to @new@.  Then @new@ is attached above @to@.
--
-- This operation should be idempotent.
spliceAbove :: Vertex -> Vertex -> Graph -> Graph
spliceAbove new to graph =
    -- If I don't filter p/=new, a duplicate splice will cause a vertex to
    -- loop back to itself.
    addEdges ((new, to) : [(p, new) | p <- ps, p /= new]) $
        removeEdges [(p, to) | p <- ps] graph
    where ps = parents graph to

-- | Splice @new@ into the graph below @to@.  The children of @to@ are
-- detached and re-attached to @new@.  Then @to@ is attached above @new@.
--
-- This operation should be idempotent.
spliceBelow :: Vertex -> Vertex -> Graph -> Graph
spliceBelow new to graph =
    addEdges ((to, new) : [(new, c) | c <- cs, c /= new]) $
        removeEdges (map ((,) to) cs) graph
    where cs = children graph to

-- | Get the parents of a Vertex.
parents :: Graph -> Vertex -> [Vertex]
parents graph v = [p | (p, cs) <- IArray.assocs graph, v `elem` cs]

-- | Get the children of a Vertex.
children :: Graph -> Vertex -> [Vertex]
children graph v
    | Array.inBounds v graph = graph!v
    | otherwise = []

wouldMakeCycle :: Edge -> Graph -> Bool
wouldMakeCycle (from, to) graph =
    from == to
    || Array.inBounds from graph && Array.inBounds to graph
        && path graph to from

hasCycle :: Graph -> Bool
hasCycle graph = any (not . null . Tree.subForest) (scc graph)
    || any (uncurry (==)) (edges graph)

hasEdge :: Graph -> Edge -> Bool
hasEdge graph (from, to) = Array.inBounds from graph && to `elem` graph!from

-- | A lonely vertex has no edges.
lonelyVertex :: Graph -> Vertex -> Bool
lonelyVertex graph vertex =
    not (Array.inBounds vertex graph) || null (graph!vertex)

addEdges :: [Edge] -> Graph -> Graph
addEdges edges graph =
    IArray.accumArray add [] newBounds
        (edges ++ [(p, c) | (p, cs) <- IArray.assocs graph, c <- cs])
    where
    (low, high) = IArray.bounds graph
    flattened = [v | (p, c) <- edges, v <- [p, c]]
    newBounds = (minimum (low : flattened), maximum (high : flattened))
    add cs c = if c `elem` cs then cs else c:cs

removeEdges :: [Edge] -> Graph -> Graph
removeEdges edges graph
    | null edges = graph
    | otherwise =
        graph // [(from, filter (`notElem` map snd groups) (graph!from))
            | (from, groups) <- grouped]
    where
    inBounds = filter ((\p -> Array.inBounds p graph) . fst) edges
    grouped = Lists.keyedGroupSort fst inBounds

-- | Increment all vertices at and above, insert new empty vertex.
insertVertex :: Int -> Graph -> Graph
insertVertex vertex graph = mapVertices incr graph
    where incr v = if v < vertex then v else v+1

-- | Remove a vertex.  All vertices pointing to the removed vertex instead
-- point to what pointed to it.
removeVertex :: Int -> Graph -> Graph
removeVertex vertex graph
    | vertex `Array.inBounds` graph =
        mapVertices decr (unlinkVertex vertex graph)
    | otherwise = graph
    where decr v = if v > vertex then v-1 else v

-- | All vertices pointing to the removed vertex instead point to what pointed
-- to it.  It will be removed from the list of roots.
unlinkVertex :: Int -> Graph -> Graph
unlinkVertex vertex graph =
    IArray.amap (Lists.replace1 v (graph!v)) graph // [(v, [])]
    where v = Array.assertInBounds "unlink_vertex" vertex graph


-- | Transform all the vertices by the given function.  If multiple vertices
-- are transformed to the same value, the one with the originally highest
-- vertex wins.
mapVertices :: (Vertex -> Vertex) -> Graph -> Graph
mapVertices f graph = Array.fromList $
    stripIndices [] $ Lists.dropInitialDups fst $
        map (\(p, cs) -> (f p, map f cs)) (IArray.assocs graph)

stripIndices :: a -> [(Int, a)] -> [a]
stripIndices def = go 0
    where
    go _ [] = []
    go prev lst@((i, v):xs)
        | prev < i = def : go (prev+1) lst
        | otherwise = v : go (prev+1) xs

-- | Move a vertex.  The graph remains the same, but the @from@ vertex number
-- will be changed to @to@ and vice versa.
move :: Vertex -> Vertex -> Graph -> Maybe Graph
move from to graph = fmap move $ Array.check to =<< Array.check from graph
    where
    -- Swap array elements.  Then swap all referents
    move graph
        | from == to = graph
        | otherwise = IArray.amap (map relink) $
            graph // [(from, graph!to), (to, graph!from)]
    relink v
        | v == from = to
        | v == to = from
        | otherwise = v
