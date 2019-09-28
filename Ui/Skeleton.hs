-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Operations on 'Skeleton's.
--
-- A skeleton is a tree, but it's stored as a "Data.Graph" and converted to
-- a tree when needed.  This seems weird, but at the time it seemed overly
-- awkward to add and remove edges to a tree, and to detect cycles, while
-- graphs have those operations built in.  In retrospect, dealing with
-- Data.Graph was probably more of a pain, so maybe someday if I have a lot of
-- extra time and feel like some aggravation I'll see about redoing Skeleton as
-- a Tree.  I could also maybe clean up "Ui.TrackTree".
module Ui.Skeleton (
    Skeleton, Edge
    , empty, make, draw
    , has_edge, add_edges, remove_edges
    , lonely_vertex, flatten, to_forest, parents
    , insert, remove, toggle_edge
    , splice_above, splice_below
    , move
#ifdef TESTING
    , module Ui.Skeleton
#endif
) where
import qualified Data.Array.IArray as IArray
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Tree as Tree

import qualified Util.Graphs as Graphs
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import Global
import Types


-- | The skeleton describes a hierarchical relationship between tracks.  It's
-- used at the UI level only to display the hierarchy visually, but the
-- deriver level will presumably use it for derivation.  A given track may
-- appear multiple times or not at all.
newtype Skeleton = Skeleton Graph.Graph
    deriving (Read, Show, Serialize.Serialize)

instance Pretty Skeleton where pretty = pretty . flatten

-- Data.Graph is just a type synonym to Data.Array, which means that
-- 'make [(1, 2), (1, 3)]' compare inequal even though they describe the same
-- graph.
instance Eq Skeleton where
    s1 == s2 = flatten s1 == flatten s2

-- | This is @(parent, child)@.
type Edge = (TrackNum, TrackNum)

-- Graph Vertex is an Int and so is TrackNum, so no conversion is required.

empty :: Skeleton
empty = Skeleton (Graph.buildG (0, -1) [])

make :: [Edge] -> Skeleton
make edges = Skeleton (Graphs.build edges)

draw :: Skeleton -> String
draw (Skeleton graph) = Graphs.draw graph

has_edge :: Skeleton -> Edge -> Bool
has_edge (Skeleton skel) edge = Graphs.has_edge skel edge

add_edges :: [Edge] -> Skeleton -> Maybe Skeleton
add_edges edges = acyclic . map_skel (Graphs.add_edges edges)

remove_edges :: [Edge] -> Skeleton -> Skeleton
remove_edges edges = map_skel (Graphs.remove_edges edges)

lonely_vertex :: Skeleton -> TrackNum -> Bool
lonely_vertex (Skeleton graph) = Graphs.lonely_vertex graph

flatten :: Skeleton -> [Edge]
flatten (Skeleton graph) = List.sort (Graph.edges graph)

to_forest :: TrackNum -- ^ Total number of tracks.  This is needed because the
    -- underlying graph may be smaller than the number of tracks.  I don't
    -- want to allow a skeleton that doesn't have certain tracks (and hence
    -- makes them invisible) so any missing tracks are appended.
     -> Skeleton
     -> [Tree.Tree TrackNum] -- ^ Each list of Nodes is sorted so the tree
     -- appears in the same order as the tracks.  This is essential for calls
     -- that want to deal with tracks left-to-right.
to_forest ntracks (Skeleton graph) = sort_tree $ Graphs.to_forest graph ++ rest
    where -- from 1 past array end to last track index (ntracks-1)
    rest = [Graph.Node n [] | n <- [snd (IArray.bounds graph) + 1 .. ntracks-1]]
    sort_tree = Seq.sort_on Tree.rootLabel
        . map (\(Tree.Node val subs) -> Tree.Node val (sort_tree subs))

-- | Get the parents of a TrackNum.
parents :: Skeleton -> TrackNum -> [TrackNum]
parents (Skeleton graph) tracknum = Graphs.parents graph tracknum

-- | Increment all vertices at and above, insert new empty vertex.
insert :: TrackNum -> Skeleton -> Skeleton
insert tracknum = map_skel (Graphs.insert_vertex tracknum)

-- | All vertices pointing to the removed vertex instead point to what it
-- pointed to.  All vertices above the removed one get (-1).
remove :: TrackNum -> Skeleton -> Skeleton
remove tracknum = map_skel (Graphs.remove_vertex tracknum)

toggle_edge :: Edge -> Skeleton -> Maybe Skeleton
toggle_edge edge (Skeleton graph) = Skeleton <$> Graphs.toggle_edge edge graph

splice_above :: TrackNum -> TrackNum -> Skeleton -> Maybe Skeleton
splice_above new to = acyclic . map_skel (Graphs.splice_above new to)

splice_below :: TrackNum -> TrackNum -> Skeleton -> Maybe Skeleton
splice_below new to = acyclic . map_skel (Graphs.splice_below new to)

acyclic :: Skeleton -> Maybe Skeleton
acyclic skel@(Skeleton graph)
    | Graphs.has_cycle graph = Nothing
    | otherwise = Just skel

-- | If from<to, then @from@ is inserted after @to@.
-- If to<from, then @from@ is inserted before @to@.
move :: TrackNum -> TrackNum -> Skeleton -> Skeleton
move from to = make . go . flatten
    -- TODO should be possible to translate into a map over the graph array
    where
    go
        | from < to = map $ bimap up up
        | to < from = map $ bimap down down
        | otherwise = id
    up x
        | x == from = to
        | x > from && x <= to = x - 1
        | otherwise = x
    down x
        | x == from = to
        | x >= to && x < from = x + 1
        | otherwise = x

map_skel :: (Graph.Graph -> Graph.Graph) -> Skeleton -> Skeleton
map_skel f (Skeleton graph) = Skeleton (f graph)
