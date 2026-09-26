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
    , hasEdge, addEdges, removeEdges
    , lonelyVertex, flatten, toForest, parents, children
    , insert, remove, toggleEdge
    , spliceAbove, spliceBelow
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
import qualified Util.Lists as Lists
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

hasEdge :: Skeleton -> Edge -> Bool
hasEdge (Skeleton skel) edge = Graphs.hasEdge skel edge

addEdges :: [Edge] -> Skeleton -> Maybe Skeleton
addEdges edges = acyclic . mapSkel (Graphs.addEdges edges)

removeEdges :: [Edge] -> Skeleton -> Skeleton
removeEdges edges = mapSkel (Graphs.removeEdges edges)

lonelyVertex :: Skeleton -> TrackNum -> Bool
lonelyVertex (Skeleton graph) = Graphs.lonelyVertex graph

flatten :: Skeleton -> [Edge]
flatten (Skeleton graph) = List.sort (Graph.edges graph)

toForest :: TrackNum -- ^ Total number of tracks.  This is needed because the
    -- underlying graph may be smaller than the number of tracks.  I don't
    -- want to allow a skeleton that doesn't have certain tracks (and hence
    -- makes them invisible) so any missing tracks are appended.
     -> Skeleton
     -> [Tree.Tree TrackNum] -- ^ Each list of Nodes is sorted so the tree
     -- appears in the same order as the tracks.  This is essential for calls
     -- that want to deal with tracks left-to-right.
toForest ntracks (Skeleton graph) = sortTree $ Graphs.toForest graph ++ rest
    where -- from 1 past array end to last track index (ntracks-1)
    rest = [Graph.Node n [] | n <- [snd (IArray.bounds graph) + 1 .. ntracks-1]]
    sortTree = Lists.sortOn Tree.rootLabel
        . map (\(Tree.Node val subs) -> Tree.Node val (sortTree subs))

-- | Get the parents of a TrackNum.
parents :: Skeleton -> TrackNum -> [TrackNum]
parents (Skeleton graph) tracknum = Graphs.parents graph tracknum

children :: Skeleton -> TrackNum -> [TrackNum]
children (Skeleton graph) tracknum = Graphs.children graph tracknum

-- | Increment all vertices at and above, insert new empty vertex.
insert :: TrackNum -> Skeleton -> Skeleton
insert tracknum = mapSkel (Graphs.insertVertex tracknum)

-- | All vertices pointing to the removed vertex instead point to what it
-- pointed to.  All vertices above the removed one get (-1).
remove :: TrackNum -> Skeleton -> Skeleton
remove tracknum = mapSkel (Graphs.removeVertex tracknum)

toggleEdge :: Edge -> Skeleton -> Maybe Skeleton
toggleEdge edge (Skeleton graph) = Skeleton <$> Graphs.toggleEdge edge graph

spliceAbove :: TrackNum -> TrackNum -> Skeleton -> Maybe Skeleton
spliceAbove new to = acyclic . mapSkel (Graphs.spliceAbove new to)

spliceBelow :: TrackNum -> TrackNum -> Skeleton -> Maybe Skeleton
spliceBelow new to = acyclic . mapSkel (Graphs.spliceBelow new to)

acyclic :: Skeleton -> Maybe Skeleton
acyclic skel@(Skeleton graph)
    | Graphs.hasCycle graph = Nothing
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

mapSkel :: (Graph.Graph -> Graph.Graph) -> Skeleton -> Skeleton
mapSkel f (Skeleton graph) = Skeleton (f graph)
