module Ui.Skeleton where
import qualified Data.Array.IArray as IArray
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Graph as Graph
import qualified Util.Seq as Seq

import Types


-- | The skeleton describes a hierarchical relationship between tracks.  It's
-- used at the UI level only to display the hierarchy visually, but the
-- deriver level will presumably use it for derivation.  A given track may
-- appear multiple times or not at all.
newtype Skeleton = Skeleton Graph.Graph deriving (Eq, Read, Show)

-- | This is @(parent, child)@.
type Edge = (TrackNum, TrackNum)

-- Graph Vertex is an Int and so is TrackNum, so no conversion is required.

empty :: Skeleton
empty = Skeleton (Graph.buildG (0, -1) [])

make :: [Edge] -> Skeleton
make edges = Skeleton (Graph.build edges)

add_edges :: [Edge] -> Skeleton -> Maybe Skeleton
add_edges edges = acyclic . map_skel (Graph.add_edges edges)

remove_edges :: [Edge] -> Skeleton -> Skeleton
remove_edges edges = map_skel (Graph.remove_edges edges)

draw :: Skeleton -> String
draw (Skeleton graph) = Graph.draw graph

lonely_vertex :: Skeleton -> TrackNum -> Bool
lonely_vertex (Skeleton graph) = Graph.lonely_vertex graph

flatten :: Skeleton -> [Edge]
flatten (Skeleton graph) = Graph.edges graph

to_forest :: TrackNum -- ^ Total number of tracks.  This is needed because the
    -- underlying graph may be smaller than the number of tracks.  I don't
    -- want to allow a skeleton that doesn't have certain tracks (and hence
    -- makes them invisible) so any missing tracks are appended.
     -> Skeleton
     -> [Tree.Tree TrackNum] -- ^ Each list of Nodes is sorted so the tree
     -- appears in the same order as the tracks.  This is essential for calls
     -- that want to deal with tracks left-to-right.
to_forest ntracks (Skeleton graph) = sort_tree $ Graph.to_forest graph ++ rest
    where -- from 1 past array end to last track index (ntracks-1)
    rest =
        [Graph.Node n [] | n <- [snd (IArray.bounds graph) + 1 .. ntracks-1]]
    sort_tree = Seq.sort_on Tree.rootLabel . map (\(Tree.Node val subs) ->
        Tree.Node val (sort_tree subs))

-- | Increment all vertices at and above, insert new empty vertex.
insert :: TrackNum -> Skeleton -> Skeleton
insert tracknum = map_skel (Graph.insert_vertex tracknum)

-- | All vertices pointing to the removed vertex instead point to what it
-- pointed to.  All vertices above the removed one get (-1).
remove :: TrackNum -> Skeleton -> Skeleton
remove tracknum = map_skel (Graph.remove_vertex tracknum)

toggle_edge :: Edge -> Skeleton -> Maybe Skeleton
toggle_edge edge (Skeleton graph) = Skeleton <$> Graph.toggle_edge edge graph

splice_above :: TrackNum -> TrackNum -> Skeleton -> Maybe Skeleton
splice_above new to = acyclic . map_skel (Graph.splice_above new to)

splice_below :: TrackNum -> TrackNum -> Skeleton -> Maybe Skeleton
splice_below new to = acyclic . map_skel (Graph.splice_below new to)

acyclic :: Skeleton -> Maybe Skeleton
acyclic skel@(Skeleton graph)
    | Graph.has_cycle graph = Nothing
    | otherwise = Just skel

move :: TrackNum -> TrackNum -> Skeleton -> Maybe Skeleton
move from to (Skeleton graph) = Skeleton <$> Graph.move from to graph

map_skel :: (Graph.Graph -> Graph.Graph) -> Skeleton -> Skeleton
map_skel f (Skeleton graph) = Skeleton (f graph)
