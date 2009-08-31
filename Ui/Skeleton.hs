module Ui.Skeleton where
import qualified Data.Array.IArray as IArray
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree
import qualified Util.Graph as Graph


-- | The skeleton describes a hierarchical relationship between tracks.  It's
-- used at the UI level only to display the hierarchy visually, but the deriver
-- level will presumably use it for derivation.  A given track may appear
-- multiple times or not at all.
newtype Skeleton = Skeleton Graph.Graph deriving (Eq, Read, Show)
map_skel f (Skeleton graph) = Skeleton (f graph)

-- TODO should probably go into Ui.Types
-- Graph Vertex is an Int and so is TrackNum, so no conversion is required.
type TrackNum = Int

empty :: Skeleton
empty = Skeleton (Graph.buildG (0, -1) [])

make :: [(TrackNum, TrackNum)] -> Skeleton
make edges = Skeleton (Graph.build edges)

lonely_vertex :: Skeleton -> TrackNum -> Bool
lonely_vertex (Skeleton graph) tracknum = Graph.lonely_vertex graph tracknum

flatten :: Skeleton -> [(TrackNum, TrackNum)]
flatten (Skeleton graph) = Graph.edges graph

-- | The underlying graph may be smaller than the number of tracks.  I don't
-- want to allow a skeleton that doesn't have certain tracks (and hence
-- makes them invisible) so any missing tracks are appended.
to_forest :: TrackNum -> Skeleton -> Tree.Forest TrackNum
to_forest ntracks (Skeleton graph) = Graph.to_forest graph ++ rest
    where -- from 1 past array end to last track index (ntracks-1)
    rest = [Graph.Node n [] | n <- [snd (IArray.bounds graph) + 1 .. ntracks-1]]

-- | Increment all vertices at and above, insert new empty vertex.
insert :: TrackNum -> Skeleton -> Skeleton
insert tracknum = map_skel (Graph.insert_vertex tracknum)

-- | All vertices pointing to the removed vertex instead point to what it
-- pointed to.  All vertices above the removed one get (-1).
remove :: TrackNum -> Skeleton -> Skeleton
remove tracknum = map_skel (Graph.remove_vertex tracknum)

toggle_edge :: (TrackNum, TrackNum) -> Skeleton -> Maybe Skeleton
toggle_edge edge (Skeleton graph) = case Graph.toggle_edge edge graph of
    Nothing -> Nothing
    Just graph -> Just (Skeleton graph)
