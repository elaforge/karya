module Ui.Skeleton where
import qualified Data.Graph as Graph
import qualified Util.Graph as Graph


-- | The skeleton describes a hierarchical relationship between tracks.  It's
-- used at the UI level only to display the hierarchy visually, but the
-- deriver level can use it.  A given track may appear multiple times or not at
-- all.
newtype Skeleton = Skeleton Graph.Graph deriving (Eq, Read, Show)
map_skel f (Skeleton graph) = Skeleton (f graph)

-- TODO should probably go into Ui.Types
type TrackNum = Int

empty :: Skeleton
empty = Skeleton (Graph.buildG (0, -1) [])

make :: [(TrackNum, TrackNum)] -> Skeleton
make edges = Skeleton (Graph.build edges)

flatten :: Skeleton -> [(TrackNum, TrackNum)]
flatten (Skeleton graph) = Graph.edges graph

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
