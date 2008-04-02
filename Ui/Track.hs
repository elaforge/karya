module Ui.Track where
import qualified Data.Map as Map

import Ui.Types
import qualified Ui.Event as Event


-- IntMap is more efficient than Map, but only takes Int keys...
newtype TrackData = TrackData (Map.Map TrackPos Event.Event) deriving Show
    -- alternate efficient version for controller tracks?
    -- | ControllerTrack (Array (TrackPos, Double))
-- This should be opaque, with a few operations to query and modify it,
-- so I keep freedom to change the implemenctation.  Also needs to expose
-- the queries to c++.

newtype TrackId = TrackId String deriving (Eq, Ord, Show)
