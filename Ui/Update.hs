module Ui.Update where

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import Ui.Block (ViewId, BlockId)
import qualified Ui.Ruler as Ruler
import Ui.Ruler (RulerId)


data Update = ViewUpdate ViewId ViewUpdate
    | BlockUpdate BlockId BlockUpdate
    | TrackUpdate Track.TrackId TrackUpdate
    -- | One of these in the updates means a serious error occurred diffing
    -- the states.
    | Error String
    deriving Show

data ViewUpdate =
    CreateView
    | DestroyView
    | ViewSize Block.Rect
    | ViewConfig Block.ViewConfig
    | SetTrackWidth Block.TrackNum Block.Width
    deriving Show

data BlockUpdate =
    BlockTitle String
    | BlockConfig Block.Config
    | RemoveTrack Block.TrackNum
    | InsertTrack Block.TrackNum Block.Tracklike Block.Width
    deriving Show

-- | track, low_pos, high_pos
data TrackUpdate = UpdateTrack TrackPos TrackPos
    deriving Show
