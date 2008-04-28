module Ui.Update where

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import Ui.Block (ViewId, BlockId)


data Update = ViewUpdate ViewId ViewUpdate
    | BlockUpdate BlockId BlockUpdate
    | TrackUpdate Track.TrackId TrackUpdate
    deriving Show

data ViewUpdate =
    CreateView
    | DestroyView
    | ViewSize Block.Rect
    | ViewConfig Block.ViewConfig
    | TrackWidth Block.TrackNum Block.Width
    | Selection Block.SelNum (Maybe Block.Selection)
    deriving Show

data BlockUpdate =
    BlockTitle String
    | BlockConfig Block.Config
    | RemoveTrack Block.TrackNum
    | InsertTrack Block.TrackNum Block.Tracklike Block.Width
    deriving Show

-- | track, low_pos, high_pos
data TrackUpdate = TrackEvents TrackPos TrackPos
    | TrackTitle String
    | TrackBg
    deriving Show
