{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Ui.Update where
import qualified Data.Typeable as Typeable

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import Ui.Block (ViewId, BlockId)


data Update
    = ViewUpdate ViewId ViewUpdate
    | BlockUpdate BlockId BlockUpdate
    | TrackUpdate Track.TrackId TrackUpdate
    deriving (Show, Typeable.Typeable)

data ViewUpdate =
    CreateView
    | DestroyView
    | ViewSize Block.Rect
    | ViewConfig Block.ViewConfig
    | TrackWidth Block.TrackNum Block.Width
    | Selection Block.SelNum (Maybe Block.Selection)
    deriving Show

data BlockUpdate
    = BlockTitle String
    | BlockStatus String
    | BlockConfig Block.Config
    | RemoveTrack Block.TrackNum
    | InsertTrack Block.TrackNum Block.TracklikeId Block.Width
    deriving Show

-- | track, low_pos, high_pos
data TrackUpdate
    = TrackEvents TrackPos TrackPos
    -- | Used when there have been unknown updates so I have to play it safe.
    | TrackAllEvents
    | TrackTitle String
    | TrackBg
    deriving Show
