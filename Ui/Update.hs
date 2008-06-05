{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Ui.Update where
import qualified Data.Typeable as Typeable
import Data.Function
import qualified Data.List as List

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import Ui.Block (ViewId, BlockId)


data Update
    = ViewUpdate ViewId ViewUpdate
    | BlockUpdate BlockId BlockUpdate
    | TrackUpdate Track.TrackId TrackUpdate
    -- | Since I expect rulers to be changed infrequently, the only kind of
    -- ruler update is a full update.
    | RulerUpdate Ruler.RulerId
    deriving (Show, Typeable.Typeable)

data ViewUpdate =
    CreateView
    | DestroyView
    | ViewSize Block.Rect
    | ViewConfig Block.ViewConfig
    | Status String
    | TrackScroll Block.Width
    | Zoom Block.Zoom
    | TrackWidth Block.TrackNum Block.Width
    | Selection Block.SelNum (Maybe Block.Selection)
    deriving Show

data BlockUpdate
    = BlockTitle String
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

-- | Some Updates have to happen before others.
sort :: [Update] -> [Update]
sort = List.sortBy (compare `on` sort_key)

sort_key :: Update -> Int
sort_key update = case update of
    -- Other updates may refer to the created view.
    ViewUpdate _ CreateView -> 0
    -- No sense syncing updates to a view that's going to go away, so destroy
    -- it right away.
    ViewUpdate _ DestroyView -> 0
    -- These may change the meaning of TrackNums.  Update TrackNums refer to
    -- the TrackNums of the destination state, except the ones for InsertTrack
    -- and RemoveTrack, of course.
    BlockUpdate _ (InsertTrack _ _ _) -> 1
    BlockUpdate _ (RemoveTrack _) -> 1
    _ -> 2
