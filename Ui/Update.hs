{-# LANGUAGE DeriveDataTypeable #-}
module Ui.Update where
import qualified Data.Generics as Generics
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Types as Types


data Update
    = ViewUpdate ViewId ViewUpdate
    | BlockUpdate BlockId BlockUpdate
    | TrackUpdate TrackId TrackUpdate
    -- | Since I expect rulers to be changed infrequently, the only kind of
    -- ruler update is a full update.
    | RulerUpdate RulerId
    deriving (Show, Generics.Typeable)

data ViewUpdate =
    CreateView
    | DestroyView
    | ViewSize Types.Rect
    | ViewConfig Block.ViewConfig
    | Status String
    | TrackScroll Types.Width
    | Zoom Types.Zoom
    | TrackWidth TrackNum Types.Width
    | Selection Types.SelNum (Maybe Types.Selection)
    -- | Bring the window to the front.  Unlike most other updates, this is
    -- recorded directly and is not reflected in Ui.State.
    | BringToFront
    deriving Show

data BlockUpdate
    = BlockTitle String
    | BlockConfig Block.Config
    | BlockSkeleton Skeleton.Skeleton
    | RemoveTrack TrackNum
    | InsertTrack TrackNum Types.Width Block.DisplayTrack
    -- | Unlike a TrackUpdate, these settings are local to the block, not
    -- global to this track in all its blocks.
    | DisplayTrack TrackNum Block.DisplayTrack
    deriving Show

-- | track, low_pos, high_pos
data TrackUpdate
    = TrackEvents ScoreTime ScoreTime
    -- | Used when there have been unknown updates so I have to play it safe.
    | TrackAllEvents
    | TrackTitle String
    | TrackBg
    | TrackRender
    deriving (Show)

-- | Updates which purely manipulate the view are treated differently by undo.
is_view_update :: Update -> Bool
is_view_update update = case update of
    ViewUpdate _ view_update -> case view_update of
        CreateView -> False
        DestroyView -> False
        _ -> True
    BlockUpdate _ block_update -> case block_update of
        BlockConfig _ -> True
        _ -> False
    TrackUpdate _ track_update -> case track_update of
        TrackBg -> True
        TrackRender -> True
        _ -> False
    _ -> False

-- | Does an update imply a change which would require rederiving?
block_changed :: Update -> Maybe BlockId
block_changed (BlockUpdate block_id update) = case update of
    BlockConfig _ -> Nothing
    -- TODO if DisplayTrack only changed collapsed, then it's not a change
    _ -> Just block_id
block_changed _ = Nothing

-- | As 'block_changed', but for track updates.
track_changed :: Update -> Maybe TrackId
track_changed (TrackUpdate track_id update) = case update of
    TrackEvents _ _ -> Just track_id
    TrackAllEvents -> Just track_id
    -- It could have changed the interpretation of the events.
    TrackTitle _ -> Just track_id
    _ -> Nothing
track_changed _ = Nothing

-- | Some Updates have to happen before others.
sort :: [Update] -> [Update]
sort = Seq.sort_on sort_key

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
