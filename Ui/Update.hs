{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Ui.Update where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Generics as Generics
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.StateConfig as StateConfig
import qualified Ui.Track as Track
import qualified Ui.Types as Types


type CmdUpdate = Update Block.Track
type DisplayUpdate = Update Block.DisplayTrack

data Update t
    = ViewUpdate ViewId ViewUpdate
    | BlockUpdate BlockId (BlockUpdate t)
    | TrackUpdate TrackId TrackUpdate
    -- | Since I expect rulers to be changed infrequently, the only kind of
    -- ruler update is a full update.
    | RulerUpdate RulerId Ruler.Ruler
    | StateConfig StateConfig.Config
    deriving (Eq, Show, Generics.Typeable)

data ViewUpdate =
    CreateView Block.View
    | DestroyView
    | ViewSize Rect.Rect
    | ViewConfig Block.ViewConfig
    | Status (Map.Map String String)
    | TrackScroll Types.Width
    | Zoom Types.Zoom
    | Selection Types.SelNum (Maybe Types.Selection)
    -- | Bring the window to the front.  Unlike most other updates, this is
    -- recorded directly and is not reflected in Ui.State.
    | BringToFront
    deriving (Eq, Show)

data BlockUpdate t
    = BlockTitle String
    | BlockConfig Block.Config
    | BlockSkeleton Skeleton.Skeleton
    | RemoveTrack TrackNum
    | InsertTrack TrackNum t
    -- | Unlike a TrackUpdate, these settings are local to the block, not
    -- global to this track in all its blocks.
    | BlockTrack TrackNum t
    deriving (Eq, Show)

data TrackUpdate =
    -- | Low pos, high pos, and the events to replace that range.
    TrackEvents ScoreTime ScoreTime Events.Events
    -- | Used when there have been unknown updates so I have to play it safe.
    | TrackAllEvents Events.Events
    | TrackTitle String
    | TrackBg Color.Color
    | TrackRender Track.RenderConfig
    deriving (Eq, Show)

instance DeepSeq.NFData (Update t) where
    rnf update = case update of
        ViewUpdate view_id update -> view_id `seq` update `seq` ()
        BlockUpdate block_id update -> block_id `seq` update `seq` ()
        TrackUpdate track_id update -> track_id `seq` update `seq` ()
        RulerUpdate ruler_id ruler -> ruler_id `seq` ruler `seq` ()
        StateConfig config -> config `seq` ()

instance Pretty.Pretty CmdUpdate where
    pretty = show
    -- pretty (ViewUpdate vid update) = 

strip :: Update a -> Maybe (Update b)
strip (ViewUpdate vid update) = Just $ ViewUpdate vid update
strip (BlockUpdate bid update) = BlockUpdate bid <$> case update of
    BlockTitle a -> Just $ BlockTitle a
    BlockConfig a -> Just $ BlockConfig a
    BlockSkeleton a -> Just $ BlockSkeleton a
    RemoveTrack a -> Just $ RemoveTrack a
    InsertTrack {} -> Nothing
    BlockTrack {} -> Nothing
strip (TrackUpdate tid update) = Just $ TrackUpdate tid update
strip (RulerUpdate rid ruler) = Just $ RulerUpdate rid ruler
strip (StateConfig config) = Just $ StateConfig config

-- * functions

-- | Updates which purely manipulate the view are treated differently by undo.
is_view_update :: CmdUpdate -> Bool
is_view_update update = case update of
    ViewUpdate _ view_update -> case view_update of
        CreateView {} -> False
        DestroyView -> False
        _ -> True
    BlockUpdate _ block_update -> case block_update of
        BlockConfig _ -> True
        _ -> False
    TrackUpdate _ track_update -> case track_update of
        TrackBg {} -> True
        TrackRender {} -> True
        _ -> False
    _ -> False

-- | Does an update imply a change which would require rederiving?
block_changed :: CmdUpdate -> Maybe BlockId
block_changed (BlockUpdate block_id update) = case update of
    BlockConfig {} -> Nothing
    InsertTrack {} -> Nothing
    BlockTrack {} -> Nothing -- TODO what about mute?
    _ -> Just block_id
block_changed _ = Nothing

-- | As 'block_changed', but for track updates.
track_changed :: CmdUpdate -> Maybe (TrackId, Ranges.Ranges ScoreTime)
track_changed (TrackUpdate tid update) = case update of
    TrackEvents start end _ -> Just (tid, Ranges.range start end)
    TrackAllEvents {} -> Just (tid, Ranges.everything)
    TrackTitle _ -> Just (tid, Ranges.everything)
    _ -> Nothing
track_changed _ = Nothing

-- | Some Updates have to happen before others.
sort :: [DisplayUpdate] -> [DisplayUpdate]
sort = Seq.sort_on sort_key

sort_key :: DisplayUpdate -> Int
sort_key update = case update of
    -- Other updates may refer to the created view.
    ViewUpdate _ (CreateView {}) -> 0
    -- No sense syncing updates to a view that's going to go away, so destroy
    -- it right away.
    ViewUpdate _ DestroyView -> 0
    -- These may change the meaning of TrackNums.  Update TrackNums refer to
    -- the TrackNums of the destination state, except the ones for InsertTrack
    -- and RemoveTrack, of course.
    BlockUpdate _ (InsertTrack {}) -> 1
    BlockUpdate _ (RemoveTrack {}) -> 1
    _ -> 2
