{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
{- | Updates are diffs against Ui.State and are used in a number of contexts.
    They are produced by "Ui.Diff".  The different uses all require slightly
    different data, and I capture the major differences in separate types.
-}
module Ui.Update where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Generics as Generics
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.StateConfig as StateConfig
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import Types


-- | 'DisplayUpdate's are sent to the UI to update the windows.  Since the UI
-- only has Views, and has a lower level version of tracks, this includes
-- only updates that directly affect display.
type DisplayUpdate = Update Block.DisplayTrack ()

-- | 'UiUpdate's reflect all changes to the underlying UI state.  They're
-- used for incremental save, and by the deriver to determine ScoreDamage.
type UiUpdate = Update Block.Track StateUpdate

-- | For efficiency, TrackEvents updates are collected directly in "Ui.State"
-- as the changes are made.  This is because I'm afraid a complete diff against
-- all the events in a score would be too expensive to run after every cmd.  To
-- express that there are only a few kinds of these updates, and since they are
-- the only ones saved with their cmds, they are given their own type,
-- 'CmdUpdate'.  These are also saved with the undo history.
data CmdUpdate =
    CmdTrackEvents TrackId ScoreTime ScoreTime
    | CmdTrackAllEvents TrackId
    | CmdBringToFront ViewId
    deriving (Eq, Show)

data Update t u
    = ViewUpdate ViewId ViewUpdate
    | BlockUpdate BlockId (BlockUpdate t)
    | TrackUpdate TrackId TrackUpdate
    -- | Since I expect rulers to be changed infrequently, the only kind of
    -- ruler update is a full update.
    | RulerUpdate RulerId Ruler.Ruler
    | StateUpdate u
    deriving (Eq, Show, Generics.Typeable)

data ViewUpdate =
    CreateView
    | DestroyView
    | ViewSize Rect.Rect
    | Status (Map.Map String String) Bool -- ^ status_map is_root
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
    -- | Low pos, high pos.
    TrackEvents ScoreTime ScoreTime
    -- | Update the entire track.
    | TrackAllEvents
    | TrackTitle String
    | TrackBg Color.Color
    | TrackRender Track.RenderConfig
    deriving (Eq, Show)

-- | These are updates to 'Ui.State.State' that have no UI presence.
data StateUpdate =
    Config StateConfig.Config
    | CreateBlock BlockId Block.Block
    | DestroyBlock BlockId
    | CreateTrack TrackId Track.Track
    | DestroyTrack TrackId
    | CreateRuler RulerId Ruler.Ruler
    | DestroyRuler RulerId
    deriving (Eq, Show, Generics.Typeable)

instance DeepSeq.NFData (Update t u) where
    rnf update = case update of
        ViewUpdate view_id update -> view_id `seq` update `seq` ()
        BlockUpdate block_id update -> block_id `seq` update `seq` ()
        TrackUpdate track_id update -> track_id `seq` update `seq` ()
        RulerUpdate ruler_id ruler -> ruler_id `seq` ruler `seq` ()
        StateUpdate u -> u `seq` ()

instance Pretty.Pretty UiUpdate where
    pretty = show

-- | Convert a UiUpdate to a DisplayUpdate by stripping out all the UiUpdate
-- parts.
to_display :: UiUpdate -> Maybe DisplayUpdate
to_display (ViewUpdate vid update) = Just $ ViewUpdate vid update
to_display (BlockUpdate bid update) = BlockUpdate bid <$> case update of
    BlockTitle a -> Just $ BlockTitle a
    BlockConfig a -> Just $ BlockConfig a
    BlockSkeleton a -> Just $ BlockSkeleton a
    RemoveTrack {} -> Nothing
    InsertTrack {} -> Nothing
    BlockTrack {} -> Nothing
to_display (TrackUpdate tid update) = Just $ TrackUpdate tid update
to_display (RulerUpdate rid ruler) = Just $ RulerUpdate rid ruler
to_display (StateUpdate {}) = Nothing

to_ui :: CmdUpdate -> UiUpdate
to_ui (CmdTrackEvents track_id s e) = TrackUpdate track_id (TrackEvents s e)
to_ui (CmdTrackAllEvents track_id) = TrackUpdate track_id TrackAllEvents
to_ui (CmdBringToFront view_id) = ViewUpdate view_id BringToFront

-- * functions

-- | Updates which purely manipulate the view are treated differently by undo.
is_view_update :: UiUpdate -> Bool
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
track_changed :: UiUpdate -> Maybe (TrackId, Ranges.Ranges ScoreTime)
track_changed (TrackUpdate tid update) = case update of
    TrackEvents start end -> Just (tid, Ranges.range start end)
    TrackAllEvents -> Just (tid, Ranges.everything)
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
