-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | Updates are diffs against Ui.State and are used in a number of contexts.
    They are produced by "Ui.Diff".  The different uses all require slightly
    different data, and I capture the major differences in separate types.
-}
module Ui.Update where
import qualified Control.DeepSeq as DeepSeq

import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import Global
import Types


-- | 'DisplayUpdate's are sent to the UI to update the windows.  Since the UI
-- only has Views, and has a lower level version of tracks, this includes
-- only updates that directly affect display.
type DisplayUpdate = Update Block.DisplayTrack ()

-- | 'UiUpdate's reflect all changes to the underlying UI state.  They're
-- used for incremental save, and by the deriver to determine ScoreDamage.
type UiUpdate = Update Block.Track State

-- | For efficiency, TrackEvents updates are collected directly in "Ui.State"
-- as the changes are made.  This is because I'm afraid a complete diff against
-- all the events in a score would be too expensive to run after every cmd.  To
-- express that there are only a few kinds of these updates, and since they are
-- the only ones saved with their cmds, they are given their own type,
-- 'CmdUpdate'.  These are also saved with the undo history.
data CmdUpdate =
    CmdTrackEvents TrackId ScoreTime ScoreTime
    | CmdTrackAllEvents TrackId
    | CmdRuler RulerId
    | CmdBringToFront ViewId
    -- | If the TrackNum is set, set keyboard focus on that track's title.
    -- Otherwise, focus on the block title.
    | CmdTitleFocus ViewId (Maybe TrackNum)
    deriving (Eq, Show)

data Update t u =
    View !ViewId !View
    | Block !BlockId !(Block t)
    | Track !TrackId !Track
    -- | Since I expect rulers to be changed infrequently, the only kind of
    -- ruler update is a full update.
    | Ruler !RulerId
    | State !u
    deriving (Eq, Show)

data View =
    CreateView
    | DestroyView
    | ViewSize !Rect.Rect
    | Status !(Map (Int, Text) Text) !Color.Color -- ^ background color
    | TrackScroll !Types.Width
    | Zoom !Zoom.Zoom
    | Selection !Sel.Num !(Maybe Sel.Selection)
    -- | Bring the window to the front.  Unlike most other updates, this is
    -- recorded directly and is not reflected in Ui.State.
    | BringToFront
    -- | Similar to BringToFront, but sets keyboard focus in a track title.
    -- If the TrackNum is not given, focus on the block title.
    | TitleFocus !(Maybe TrackNum)
    deriving (Eq, Show)

data Block t =
    BlockTitle !Text
    | BlockConfig !Block.Config
    -- | The second is the \"integrate skeleton\", which is drawn in the same
    -- place.  It could be Skeleton too, but since it never was a skeleton it
    -- seems pointless to convert it to one just so it can be flattened again.
    -- Arguably it's the first arg which should be edges, but at least this way
    -- the two args can't be mixed up.
    | BlockSkeleton !Skeleton.Skeleton ![(Color.Color, [(TrackNum, TrackNum)])]
    | RemoveTrack !TrackNum
    | InsertTrack !TrackNum !t
    -- | Unlike 'Track', these settings are local to the block, not global to
    -- this track in all its blocks.
    | BlockTrack !TrackNum !t
    deriving (Eq, Show)

data Track =
    -- | Low pos, high pos.
    TrackEvents !ScoreTime !ScoreTime
    -- | Update the entire track.
    | TrackAllEvents
    | TrackTitle !Text
    | TrackBg !Color.Color
    | TrackRender !Track.RenderConfig
    deriving (Eq, Show)

-- | These are updates to 'Ui.Ui.State' that have no UI presence.
data State =
    Config !UiConfig.Config
    | CreateBlock !BlockId !Block.Block
    | DestroyBlock !BlockId
    | CreateTrack !TrackId !Track.Track
    | DestroyTrack !TrackId
    | CreateRuler !RulerId !Ruler.Ruler
    | DestroyRuler !RulerId
    deriving (Eq, Show)

instance DeepSeq.NFData (Update t u) where
    rnf update = case update of
        View view_id update -> view_id `seq` update `seq` ()
        Block block_id update -> block_id `seq` update `seq` ()
        Track track_id update -> track_id `seq` update `seq` ()
        Ruler ruler_id -> ruler_id `seq` ()
        State u -> u `seq` ()

instance (Pretty t, Pretty u) => Pretty (Update t u) where
    format upd = case upd of
        View view_id update -> Pretty.constructor "View"
            [Pretty.format view_id, Pretty.format update]
        Block block_id update -> Pretty.constructor "Block"
            [Pretty.format block_id, Pretty.format update]
        Track track_id update -> Pretty.constructor "Track"
            [Pretty.format track_id, Pretty.format update]
        Ruler ruler_id -> Pretty.constructor "Ruler" [Pretty.format ruler_id]
        State update -> Pretty.constructor "State" [Pretty.format update]

instance Pretty View where
    format update = case update of
        CreateView -> Pretty.text "CreateView"
        DestroyView -> Pretty.text "DestroyView"
        ViewSize rect -> Pretty.constructor "ViewSize" [Pretty.format rect]
        Status status is_root -> Pretty.constructor "Status"
            [Pretty.format status, Pretty.format is_root]
        TrackScroll width ->
            Pretty.constructor "TrackScroll" [Pretty.format width]
        Zoom zoom -> Pretty.constructor "Zoom" [Pretty.format zoom]
        Selection selnum sel -> Pretty.constructor "Selection"
            [Pretty.format selnum, Pretty.format sel]
        BringToFront -> Pretty.text "BringToFront"
        TitleFocus tracknum ->
            Pretty.constructor "TitleFocus" [Pretty.format tracknum]

instance Pretty t => Pretty (Block t) where
    format update = case update of
        BlockTitle s -> Pretty.constructor "BlockTitle"
            [Pretty.format s]
        BlockConfig config -> Pretty.constructor "BlockConfig"
            [Pretty.format config]
        BlockSkeleton skel int_skel -> Pretty.constructor "BlockSkeleton"
            [Pretty.format skel, Pretty.format int_skel]
        RemoveTrack n -> Pretty.constructor "RemoveTrack"
            [Pretty.format n]
        InsertTrack n t -> Pretty.constructor "InsertTrack"
            [Pretty.format n, Pretty.format t]
        BlockTrack n t -> Pretty.constructor "BlockTrack"
            [Pretty.format n, Pretty.format t]

instance Pretty Track where
    format update = case update of
        TrackEvents s e -> Pretty.constructor "TrackEvents"
            [Pretty.format s, Pretty.format e]
        TrackAllEvents -> Pretty.text "TrackAllEvents"
        TrackTitle s -> Pretty.constructor "TrackTitle" [Pretty.format s]
        TrackBg c -> Pretty.constructor "TrackTitle" [Pretty.format c]
        TrackRender config -> Pretty.constructor "TrackTitle"
            [Pretty.format config]

instance Pretty State where
    format update = case update of
        Config config -> Pretty.constructor "Config" [Pretty.format config]
        CreateBlock block_id _ -> Pretty.constructor "CreateBlock"
            [Pretty.format block_id]
        DestroyBlock block_id -> Pretty.constructor "DestroyBlock"
            [Pretty.format block_id]
        CreateTrack track_id _ -> Pretty.constructor "CreateTrack"
            [Pretty.format track_id]
        DestroyTrack track_id -> Pretty.constructor "DestroyTrack"
            [Pretty.format track_id]
        CreateRuler ruler_id _ -> Pretty.constructor "CreateRuler"
            [Pretty.format ruler_id]
        DestroyRuler ruler_id -> Pretty.constructor "DestroyRuler"
            [Pretty.format ruler_id]

instance Pretty CmdUpdate where
    pretty = showt

update_id :: Update t State -> Maybe Id.Id
update_id u = case u of
    View view_id _ -> ident view_id
    Block block_id _ -> ident block_id
    Track track_id _ -> ident track_id
    Ruler ruler_id -> ident ruler_id
    State st -> case st of
        Config {} -> Nothing
        CreateBlock block_id _ -> ident block_id
        DestroyBlock block_id -> ident block_id
        CreateTrack track_id _ -> ident track_id
        DestroyTrack track_id -> ident track_id
        CreateRuler ruler_id _ -> ident ruler_id
        DestroyRuler ruler_id -> ident ruler_id
    where
    ident :: Id.Ident a => a -> Maybe Id.Id
    ident = Just . Id.unpack_id

-- | Convert a UiUpdate to a DisplayUpdate by stripping out all the UiUpdate
-- parts.
to_display :: UiUpdate -> Maybe DisplayUpdate
to_display (View vid update) = Just $ View vid update
to_display (Block bid update) = Block bid <$> case update of
    BlockTitle a -> Just $ BlockTitle a
    BlockConfig a -> Just $ BlockConfig a
    BlockSkeleton a b -> Just $ BlockSkeleton a b
    RemoveTrack {} -> Nothing
    InsertTrack {} -> Nothing
    BlockTrack {} -> Nothing
to_display (Track tid update) = Just $ Track tid update
to_display (Ruler rid) = Just $ Ruler rid
to_display (State {}) = Nothing

to_ui :: CmdUpdate -> UiUpdate
to_ui (CmdTrackEvents track_id s e) = Track track_id (TrackEvents s e)
to_ui (CmdTrackAllEvents track_id) = Track track_id TrackAllEvents
to_ui (CmdRuler ruler_id) = Ruler ruler_id
to_ui (CmdBringToFront view_id) = View view_id BringToFront
to_ui (CmdTitleFocus view_id tracknum) = View view_id (TitleFocus tracknum)

-- | Pull the CmdUpdate out of a UiUpdate, if any.  Discard BringToFront and
-- TitleFocus since they're just instructions to Sync and I don't need to
-- remember them.
to_cmd :: UiUpdate -> Maybe CmdUpdate
to_cmd (Track track_id (TrackEvents s e)) = Just $ CmdTrackEvents track_id s e
to_cmd (Track track_id TrackAllEvents) = Just $ CmdTrackAllEvents track_id
to_cmd (Ruler ruler_id) = Just $ CmdRuler ruler_id
to_cmd _ = Nothing

-- * functions

-- | Updates which purely manipulate the view are not recorded by undo.
is_view_update :: UiUpdate -> Bool
is_view_update update = case update of
    View _ view_update -> case view_update of
        CreateView {} -> False
        DestroyView -> False
        _ -> True
    Block _ block_update -> case block_update of
        BlockConfig _ -> True
        _ -> False
    Track _ track_update -> case track_update of
        TrackBg {} -> True
        TrackRender {} -> True
        _ -> False
    _ -> False

-- | True if this CmdUpdate implies score damage.
is_score_update :: CmdUpdate -> Bool
is_score_update update = case update of
    CmdTrackEvents {} -> True
    CmdTrackAllEvents {} -> True
    CmdRuler {} -> True
    CmdBringToFront {} -> False
    CmdTitleFocus {} -> False

-- | TrackUpdates can overlap.  Merge them together here.  Technically I can
-- also cancel out all TrackUpdates that only apply to newly created views, but
-- this optimization is probably not worth it.
collapse_updates :: [Update t u] -> [Update t u]
collapse_updates updates = collapse tracks ++ rest
    where
    collapse = concatMap (to_track . second mconcat) . Seq.group_fst
    to_track (track_id, range) = case Ranges.extract range of
        Nothing -> [Track track_id TrackAllEvents]
        Just rs -> map (Track track_id . uncurry TrackEvents) rs

    (tracks, rest) = Seq.partition_on track_range updates
    track_range (Track track_id (TrackEvents s e)) =
        Just (track_id, Ranges.range s e)
    track_range (Track track_id TrackAllEvents) =
        Just (track_id, Ranges.everything)
    track_range _ = Nothing

-- | Does an update imply a change which would require rederiving?
track_changed :: UiUpdate -> Maybe (TrackId, Ranges.Ranges ScoreTime)
track_changed (Track tid update) = case update of
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
    View _ (CreateView {}) -> 0
    -- No sense syncing updates to a view that's going to go away, so destroy
    -- it right away.
    View _ DestroyView -> 0
    -- These may change the meaning of TrackNums.  Update TrackNums refer to
    -- the TrackNums of the destination state, except the ones for InsertTrack
    -- and RemoveTrack, of course.
    Block _ (InsertTrack {}) -> 1
    Block _ (RemoveTrack {}) -> 1
    -- Make sure to focus after creating it.
    View _ (TitleFocus {}) -> 10
    _ -> 2
