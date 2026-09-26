-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
{- | Updates are diffs against Ui.State and are used in a number of contexts.
    They are produced by "Ui.Diff".  The different uses all require slightly
    different data, and I capture the major differences in separate types.
-}
module Ui.Update where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Generics as Generics

import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Lists as Lists

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

import           Global
import           Types


-- | 'DisplayUpdate's are sent to the UI to update the windows.  Since the UI
-- only has Views, and has a lower level version of tracks, this includes
-- only updates that directly affect display.
type DisplayUpdate = Update Block.DisplayTrack ()

-- | 'UiUpdate's reflect all changes to the underlying UI state.  They're
-- used for incremental save, and by the deriver to determine ScoreDamage.
type UiUpdate = Update Block.Track State

-- | This collects damaged Ui.State elements, manually added by the various
-- Ui functions.  I use "Ui.Diff" to get the exact changes, but it's too
-- slow to compare the entire Ui.State, so UiDamage is used to restrict the
-- diff to just parts that may have changed.
--
-- There are also a few mutations that correspond directly 'UiUpdate's which
-- I just emit directly rather than relying on diff.  Those are converted from
-- UiDamage by 'to_ui'.
--
-- TODO The indirection for BringToFront, TitleFocus, seems a bit fussy.  Can I
-- just store some Updates in here?
data UiDamage = UiDamage
    { _views :: Set ViewId
    -- | This means there may have been any change at all inside these blocks,
    -- and Diff should check for them.  It doesn't mean it's an interesting
    -- change, that's up to Diff to decide!
    , _blocks :: Set BlockId
    -- | This specifically means there was change on the given tracks.
    , _tracks :: Map TrackId (Ranges.Ranges TrackTime)
    , _rulers :: Set RulerId
    , _bringToFront :: Set ViewId
    -- | If the TrackNum is set, set keyboard focus on that track's title.
    -- Otherwise, focus on the block title.
    , _titleFocus :: Maybe (ViewId, Maybe TrackNum)
    -- | This is purely to trigger 'is_score_damage'.
    -- TODO: Ui.state_config changes should do this so Diff.score_changed
    -- dosen't need to compare UiConfig.Config each time, but modification
    -- happens through lenses...
    , _scoreDamage :: Bool
    } deriving (Eq, Show, Generics.Generic)

instance Semigroup UiDamage where
    (<>)    (UiDamage v1 b1 t1 r1 bring1 title1 sd1)
            (UiDamage v2 b2 t2 r2 bring2 title2 sd2) =
        UiDamage (v1<>v2) (b1<>b2) (Maps.mappend t1 t2) (r1<>r2)
            (bring1<>bring2) (title1<|>title2) (sd1||sd2)

instance Monoid UiDamage where
    mempty = UiDamage mempty mempty mempty mempty mempty Nothing False
    mappend = (<>)

instance Pretty UiDamage where format = Pretty.formatG_

viewDamage :: ViewId -> UiDamage
viewDamage id = mempty { _views = Set.singleton id }

blockDamage :: BlockId -> UiDamage
blockDamage id = mempty { _blocks = Set.singleton id }

trackDamage :: TrackId -> Ranges.Ranges TrackTime -> UiDamage
trackDamage id range = mempty { _tracks = Map.singleton id range }

rulerDamage :: RulerId -> UiDamage
rulerDamage id = mempty { _rulers = Set.singleton id }

data Update t u
    = View ViewId View
    | Block BlockId (Block t)
    | Track TrackId Track
    -- | Since I expect rulers to be changed infrequently, the only kind of
    -- ruler update is a full update.
    | Ruler RulerId
    | State u
    deriving (Eq, Show)

data View
    = CreateView
    | DestroyView
    | ViewSize Rect.Rect
    | Status (Map (Int, Text) Text) Color.Color -- ^ background color
    | TrackScroll Types.Width
    | Zoom Zoom.Zoom
    | Selection Sel.Num (Maybe Sel.Selection)
    -- | Bring the window to the front.  Unlike most other updates, this is
    -- recorded directly and is not reflected in Ui.State.
    | BringToFront
    -- | Similar to BringToFront, but sets keyboard focus in a track title.
    -- If the TrackNum is not given, focus on the block title.
    | TitleFocus (Maybe TrackNum)
    deriving (Eq, Show)

data Block t
    = BlockTitle Text
    | BlockConfig Block.Config
    -- | The second is the \"integrate skeleton\", which is drawn in the same
    -- place.  It could be Skeleton too, but since it never was a skeleton it
    -- seems pointless to convert it to one just so it can be flattened again.
    -- Arguably it's the first arg which should be edges, but at least this way
    -- the two args can't be mixed up.
    | BlockSkeleton Skeleton.Skeleton [(Color.Color, [(TrackNum, TrackNum)])]
    | RemoveTrack TrackNum
    | InsertTrack TrackNum t
    -- | Unlike 'Track', these settings are local to the block, not global to
    -- this track in all its blocks.
    | BlockTrack TrackNum t
    deriving (Eq, Show)

data Track
    -- | Low pos, high pos.
    = TrackEvents ScoreTime ScoreTime
    -- | Update the entire track.
    | TrackAllEvents
    | TrackTitle Text
    | TrackBg Color.Color
    | TrackRender Track.RenderConfig
    deriving (Eq, Show)

-- | These are updates to 'Ui.Ui.State' that have no UI presence.
data State
    = Config UiConfig.Config
    | CreateBlock BlockId Block.Block
    | DestroyBlock BlockId
    | CreateTrack TrackId Track.Track
    | DestroyTrack TrackId
    | CreateRuler RulerId Ruler.Ruler
    | DestroyRuler RulerId
    deriving (Eq, Show)

instance DeepSeq.NFData (Update t u) where
    rnf update = case update of
        View viewId update -> viewId `seq` update `seq` ()
        Block blockId update -> blockId `seq` update `seq` ()
        Track trackId update -> trackId `seq` update `seq` ()
        Ruler rulerId -> rulerId `seq` ()
        State u -> u `seq` ()

instance (Pretty t, Pretty u) => Pretty (Update t u) where
    format = \case
        View viewId update -> Pretty.constructor "View"
            [Pretty.format viewId, Pretty.format update]
        Block blockId update -> Pretty.constructor "Block"
            [Pretty.format blockId, Pretty.format update]
        Track trackId update -> Pretty.constructor "Track"
            [Pretty.format trackId, Pretty.format update]
        Ruler rulerId -> Pretty.constructor "Ruler" [Pretty.format rulerId]
        State update -> Pretty.constructor "State" [Pretty.format update]

instance Pretty View where
    format update = case update of
        CreateView -> "CreateView"
        DestroyView -> "DestroyView"
        ViewSize rect -> Pretty.constructor "ViewSize" [Pretty.format rect]
        Status status isRoot -> Pretty.constructor "Status"
            [Pretty.format status, Pretty.format isRoot]
        TrackScroll width ->
            Pretty.constructor "TrackScroll" [Pretty.format width]
        Zoom zoom -> Pretty.constructor "Zoom" [Pretty.format zoom]
        Selection selnum sel -> Pretty.constructor "Selection"
            [Pretty.format selnum, Pretty.format sel]
        BringToFront -> "BringToFront"
        TitleFocus tracknum ->
            Pretty.constructor "TitleFocus" [Pretty.format tracknum]

instance Pretty t => Pretty (Block t) where
    format update = case update of
        BlockTitle s -> Pretty.constructor "BlockTitle"
            [Pretty.format s]
        BlockConfig config -> Pretty.constructor "BlockConfig"
            [Pretty.format config]
        BlockSkeleton skel intSkel -> Pretty.constructor "BlockSkeleton"
            [Pretty.format skel, Pretty.format intSkel]
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
        CreateBlock blockId _ -> Pretty.constructor "CreateBlock"
            [Pretty.format blockId]
        DestroyBlock blockId -> Pretty.constructor "DestroyBlock"
            [Pretty.format blockId]
        CreateTrack trackId _ -> Pretty.constructor "CreateTrack"
            [Pretty.format trackId]
        DestroyTrack trackId -> Pretty.constructor "DestroyTrack"
            [Pretty.format trackId]
        CreateRuler rulerId _ -> Pretty.constructor "CreateRuler"
            [Pretty.format rulerId]
        DestroyRuler rulerId -> Pretty.constructor "DestroyRuler"
            [Pretty.format rulerId]

updateId :: Update t State -> Maybe Id.Id
updateId = \case
    View viewId _ -> ident viewId
    Block blockId _ -> ident blockId
    Track trackId _ -> ident trackId
    Ruler rulerId -> ident rulerId
    State st -> case st of
        Config {} -> Nothing
        CreateBlock blockId _ -> ident blockId
        DestroyBlock blockId -> ident blockId
        CreateTrack trackId _ -> ident trackId
        DestroyTrack trackId -> ident trackId
        CreateRuler rulerId _ -> ident rulerId
        DestroyRuler rulerId -> ident rulerId
    where
    ident :: Id.Ident a => a -> Maybe Id.Id
    ident = Just . Id.unpackId

-- | Convert a UiUpdate to a DisplayUpdate by stripping out all the UiUpdate
-- parts.
toDisplay :: UiUpdate -> Maybe DisplayUpdate
toDisplay = \case
    View vid update -> Just $ View vid update
    Block bid update -> Block bid <$> case update of
        BlockTitle a -> Just $ BlockTitle a
        BlockConfig a -> Just $ BlockConfig a
        BlockSkeleton a b -> Just $ BlockSkeleton a b
        RemoveTrack {} -> Nothing
        InsertTrack {} -> Nothing
        BlockTrack {} -> Nothing
    Track tid update -> Just $ Track tid update
    Ruler rid -> Just $ Ruler rid
    State {} -> Nothing

toUi :: UiDamage -> [UiUpdate]
toUi (UiDamage { _tracks, _rulers, _bringToFront, _titleFocus }) = concat
    [ [ Track tid $ maybe TrackAllEvents (uncurry TrackEvents) mbRange
      | (tid, range) <- Map.toList _tracks
      , Just mbRange <- [Ranges.extract1 range]
      ]
    , map Ruler (Set.toList _rulers)
    , map (flip View BringToFront) (Set.toList _bringToFront)
    , maybe [] (\(vid, tracknum) -> [View vid (TitleFocus tracknum)])
        _titleFocus
    ]
    -- views and blocks not converted, but they tell diff where to look.

-- | Reduce a UiUpdate to its corresponding UiDamage.  UiUpdates are more
-- specific, so this is discarding information, which I'll have to recover
-- later via Diff.
--
-- This seems silly, and maybe it is.  It's because I originally used only diff
-- for updates, but then added UiDamage to make diff more efficient.  Perhaps
-- I should move entirely to collecting updates and get rid of diff.  But as
-- long as diff is fairly efficient, when directed to the appropriate places
-- via UiDamage, then it still seems less error prone to do the diff.
toDamage :: UiUpdate -> UiDamage
toDamage = \case
    View viewId view -> case view of
        -- I can discard BringToFront and TitleFocus because they're just
        -- instructions to Sync and don't indicate UI damage.
        BringToFront {} -> mempty
        TitleFocus {} -> mempty
        _ -> viewDamage viewId
    Block blockId _ -> blockDamage blockId
    Track trackId track -> trackDamage trackId $ case track of
        TrackEvents s e -> Ranges.range s e
        _ -> Ranges.everything
    Ruler rulerId -> rulerDamage rulerId
    State state -> case state of
        Config {} -> mempty
        CreateBlock blockId _ -> blockDamage blockId
        DestroyBlock blockId -> blockDamage blockId
        CreateTrack trackId _ -> trackDamage trackId Ranges.everything
        DestroyTrack trackId -> trackDamage trackId Ranges.everything
        CreateRuler rulerId _ -> rulerDamage rulerId
        DestroyRuler rulerId -> rulerDamage rulerId

-- * functions

-- | Updates which purely manipulate the view are not recorded by undo.
isViewUpdate :: UiUpdate -> Bool
isViewUpdate = \case
    View _ viewUpdate -> case viewUpdate of
        CreateView {} -> False
        DestroyView -> False
        _ -> True
    Block _ blockUpdate -> case blockUpdate of
        BlockConfig _ -> True
        _ -> False
    Track _ trackUpdate -> case trackUpdate of
        TrackBg {} -> True
        TrackRender {} -> True
        _ -> False
    _ -> False

-- | Score damage is used by Diff.score_changed to determine if a change has
-- happened which is worthy of saving to disk, or recording for undo.
isScoreDamage :: UiDamage -> Bool
isScoreDamage (UiDamage { _tracks, _rulers, _scoreDamage }) =
    -- This doesn't check _blocks, because those happen on any block change at
    -- all, which includes UI-only things like Block.config_skel_box.
    _scoreDamage || not (Map.null _tracks) || not (Set.null _rulers)

-- | Does an update imply a change which would require rederiving?
trackChanged :: UiUpdate -> Maybe (TrackId, Ranges.Ranges ScoreTime)
trackChanged (Track tid update) = case update of
    TrackEvents start end -> Just (tid, Ranges.range start end)
    TrackAllEvents -> Just (tid, Ranges.everything)
    TrackTitle _ -> Just (tid, Ranges.everything)
    _ -> Nothing
trackChanged _ = Nothing

-- | Some Updates have to happen before others.
sort :: [DisplayUpdate] -> [DisplayUpdate]
sort = Lists.sortOn sortKey

sortKey :: DisplayUpdate -> Int
sortKey = \case
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
