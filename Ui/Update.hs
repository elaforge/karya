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

import qualified Util.Maps as Maps
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
data UiDamage = UiDamage {
    _views :: Set ViewId
    , _blocks :: Set BlockId
    , _tracks :: Map TrackId (Ranges.Ranges TrackTime)
    , _rulers :: Set RulerId
    , _bring_to_front :: Set ViewId
    -- | If the TrackNum is set, set keyboard focus on that track's title.
    -- Otherwise, focus on the block title.
    , _title_focus :: Maybe (ViewId, Maybe TrackNum)
    } deriving (Eq, Show)

instance Semigroup UiDamage where
    (<>)    (UiDamage v1 b1 t1 r1 bring1 title1)
            (UiDamage v2 b2 t2 r2 bring2 title2) =
        UiDamage (v1<>v2) (b1<>b2) (Maps.mappend t1 t2) (r1<>r2)
            (bring1<>bring2) (title1<|>title2)

instance Monoid UiDamage where
    mempty = UiDamage mempty mempty mempty mempty mempty Nothing
    mappend = (<>)

instance Pretty UiDamage where
    format (UiDamage views blocks tracks rulers bring_to_front title_focus) =
        Pretty.record "UiDamage"
            [ ("views", Pretty.format views)
            , ("blocks", Pretty.format blocks)
            , ("tracks", Pretty.format tracks)
            , ("rulers", Pretty.format rulers)
            , ("bring_to_front", Pretty.format bring_to_front)
            , ("title_focus", Pretty.format title_focus)
            ]

view_damage :: ViewId -> UiDamage
view_damage id = mempty { _views = Set.singleton id }

block_damage :: BlockId -> UiDamage
block_damage id = mempty { _blocks = Set.singleton id }

track_damage :: TrackId -> Ranges.Ranges TrackTime -> UiDamage
track_damage id range = mempty { _tracks = Map.singleton id range }

ruler_damage :: RulerId -> UiDamage
ruler_damage id = mempty { _rulers = Set.singleton id }

data Update t u =
    View ViewId View
    | Block BlockId (Block t)
    | Track TrackId Track
    -- | Since I expect rulers to be changed infrequently, the only kind of
    -- ruler update is a full update.
    | Ruler RulerId
    | State u
    deriving (Eq, Show)

data View =
    CreateView
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

data Block t =
    BlockTitle Text
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

data Track =
    -- | Low pos, high pos.
    TrackEvents ScoreTime ScoreTime
    -- | Update the entire track.
    | TrackAllEvents
    | TrackTitle Text
    | TrackBg Color.Color
    | TrackRender Track.RenderConfig
    deriving (Eq, Show)

-- | These are updates to 'Ui.Ui.State' that have no UI presence.
data State =
    Config UiConfig.Config
    | CreateBlock BlockId Block.Block
    | DestroyBlock BlockId
    | CreateTrack TrackId Track.Track
    | DestroyTrack TrackId
    | CreateRuler RulerId Ruler.Ruler
    | DestroyRuler RulerId
    deriving (Eq, Show)

instance DeepSeq.NFData (Update t u) where
    rnf update = case update of
        View view_id update -> view_id `seq` update `seq` ()
        Block block_id update -> block_id `seq` update `seq` ()
        Track track_id update -> track_id `seq` update `seq` ()
        Ruler ruler_id -> ruler_id `seq` ()
        State u -> u `seq` ()

instance (Pretty t, Pretty u) => Pretty (Update t u) where
    format = \case
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

update_id :: Update t State -> Maybe Id.Id
update_id = \case
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

to_ui :: UiDamage -> [UiUpdate]
to_ui (UiDamage { _tracks, _rulers, _bring_to_front, _title_focus }) = concat
    [ [ Track tid $ maybe TrackAllEvents (uncurry TrackEvents) mb_range
      | (tid, range) <- Map.toList _tracks
      , Just mb_range <- [Ranges.extract1 range]
      ]
    , map Ruler (Set.toList _rulers)
    , map (flip View BringToFront) (Set.toList _bring_to_front)
    , maybe [] (\(vid, tracknum) -> [View vid (TitleFocus tracknum)])
        _title_focus
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
to_damage :: UiUpdate -> UiDamage
to_damage = \case
    View view_id view -> case view of
        -- I can discard BringToFront and TitleFocus because they're just
        -- instructions to Sync and don't indicate UI damage.
        BringToFront {} -> mempty
        TitleFocus {} -> mempty
        _ -> view_damage view_id
    Block block_id _ -> block_damage block_id
    Track track_id track -> track_damage track_id $ case track of
        TrackEvents s e -> Ranges.range s e
        _ -> Ranges.everything
    Ruler ruler_id -> ruler_damage ruler_id
    State state -> case state of
        Config {} -> mempty
        CreateBlock block_id _ -> block_damage block_id
        DestroyBlock block_id -> block_damage block_id
        CreateTrack track_id _ -> track_damage track_id Ranges.everything
        DestroyTrack track_id -> track_damage track_id Ranges.everything
        CreateRuler ruler_id _ -> ruler_damage ruler_id
        DestroyRuler ruler_id -> ruler_damage ruler_id

-- * functions

-- | Updates which purely manipulate the view are not recorded by undo.
is_view_update :: UiUpdate -> Bool
is_view_update = \case
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

-- | True if this UiDamage implies score damage.
is_score_damage :: UiDamage -> Bool
is_score_damage (UiDamage { _tracks, _rulers }) =
    not (Map.null _tracks) || not (Set.null _rulers)

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
sort_key = \case
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
