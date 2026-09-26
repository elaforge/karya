-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Block (
    -- * Block
    Block(..)
    , Skeleton(..)
    , Meta, TrackDestinations(..), ScoreDestinations, NoteDestination(..)
    , destTrackIds, noteDestTrackIds, emptyDestination
    , Source(..), destinationToSource
    , ManualDestinations, SourceKey
    , EventIndex, shortEventIndex
    , integrateSkeleton
    , blockTracklikeIds, blockTrackIds, blockRulerIds
    , block
    , Config(..), defaultConfig
    , Box(..)
    -- * Track
    , Track(..), trackId, track
    , modifyId
    , divider
    , isCollapsed
    , trackSelectable, trackWantsSignal
    -- ** DisplayTrack
    , DisplayTrack(..), Status(..), emptyStatus, TrackFlag(..)
    , blockDisplayTracks
    , displayTrackWidth
    , flagsToStatus, flagChar
    -- ** TracklikeId
    , TracklikeId(..)
    , trackIdOf, trackIdsOf, rulerIdOf, rulerIdsOf
    , setRulerId
    , Tracklike(..)
    , trackOf, tracksOf, rulerOf, rulersOf
    , Divider(..)
    -- * View
    , View(..)
    , view
    , Padding(..)
    , statusColor, showStatus
    , visibleTrack, visibleTime
    , trackRect, setTrackRect
    , viewVisibleTime, viewVisibleTrack
    , screenPixels
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified GHC.Generics as Generics

import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect

import qualified App.Config as Config
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Zoom as Zoom

import           Global
import           Types


-- * block

-- | This is the data behind a single block.
data Block = Block
    { blockTitle :: !Text
    , blockConfig :: !Config
    , blockTracks :: ![Track]
    , blockSkeleton :: !Skeleton.Skeleton
    -- | Present if this block was integrated from another.  If the
    -- TrackDestinations is empty, then this is an empty block that was created
    -- to receive integration.
    , blockIntegrated :: !(Maybe (BlockId, TrackDestinations))

    -- | Each pair is a set of tracks integrated from a source TrackId.
    -- A single source can have multiple destination sets.  Similar to
    -- block_integrated, if the TrackDestinations is empty, then new integrated
    -- tracks should be created.
    , blockIntegratedTracks :: ![(TrackId, TrackDestinations)]
    , blockIntegratedManual :: !ManualDestinations
    , blockMeta :: !Meta
    } deriving (Eq, Show, Generics.Generic)

instance Pretty Block where format = Pretty.formatG_

instance DeepSeq.NFData Block where
    rnf = DeepSeq.rnf . blockTitle

data Skeleton = Explicit | Implicit
    deriving (Eq, Show)
instance Pretty Skeleton where pretty = showt

{- | Block metadata is extra data that doesn't affect normal derivation, but
    may be of interest to cmds.

    Previously this was used to mark blocks for automatic lilypond derivation,
    currently tscore uses it to mark sub-blocks.
-}
type Meta = Map Text Text

data Source
    -- | Integrated from a manually-invoked integration.  A single SourceKey
    -- can have destinations on multiple tracks within the block.
    = ManualSource !SourceKey
    | TrackSource !TrackId
    | BlockSource !BlockId
    deriving (Eq, Ord, Show)

instance Pretty Source where
    pretty (ManualSource key) = key
    pretty (TrackSource tid) = pretty tid
    pretty (BlockSource bid) = pretty bid

-- | TODO I intended to unify all the integration muddle into one dest to
-- source map, and maybe I stil can, but meanwhile, here's what it could look
-- like.
destinationToSource :: Block -> [(TrackId, (Source, EventIndex))]
destinationToSource block = concat
    [ case blockIntegrated block of
        Nothing -> []
        Just (sourceBlock, tdests) ->
            trackDests (BlockSource sourceBlock) tdests
    , integratedTracks (blockIntegratedTracks block)
    , integratedManual (blockIntegratedManual block)
    ]
    where
    integratedTracks itracks = concat
        [ trackDests (TrackSource sourceTrack) tdests
        | (sourceTrack, tdests) <- itracks
        ]
    integratedManual manual = concat
        [ concatMap (noteDest (ManualSource sourceKey)) ndests
        | (sourceKey, ndests) <- Map.toList manual
        ]
    trackDests source (DeriveDestinations ndests) =
        concatMap (noteDest source) ndests
    trackDests _source (ScoreDestinations sdests) =
        [ (destTrack, (TrackSource sourceTrack, index))
        | (sourceTrack, (destTrack, index)) <- sdests
        ]
    noteDest source (NoteDestination _key note controls) =
        (fst note, (source, snd note))
            : [(tid, (source, index)) | (tid, index) <- Map.elems controls]

data TrackDestinations
    -- | A derive integrate can produce multiple note tracks, and each one gets
    -- its own NoteDestination.  The TrackIds in here should point to tracks
    -- within the block that contains the TrackDestinations.
    = DeriveDestinations ![NoteDestination]
    -- | A score integrate is always just one track along with its descendents.
    -- It's not necessarily a note track.
    | ScoreDestinations !ScoreDestinations
    deriving (Eq, Show)

destTrackIds :: TrackDestinations -> [TrackId]
destTrackIds = \case
    DeriveDestinations dests -> concatMap noteDestTrackIds dests
    ScoreDestinations dests -> map (fst . snd) dests

{- | Score derivation creates destination tracks 1:1 with their source tracks,
    so I can key them with their source TrackIds.

    (source_track_id, (destination_track_id, index))

    For 'block_integrated', the source_track_id should point to a track in the
    source block.  For 'block_integrated_tracks' it should point to a track
    within its own block.  The destination_track_id should always point to
    a track in the same block.
-}
type ScoreDestinations = [(TrackId, (TrackId, EventIndex))]

-- | Destinations for a manually-invoked integration.  A single SourceKey
-- can have destinations on multiple tracks within the block.
type ManualDestinations = Map SourceKey [NoteDestination]

-- | Arbitrary text used to identify the source of this integration.  Unlike
-- automatic integrations, which come from another block or track, a manual
-- integration can bring in events from anywhere, so it needs a unique key
-- to identify the source.
type SourceKey = Text

instance Pretty TrackDestinations where
    format (DeriveDestinations dests) =
        "DeriveDestinations" Pretty.<+> Pretty.format dests
    format (ScoreDestinations dests) =
        "ScoreDestinations" Pretty.<+> Pretty.format dests

-- | This holds the 'EventIndex' for one note track, along with its dependent
-- control tracks.
data NoteDestination = NoteDestination
    -- | The key should uniquely identify this particular destination.  The next
    -- time there is a merge, the source tracks can match by key.  Otherwise,
    -- they just have to blindly zip sources and dests, and can't detect
    -- deletions or moves.
    { destKey :: !Text
    -- | (dest_track, index)
    , destNote :: !(TrackId, EventIndex)
    -- | Map from control name to the track which was created for it.
    , destControls :: !(Map Text (TrackId, EventIndex))
    } deriving (Eq, Show)

noteDestTrackIds :: NoteDestination -> [TrackId]
noteDestTrackIds (NoteDestination _ note controls) =
    fst note : map fst (Map.elems controls)

-- | Create an empty destination for the first integration.
emptyDestination :: Text -> TrackId -> [(Text, TrackId)] -> NoteDestination
emptyDestination key note controls = NoteDestination
    { destKey = key
    , destNote = (note, mempty)
    , destControls = Map.fromList $ map (second (,mempty)) controls
    }

-- | This is a picture of the integrated events that were used to create an
-- integrated block or track.  By taking its difference against the current
-- contents of the block I can figure out user edits.
type EventIndex = Map Event.IndexKey Event.Event

shortEventIndex :: EventIndex -> Text
shortEventIndex index
    | Map.null index = "((empty))"
    | otherwise = mconcat
        [ "((index size:", showt (Map.size index)
        , " ", pretty (fst (Map.findMin index))
        , "--", pretty (fst (Map.findMax index))
        , "))"
        ]

instance Pretty NoteDestination where
    format (NoteDestination key note controls) = Pretty.record "NoteDestination"
        [ ("key", Pretty.format key)
        , ("note", Pretty.format note)
        , ("controls", Pretty.format controls)
        ]

-- | Arrows that should be drawn to indicate integrate relationships.
integrateSkeleton :: Block -> [(Color.Color, [(TrackNum, TrackNum)])]
integrateSkeleton block = map integrateEdges (blockIntegratedTracks block)
    where
    integrateEdges (sourceId, ScoreDestinations dests) =
        (,) Config.scoreIntegrateSkeleton $ maybe [] (:[]) $ do
            (_, (destId, _)) <- Lists.head dests
            dest <- tracknumOf destId
            source <- tracknumOf sourceId
            pure (source, dest)
    integrateEdges (sourceId, DeriveDestinations dests) =
        (Config.integrateSkeleton, edgesOf sourceId dests)
    edgesOf sourceId dests = do
        source <- maybe [] (:[]) $ tracknumOf sourceId
        dest <- mapMaybe (tracknumOf . fst . destNote) dests
        pure (source, dest)
    tracknumOf = flip Map.lookup tracknums
    tracknums = Map.fromList
        [ (trackId, tracknum)
        | (tracknum, Just trackId) <- zip [0..]
            (map trackId (blockTracks block))
        ]

blockTracklikeIds :: Block -> [TracklikeId]
blockTracklikeIds = map tracklikeId . blockTracks

blockTrackIds :: Block -> [TrackId]
blockTrackIds = trackIdsOf . blockTracklikeIds

blockRulerIds :: Block -> [RulerId]
blockRulerIds = rulerIdsOf . blockTracklikeIds

block :: Config -> Text -> [Track] -> Block
block config title tracks = Block
    { blockTitle = title
    , blockConfig = config
    , blockTracks = tracks
    , blockSkeleton = Skeleton.empty
    , blockIntegrated = Nothing
    , blockIntegratedTracks = []
    , blockIntegratedManual = mempty
    , blockMeta = Map.empty
    }

-- | Per-block configuration.
data Config = Config
    { configSkelBox :: !Box
    , configTrackBox :: !Box
    , configSbBox :: !Box
    , configSkeleton :: !Skeleton
    } deriving (Eq, Show, Generics.Generic)

defaultConfig :: Config
defaultConfig = Config
    { configSkelBox = box Config.bconfigBox
    , configTrackBox = box Config.bconfigBox
    , configSbBox = box Config.bconfigBox
    , configSkeleton = Implicit
    } where box = uncurry Box

instance Pretty Config where format = Pretty.formatG_

-- | One of those colored boxes wedged into the corners of the block window.
data Box = Box { boxColor :: !Color.Color, boxChar :: !Char }
    deriving (Eq, Show)

instance Pretty Box where
    pretty (Box color c) =
        pretty color <> if c == ' ' then "" else " '" <> Text.singleton c <> "'"

-- | Like 'Track.Track', this has per-track data, but unlike Track.Track,
-- this is data that can vary per-block.
--
-- This is the higher level track that is visible at the haskell level, as
-- opposed to 'DisplayTrack', which is what is visible in the UI.
data Track = Track
    { tracklikeId :: !TracklikeId
    -- | Formerly the width was in the view since each view could have
    -- a different width and this was just the default width, but that turned
    -- out to be too much of a hassle, so now all occurences of a track have
    -- the same width.
    , trackWidth :: !Types.Width
    -- | This is a width suggested by fltk that should be enough to show all
    -- the text.  Unlike the other fields in Track, it's derived from the track
    -- contents and is automatically updated by the UI.
    , trackSuggestedWidth :: !Types.Width
    -- | Track display state flags.
    , trackFlags :: !(Set TrackFlag)
    -- | Other tracks are displayed behind this one.  Useful to merge a pitch
    -- track into its note track.
    , trackMerged :: !(Set TrackId)
    } deriving (Eq, Show)

trackId :: Track -> Maybe TrackId
trackId = trackIdOf . tracklikeId

instance Pretty Track where
    pretty (Track tid width suggested flags merged) = pretty tid <> ": "
        <> Text.unwords
            [pretty width, pretty suggested, pretty flags, pretty merged]

-- | Construct a 'Track' with defaults.
track :: TracklikeId -> Types.Width -> Track
track tracklikeId width = Track
    { tracklikeId = tracklikeId
    , trackWidth = width
    , trackSuggestedWidth = width
    , trackFlags = mempty
    , trackMerged = mempty
    }

modifyId :: (TracklikeId -> TracklikeId) -> Track -> Track
modifyId f track = track { tracklikeId = f (tracklikeId track) }

coloredDivider :: Color.Color -> Track
coloredDivider color = track (DId (Divider color)) 3

-- | A generic divider to insert manually as a visual aid.  Imitates the
-- background color.
divider :: Track
divider = coloredDivider (Color.rgb 0.8 0.8 0.8)

isCollapsed :: Set TrackFlag -> Bool
isCollapsed flags = Set.member Collapse flags || Set.member Merge flags

trackSelectable :: Track -> Bool
trackSelectable track@(Track { tracklikeId = TId _ _}) =
    not $ isCollapsed (trackFlags track)
trackSelectable _ = False

-- | Don't send a track signal to a track unless it actually wants to draw it.
trackWantsSignal :: Set TrackFlag -> Track.Track -> Bool
trackWantsSignal flags track =
    Track.renderStyle (Track.trackRender track) /= Track.NoRender
    && Collapse `Set.notMember` flags
    && Merge `Set.notMember` flags

-- | This is the low-level representation of a track, which directly
-- corresponds with what is displayed by the UI.  The DisplayTracks should be
-- derivable from a 'Block' deterministically.
data DisplayTrack = DisplayTrack
    { dtracklikeId :: !TracklikeId
    , dtrackWidth :: !Types.Width
    , dtrackMerged :: !(Set TrackId)
    , dtrackStatus :: !Status
    , dtrackEventBrightness :: !Double
    } deriving (Eq, Show)

instance Pretty DisplayTrack where
    format (DisplayTrack tlikeId width merged status _bright) =
        Pretty.record "DisplayTrack"
            [ ("tracklike_id", Pretty.format tlikeId)
            , ("width", Pretty.format width)
            , ("merged", Pretty.format merged)
            , ("status", Pretty.format status)
            ]

-- | This has a 2 character string to display above the track, and a background
-- color.  TODO change to Char Char
data Status = Status !String !Color.Color
    deriving (Eq, Show)

instance Pretty Status where pretty (Status cs color) = pretty (cs, color)

emptyStatus :: Status
emptyStatus = Status "" Color.black

-- | Most of these only make sense for event tracks.
data TrackFlag
    -- | Track is collapsed to take up less space.
    = Collapse
    | Merge
    -- | UI shows solo indication.  If any tracks are soloed on a block, only
    -- those tracks are played.
    | Solo
    -- | UI shows muted indication, player should filter out events from this
    -- track.
    | Mute
    -- | This is like Mute, except that the track is entirely omitted from
    -- derivation.  Since Mute and Solo work after derivation, they don't
    -- require a rederive but also can't mute a single control track.
    | Disable
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Pretty TrackFlag where pretty = showt

-- | Convert logical block level tracks to display tracks.
blockDisplayTracks :: Block -> [DisplayTrack]
blockDisplayTracks = joinCollapsed . map displayTrack . blockTracks

-- | Merge consecutive collapsed tracks.
joinCollapsed :: [DisplayTrack] -> [DisplayTrack]
joinCollapsed = id
-- TODO I tried this and it looks nice but I'd need to also eliminate the
-- tracks from a Block.display_skeleton and Block.display_integrate_skeleton
-- and I'm not sure it's worth it.
-- join_collapsed = mapMaybe Lists.head
--     . List.groupBy (\a b -> a == collapsed_track && b == collapsed_track)

-- | This is not exported so callers are forced to go through
-- 'block_display_tracks'.
displayTrack :: Track -> DisplayTrack
displayTrack track
    | hasFlag Merge = mergedTrack
    | hasFlag Collapse = collapsedTrack
    | otherwise = DisplayTrack
        { dtracklikeId = tracklikeId track
        , dtrackWidth = trackWidth track
        , dtrackMerged = trackMerged track
        , dtrackStatus = status
        , dtrackEventBrightness = brightness
        }
    where
    (status, brightness) = flagsToStatus (trackFlags track)
    hasFlag flag = Set.member flag (trackFlags track)

-- | Collapsed tracks are replaced with a divider.
collapsedTrack :: DisplayTrack
collapsedTrack = DisplayTrack
    { dtracklikeId = DId $ Divider $
        Color.brightness 0.5 Config.abbreviationColor
    , dtrackWidth = Config.collapsedWidth -- TODO maybe a bit wider?
    , dtrackMerged = mempty
    , dtrackStatus = emptyStatus
    , dtrackEventBrightness = 1
    }

mergedTrack :: DisplayTrack
mergedTrack = DisplayTrack
    { dtracklikeId = DId (Divider Config.abbreviationColor)
    , dtrackWidth = Config.collapsedWidth
    , dtrackMerged = mempty
    , dtrackStatus = emptyStatus
    , dtrackEventBrightness = 1
    }

displayTrackWidth :: Track -> Types.Width
displayTrackWidth = dtrackWidth . displayTrack

flagsToStatus :: Set TrackFlag -> (Status, Double)
flagsToStatus flags
    | Disable `Set.member` flags =
        (Status (chars Disable) Config.muteColor, 0.7)
    | Solo `Set.member` flags = (Status (chars Solo) Config.soloColor, 1)
    | Mute `Set.member` flags = (Status (chars Mute) Config.muteColor, 0.85)
    | otherwise = (emptyStatus, 1)
    where
    chars flag = filter (/=' ') $ flagChar flag
        : map (Char.toLower . flagChar) (Set.toList (Set.delete flag flags))

flagChar :: TrackFlag -> Char
flagChar status = case status of
    Disable -> 'D'
    Solo -> 'S'
    Mute -> 'M'
    Collapse -> ' '
    Merge -> ' '

data TracklikeId
    -- | Tracks may have a Ruler overlay
    = TId TrackId RulerId
    | RId RulerId
    | DId Divider
    deriving (Eq, Ord, Show)

instance Pretty TracklikeId where
    pretty tlikeId = case tlikeId of
        TId tid rid -> pretty tid <> "/" <> pretty rid
        RId rid -> pretty rid
        DId divider -> showt divider

trackIdOf :: TracklikeId -> Maybe TrackId
trackIdOf (TId tid _) = Just tid
trackIdOf _ = Nothing

trackIdsOf :: [TracklikeId] -> [TrackId]
trackIdsOf = mapMaybe trackIdOf

rulerIdOf :: TracklikeId -> Maybe RulerId
rulerIdOf (TId _ rid) = Just rid
rulerIdOf (RId rid) = Just rid
rulerIdOf _ = Nothing

rulerIdsOf :: [TracklikeId] -> [RulerId]
rulerIdsOf = mapMaybe rulerIdOf

setRulerId :: RulerId -> TracklikeId -> TracklikeId
setRulerId rid (TId tid _) = TId tid rid
setRulerId rid (RId _) = RId rid
setRulerId _ t = t

data Tracklike
    = T Track.Track Ruler.Ruler
    | R Ruler.Ruler
    | D Divider
    deriving (Show)

trackOf :: Tracklike -> Maybe Track.Track
trackOf (T track _) = Just track
trackOf _ = Nothing

tracksOf :: [Tracklike] -> [Track.Track]
tracksOf = mapMaybe trackOf

rulerOf :: Tracklike -> Maybe Ruler.Ruler
rulerOf (T _ ruler) = Just ruler
rulerOf (R ruler) = Just ruler
rulerOf _ = Nothing

rulersOf :: [Tracklike] -> [Ruler.Ruler]
rulersOf = mapMaybe rulerOf

-- | A divider separating tracks.  Defined here in Block since it's so trivial.
newtype Divider = Divider Color.Color deriving (Eq, Ord, Show)

-- * block view

-- | A view is a single window on screen.  Views are destroyed when the window
-- is closed.
data View = View
    -- | view_block should never change.
    { viewBlock :: !BlockId
    , viewRect :: !Rect.Rect
    , viewPadding :: !Padding
    -- | Contents of the status line.  Map (sort_order, name) contents.  This
    -- is formatted with 'show_status' and goes in the bar at the bottom of the
    -- window.
    , viewStatus :: !(Map (Int, Text) Text)
    , viewTrackScroll :: !Types.Width
    , viewZoom :: !Zoom.Zoom
    , viewSelections :: !(Map Sel.Num Sel.Selection)
    } deriving (Eq, Ord, Show)

instance Pretty View where
    format (View block rect padding status tscroll zoom sels) =
        Pretty.record "View"
            [ ("block", Pretty.format block)
            , ("rect", Pretty.format rect)
            , ("padding", Pretty.format padding)
            , ("status", Pretty.format status)
            , ("scroll/zoom", Pretty.format (tscroll, zoom))
            , ("selections", Pretty.format sels)
            ]

instance DeepSeq.NFData View where
    rnf (View bid rect padding status scroll zoom selections) =
        bid `seq` rect `seq` padding `seq` status `seq` scroll
        `seq` zoom `seq` selections `seq` ()

-- | Construct a View, using default values for most of its fields.
-- Don't construct views using View directly since 'State.create_view'
-- overwrites view_tracks, and maybe more in the future.
view :: Block -> BlockId -> Rect.Rect -> Zoom.Zoom -> View
view block blockId rect zoom = View
    { viewBlock = blockId
    , viewRect = rect
    -- These will be filled in when the new view emits its initial resize msg,
    -- but it should start off with the defaults.
    , viewPadding = defaultPadding block
    , viewStatus = Map.empty
    , viewTrackScroll = 0
    , viewZoom = zoom
    , viewSelections = Map.empty
    }

-- | Pixel width and height of stuff in the view that is not the track area,
-- i.e. scrollbars, skeleton display, block title, etc.
--
-- Only fltk knows the width of all the various widgets, but it's cached
-- here so pure code doesn't have to call to the UI and import BlockC.
-- Only 'UiMsg.UpdateViewResize' should set this, which in turn comes from
-- MsgCollector::msg_resize, which in turn comes from Block::get_padding.
data Padding = Padding
    { left :: !Int
    , top :: !Int
    , bottom :: !Int
    -- Right padding is not necessary, because I can add up track widths.
    } deriving (Eq, Ord, Show)

instance Pretty Padding where
    pretty (Padding left top bottom) = pretty (left, top, bottom)

defaultPadding :: Block -> Padding
defaultPadding block = Padding
    { left = Config.viewLeftPadding
    , top = Config.viewTopPadding
        + if Text.null (blockTitle block) then 0 else Config.blockTitleHeight
    , bottom = Config.viewBottomPadding
    }

-- | Figure out what color the background of the status line should be.
statusColor :: BlockId -> Block -> Maybe BlockId -> Color.Color
statusColor blockId block maybeRootId
    | Just blockId == maybeRootId = Config.statusRoot
    | Just (_, dests) <- blockIntegrated block = case dests of
        DeriveDestinations {} -> Config.statusIntegrateDestination
        ScoreDestinations {} -> Config.statusScoreIntegrateDestination
    | otherwise = Config.statusDefault

showStatus :: Map (Int, Text) Text -> Text
showStatus = Text.intercalate " | " . map snd . Map.toAscList
    -- I used to display the keys, but they take up too much space, so now
    -- I just use them for sorting.

-- | Return how much track is in view.
visibleTime :: View -> TrackTime
visibleTime view = Zoom.toTime (viewZoom view) (viewVisibleTime view)

visibleTrack :: View -> Types.Width
visibleTrack = viewVisibleTrack

-- | Get the rect of the track area within the view.  This is used with
-- 'set_track_rect' to set the size of the visible track area, minus the
-- extra GUI bits around it.
trackRect :: View -> Rect.Rect
trackRect (View { viewRect = rect, viewPadding = padding }) = Rect.Rect
    { x = Rect.x rect + left padding
    , y = Rect.y rect + top padding
    , w = Rect.w rect - left padding
    , h = Rect.h rect - top padding - bottom padding
    }

-- | The inverse of 'track_rect'  Use this to set the track area area to
-- a certain size.
setTrackRect :: View -> Rect.Rect -> Rect.Rect
setTrackRect (View { viewPadding = padding }) rect = Rect.Rect
    { x = Rect.x rect - left padding
    , y = Rect.y rect - top padding
    , w = Rect.w rect + left padding
    , h = Rect.h rect + top padding + bottom padding
    }

viewVisibleTrack, viewVisibleTime :: View -> Int
viewVisibleTrack view = Rect.w (viewRect view) - left (viewPadding view)
viewVisibleTime view = Rect.h (viewRect view) - top padding - bottom padding
    where padding = viewPadding view

-- | Y coordinate of the given TrackTime.
screenPixels :: View -> TrackTime -> Int
screenPixels view t =
    Zoom.toPixels (viewZoom view) (t - Zoom.offset (viewZoom view))
    + top (viewPadding view) + Rect.y (viewRect view)
