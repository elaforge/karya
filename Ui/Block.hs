{-# LANGUAGE DeriveDataTypeable #-}
module Ui.Block where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Generics as Generics
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified App.Config as Config
import Types


-- * block model

data Block = Block {
    block_title :: !String
    , block_config :: !Config
    , block_tracks :: ![Track]
    , block_skeleton :: !Skeleton.Skeleton
    -- | Present if this block was integrated from another.
    , block_integrated :: !(Maybe (BlockId, [TrackDestination]))
    -- | [(source_track, destinations)]
    , block_integrated_tracks :: ![(TrackId, [TrackDestination])]
    , block_meta :: !Meta
    } deriving (Eq, Read, Show)

instance Pretty.Pretty Block where
    format (Block title _config tracks skel integrated integrated_tracks meta) =
        Pretty.record_title "Block"
            [ ("title", Pretty.format title)
            , ("tracks", Pretty.format tracks)
            , ("skel", Pretty.format skel)
            , ("integrated", Pretty.format integrated)
            , ("integrated_tracks", Pretty.format integrated_tracks)
            , ("meta", Pretty.format meta)
            ]

instance DeepSeq.NFData Block where
    rnf = DeepSeq.rnf . block_title

-- | Block metadata is extra data that doesn't affect normal derivation, but
-- may be of interest to cmds.  For instance, it can mark if this block should
-- be rendered to lilypond and provide arguments for it.
type Meta = Map.Map String String

data TrackDestination = TrackDestination {
    dest_note :: !(TrackId, EventIndex)
    , dest_controls :: !(Map.Map String (TrackId, EventIndex))
    } deriving (Eq, Show, Read)

instance Pretty.Pretty TrackDestination where
    format (TrackDestination note controls) =
        Pretty.format (fst note, Map.map fst controls)


-- If a track was deleted, then its track id will be missing.
-- Since merge recreates the track, then it will always be the same as convert
-- output.

-- | This is a picture of the integrated events that were used to create an
-- integrated block.  By taking its difference against the current contents of
-- the block I can figure out user edits.
type EventIndex = Map.Map Event.Stack Events.PosEvent

block_tracklike_ids :: Block -> [TracklikeId]
block_tracklike_ids = map tracklike_id . block_tracks

block_track_ids :: Block -> [TrackId]
block_track_ids = track_ids_of . block_tracklike_ids

block_ruler_ids :: Block -> [RulerId]
block_ruler_ids = ruler_ids_of . block_tracklike_ids

block :: Config -> String -> [Track] -> Block
block config title tracks = Block
    { block_title = title
    , block_config = config
    , block_tracks = tracks
    , block_skeleton = Skeleton.empty
    , block_integrated = Nothing
    , block_integrated_tracks = []
    , block_meta = Map.empty
    }

-- | Per-block configuration.
data Config = Config {
    config_skel_box :: !Box
    , config_track_box :: !Box
    , config_sb_box :: !Box
    } deriving (Eq, Show, Read)

default_config :: Config
default_config = Config
    (box Config.bconfig_box) (box Config.bconfig_box) (box Config.bconfig_box)
    where box = uncurry Box

instance Pretty.Pretty Config where
    format (Config skel track sb) = Pretty.constructor "Config"
        [Pretty.format skel, Pretty.format track, Pretty.format sb]

data Box = Box { box_color :: !Color.Color, box_char :: !Char }
    deriving (Eq, Show, Read)

instance Pretty.Pretty Box where
    pretty (Box color c) = Pretty.pretty color ++ if c == ' ' then [] else [c]

-- | Like 'Track.Track', this has per-track data, but unlike Track.Track,
-- this is data that can vary per-block.
--
-- This is the higher level track that is visible at the haskell level.
data Track = Track {
    tracklike_id :: TracklikeId
    -- | The current width is in the View, but this width is a default if
    -- a new View is created from this Block.
    , track_width :: Types.Width
    -- | Track display state flags.
    , track_flags :: [TrackFlag]
    -- | Other tracks are displayed behind this one.  Useful to merge a pitch
    -- track into its note track.
    , track_merged :: [TrackId]
    } deriving (Eq, Show, Read, Generics.Typeable)

instance Pretty.Pretty Track where
    pretty (Track tid width flags merged) =
        Pretty.pretty tid ++ ": " ++ unwords
            [Pretty.pretty width, Pretty.pretty flags, Pretty.pretty merged]

-- | Construct a 'Track' with defaults.
track :: TracklikeId -> Types.Width -> Track
track tracklike_id width = Track tracklike_id width [] []

track_collapsed :: Track -> Bool
track_collapsed = (Collapse `elem`) . track_flags

-- | This is the low-level representation of a track, which directly
-- corresponds with what is displayed by the UI.  The DisplayTracks should be
-- derivable from a 'Block' deterministically.
data DisplayTrack = DisplayTrack {
    dtracklike_id :: TracklikeId
    , dtrack_width :: Types.Width
    , dtrack_merged :: [TrackId]
    , dtrack_status :: Maybe (Char, Color.Color)
    , dtrack_event_brightness :: Double
    } deriving (Eq, Show, Read)

instance Pretty.Pretty DisplayTrack where
    pretty = show

-- | Most of these only make sense for event tracks.
data TrackFlag =
    -- | Track is collapsed to take up less space.
    Collapse
    -- | UI shows solo indication.  If any tracks are soloed on a block, only
    -- those tracks are derived.
    | Solo
    -- | UI shows muted indication, deriver should skip this track.
    | Mute
    deriving (Eq, Show, Read)

instance Pretty.Pretty TrackFlag where pretty = show

-- | Convert logical block level tracks to display tracks.
block_display_tracks :: Block -> [DisplayTrack]
block_display_tracks = map display_track . block_tracks

display_track :: Track -> DisplayTrack
display_track track =
    DisplayTrack tracklike width (track_merged track) status brightness
    where
    (status, brightness) = flags_to_status (track_flags track)
    (tracklike, width)
        | track_collapsed track =
            (DId (Divider Config.abbreviation_color), Config.collapsed_width)
        | otherwise = (tracklike_id track, track_width track)

display_track_width :: Track -> Types.Width
display_track_width = dtrack_width . display_track

flags_to_status :: [TrackFlag] -> (Maybe (Char, Color.Color), Double)
flags_to_status flags
    | Solo `elem` flags = (Just ('S', Config.solo_color), 1)
    | Mute `elem` flags = (Just ('M', Config.mute_color), 0.75)
    | otherwise = (Nothing, 1)

modify_id :: (TracklikeId -> TracklikeId) -> Track -> Track
modify_id f track = track { tracklike_id = f (tracklike_id track) }

data TracklikeId =
    -- | Tracks may have a Ruler overlay
    TId TrackId RulerId
    | RId RulerId
    | DId Divider
    deriving (Eq, Show, Read)

instance Pretty.Pretty TracklikeId where
    pretty tlike_id = case tlike_id of
        TId tid rid -> Pretty.pretty tid ++ "/" ++ Pretty.pretty rid
        RId rid -> Pretty.pretty rid
        DId divider -> show divider

track_id_of :: TracklikeId -> Maybe TrackId
track_id_of (TId tid _) = Just tid
track_id_of _ = Nothing

track_ids_of :: [TracklikeId] -> [TrackId]
track_ids_of = Maybe.mapMaybe track_id_of

ruler_id_of :: TracklikeId -> Maybe RulerId
ruler_id_of (TId _ rid) = Just rid
ruler_id_of (RId rid) = Just rid
ruler_id_of _ = Nothing

ruler_ids_of :: [TracklikeId] -> [RulerId]
ruler_ids_of = Maybe.mapMaybe ruler_id_of

set_rid :: RulerId -> TracklikeId -> TracklikeId
set_rid rid (TId tid _) = TId tid rid
set_rid rid (RId _) = RId rid
set_rid _ t = t

data Tracklike =
    T Track.Track Ruler.Ruler
    | R Ruler.Ruler
    | D Divider
    deriving (Show)

track_of :: Tracklike -> Maybe Track.Track
track_of (T track _) = Just track
track_of _ = Nothing

tracks_of :: [Tracklike] -> [Track.Track]
tracks_of = Maybe.mapMaybe track_of

ruler_of :: Tracklike -> Maybe Ruler.Ruler
ruler_of (T _ ruler) = Just ruler
ruler_of (R ruler) = Just ruler
ruler_of _ = Nothing

rulers_of :: [Tracklike] -> [Ruler.Ruler]
rulers_of = Maybe.mapMaybe ruler_of

-- | A divider separating tracks.
-- Defined here in Block since it's so trivial.
newtype Divider = Divider Color.Color deriving (Eq, Ord, Show, Read)

-- * block view

data View = View {
    -- | view_block should never change.
    -- TODO Views that point to a BlockId not in state_blocks should be
    -- destroyed.
    view_block :: BlockId
    , view_rect :: Rect.Rect

    -- | Pixel width and height of the track area of the view, i.e. view_rect
    -- with scrollbars and other things subtracted.
    --
    -- These two are derived from view_rect, but only fltk knows the width of
    -- all the various widgets.  It's cached here so pure code doesn't have
    -- to call to the UI and import BlockC.
    , view_visible_track :: Int
    , view_visible_time :: Int
    , view_status :: Map.Map String String

    -- | Scroll and zoom
    , view_track_scroll :: Types.Width
    , view_zoom :: Types.Zoom

    , view_selections :: Map.Map Types.SelNum Types.Selection
    } deriving (Eq, Ord, Show, Read)

instance Pretty.Pretty View where
    format (View block rect vis_track vis_time status tscroll zoom sels) =
        Pretty.record_title "View"
            [ ("block", Pretty.format block)
            , ("rect", Pretty.format rect)
            , ("visible", Pretty.format (vis_track, vis_time))
            , ("status", Pretty.format status)
            , ("scroll/zoom", Pretty.format (tscroll, zoom))
            , ("selections", Pretty.format sels)
            ]

instance DeepSeq.NFData View where
    rnf (View bid rect track time status scroll zoom selections) =
        bid `seq` rect `seq` track `seq` time `seq` status `seq` scroll
        `seq` zoom `seq` selections `seq` ()

-- | Construct a View, using default values for most of its fields.
-- Don't construct views using View directly since 'State.create_view'
-- overwrites view_tracks, and maybe more in the future.
view :: BlockId -> Rect.Rect -> Types.Zoom -> View
view block_id rect zoom = View
    { view_block = block_id
    , view_rect = rect
    -- view_visible_track and view_visible_time are unknown, but will
    -- be filled in when the new view emits its initial resize msg.
    , view_visible_track = 0
    , view_visible_time = 0
    , view_status = Map.empty
    , view_track_scroll = 0
    , view_zoom = zoom
    , view_selections = Map.empty
    }

-- | Figure out what color the background of the status line should be.
status_color :: BlockId -> Block -> Maybe BlockId -> Color.Color
status_color block_id block maybe_root_id
    | Just block_id == maybe_root_id = Config.status_root
    | Maybe.isJust (block_integrated block) =
        Config.status_integrate_destination
    | otherwise = Config.status_default

show_status :: Map.Map String String -> String
show_status = Seq.join " | " . map (\(k, v) -> k ++ ": " ++ v) . Map.assocs

-- | Return how much track is in view.
visible_time :: View -> ScoreTime
visible_time view =
    Types.zoom_to_time (view_zoom view) (view_visible_time view)

visible_track :: View -> Types.Width
visible_track = view_visible_track

-- | If the given Rect is the visible area, expand it to be what the
-- 'view_rect' would be for that visible area.  Use this to set the visible
-- area to a certain size.
set_visible_rect :: View -> Rect.Rect -> Rect.Rect
set_visible_rect view rect = rect
    -- Add a bit of padding to look nicer.
    { Rect.rw = Rect.rw rect + dw + 2
    , Rect.rh = Rect.rh rect + dh
    }
    where
    dw = Rect.rw (view_rect view) - view_visible_track view
    dh = Rect.rh (view_rect view) - view_visible_time view

-- | The actual window size is this much larger than the sum of the widths
-- of the tracks, but only after first creation, when 'view_visible_track'
-- has not yet been set by the UI.
default_time_padding, default_track_padding :: Block -> Int
default_time_padding block = Config.view_time_padding
    + if not (null (block_title block))
        then Config.block_title_height else 0
default_track_padding = const $ Config.view_track_padding + 2
