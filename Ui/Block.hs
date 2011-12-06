{-# LANGUAGE DeriveDataTypeable #-}
module Ui.Block where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Generics as Generics
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified App.Config as Config
import Types


-- * block model

data Block = Block {
    block_title :: String
    , block_config :: Config
    , block_tracks :: [Track]
    , block_skeleton :: Skeleton.Skeleton
    } deriving (Eq, Show, Read)

instance DeepSeq.NFData Block where
    -- I don't bother to force anything deep, but there isn't much data down
    -- there anyway.
    rnf (Block title config tracks skel) = title `seq` config `seq` tracks `seq` skel `seq` ()

block_tracklike_ids :: Block -> [TracklikeId]
block_tracklike_ids = map tracklike_id . block_tracks

block_track_ids :: Block -> [TrackId]
block_track_ids = track_ids_of . block_tracklike_ids

block_ruler_ids :: Block -> [RulerId]
block_ruler_ids = ruler_ids_of . block_tracklike_ids

block :: Config -> String -> [Track] -> Block
block config title tracks = Block title config tracks Skeleton.empty

-- | Per-block configuration.
data Config = Config {
    config_selection_colors :: [Color.Color]
    , config_bg_color :: Color.Color
    , config_track_box :: (Color.Color, Char)
    , config_sb_box :: (Color.Color, Char)
    } deriving (Eq, Show, Read)

default_config :: Config
default_config = Config
    Config.bconfig_selection_colors Config.bconfig_bg_color
    Config.bconfig_track_box Config.bconfig_sb_box

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

modify_id :: Track -> (TracklikeId -> TracklikeId) -> Track
modify_id track f = track { tracklike_id = f (tracklike_id track) }

data TracklikeId =
    -- | Tracks may have a Ruler overlay
    TId TrackId RulerId
    | RId RulerId
    | DId Divider
    deriving (Eq, Show, Read)

track_id_of :: TracklikeId -> Maybe TrackId
track_id_of (TId tid _) = Just tid
track_id_of _ = Nothing

track_ids_of = Maybe.mapMaybe track_id_of

ruler_id_of :: TracklikeId -> Maybe RulerId
ruler_id_of (TId _ rid) = Just rid
ruler_id_of (RId rid) = Just rid
ruler_id_of _ = Nothing

ruler_ids_of = Maybe.mapMaybe ruler_id_of

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

tracks_of = Maybe.mapMaybe track_of

ruler_of :: Tracklike -> Maybe Ruler.Ruler
ruler_of (T _ ruler) = Just ruler
ruler_of (R ruler) = Just ruler
ruler_of _ = Nothing

rulers_of = Maybe.mapMaybe ruler_of

-- | A divider separating tracks.
-- Defined here in Block since it's so trivial.
data Divider = Divider Color.Color deriving (Eq, Ord, Show, Read)

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

instance DeepSeq.NFData View where
    rnf (View bid rect track time status scroll zoom selections) =
        bid `seq` rect `seq` track `seq` time `seq` status `seq` scroll
        `seq` zoom `seq` selections `seq` ()

-- | Construct a View, using default values for most of its fields.
-- Don't construct views using View directly since State.create_view overwrites
-- view_tracks, and maybe more in the future.
view :: BlockId -> Rect.Rect -> Types.Zoom -> View
view block_id rect zoom =
    -- view_visible_track and view_visible_time are unknown, but will
    -- be filled in when the new view emits its initial resize msg.
    View block_id rect 0 0 Map.empty 0 zoom Map.empty

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
