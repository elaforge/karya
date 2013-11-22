-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
module Ui.Block where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Char as Char
import qualified Data.Generics as Generics
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect

import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified App.Config as Config
import Types


-- * block model

-- | This is the data behind a single block.
data Block = Block {
    block_title :: !Text
    , block_config :: !Config
    , block_tracks :: ![Track]
    , block_skeleton :: !Skeleton.Skeleton
    -- | Present if this block was integrated from another.
    -- If the TrackDestinations is empty, then this is an existing block
    -- that was created to receive integration.
    , block_integrated :: !(Maybe (BlockId, [TrackDestination]))
    -- | Similar to block_integrated, if the TrackDestinations is empty, then
    -- new integrated tracks should be created.
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
-- be rendered to lilypond and provide arguments for it.  TODO But lilypond
-- is always kicked off manually now, so this has no use at the moment.  Maybe
-- you could put notes in there.
type Meta = Map.Map Text Text

-- | This holds the 'EventIndex' for one track or block.
data TrackDestination = TrackDestination {
    dest_note :: !(TrackId, EventIndex)
    , dest_controls :: !(Map.Map Text (TrackId, EventIndex))
    } deriving (Eq, Show, Read)

-- | This is a picture of the integrated events that were used to create an
-- integrated block.  By taking its difference against the current contents of
-- the block I can figure out user edits.
type EventIndex = Map.Map Event.IndexKey Event.Event

instance Pretty.Pretty TrackDestination where
    format (TrackDestination note controls) =
        Pretty.format (fst note, Map.map fst controls)

-- | Arrows that should be drawn to indicate integrate relationships.
integrate_skeleton :: Block -> [(TrackNum, TrackNum)]
integrate_skeleton block =
    concat $ mapMaybe edge_of (block_integrated_tracks block)
    where
    edge_of (source_id, dests) = do
        source <- tracknum_of source_id
        Just $ map ((,) source) (mapMaybe (tracknum_of . fst . dest_note) dests)
    tracknum_of track_id = List.elemIndex (Just track_id) track_ids
    track_ids = map track_id (block_tracks block)

block_tracklike_ids :: Block -> [TracklikeId]
block_tracklike_ids = map tracklike_id . block_tracks

block_track_ids :: Block -> [TrackId]
block_track_ids = track_ids_of . block_tracklike_ids

block_ruler_ids :: Block -> [RulerId]
block_ruler_ids = ruler_ids_of . block_tracklike_ids

block :: Config -> Text -> [Track] -> Block
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

-- | One of those colored boxes wedged into the corners of the block window.
data Box = Box { box_color :: !Color.Color, box_char :: !Char }
    deriving (Eq, Show, Read)

instance Pretty.Pretty Box where
    pretty (Box color c) = Pretty.pretty color ++ if c == ' ' then [] else [c]

-- | Like 'Track.Track', this has per-track data, but unlike Track.Track,
-- this is data that can vary per-block.
--
-- This is the higher level track that is visible at the haskell level, as
-- opposed to 'DisplayTrack', which is what is visible in the UI.
data Track = Track {
    tracklike_id :: !TracklikeId
    -- | Formerly the width was in the view since each view could have
    -- a different width and this was just the default width, but that turned
    -- out to be too much of a hassle, so now all occurences of a track have
    -- the same width.
    , track_width :: !Types.Width
    -- | Track display state flags.
    , track_flags :: !(Set.Set TrackFlag)
    -- | Other tracks are displayed behind this one.  Useful to merge a pitch
    -- track into its note track.
    , track_merged :: ![TrackId]
    } deriving (Eq, Show, Read, Generics.Typeable)

track_id :: Track -> Maybe TrackId
track_id = track_id_of . tracklike_id

instance Pretty.Pretty Track where
    pretty (Track tid width flags merged) =
        Pretty.pretty tid ++ ": " ++ unwords
            [Pretty.pretty width, Pretty.pretty flags, Pretty.pretty merged]

-- | Construct a 'Track' with defaults.
track :: TracklikeId -> Types.Width -> Track
track tracklike_id width = Track tracklike_id width mempty []

colored_divider :: Color.Color -> Track
colored_divider color = track (DId (Divider color)) 3

divider :: Track
divider = colored_divider (Color.rgb 0.8 0.8 0.8)

track_collapsed :: Track -> Bool
track_collapsed = (Collapse `Set.member`) . track_flags

-- | This is the low-level representation of a track, which directly
-- corresponds with what is displayed by the UI.  The DisplayTracks should be
-- derivable from a 'Block' deterministically.
data DisplayTrack = DisplayTrack {
    dtracklike_id :: TracklikeId
    , dtrack_width :: Types.Width
    , dtrack_merged :: [TrackId]
    , dtrack_status :: Status
    , dtrack_event_brightness :: Double
    } deriving (Eq, Show, Read)

type Status = Maybe (String, Color.Color)

instance Pretty.Pretty DisplayTrack where
    format (DisplayTrack tlike_id width merged status _bright) =
        Pretty.record_title "DisplayTrack"
            [ ("tracklike_id", Pretty.format tlike_id)
            , ("width", Pretty.format width)
            , ("merged", Pretty.format merged)
            , ("status", Pretty.format status)
            ]

-- | Most of these only make sense for event tracks.
data TrackFlag =
    -- | Track is collapsed to take up less space.
    Collapse
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
    deriving (Eq, Ord, Show, Read)

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

flags_to_status :: Set.Set TrackFlag -> (Status, Double)
flags_to_status flags
    | Disable `Set.member` flags =
        (Just (chars Disable, Config.mute_color), 0.5)
    | Solo `Set.member` flags = (Just (chars Solo, Config.solo_color), 1)
    | Mute `Set.member` flags = (Just (chars Mute, Config.mute_color), 0.75)
    | otherwise = (Nothing, 1)
    where
    chars flag = filter (/=' ') $ flag_char flag
        : map (Char.toLower . flag_char) (Set.toList (Set.delete flag flags))

flag_char :: TrackFlag -> Char
flag_char status = case status of
    Disable -> 'D'
    Solo -> 'S'
    Mute -> 'M'
    Collapse -> ' '

-- | True if this track wants to render a 'Track.TrackSignal'.
wants_track_signal :: Set.Set TrackFlag -> Track.Track -> Bool
wants_track_signal flags track =
    Track.render_style (Track.track_render track) /= Track.NoRender
    && Collapse `Set.notMember` flags

modify_id :: (TracklikeId -> TracklikeId) -> Track -> Track
modify_id f track = track { tracklike_id = f (tracklike_id track) }

data TracklikeId =
    -- | Tracks may have a Ruler overlay
    TId TrackId RulerId
    | RId RulerId
    | DId Divider
    deriving (Eq, Ord, Show, Read)

instance Pretty.Pretty TracklikeId where
    pretty tlike_id = case tlike_id of
        TId tid rid -> Pretty.pretty tid ++ "/" ++ Pretty.pretty rid
        RId rid -> Pretty.pretty rid
        DId divider -> show divider

track_id_of :: TracklikeId -> Maybe TrackId
track_id_of (TId tid _) = Just tid
track_id_of _ = Nothing

track_ids_of :: [TracklikeId] -> [TrackId]
track_ids_of = mapMaybe track_id_of

ruler_id_of :: TracklikeId -> Maybe RulerId
ruler_id_of (TId _ rid) = Just rid
ruler_id_of (RId rid) = Just rid
ruler_id_of _ = Nothing

ruler_ids_of :: [TracklikeId] -> [RulerId]
ruler_ids_of = mapMaybe ruler_id_of

set_ruler_id :: RulerId -> TracklikeId -> TracklikeId
set_ruler_id rid (TId tid _) = TId tid rid
set_ruler_id rid (RId _) = RId rid
set_ruler_id _ t = t

data Tracklike =
    T Track.Track Ruler.Ruler
    | R Ruler.Ruler
    | D Divider
    deriving (Show)

track_of :: Tracklike -> Maybe Track.Track
track_of (T track _) = Just track
track_of _ = Nothing

tracks_of :: [Tracklike] -> [Track.Track]
tracks_of = mapMaybe track_of

ruler_of :: Tracklike -> Maybe Ruler.Ruler
ruler_of (T _ ruler) = Just ruler
ruler_of (R ruler) = Just ruler
ruler_of _ = Nothing

rulers_of :: [Tracklike] -> [Ruler.Ruler]
rulers_of = mapMaybe ruler_of

-- | A divider separating tracks.
-- Defined here in Block since it's so trivial.
newtype Divider = Divider Color.Color deriving (Eq, Ord, Show, Read)

-- * block view

-- | A view is a single window on screen.  Views are destroyed when the window
-- is closed.
data View = View {
    -- | view_block should never change.
    view_block :: !BlockId
    , view_rect :: !Rect.Rect

    -- | Pixel width and height of stuff in the view that is not the track area,
    -- i.e. scrollbars, skeleton display, block title, etc.
    --
    -- Only fltk knows the width of all the various widgets, but it's cached
    -- here so pure code doesn't have to call to the UI and import BlockC.
    , view_track_padding :: !Int
    , view_time_padding :: !Int
    -- | Map (sort_order, name) contents
    , view_status :: !(Map.Map (Int, Text) Text)

    -- | Scroll and zoom
    , view_track_scroll :: !Types.Width
    , view_zoom :: !Types.Zoom

    , view_selections :: !(Map.Map Types.SelNum Types.Selection)
    } deriving (Eq, Ord, Show, Read)

instance Pretty.Pretty View where
    format (View block rect track_pad time_pad status tscroll zoom sels) =
        Pretty.record_title "View"
            [ ("block", Pretty.format block)
            , ("rect", Pretty.format rect)
            , ("padding", Pretty.format (track_pad, time_pad))
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
view :: Block -> BlockId -> Rect.Rect -> Types.Zoom -> View
view block block_id rect zoom = View
    { view_block = block_id
    , view_rect = rect
    -- These will be filled in when the new view emits its initial resize msg,
    -- but it should start off with the defaults.
    , view_track_padding = default_track_padding block
    , view_time_padding = default_time_padding block
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

show_status :: Map.Map (Int, Text) Text -> Text
show_status = Text.intercalate " | "
    . map (\((_, k), v) -> k <> ": " <> v) . Map.toAscList

-- | Return how much track is in view.
visible_time :: View -> ScoreTime
visible_time view = Types.zoom_to_time (view_zoom view) (view_visible_time view)

visible_track :: View -> Types.Width
visible_track = view_visible_track

view_visible_rect :: View -> Rect.Rect
view_visible_rect view@(View { view_rect = rect }) = rect
    { Rect.rw = Rect.rw rect - view_track_padding view
    , Rect.rh = Rect.rh rect - view_time_padding view
    }

-- | If the given Rect is the visible area, expand it to be what the
-- 'view_rect' would be for that visible area.  Use this to set the visible
-- area to a certain size.
set_visible_rect :: View -> Rect.Rect -> Rect.Rect
set_visible_rect view rect = rect
    { Rect.rw = Rect.rw rect + view_track_padding view
    , Rect.rh = Rect.rh rect + view_time_padding view
    }

view_visible_track, view_visible_time :: View -> Int
view_visible_track view = Rect.rw (view_rect view) - view_track_padding view
view_visible_time view = Rect.rh (view_rect view) - view_time_padding view

-- | The actual window size is this much larger than the sum of the widths
-- of the tracks, but only after first creation, when 'view_visible_track'
-- has not yet been set by the UI.
default_time_padding, default_track_padding :: Block -> Int
default_time_padding block = Config.view_time_padding
    + if not (Text.null (block_title block))
        then Config.block_title_height else 0
default_track_padding = const Config.view_track_padding
