{-# LANGUAGE EmptyDataDecls #-}
module Ui.Block where
import Control.Monad
import qualified Control.Concurrent.MVar as MVar
import qualified Foreign
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import Ui
import qualified Ui.Types as Types
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track

import qualified App.Config as Config


-- * block model

data GenericBlock track = Block {
    block_title :: String
    , block_config :: Config
    , block_tracks :: [track]
    , block_skeleton :: Skeleton.Skeleton
    , block_schema :: SchemaId
    } deriving (Eq, Show, Read)

type Block = GenericBlock BlockTrack
type DisplayBlock = GenericBlock DisplayTrack

block_tracklike_ids :: Block -> [TracklikeId]
block_tracklike_ids = map tracklike_id . block_tracks

block_track_ids :: Block -> [TrackId]
block_track_ids = track_ids_of . block_tracklike_ids

block_ruler_ids :: Block -> [RulerId]
block_ruler_ids = ruler_ids_of . block_tracklike_ids

block :: Config -> String  -> [BlockTrack] -> SchemaId -> Block
block config title tracks schema_id =
    Block title config tracks Skeleton.empty schema_id

-- | Per-block configuration.
data Config = Config {
    config_selection_colors :: [Color]
    , config_bg_color :: Color
    , config_track_box :: (Color, Char)
    , config_sb_box :: (Color, Char)
    } deriving (Eq, Show, Read)

default_config :: Config
default_config = Config
    Config.bconfig_selection_colors Config.bconfig_bg_color
    Config.bconfig_track_box Config.bconfig_sb_box

data BlockTrack = BlockTrack {
    tracklike_id :: TracklikeId
    -- | The current width is in the View, but this width is a default if
    -- a new View is created from this Block.
    , track_width :: Types.Width
    -- | Track display state flags.
    , track_flags :: [TrackFlag]
    -- | Other tracks are displayed behind this one.  Useful to merge a pitch
    -- track into its note track.
    , track_merged :: [TrackId]
    } deriving (Eq, Show, Read)

-- | Construct a 'BlockTrack' with defaults.
block_track :: TracklikeId -> Types.Width -> BlockTrack
block_track tracklike_id width = BlockTrack tracklike_id width [] []

-- | Similar to Track.Track, except this data can vary per-block.
data DisplayTrack = DisplayTrack {
    dtrack_tracklike_id :: TracklikeId
    , dtrack_merged :: [TrackId]
    , dtrack_status :: Maybe (Char, Color)
    , dtrack_event_brightness :: Double
    , dtrack_collapsed :: Bool
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

-- | Convert logical block level tracks to display tracks.  Return the
-- track creation width since it doesn't belong in DisplayTrack.
block_display_tracks :: Block -> [(DisplayTrack, Types.Width)]
block_display_tracks block =
    [(block_track_config t, track_width t) | t <- block_tracks block]

block_track_config :: BlockTrack -> DisplayTrack
block_track_config btrack =
    DisplayTrack (tracklike_id btrack) (track_merged btrack) status brightness
        (Collapse `elem` track_flags btrack)
    where (status, brightness) = flags_to_status (track_flags btrack)

flags_to_status :: [TrackFlag] -> (Maybe (Char, Color), Double)
flags_to_status flags
    | Solo `elem` flags = (Just ('S', Config.solo_color), 1)
    | Mute `elem` flags = (Just ('M', Config.mute_color), 0.75)
    | otherwise = (Nothing, 1)

modify_id :: BlockTrack -> (TracklikeId -> TracklikeId) -> BlockTrack
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

track_ids_of = Seq.map_maybe track_id_of

ruler_id_of :: TracklikeId -> Maybe RulerId
ruler_id_of (TId _ rid) = Just rid
ruler_id_of (RId rid) = Just rid
ruler_id_of _ = Nothing

ruler_ids_of = Seq.map_maybe ruler_id_of

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

tracks_of = Seq.map_maybe track_of

ruler_of :: Tracklike -> Maybe Ruler.Ruler
ruler_of (T _ ruler) = Just ruler
ruler_of (R ruler) = Just ruler
ruler_of _ = Nothing

rulers_of = Seq.map_maybe ruler_of

-- | A divider separating tracks.
-- Defined here in Block since it's so trivial.
data Divider = Divider Color deriving (Eq, Ord, Show, Read)

-- * block view

data View = View {
    -- | view_block should never change.
    -- TODO Views that point to a BlockId not in state_blocks should be
    -- destroyed.
    view_block :: BlockId
    , view_rect :: Types.Rect

    -- | These two are derived from view_rect, but cached here so pure code
    -- doesn't have to call to the UI and import BlockC.
    , view_visible_track :: Int
    , view_visible_time :: Int

    , view_config :: ViewConfig
    , view_status :: Map.Map String String

    -- | Scroll and zoom
    , view_track_scroll :: Types.Width
    , view_zoom :: Types.Zoom

    , view_selections :: Map.Map Types.SelNum Types.Selection
    -- | These are the per-view settings for the tracks.  There should be one
    -- corresponding to each TracklikeId in the Block.  The StateT operations
    -- should maintain this invariant.
    , view_tracks :: [TrackView]
    } deriving (Eq, Ord, Show, Read)

-- | Construct a View, using default values for most of its fields.
-- Don't construct views using View directly since State.create_view overwrites
-- view_tracks, and maybe more in the future.
view :: BlockId -> Types.Rect -> Types.Zoom -> View
view block_id rect zoom =
    -- view_visible_track and view_visible_time are unknown, but will
    -- be filled in when the new view emits its initial resize msg.
    View block_id rect 0 0 default_view_config Map.empty 0 zoom Map.empty []

show_status :: View -> String
show_status = Seq.join " | " . map (\(k, v) -> k ++ ": " ++ v)
    . Map.assocs . view_status

-- | Return how much track is in view.
visible_time :: View -> ScoreTime
visible_time view =
    pixels_to_track_pos (view_zoom view) (view_visible_time view)

visible_track :: View -> Types.Width
visible_track = view_visible_track

pixels_to_track_pos :: Types.Zoom -> Int -> ScoreTime
pixels_to_track_pos zoom pixels =
    ScoreTime (fromIntegral pixels) / ScoreTime (Types.zoom_factor zoom)

data TrackView = TrackView {
    track_view_width :: Types.Width
    -- TODO add track_view_collapsed here
    } deriving (Eq, Ord, Show, Read)

-- | These are defaults for newly created blocks.
data ViewConfig = ViewConfig {
    vconfig_block_title_height :: Int
    , vconfig_track_title_height :: Int
    , vconfig_skel_height :: Int
    , vconfig_sb_size :: Int
    , vconfig_status_size :: Int
    } deriving (Eq, Ord, Show, Read)

default_view_config :: ViewConfig
default_view_config = ViewConfig
    Config.vconfig_block_title_height
    Config.vconfig_track_title_height
    Config.vconfig_skel_height
    Config.vconfig_sb_size
    Config.vconfig_status_size

-- This stuff belongs in BlockC.  However, Ui.State uses map_ids, and making
-- State import BlockC makes practically everything import BlockC, which makes
-- ghci freak out since it wants explicit linking for all FFI-using modules.

-- Phantom type for block view ptrs.
data CView

-- | Global map of view IDs to their windows.  This is global mutable state
-- because the underlying window system is also global mutable state, and is
-- not well represented by a persistent functional state.
view_id_to_ptr :: MVar.MVar (Map.Map ViewId (Foreign.Ptr CView))
view_id_to_ptr = Foreign.unsafePerformIO (MVar.newMVar Map.empty)

-- | Rename view ids.  Throws if the views collide, because that would make
-- things break.
map_ids :: (ViewId -> ViewId) -> IO ()
map_ids f = MVar.modifyMVar_ view_id_to_ptr $ \ptrs -> do
    let new_ptrs = Map.mapKeys f ptrs
    when (Map.size new_ptrs /= Map.size ptrs) $
        -- TODO should actually be BlockC exception type, when I have that
        error $ "keys collided in view_id_to_ptr: "
            ++ show (Map.keys (Map.difference ptrs new_ptrs))
    return new_ptrs
