module Ui.Block where
import qualified Foreign

import qualified Data.Map as Map

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Track as Track
import qualified Ui.Ruler as Ruler


-- These would have to be hierarchical names, so if you load
-- another song you don't get ID collisions.

-- | Reference to a Block.  Use this to look up Blocks in the State.
-- Even though the constructor is exported, you should only create them
-- through the 'State.StateT' interface.
newtype BlockId = BlockId String deriving (Eq, Ord, Show)
-- | Reference to a View, as per 'BlockId'.
newtype ViewId = ViewId String deriving (Eq, Ord, Show)

-- * block model

data Block = Block {
    block_title :: String
    , block_config :: Config
    , block_ruler_track :: Tracklike
    -- The Width here is the default if a new View is created from this Block.
    , block_tracks :: [(Tracklike, Width)]
    } deriving (Eq, Ord, Show)

data Config = Config {
    config_selection_colors :: [Color]
    , config_bg_color :: Color
    , config_track_box_color :: Color
    , config_sb_box_color :: Color
    } deriving (Eq, Ord, Show)

-- Tracks may have a Ruler overlay
data Tracklike =
    T Track.TrackId Ruler.RulerId
    | R Ruler.RulerId
    | D Divider
    deriving (Eq, Ord, Show)

-- | A divider separating tracks.
-- Declared here in Block since it's so trivial.
data Divider = Divider Color deriving (Eq, Ord, Show)

-- * block view

data View = View {
    -- view_block should never change.  Views that point to a BlockId not
    -- in state_blocks will be destroyed.
    view_block :: BlockId
    , view_rect :: Rect
    , view_config :: ViewConfig

    -- Scroll and zoom
    , view_track_scroll :: Width
    , view_zoom :: Zoom

    -- Visible Selections, indexed by their SelNum.
    , view_selections :: Map.Map SelNum Selection
    -- These are the per-view settings for the tracks.  There should be one
    -- corresponding to each Tracklike in the Block.  The StateT operations
    -- should maintain this invariant.
    , view_tracks :: [TrackView]
    } deriving (Eq, Ord, Show)

data TrackView = TrackView {
    track_view_width :: Width
    } deriving (Eq, Ord, Show)

-- Construct a View, using default values for most of its fields.
-- Don't construct views using View directly.
view block_id rect config = View block_id rect config
    0 default_zoom Map.empty []

data Rect = Rect (Int, Int) (Int, Int) deriving (Eq, Ord, Show)

-- The defaults for newly created blocks and the trackviews automatically
-- created.
data ViewConfig = ViewConfig
    { vconfig_zoom_speed :: Double
    , vconfig_block_title_height :: Int
    , vconfig_track_title_height :: Int
    , vconfig_sb_size :: Int
    , vconfig_ruler_size :: Int
    , vconfig_status_size :: Int
    } deriving (Eq, Ord, Show)

-- | Zoom offset factor
data Zoom = Zoom TrackPos Double deriving (Eq, Ord, Show)
default_zoom = Zoom (TrackPos 0) 1

-- TODO: remove color and put it in BlockC.SelectionC, which gets its color
-- from a BlockConfig list
data Selection = Selection
    { sel_color :: Color
    , sel_start_track :: TrackNum
    , sel_start_pos :: TrackPos
    , sel_tracks :: TrackNum
    , sel_duration :: TrackPos
    } deriving (Eq, Ord, Show)

-- | A Selection with 0 tracks is considered no selection.
null_selection :: Selection
null_selection = Selection Color.black 0 (TrackPos 0) 0 (TrackPos 0)

-- | Index of the non-scrolling ruler track (it doesn't necessarily have
-- a ruler, it's just meant for one).
-- This could be translated by BlockC into BlockView::ruler_tracknum, but for
-- now I'm just going to trust that they're the same.
ruler_tracknum :: TrackNum
ruler_tracknum = -1

-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
-- | Index into the the selection list.
type SelNum = Int
