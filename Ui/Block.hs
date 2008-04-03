module Ui.Block where

import Ui.Types
import qualified Ui.Track as Track
import qualified Ui.Ruler as Ruler


-- These would have to be hierarchical names, so if you load
-- another song you don't get ID collisions.
newtype BlockId = BlockId String deriving (Eq, Ord, Show)
newtype ViewId = ViewId String deriving (Eq, Ord, Show)

-- * block model

data Block = Block {
    block_title :: String
    , block_config :: Config
    , block_tracks :: [Tracklike]
    } deriving (Eq, Ord, Show)

data Config = Config {
    config_select_colors :: [Color]
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
    , view_ruler :: Ruler.RulerId
    , view_config :: ViewConfig
    } deriving (Eq, Ord, Show)

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
data Zoom = Zoom TrackPos Double deriving (Show)

-- | A selection may span multiple tracks.
data Selection = Selection
    { sel_start_track :: TrackNum
    , sel_start_pos :: TrackPos
    , sel_tracks :: TrackNum
    , sel_duration :: TrackPos
    } deriving (Show)

-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
-- | Index into the the selection list.
type SelNum = Int
