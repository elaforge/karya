{-
The main window is the Block.  Blocks have scroll and zoom controls and 0 or
more Tracks, running either horizontally or vertically, depending on the
Block's Orientation.  They have the following controls:

Zoom vertically or horizontally with api or mouse (hold a key + drag, hold
a key and draw a zoom box, or press a key and zoom around the cursor).

Scroll around if you are zoomed in.  Scrollbars are proportional, possibly
display a mini view, and move with the plan9 style left/right/middle.

Resize tracks by dragging the dividers.  Reorder tracks by dragging.

The Block also tracks selections.  You can select with any button.  A Msg is
sent on the mousedown, and on the mouseup.
-}

module Interface.Block where
import Interface.Types
import qualified Interface.Track as Track
import qualified Interface.Ruler as Ruler

data Block = Block deriving (Show) {- opaque
    title :: String
    tracks :: [RulerView | TrackView | Divider]
    attrs :: Attrs
-}

create :: UI Block -- empty block
create = undefined

get_title :: Block -> UI String
get_title block = undefined

set_title :: Block -> String -> UI ()
set_title block s = undefined

get_attrs :: Block -> UI Attrs
get_attrs block = undefined

set_attrs :: Block -> Attrs -> UI ()
set_attrs block attrs = undefined

type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int

-- Tracks may have a Ruler overlay
data Tracklike = R Ruler.T | T (Track.T, Ruler.T) | D Divider deriving (Show)

add_track :: Block -> TrackNum -> Tracklike -> Width -> UI ()
add_track block i track width = undefined

remove_track :: Block -> TrackNum -> UI ()
remove_track block i = undefined

track_at :: Block -> TrackNum -> UI (Tracklike, Width)
track_at block i = undefined


-- | block view

data BlockView = BlockView deriving (Show) {- opaque, a window on a Block
    zoom :: Zoom
    size :: (x, y, w, h)
    zoom_speed :: Double
    selections :: [Selection]
    select_colors :: [Color]

    state :: Block
-}

view :: Block -> BlockConfig -> UI BlockView
view block config = undefined

-- | Changes to any of the UI objects will not be reflected on the screen until
-- you call redraw on their Block.
redraw :: BlockView -> UI ()
redraw bview = undefined

hide :: BlockView -> UI ()
hide bview = undefined

resize :: BlockView -> (Int, Int) -> (Int, Int) -> UI ()
resize bview (x, y) (w, h) = undefined

-- the defaults for newly created blocks and the trackviews automatically
-- created
data BlockConfig = BlockConfig
    { config_orientation :: Orientation
    , config_zoom_speed :: Double
    , config_select_colors :: [Color]
    , config_bg :: Color
    } deriving (Show)

data Orientation = Horizontal | Vertical deriving (Show)

-- | Zoom (trackpos_offset, y_offset) (pixel_per_trackpos, pixel_per_y)
data Zoom = Zoom (TrackPos, Int) (Double, Double) deriving (Show)

get_zoom :: BlockView -> UI Zoom
get_zoom bview = undefined

set_zoom :: BlockView -> Zoom -> UI ()
set_zoom bview zoom = undefined

get_config :: BlockView -> UI BlockConfig
get_config bview = undefined

set_config :: BlockView -> BlockConfig -> UI ()
set_config bview config = undefined

-- A selection may span multiple tracks.
data Selection = Selection Block (TrackNum, TrackPos) (TrackNum, TrackPos)

get_selection :: BlockView -> UI (Track.T, Selection)
get_selection bview = undefined

set_selection :: BlockView -> Selection -> UI ()
set_selection bview sel = undefined


-- Divider, declared here since a separate module would be overkill.
{-
A divider is a line between tracks.  It's used to divide logical track groups
visually.  The UI may also let you collapse entire track groups.
-}

-- | Divider color width
data Divider = Divider Color Double deriving (Show)
