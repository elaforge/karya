{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
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
module Interface.Block (
    BlockModelConfig(..), Block -- no constructors for Block
    , create
    , select_colors
    , get_title, set_title, get_attrs, set_attrs

    -- * Track management
    , Tracklike(..)
    , insert_track, remove_track
) where

import qualified Interface.Util as Util
import Interface.Types
import qualified Interface.Color as Color
import qualified Interface.Track as Track
import qualified Interface.Ruler as Ruler

#include "c_interface.h"

import Foreign
import Foreign.C

data Block = Block (ForeignPtr CBlockModel) Attrs
data CBlockModel
{- data Block = Block
    title :: String
    tracks :: [RulerView | TrackView | Divider]
    attrs :: Attrs
-}

data BlockModelConfig = BlockModelConfig
    { block_select_colors :: [Color.Color]
    , block_bg_color :: Color.Color
    , block_track_box_color :: Color.Color
    , block_sb_box_color :: Color.Color
    }

create :: BlockModelConfig -> IO Block
create config = do
    blockp <- with config $ \configp ->
        c_block_model_create configp
    blockfp <- newForeignPtr c_block_model_destroy blockp
    return $ Block blockfp []

select_colors :: Int
select_colors = fromIntegral (#const select_colors)

instance Storable BlockModelConfig where
    sizeOf _ = #size BlockModelConfig
    alignment _ = 1
    peek = _peek_block_model_config
    poke = _poke_block_model_config

_peek_block_model_config configp = do
    select <- (#peek BlockModelConfig, select) configp
        >>= peekArray select_colors
    bg <- (#peek BlockModelConfig, bg) configp >>= peek
    track_box <- (#peek BlockModelConfig, track_box) configp >>= peek
    sb_box <- (#peek BlockModelConfig, sb_box) configp >>= peek
    return $ BlockModelConfig select bg track_box sb_box

_poke_block_model_config configp (BlockModelConfig
        { block_select_colors = select, block_bg_color = bg
        , block_track_box_color = track_box, block_sb_box_color = sb_box
        })
    = do
        pokeArray ((#ptr BlockModelConfig, select) configp)
            (Util.bounded_list Color.black select_colors select)
        (#poke BlockModelConfig, bg) configp bg
        (#poke BlockModelConfig, track_box) configp track_box
        (#poke BlockModelConfig, sb_box) configp sb_box

foreign import ccall unsafe "block_model_create"
    c_block_model_create :: Ptr BlockModelConfig -> IO (Ptr CBlockModel)
foreign import ccall unsafe "&block_model_destroy"
    c_block_model_destroy :: FunPtr (Ptr CBlockModel -> IO ())


get_title :: Block -> UI String
get_title (Block blockfp _) =
    withForeignPtr blockfp c_block_model_get_title >>= peekCString

foreign import ccall unsafe "block_model_get_title"
    c_block_model_get_title :: Ptr CBlockModel -> IO CString

set_title :: Block -> String -> UI ()
set_title (Block blockfp _) s = withForeignPtr blockfp $ \blockp ->
    withCString s $ \cstr -> c_block_model_set_title blockp cstr

foreign import ccall unsafe "block_model_set_title"
    c_block_model_set_title :: Ptr CBlockModel -> CString -> IO ()

get_attrs :: Block -> Attrs
get_attrs (Block _ attrs) = attrs

set_attrs :: Block -> Attrs -> Block
set_attrs (Block ptr _) attrs = Block ptr attrs

-- * Track management

type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int

-- Tracks may have a Ruler overlay
data Tracklike = T (Track.Track, Ruler.Ruler) | R Ruler.Ruler | D Color.Color

insert_track :: Block -> TrackNum -> Tracklike -> Width -> UI ()
insert_track (Block blockfp _) at track width =
    withForeignPtr blockfp $ \blockp -> case track of
        T ((Track.Track trackfp _), (Ruler.Ruler rulerfp _)) ->
            withFP trackfp $ \trackp -> withFP rulerfp $ \rulerp ->
                c_block_model_insert_event_track blockp at' width' trackp rulerp
        R (Ruler.Ruler rulerfp _) -> withFP rulerfp $ \rulerp ->
            c_block_model_insert_ruler_track blockp at' width' rulerp
        D color -> with color $ \colorp ->
            c_block_model_insert_divider blockp at' width' colorp
    where
    at' = Util.c_int at
    width' = Util.c_int width
    withFP = withForeignPtr

remove_track :: Block -> TrackNum -> UI ()
remove_track (Block blockfp _) at = withForeignPtr blockfp $ \blockp ->
    c_block_model_remove_track blockp (Util.c_int at)

foreign import ccall unsafe "block_model_insert_event_track"
    c_block_model_insert_event_track :: Ptr CBlockModel -> CInt -> CInt
        -> Ptr Track.CEventTrackModel -> Ptr Ruler.CRulerTrackModel -> IO ()
foreign import ccall unsafe "block_model_insert_ruler_track"
    c_block_model_insert_ruler_track :: Ptr CBlockModel -> CInt -> CInt
        -> Ptr Ruler.CRulerTrackModel -> IO ()
foreign import ccall unsafe "block_model_insert_divider"
    c_block_model_insert_divider :: Ptr CBlockModel -> CInt -> CInt
        -> Ptr Color.Color -> IO ()

foreign import ccall unsafe "block_model_remove_track"
    c_block_model_remove_track :: Ptr CBlockModel -> CInt -> IO ()

{-

track_at :: Block -> TrackNum -> UI (Tracklike, Width)
track_at block i = undefined

-- | block view

data BlockView = BlockView deriving (Show) {- opaque, a window on a Block
    zoom :: Zoom
    size :: (x, y, w, h)
    zoom_speed :: Double
    selections :: [Selection]
    select_colors :: [Color.Color]

    state :: Block
-}

create_view :: Block -> BlockConfig -> UI BlockView
create_view block config = undefined

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
    , config_select_colors :: [Color.Color]
    , config_bg :: Color.Color
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

set_selection :: BlockView -> Int -> Selection -> UI ()
set_selection bview selnum sel = undefined


-- Divider, declared here since a separate module would be overkill.
{-
A divider is a line between tracks.  It's used to divide logical track groups
visually.  The UI may also let you collapse entire track groups.
-}

-- | Divider color width
data Divider = Divider Color.Color Double deriving (Show)

-}
