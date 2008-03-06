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
module Interface.BlockImpl where

import qualified Interface.Util as Util
import Interface.Types
import qualified Interface.Color as Color
import qualified Interface.TrackImpl as TrackImpl
import qualified Interface.RulerImpl as RulerImpl

#include "c_interface.h"

import Foreign
import Foreign.C

data Block = Block
    { block_p :: ForeignPtr CBlockModel
    , block_attrs :: Attrs
    } deriving (Show)
data CBlockModel

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
    peek = peek_block_model_config
    poke = poke_block_model_config

peek_block_model_config configp = do
    select <- (#peek BlockModelConfig, select) configp
        >>= peekArray select_colors
    bg <- (#peek BlockModelConfig, bg) configp >>= peek
    track_box <- (#peek BlockModelConfig, track_box) configp >>= peek
    sb_box <- (#peek BlockModelConfig, sb_box) configp >>= peek
    return $ BlockModelConfig select bg track_box sb_box

poke_block_model_config configp (BlockModelConfig
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
get_title block =
    withForeignPtr (block_p block) c_block_model_get_title >>= peekCString
foreign import ccall unsafe "block_model_get_title"
    c_block_model_get_title :: Ptr CBlockModel -> IO CString

set_title :: Block -> String -> UI ()
set_title block s = withForeignPtr (block_p block) $ \blockp ->
    withCString s $ \cstr -> c_block_model_set_title blockp cstr
foreign import ccall unsafe "block_model_set_title"
    c_block_model_set_title :: Ptr CBlockModel -> CString -> IO ()

get_attrs :: Block -> Attrs
get_attrs = block_attrs
set_attrs :: Block -> Attrs -> Block
set_attrs block attrs = block { block_attrs = attrs }

-- * Track management

type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int

-- Tracks may have a Ruler overlay
data Tracklike = T (TrackImpl.Track, RulerImpl.Ruler) | R RulerImpl.Ruler
    | D Color.Color

insert_track :: Block -> TrackNum -> Tracklike -> Width -> UI ()
insert_track block at track width =
    withFP (block_p block) $ \blockp -> case track of
        T (track, ruler) -> withFP (TrackImpl.track_p track) $ \trackp ->
            withFP (RulerImpl.ruler_p ruler) $ \rulerp ->
                c_block_model_insert_event_track blockp at' width' trackp rulerp
        R ruler -> withFP (RulerImpl.ruler_p ruler) $ \rulerp ->
            c_block_model_insert_ruler_track blockp at' width' rulerp
        D color -> with color $ \colorp ->
            c_block_model_insert_divider blockp at' width' colorp
    where
    at' = Util.c_int at
    width' = Util.c_int width
    withFP = withForeignPtr

remove_track :: Block -> TrackNum -> UI ()
remove_track block at = withForeignPtr (block_p block) $ \blockp ->
    c_block_model_remove_track blockp (Util.c_int at)

foreign import ccall unsafe "block_model_insert_event_track"
    c_block_model_insert_event_track :: Ptr CBlockModel -> CInt -> CInt
        -> Ptr TrackImpl.CEventTrackModel -> Ptr RulerImpl.CRulerTrackModel
        -> IO ()
foreign import ccall unsafe "block_model_insert_ruler_track"
    c_block_model_insert_ruler_track :: Ptr CBlockModel -> CInt -> CInt
        -> Ptr RulerImpl.CRulerTrackModel -> IO ()
foreign import ccall unsafe "block_model_insert_divider"
    c_block_model_insert_divider :: Ptr CBlockModel -> CInt -> CInt
        -> Ptr Color.Color -> IO ()

foreign import ccall unsafe "block_model_remove_track"
    c_block_model_remove_track :: Ptr CBlockModel -> CInt -> IO ()

track_at :: Block -> TrackNum -> UI (Tracklike, Width)
track_at block i = undefined

-- * view

data BlockView = BlockView (Ptr CBlockView) Block deriving (Show)
data BlockView = BlockView
    { view_p :: Ptr CBlockView
    , view_block :: Block
    } deriving (Show)
data CBlockView

instance Util.Widget BlockView where
    show_children view = Util.do_show_children (view_p view)

-- The defaults for newly created blocks and the trackviews automatically
-- created.
data BlockViewConfig = BlockViewConfig
    { config_zoom_speed :: Double
    } deriving (Show)

-- | Zoom offset factor
data Zoom = Zoom TrackPos Double deriving (Show)

-- | A selection may span multiple tracks.
data Selection = Selection (TrackNum, TrackPos) (TrackNum, TrackPos)
    deriving (Show)


create_view :: (Int, Int) -> (Int, Int) -> Block -> RulerImpl.Ruler
    -> BlockViewConfig -> UI BlockView
create_view (x, y) (w, h) block ruler config = do
    viewp <- withFP (block_p block) $ \blockp ->
        withFP (RulerImpl.ruler_p ruler) $ \rulerp ->
            with config $ \configp ->
                c_block_view_create (i x) (i y) (i w) (i h) blockp rulerp
                    configp
    return $ BlockView viewp block
    where
    i = Util.c_int
    withFP = withForeignPtr

foreign import ccall unsafe "block_view_create"
    c_block_view_create :: CInt -> CInt -> CInt -> CInt -> Ptr CBlockModel
        -> Ptr RulerImpl.CRulerTrackModel -> Ptr BlockViewConfig
        -> IO (Ptr CBlockView)

destroy_view view = c_block_view_destroy (view_p view)
foreign import ccall unsafe "block_view_destroy"
    c_block_view_destroy :: Ptr CBlockView -> IO ()

resize :: BlockView -> (Int, Int) -> (Int, Int) -> UI ()
resize view (x, y) (w, h) =
    c_block_view_resize (view_p view) (i x) (i y) (i w) (i h)
    where i = Util.c_int
foreign import ccall unsafe "block_view_resize"
    c_block_view_resize :: Ptr CBlockView -> CInt -> CInt -> CInt -> CInt
        -> IO ()

get_zoom :: BlockView -> UI Zoom
get_zoom view = c_block_view_get_zoom (view_p view) >>= peek
foreign import ccall unsafe "block_view_get_zoom"
    c_block_view_get_zoom :: Ptr CBlockView -> IO (Ptr Zoom)

set_zoom :: BlockView -> Zoom -> UI ()
set_zoom view zoom =
    with zoom $ \zoomp -> c_block_view_set_zoom (view_p view) zoomp
foreign import ccall unsafe "block_view_set_zoom"
    c_block_view_set_zoom :: Ptr CBlockView -> Ptr Zoom -> IO ()

get_selection :: BlockView -> UI Selection
get_selection view = c_block_view_get_selection (view_p view) >>= peek
foreign import ccall unsafe "block_view_get_selection"
    c_block_view_get_selection :: Ptr CBlockView -> IO (Ptr Selection)

set_selection :: BlockView -> Selection -> UI ()
set_selection view sel =
    with sel $ \selp -> c_block_view_set_selection (view_p view) selp
foreign import ccall unsafe "block_view_set_selection"
    c_block_view_set_selection :: Ptr CBlockView -> Ptr Selection -> IO ()

get_view_config :: BlockView -> UI BlockViewConfig
get_view_config view = undefined

set_view_config :: BlockView -> BlockViewConfig -> UI ()
set_view_config view config =
    with config $ \configp -> c_block_view_set_config (view_p view) configp
foreign import ccall unsafe "block_view_set_config"
    c_block_view_set_config :: Ptr CBlockView -> Ptr BlockViewConfig -> IO ()


-- * storable

instance Storable Selection where
    sizeOf _ = #size Selection
    alignment _ = 4
    peek = peek_selection
    poke = poke_selection

peek_selection selp = do
    start_track <- (#peek Selection, start_track) selp :: IO CInt
    start_pos <- (#peek Selection, start_pos) selp
    end_track <- (#peek Selection, end_track) selp :: IO CInt
    end_pos <- (#peek Selection, end_pos) selp
    return $ Selection (fromIntegral start_track, start_pos)
        (fromIntegral end_track, end_pos)

poke_selection selp (Selection (start_track, start_pos) (end_track, end_pos)) =
    do
        (#poke Selection, start_track) selp (Util.c_int start_track)
        (#poke Selection, start_pos) selp start_pos
        (#poke Selection, end_track) selp (Util.c_int end_track)
        (#poke Selection, end_pos) selp end_pos

instance Storable Zoom where
    sizeOf _ = #size ZoomInfo
    alignment _ = 4
    peek = peek_zoom
    poke = poke_zoom

peek_zoom zoomp = do
    offset <- (#peek ZoomInfo, offset) zoomp
    factor <- (#peek ZoomInfo, factor) zoomp :: IO CDouble
    return $ Zoom offset (realToFrac factor)

poke_zoom zoomp (Zoom offset factor) = do
    (#poke ZoomInfo, offset) zoomp offset
    (#poke ZoomInfo, factor) zoomp (Util.c_double factor)

instance Storable BlockViewConfig where
    sizeOf _ = #size BlockViewConfig
    alignment _ = 1
    peek = error "no peek for BlockViewConfig"
    poke = poke_config

-- It actually has more fields, but they're set by constants in the interface
-- for now.
poke_config configp (BlockViewConfig zoom_speed) = do
    (#poke BlockViewConfig, zoom_speed) configp zoom_speed
