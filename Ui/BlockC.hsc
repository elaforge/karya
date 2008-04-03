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


Blocks have:

    Block.hsc
- Block and View data types, along with configs ()

- access methods to EventData and Marklist, must be with the data declaration
so it can hide it (data)

- Storable instances - should be next to their data? (data, hsc)

    BlockFfi.hs
foreign function calls to create and manipulate c++ views, + callbacks for
events (data, ffi)

    BlockState.hs
monadic access to State (HandlerM, State, block data)



BlockAttrs (must be serializable if a block is to be serializable)

key responder
deriver
cached derivation
cached realization
-}
module Ui.BlockC where
{-
    -- * Block model
    Config(..), Block -- no constructors for Block
    , create
    -- ** Model modification
    , get_config, set_config
    , get_title, set_title, get_attrs, set_attrs

    -- ** Track management
    , TrackNum, Width, SelNum, Tracklike(..)
    , tracks, track_at, insert_track, remove_track

    -- * Block view
    , View, Rect(..), ViewConfig(..), Zoom(..), Selection(..)
    , create_view, view_block

    -- ** View modification
    , get_size, set_size
    , get_view_config, set_view_config
    , get_zoom, set_zoom
    , get_track_scroll, set_track_scroll
    , get_selection, set_selection
    , get_track_width, set_track_width
-}

import qualified Ui.Util as Util
import Ui.Util (Fltk)
-- import Ui.Types
import qualified Ui.Color as Color

import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.RulerC as RulerC
import qualified Ui.Track as Track
import qualified Ui.TrackC as TrackC

import Foreign
import Foreign.C

#include "c_interface.h"

-- * view creation

newtype ViewPtr = ViewPtr (Ptr CView) deriving (Show)
data CView

create_view :: Block.Block -> Block.Rect -> Ruler.Ruler -> Block.ViewConfig
    -> Fltk ViewPtr
create_view block (Block.Rect (x, y) (w, h)) ruler view_config = do
    viewp <- with config $ \configp -> with view_config $ \view_configp ->
        RulerC.with_ruler ruler $ \(rulerp, mlistp, len) ->
            c_block_view_create (i x) (i y) (i w) (i h) configp view_configp
                rulerp mlistp len
    -- TODO set title
    -- TODO add tracks
    return (ViewPtr viewp)
    where
    i = Util.c_int
    config = Block.block_config block

foreign import ccall "block_view_create"
    c_block_view_create :: CInt -> CInt -> CInt -> CInt -> Ptr Block.Config
        -> Ptr Block.ViewConfig -> Ptr Ruler.Ruler
        -> Ptr Ruler.Marklist -> CInt -> IO (Ptr CView)

destroy_view (ViewPtr viewp) = c_block_view_destroy viewp
foreign import ccall "block_view_destroy"
    c_block_view_destroy :: Ptr CView -> IO ()


-- ** view storable

-- | Max number of selections, hardcoded in ui/config.h.
max_selections :: Int
max_selections = (#const Config::max_selections)

instance Storable Block.Config where
    sizeOf _ = #size BlockModelConfig
    alignment _ = undefined
    peek = peek_block_model_config
    poke = poke_block_model_config

peek_block_model_config configp = do
    select <- (#peek BlockModelConfig, select) configp
        >>= peekArray max_selections
    bg <- (#peek BlockModelConfig, bg) configp >>= peek
    track_box <- (#peek BlockModelConfig, track_box) configp >>= peek
    sb_box <- (#peek BlockModelConfig, sb_box) configp >>= peek
    return $ Block.Config select bg track_box sb_box

poke_block_model_config configp (Block.Config
        { Block.config_select_colors = select
        , Block.config_bg_color = bg
        , Block.config_track_box_color = track_box
        , Block.config_sb_box_color = sb_box
        })
    = do
        pokeArray ((#ptr BlockModelConfig, select) configp)
            (Util.bounded_list Color.black max_selections select)
        (#poke BlockModelConfig, bg) configp bg
        (#poke BlockModelConfig, track_box) configp track_box
        (#poke BlockModelConfig, sb_box) configp sb_box

instance Storable Block.ViewConfig where
    sizeOf _ = #size BlockViewConfig
    alignment _ = undefined
    peek = error "no peek for ViewConfig"
    poke = poke_config

poke_config configp (Block.ViewConfig
        { Block.vconfig_zoom_speed = zoom_speed
        , Block.vconfig_block_title_height = block_title_height
        , Block.vconfig_track_title_height = track_title_height
        , Block.vconfig_sb_size = sb_size
        , Block.vconfig_ruler_size = ruler_size
        , Block.vconfig_status_size = status_size
        })
    = do
        (#poke BlockViewConfig, zoom_speed) configp zoom_speed
        (#poke BlockViewConfig, block_title_height) configp
            (Util.c_int block_title_height)
        (#poke BlockViewConfig, track_title_height) configp
            (Util.c_int track_title_height)
        (#poke BlockViewConfig, sb_size) configp (Util.c_int sb_size)
        (#poke BlockViewConfig, ruler_size) configp (Util.c_int ruler_size)
        (#poke BlockViewConfig, status_size) configp (Util.c_int status_size)


-- ** Track insert / remove

insert_track :: ViewPtr -> Block.TrackNum -> Tracklike -> Block.Width
    -> Fltk ()
insert_track (ViewPtr viewp) tracknum tracklike width = case tracklike of
    T track ruler -> RulerC.with_ruler ruler $ \(rulerp, mlistp, len) ->
        with track $ \trackp -> with (TPtr trackp rulerp) $ \tp ->
            c_block_view_insert_track viewp ctracknum tp cwidth  mlistp len
    R ruler -> RulerC.with_ruler ruler $ \(rulerp, mlistp, len) ->
        with (RPtr rulerp) $ \tp ->
            c_block_view_insert_track viewp ctracknum tp cwidth mlistp len
    D div -> with div $ \dividerp -> with (DPtr dividerp) $ \tp ->
        c_block_view_insert_track viewp ctracknum tp cwidth nullPtr 0
    where
    ctracknum = Util.c_int tracknum
    cwidth = Util.c_int width

remove_track :: ViewPtr -> Block.TrackNum -> Fltk ()
remove_track (ViewPtr viewp) tracknum = do
    c_block_view_remove_track viewp (Util.c_int tracknum)

foreign import ccall "block_view_insert_track"
    c_block_view_insert_track :: Ptr CView -> CInt -> Ptr TracklikePtr -> CInt
        -> Ptr Ruler.Marklist -> CInt -> IO ()
foreign import ccall "block_view_remove_track"
    c_block_view_remove_track :: Ptr CView -> CInt -> IO ()

-- | Like Block.Tracklike, except it has actual values instead of IDs.
data Tracklike =
    T Track.Track Ruler.Ruler
    | R Ruler.Ruler
    | D Block.Divider
    deriving (Show)

instance Storable Block.Divider where
    sizeOf _ = #size DividerConfig
    poke dividerp (Block.Divider color) =
        (#poke DividerConfig, color) dividerp color

data TracklikePtr =
    TPtr (Ptr Track.Track) (Ptr Ruler.Ruler)
    | RPtr (Ptr Ruler.Ruler)
    | DPtr (Ptr Block.Divider)

instance Storable TracklikePtr where
    sizeOf _ = #size Tracklike
    alignment _ = undefined
    poke = poke_tracklike_ptr

poke_tracklike_ptr tp (TPtr trackp rulerp) = do
    (#poke Tracklike, track) tp trackp
    (#poke Tracklike, ruler) tp rulerp
poke_tracklike_ptr tp (RPtr rulerp) = do
    (#poke Tracklike, ruler) tp rulerp
poke_tracklike_ptr tp (DPtr dividerp) = do
    (#poke Tracklike, divider) tp dividerp

{-
instance Util.Widget View where
    show_children view = Util.do_show_children (view_p view)
-}


{-

set_size :: View -> Rect -> Fltk ()
set_size view (Rect (x, y) (w, h)) =
    c_block_view_set_size (view_p view) (i x) (i y) (i w) (i h)
    where i = Util.c_int
foreign import ccall unsafe "block_view_set_size"
    c_block_view_set_size :: Ptr CBlockView -> CInt -> CInt -> CInt -> CInt
        -> IO ()

get_size :: View -> Fltk Rect
get_size view = do
    sz <- allocaArray 4 $ \sizep -> do
        c_block_view_get_size (view_p view) sizep
        peekArray 4 sizep
    let [x, y, w, h] = map fromIntegral sz
    return (Rect (x, y) (w, h))
foreign import ccall unsafe "block_view_get_size"
    c_block_view_get_size :: Ptr CBlockView -> Ptr CInt -> IO ()

get_view_config :: View -> IO ViewConfig
get_view_config view = MVar.readMVar (view_config view)

set_view_config :: View -> ViewConfig -> Fltk ()
set_view_config view config = do
    MVar.swapMVar (view_config view) config
    with config $ \configp -> c_block_view_set_config (view_p view) configp
foreign import ccall unsafe "block_view_set_config"
    c_block_view_set_config :: Ptr CBlockView -> Ptr ViewConfig -> IO ()

get_ruler :: View -> IO RulerImpl.Ruler
get_ruler view = MVar.readMVar (view_ruler view)

get_zoom :: View -> Fltk Zoom
get_zoom view = c_block_view_get_zoom (view_p view) >>= peek
foreign import ccall unsafe "block_view_get_zoom"
    c_block_view_get_zoom :: Ptr CBlockView -> IO (Ptr Zoom)

set_zoom :: View -> Zoom -> Fltk ()
set_zoom view zoom =
    with zoom $ \zoomp -> c_block_view_set_zoom (view_p view) zoomp
foreign import ccall unsafe "block_view_set_zoom"
    c_block_view_set_zoom :: Ptr CBlockView -> Ptr Zoom -> IO ()

-- | Get and set the scroll along the track dimension, in pixels.
get_track_scroll :: View -> Fltk Int
get_track_scroll view = fmap fromIntegral (c_get_track_scroll (view_p view))
foreign import ccall unsafe "block_view_get_track_scroll"
    c_get_track_scroll :: Ptr CBlockView -> IO CInt

set_track_scroll :: View -> Int -> Fltk ()
set_track_scroll view offset
    = c_set_track_scroll (view_p view) (Util.c_int offset)
foreign import ccall unsafe "block_view_set_track_scroll"
    c_set_track_scroll :: Ptr CBlockView -> CInt -> IO ()

get_selection :: View -> SelNum -> Fltk Selection
get_selection view selnum
    = c_block_view_get_selection (view_p view) (Util.c_int selnum) >>= peek
foreign import ccall unsafe "block_view_get_selection"
    c_block_view_get_selection :: Ptr CBlockView -> CInt -> IO (Ptr Selection)

set_selection :: View -> SelNum -> Selection -> Fltk ()
set_selection view selnum sel =
    with sel $ \selp ->
        c_block_view_set_selection (view_p view) (Util.c_int selnum) selp
foreign import ccall unsafe "block_view_set_selection"
    c_block_view_set_selection :: Ptr CBlockView -> CInt -> Ptr Selection
        -> IO ()

get_track_width :: View -> TrackNum -> Fltk Width
get_track_width view at_ = do
    ntracks <- tracks (view_block view)
    width <- c_block_view_get_track_width (view_p view)
        (Util.in_range "get_track_width" 0 ntracks at_)
    return (fromIntegral width)
foreign import ccall unsafe "block_view_get_track_width"
    c_block_view_get_track_width :: Ptr CBlockView -> CInt -> IO CInt

set_track_width :: View -> TrackNum -> Width -> Fltk ()
set_track_width view at_ width = do
    ntracks <- tracks (view_block view)
    c_block_view_set_track_width (view_p view)
        (Util.in_range "set_track_width" 0 ntracks at_) (Util.c_int width)
foreign import ccall unsafe "block_view_set_track_width"
    c_block_view_set_track_width :: Ptr CBlockView -> CInt -> CInt -> IO ()


-- * storable

instance Storable Selection where
    sizeOf _ = #size Selection
    alignment _ = undefined
    peek = peek_selection
    poke = poke_selection

peek_selection selp = do
    start_track <- (#peek Selection, start_track) selp :: IO CInt
    start_pos <- (#peek Selection, start_pos) selp
    tracks <- (#peek Selection, tracks) selp :: IO CInt
    duration <- (#peek Selection, duration) selp
    return $ Selection (fromIntegral start_track) start_pos
        (fromIntegral tracks) duration

poke_selection selp (Selection start_track start_pos tracks duration) =
    do
        (#poke Selection, start_track) selp (Util.c_int start_track)
        (#poke Selection, start_pos) selp start_pos
        (#poke Selection, tracks) selp (Util.c_int tracks)
        (#poke Selection, duration) selp duration

instance Storable Zoom where
    sizeOf _ = #size ZoomInfo
    alignment _ = undefined
    peek = peek_zoom
    poke = poke_zoom

peek_zoom zoomp = do
    offset <- (#peek ZoomInfo, offset) zoomp
    factor <- (#peek ZoomInfo, factor) zoomp :: IO CDouble
    return $ Zoom offset (realToFrac factor)

poke_zoom zoomp (Zoom offset factor) = do
    (#poke ZoomInfo, offset) zoomp offset
    (#poke ZoomInfo, factor) zoomp (Util.c_double factor)

-}
