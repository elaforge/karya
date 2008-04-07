{-# LANGUAGE ForeignFunctionInterface #-}
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
import qualified Control.Exception as Exception
import Foreign
import Foreign.C

import qualified Ui.Util as Util
import Ui.Util (Fltk)
import Ui.Types
import qualified Ui.Color as Color

import qualified Ui.Block as Block
import Ui.Block (ViewPtr(..), CView)
import qualified Ui.Ruler as Ruler
import qualified Ui.RulerC as RulerC
import qualified Ui.Track as Track
import qualified Ui.TrackC as TrackC () -- just want Storable instance

#include "c_interface.h"

-- * view creation

create_view :: Block.Rect -> Block.ViewConfig -> Block.Config -> Ruler.Ruler
    -> Fltk ViewPtr
create_view (Block.Rect (x, y) (w, h)) view_config block_config ruler = do
    viewp <- with block_config $ \configp -> with view_config $ \view_configp ->
        RulerC.with_ruler ruler $ \rulerp mlistp len ->
            c_create (i x) (i y) (i w) (i h) configp view_configp
                rulerp mlistp len
    return (ViewPtr viewp)
    where
    i = Util.c_int

foreign import ccall "create"
    c_create :: CInt -> CInt -> CInt -> CInt -> Ptr Block.Config
        -> Ptr Block.ViewConfig -> Ptr Ruler.Ruler
        -> Ptr Ruler.Marklist -> CInt -> IO (Ptr CView)

destroy_view (ViewPtr viewp) = Exception.bracket
    make_free_fun_ptr freeHaskellFunPtr
    (\finalize -> c_destroy viewp finalize)
foreign import ccall "destroy"
    c_destroy :: Ptr CView -> FunPtr (FunPtrFinalizer a) -> IO ()

-- ** Set other attributes

-- Unlike the other view attributes, I have a getter for the size.  This is
-- because the OS doesn't seem to say when the window gets moved, so I have
-- to ask.
get_size :: ViewPtr -> Fltk Block.Rect
get_size (ViewPtr viewp) = do
    sz <- allocaArray 4 $ \sizep -> do
        c_get_size viewp sizep
        peekArray 4 sizep
    let [x, y, w, h] = map fromIntegral sz
    return (Block.Rect (x, y) (w, h))
foreign import ccall unsafe "get_size"
    c_get_size :: Ptr CView -> Ptr CInt -> IO ()

set_size :: ViewPtr -> Block.Rect -> Fltk ()
set_size (ViewPtr viewp) (Block.Rect (x, y) (w, h)) =
    c_set_size viewp (i x) (i y) (i w) (i h)
    where i = Util.c_int
foreign import ccall "set_size"
    c_set_size :: Ptr CView -> CInt -> CInt -> CInt -> CInt -> IO ()

set_view_config :: ViewPtr -> Block.ViewConfig -> Fltk ()
set_view_config (ViewPtr viewp) config =
    with config $ \configp -> c_set_view_config viewp configp
foreign import ccall "set_view_config"
    c_set_view_config :: Ptr CView -> Ptr Block.ViewConfig -> IO ()

set_zoom :: ViewPtr -> Block.Zoom -> Fltk ()
set_zoom (ViewPtr viewp) zoom =
    with zoom $ \zoomp -> c_set_zoom viewp zoomp
foreign import ccall "set_zoom"
    c_set_zoom :: Ptr CView -> Ptr Block.Zoom -> IO ()

-- | Set the scroll along the track dimension, in pixels.
set_track_scroll :: ViewPtr -> Block.Width -> Fltk ()
set_track_scroll (ViewPtr viewp) offset =
    c_set_track_scroll viewp (Util.c_int offset)
foreign import ccall "set_track_scroll"
    c_set_track_scroll :: Ptr CView -> CInt -> IO ()

set_selection :: ViewPtr -> Block.SelNum -> Block.Selection -> Fltk ()
set_selection (ViewPtr viewp) selnum sel =
    with sel $ \selp ->
        c_set_selection viewp (Util.c_int selnum) selp
foreign import ccall "set_selection"
    c_set_selection :: Ptr CView -> CInt -> Ptr Block.Selection -> IO ()

set_track_width :: ViewPtr -> Block.TrackNum -> Block.Width -> Fltk ()
set_track_width (ViewPtr viewp) tracknum width =
    c_set_track_width viewp (Util.c_int tracknum) (Util.c_int width)
foreign import ccall "set_track_width"
    c_set_track_width :: Ptr CView -> CInt -> CInt -> IO ()


-- * Block operations

-- These operate on ViewPtrs too because there is no block/view distinction at
-- this layer.

set_title :: ViewPtr -> String -> Fltk ()
set_title (ViewPtr viewp) title =
    withCString title (c_set_title viewp)
foreign import ccall "set_title"
    c_set_title :: Ptr CView -> CString -> IO ()

set_block_config :: ViewPtr -> Block.Config -> Fltk ()
set_block_config (ViewPtr viewp) config = with config $ \configp ->
    c_block_set_model_config viewp configp
foreign import ccall "set_model_config"
    c_block_set_model_config :: Ptr CView -> Ptr Block.Config -> IO ()

-- ** Track operations

insert_track :: ViewPtr -> Block.TrackNum -> CTracklike -> Block.Width
    -> Fltk ()
insert_track (ViewPtr viewp) tracknum tracklike width = do
    with_tracklike tracklike $ \tp mlistp len ->
        c_insert_track viewp (Util.c_int tracknum) tp
            (Util.c_int width) mlistp len

remove_track :: ViewPtr -> Block.TrackNum -> Fltk ()
remove_track (ViewPtr viewp) tracknum = Exception.bracket
    make_free_fun_ptr freeHaskellFunPtr $ \finalize ->
        c_remove_track viewp (Util.c_int tracknum) finalize

update_track :: ViewPtr -> Block.TrackNum -> CTracklike -> TrackPos -> TrackPos
    -> Fltk ()
update_track (ViewPtr viewp) tracknum tracklike start end = Exception.bracket
    make_free_fun_ptr
    freeHaskellFunPtr $ \finalize ->
        with start $ \startp -> with end $ \endp ->
            with_tracklike tracklike $ \tp mlistp len ->
                c_update_track viewp (Util.c_int tracknum) tp
                    mlistp len finalize startp endp

-- When I do anything that will destroy previous callbacks, I have to pass
-- yet another callback which will be used to mark the old callbacks as done,
-- so that the haskell GC knows it can collected data those callbacks use.
type FunPtrFinalizer a = FunPtr a -> IO ()
foreign import ccall "wrapper"
    c_make_free_fun_ptr :: FunPtrFinalizer a -> IO (FunPtr (FunPtrFinalizer a))
-- make_free_fun_ptr = c_make_free_fun_ptr
--     (\p -> putStrLn ("free: " ++ show p) >> freeHaskellFunPtr p)
make_free_fun_ptr = c_make_free_fun_ptr freeHaskellFunPtr

with_tracklike tracklike f = case tracklike of
    T track ruler -> RulerC.with_ruler ruler $ \rulerp mlistp len ->
        with track $ \trackp -> with (TPtr trackp rulerp) $ \tp ->
            f tp mlistp len
    R ruler -> RulerC.with_ruler ruler $ \rulerp mlistp len ->
        with (RPtr rulerp) $ \tp ->
            f tp mlistp len
    D div -> with div $ \dividerp -> with (DPtr dividerp) $ \tp ->
        f tp nullPtr 0

foreign import ccall "insert_track"
    c_insert_track :: Ptr CView -> CInt -> Ptr TracklikePtr -> CInt
        -> Ptr Ruler.Marklist -> CInt -> IO ()
foreign import ccall "remove_track"
    c_remove_track :: Ptr CView -> CInt -> FunPtr (FunPtrFinalizer a) -> IO ()
foreign import ccall "update_track"
    c_update_track :: Ptr CView -> CInt -> Ptr TracklikePtr
        -> Ptr Ruler.Marklist -> CInt -> FunPtr (FunPtrFinalizer a)
        -> Ptr TrackPos -> Ptr TrackPos -> IO ()

-- | Like Block.Tracklike, except it has actual values instead of IDs.
data CTracklike =
    T Track.Track Ruler.Ruler
    | R Ruler.Ruler
    | D Block.Divider
    deriving (Show)

instance Storable Block.Divider where
    sizeOf _ = #size DividerConfig
    alignment _ = undefined
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

poke_tracklike_ptr tp trackp = do
    -- Apparently 'with' doesn't zero out the memory it allocates.
    (#poke Tracklike, track) tp nullPtr
    (#poke Tracklike, ruler) tp nullPtr
    (#poke Tracklike, divider) tp nullPtr
    case trackp of
        TPtr trackp rulerp -> do
            (#poke Tracklike, track) tp trackp
            (#poke Tracklike, ruler) tp rulerp
        RPtr rulerp -> (#poke Tracklike, ruler) tp rulerp
        DPtr dividerp -> (#poke Tracklike, divider) tp dividerp

-- ** debugging

show_children :: ViewPtr -> IO String
show_children (ViewPtr viewp) =
    c_show_children viewp (Util.c_int (-1)) >>= peekCString
foreign import ccall "i_show_children"
    c_show_children :: Ptr CView -> CInt -> IO CString

-- * storable

-- ** configs

-- | Max number of selections, hardcoded in ui/config.h.
max_selections :: Int
max_selections = (#const Config::max_selections)

instance Storable Block.Config where
    sizeOf _ = #size BlockModelConfig
    alignment _ = undefined
    peek = peek_block_model_config
    poke = poke_block_model_config

peek_block_model_config configp = do
    select <- (#peek BlockModelConfig, selections) configp
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
        pokeArray ((#ptr BlockModelConfig, selections) configp)
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

-- ** selection

instance Storable Block.Selection where
    sizeOf _ = #size Selection
    alignment _ = undefined
    peek = peek_selection
    poke = poke_selection

peek_selection selp = do
    color <- (#peek Selection, color) selp :: IO Color
    start_track <- (#peek Selection, start_track) selp :: IO CInt
    start_pos <- (#peek Selection, start_pos) selp
    tracks <- (#peek Selection, tracks) selp :: IO CInt
    duration <- (#peek Selection, duration) selp
    return $ Block.Selection color (fromIntegral start_track) start_pos
        (fromIntegral tracks) duration

poke_selection selp (Block.Selection c start_track start_pos tracks duration) =
    do
        (#poke Selection, color) selp c
        (#poke Selection, start_track) selp (Util.c_int start_track)
        (#poke Selection, start_pos) selp start_pos
        (#poke Selection, tracks) selp (Util.c_int tracks)
        (#poke Selection, duration) selp duration

-- ** zoom

instance Storable Block.Zoom where
    sizeOf _ = #size ZoomInfo
    alignment _ = undefined
    peek = peek_zoom
    poke = poke_zoom

peek_zoom zoomp = do
    offset <- (#peek ZoomInfo, offset) zoomp
    factor <- (#peek ZoomInfo, factor) zoomp :: IO CDouble
    return $ Block.Zoom offset (realToFrac factor)

poke_zoom zoomp (Block.Zoom offset factor) = do
    (#poke ZoomInfo, offset) zoomp offset
    (#poke ZoomInfo, factor) zoomp (Util.c_double factor)
