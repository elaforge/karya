{-# LANGUAGE ForeignFunctionInterface #-}
{- | This layer gives direct wrapped access to the fltk API.

It maintains a map from ViewIds to window pointers, which represents the on
screen state.  All functions here take ViewIds, and will throw an exception if
the view_id isn't in the pointer map.  C++ exceptions coming from FLTK should
be converted and thrown as haskell exceptions.

TODO exceptions are not implemented yet
-}
module Ui.BlockC (
    -- * errors, and ptr access
    throw
    -- | get_id and CView are only exported for Ui.UiMsgC which is a slight
    -- abstraction breakage.
    , get_id, CView

    -- * view creation
    , create_view, destroy_view
    -- ** set other attributes
    , set_size
    , set_view_config
    , set_zoom
    , set_track_scroll
    , CSelection(..)
    , set_selection, set_track_selection
    -- ** constants
    , max_selections

    -- * Block operations
    , set_model_config
    , set_title
    , set_status

    -- ** Track operations
    , insert_track, remove_track, update_track, update_entire_track
    , set_track_width
    , set_track_title

    -- * debugging
    , show_children
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Foreign
import Foreign.C

import qualified Ui.Util as Util
import Ui.Util (Fltk)
import Ui.Types
import qualified Ui.Color as Color

import qualified Ui.Block as Block
-- They properly belong here but are in Ui.Block for convenience.
import Ui.Block (CView, view_id_to_ptr)
import qualified Ui.Ruler as Ruler
import qualified Ui.RulerC as RulerC
import qualified Ui.Track as Track
import qualified Ui.TrackC as TrackC

#include "c_interface.h"

-- * errors

-- TODO have a BlockC exception type
-- also, turn c++ exceptions into this exception
throw = error

get_ptr :: Block.ViewId -> IO (Ptr CView)
get_ptr view_id = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    case Map.lookup view_id ptr_map of
        Nothing -> throw $ show view_id ++ " not in displayed view list: "
            ++ show (Map.elems ptr_map)
        Just viewp -> return viewp

get_id :: Ptr CView -> IO Block.ViewId
get_id viewp = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    case List.find ((==viewp) . snd) (Map.assocs ptr_map) of
        -- If it's Nothing, the UI returned a view ptr I didn't know I had.
        -- That really should not happen.
        Nothing -> throw $ show viewp ++ " not in displayed view list: "
            ++ show (Map.assocs ptr_map)
        Just (view_id, _) -> return view_id

-- * view creation

-- | Create an empty block view with the given configs.  Tracks must be
-- inserted separately.
create_view :: Block.ViewId -> String -> Block.Rect -> Block.ViewConfig
    -> Block.Config -> Fltk ()
create_view view_id window_title rect view_config block_config = do
    MVar.modifyMVar_ view_id_to_ptr $ \ptr_map -> do
        when (view_id `Map.member` ptr_map) $
            throw $ show view_id ++ " already in displayed view list: "
                ++ show (Map.elems ptr_map)
        viewp <- withCString window_title $ \titlep ->
            with block_config $ \configp -> with view_config $ \view_configp ->
                c_create (i x) (i y) (i w) (i h) titlep configp view_configp
        return $ Map.insert view_id viewp ptr_map
    where
    Block.Rect x y w h = rect
    i = Util.c_int

foreign import ccall "create"
    c_create :: CInt -> CInt -> CInt -> CInt -> CString -> Ptr Block.Config
        -> Ptr Block.ViewConfig -> IO (Ptr CView)

destroy_view :: Block.ViewId -> Fltk ()
destroy_view view_id = do
    viewp <- get_ptr view_id
    MVar.modifyMVar_ view_id_to_ptr $ \ptr_map -> do
        with_finalizer $ \finalize -> c_destroy viewp finalize
        return $ Map.delete view_id ptr_map
foreign import ccall "destroy"
    c_destroy :: Ptr CView -> FunPtr (FunPtrFinalizer a) -> IO ()

-- ** Set other attributes

-- TODO unused now that window moves are sent in msgs, remove this?
{-
get_size :: Block.ViewId -> Fltk Block.Rect
get_size view_id = do
    viewp <- get_ptr view_id
    sz <- allocaArray 4 $ \sizep -> do
        c_get_size viewp sizep
        peekArray 4 sizep
    let [x, y, w, h] = map fromIntegral sz
    return (Block.Rect (x, y) (w, h))
foreign import ccall unsafe "get_size"
    c_get_size :: Ptr CView -> Ptr CInt -> IO ()
-}

set_size :: Block.ViewId -> Block.Rect -> Fltk ()
set_size view_id (Block.Rect x y w h) = do
    viewp <- get_ptr view_id
    c_set_size viewp (i x) (i y) (i w) (i h)
    where i = Util.c_int
foreign import ccall "set_size"
    c_set_size :: Ptr CView -> CInt -> CInt -> CInt -> CInt -> IO ()

set_view_config :: Block.ViewId -> Block.ViewConfig -> Fltk ()
set_view_config view_id config = do
    viewp <- get_ptr view_id
    with config $ \configp -> c_set_view_config viewp configp
foreign import ccall "set_view_config"
    c_set_view_config :: Ptr CView -> Ptr Block.ViewConfig -> IO ()

set_zoom :: Block.ViewId -> Block.Zoom -> Fltk ()
set_zoom view_id zoom = do
    viewp <- get_ptr view_id
    with zoom $ \zoomp -> c_set_zoom viewp zoomp
foreign import ccall "set_zoom"
    c_set_zoom :: Ptr CView -> Ptr Block.Zoom -> IO ()

-- | Set the scroll along the track dimension, in pixels.
set_track_scroll :: Block.ViewId -> Block.Width -> Fltk ()
set_track_scroll view_id offset = do
    viewp <- get_ptr view_id
    c_set_track_scroll viewp (Util.c_int offset)
foreign import ccall "set_track_scroll"
    c_set_track_scroll :: Ptr CView -> CInt -> IO ()

set_selection :: Block.ViewId -> Block.SelNum -> Maybe CSelection -> Fltk ()
set_selection view_id selnum maybe_sel = do
    viewp <- get_ptr view_id
    maybeWith with maybe_sel $ \selp ->
        c_set_selection viewp (Util.c_int selnum) selp
foreign import ccall "set_selection"
    c_set_selection :: Ptr CView -> CInt -> Ptr CSelection -> IO ()

set_track_selection :: Block.ViewId -> Block.SelNum -> Block.TrackNum
    -> Maybe CSelection -> Fltk ()
set_track_selection view_id selnum tracknum maybe_sel = do
    viewp <- get_ptr view_id
    maybeWith with maybe_sel $ \selp ->
        c_set_multi_selection viewp (Util.c_int selnum)
            (Util.c_int tracknum) selp
foreign import ccall "set_track_selection"
    c_set_multi_selection :: Ptr CView -> CInt -> CInt -> Ptr CSelection
        -> IO ()

-- | Max number of selections, hardcoded in ui/config.h.
max_selections :: Int
max_selections = (#const Config::max_selections)


-- * Block operations

-- These operate on ViewIds too because there is no block/view distinction at
-- this layer.

set_model_config :: Block.ViewId -> Block.Config -> Fltk ()
set_model_config view_id config = do
    viewp <- get_ptr view_id
    with config $ \configp -> c_set_model_config viewp configp
foreign import ccall "set_model_config"
    c_set_model_config :: Ptr CView -> Ptr Block.Config -> IO ()

set_title :: Block.ViewId -> String -> Fltk ()
set_title view_id title = do
    viewp <- get_ptr view_id
    withCString title (c_set_title viewp)
foreign import ccall "set_title" c_set_title :: Ptr CView -> CString -> IO ()

set_status :: Block.ViewId -> String -> Fltk ()
set_status view_id status = do
    viewp <- get_ptr view_id
    withCString status (c_set_status viewp)
foreign import ccall "set_status" c_set_status :: Ptr CView -> CString -> IO ()

-- ** Track operations

insert_track :: Block.ViewId -> Block.TrackNum -> Block.Tracklike
    -> Track.Samples -> Block.Width -> Fltk ()
insert_track view_id tracknum tracklike samples width = do
    viewp <- get_ptr view_id
    with_tracklike tracklike samples $ \tp mlistp len ->
        c_insert_track viewp (Util.c_int tracknum) tp
            (Util.c_int width) mlistp len

remove_track :: Block.ViewId -> Block.TrackNum -> Fltk ()
remove_track view_id tracknum = do
    viewp <- get_ptr view_id
    with_finalizer $ \finalize ->
        c_remove_track viewp (Util.c_int tracknum) finalize

update_track :: Block.ViewId -> Block.TrackNum -> Block.Tracklike
    -> Track.Samples -> TrackPos -> TrackPos -> Fltk ()
update_track view_id tracknum tracklike samples start end = do
    viewp <- get_ptr view_id
    with_finalizer $ \finalize ->
        with start $ \startp -> with end $ \endp ->
            with_tracklike tracklike samples $ \tp mlistp len ->
                c_update_track viewp (Util.c_int tracknum) tp
                    mlistp len finalize startp endp

-- | Like 'update_track' except update everywhere.
update_entire_track :: Block.ViewId -> Block.TrackNum -> Block.Tracklike
    -> Track.Samples -> Fltk ()
update_entire_track view_id tracknum tracklike samples =
    -- -1 is special cased in c++.
    update_track view_id tracknum tracklike samples
        (TrackPos (-1)) (TrackPos (-1))

foreign import ccall "insert_track"
    c_insert_track :: Ptr CView -> CInt -> Ptr TracklikePtr -> CInt
        -> Ptr Ruler.Marklist -> CInt -> IO ()
foreign import ccall "remove_track"
    c_remove_track :: Ptr CView -> CInt -> FunPtr (FunPtrFinalizer a) -> IO ()
foreign import ccall "update_track"
    c_update_track :: Ptr CView -> CInt -> Ptr TracklikePtr
        -> Ptr Ruler.Marklist -> CInt -> FunPtr (FunPtrFinalizer a)
        -> Ptr TrackPos -> Ptr TrackPos -> IO ()

-- | When I do anything that will destroy previous callbacks, I have to pass
-- yet another callback which will be used to mark the old callbacks as done,
-- so that the haskell GC knows it can collect the data those callbacks use.
type FunPtrFinalizer a = FunPtr a -> IO ()
foreign import ccall "wrapper"
    c_make_free_fun_ptr :: FunPtrFinalizer a -> IO (FunPtr (FunPtrFinalizer a))
make_free_fun_ptr = c_make_free_fun_ptr freeHaskellFunPtr

with_finalizer :: (FunPtr (FunPtrFinalizer a) -> IO c) -> IO c
with_finalizer = Exception.bracket make_free_fun_ptr freeHaskellFunPtr

with_tracklike tracklike samples f = case tracklike of
    Block.T track ruler -> RulerC.with_ruler ruler $ \rulerp mlistp len ->
        with track $ \trackp -> with (TPtr trackp samples rulerp) $ \tp ->
            f tp mlistp len
    Block.R ruler -> RulerC.with_ruler ruler $ \rulerp mlistp len ->
        with (RPtr rulerp) $ \tp ->
            f tp mlistp len
    Block.D div -> with div $ \dividerp -> with (DPtr dividerp) $ \tp ->
        f tp nullPtr 0

data TracklikePtr =
    TPtr (Ptr Track.Track) Track.Samples (Ptr Ruler.Ruler)
    | RPtr (Ptr Ruler.Ruler)
    | DPtr (Ptr Block.Divider)


set_track_width :: Block.ViewId -> Block.TrackNum -> Block.Width -> Fltk ()
set_track_width view_id tracknum width = do
    viewp <- get_ptr view_id
    c_set_track_width viewp (Util.c_int tracknum) (Util.c_int width)
foreign import ccall "set_track_width"
    c_set_track_width :: Ptr CView -> CInt -> CInt -> IO ()

set_track_title :: Block.ViewId -> Block.TrackNum -> String -> Fltk ()
set_track_title view_id tracknum title = do
    viewp <- get_ptr view_id
    withCString title (c_set_track_title viewp (Util.c_int tracknum))
foreign import ccall "set_track_title"
    c_set_track_title :: Ptr CView -> CInt -> CString -> IO ()

-- ** debugging

show_children :: Block.ViewId -> IO String
show_children view_id = do
    viewp <- get_ptr view_id
    c_show_children viewp (Util.c_int (-1)) >>= peekCString
foreign import ccall "i_show_children"
    c_show_children :: Ptr CView -> CInt -> IO CString

-- * storable

-- ** tracks

instance Storable Block.Divider where
    sizeOf _ = #size DividerConfig
    alignment _ = undefined
    poke dividerp (Block.Divider color) =
        (#poke DividerConfig, color) dividerp color

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
        TPtr trackp samples rulerp -> do
            (#poke Tracklike, track) tp trackp
            (#poke Tracklike, ruler) tp rulerp
            TrackC.insert_render_samples trackp samples
        RPtr rulerp -> (#poke Tracklike, ruler) tp rulerp
        DPtr dividerp -> (#poke Tracklike, divider) tp dividerp

-- ** configs

instance Storable Block.Config where
    sizeOf _ = #size BlockModelConfig
    alignment _ = undefined
    poke = poke_block_model_config

poke_block_model_config configp (Block.Config
        { Block.config_bg_color = bg
        , Block.config_track_box_color = track_box
        , Block.config_sb_box_color = sb_box
        })
    = do
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
        , Block.vconfig_status_size = status_size
        })
    = do
        (#poke BlockViewConfig, zoom_speed) configp zoom_speed
        (#poke BlockViewConfig, block_title_height) configp
            (Util.c_int block_title_height)
        (#poke BlockViewConfig, track_title_height) configp
            (Util.c_int track_title_height)
        (#poke BlockViewConfig, sb_size) configp (Util.c_int sb_size)
        (#poke BlockViewConfig, status_size) configp (Util.c_int status_size)

-- ** selection

-- | C++ Selections have a color, but in haskell the color is separated into
-- Block.config_selection_colors.
data CSelection = CSelection Color.Color Block.Selection deriving (Show)

instance Storable CSelection where
    sizeOf _ = #size Selection
    alignment _ = undefined
    peek = error "no peek selection"
    poke = poke_selection

poke_selection selp (CSelection color
        (Block.Selection track0 pos0 track1 pos1)) = do
    -- TODO convert c++ selections to hs style
    let start_pos = min pos0 pos1
        dur = max pos0 pos1 - min pos0 pos1
        start_track = min track0 track1
        -- Tracks are an inclusive range.
        tracks = max track0 track1 - min track0 track1 + 1
    (#poke Selection, color) selp color
    (#poke Selection, start_track) selp (Util.c_int start_track)
    (#poke Selection, start_pos) selp start_pos
    (#poke Selection, tracks) selp (Util.c_int tracks)
    (#poke Selection, duration) selp dur

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

-- ** rect

instance Storable Block.Rect where
    sizeOf _ = #size Rect
    alignment _ = undefined
    peek = peek_rect

peek_rect rectp = do
    x <- (#peek Rect, x) rectp :: IO CInt
    y <- (#peek Rect, y) rectp :: IO CInt
    w <- (#peek Rect, w) rectp :: IO CInt
    h <- (#peek Rect, h) rectp :: IO CInt
    return $ Block.Rect (i x) (i y) (i w) (i h)
    where i = fromIntegral
