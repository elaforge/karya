{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-} -- for data CView
{- | This layer gives direct wrapped access to the fltk API.

    It maintains a map from ViewIds to window pointers, which represents the on
    screen state.  All functions here take ViewIds, and will throw an exception
    if the view_id isn't in the pointer map.  C++ exceptions coming from FLTK
    should be converted and thrown as haskell exceptions.

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
    , bring_to_front

    -- ** constants
    , max_selections

    -- * Block operations
    , set_model_config, set_skeleton, set_title, set_status
    , set_display_track

    -- ** Track operations
    , insert_track, remove_track, update_track, update_entire_track
    , set_track_signal
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

import Util.Control
import qualified Util.Rect as Rect

import Ui
import qualified Ui.Color as Color
import qualified Ui.Types as Types
import qualified Ui.Util as Util
import Ui.Util (Fltk)

import qualified Ui.Block as Block
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ruler as Ruler
import qualified Ui.RulerC as RulerC
import qualified Ui.Track as Track
import qualified Ui.TrackC as TrackC

#include "c_interface.h"
-- This is from http://haskell.org/haskellwiki/FFI_cook_book.  Is there a
-- better way?  I dunno, but this is clever and looks like it should work.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- * errors

-- Phantom type for block view ptrs.
data CView

-- | Global map of view IDs to their windows.  This is global mutable state
-- because the underlying window system is also global mutable state, and is
-- not well represented by a persistent functional state.
view_id_to_ptr :: MVar.MVar (Map.Map ViewId (Foreign.Ptr CView))
{-# NOINLINE view_id_to_ptr #-}
view_id_to_ptr = Foreign.unsafePerformIO (MVar.newMVar Map.empty)

-- TODO have a BlockC exception type
-- also, turn c++ exceptions into this exception
throw = error

get_ptr :: ViewId -> IO (Ptr CView)
get_ptr view_id = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    case Map.lookup view_id ptr_map of
        Nothing -> throw $ show view_id ++ " not in displayed view list: "
            ++ show (Map.assocs ptr_map)
        Just viewp -> return viewp

lookup_ptr :: ViewId -> IO (Maybe (Ptr CView))
lookup_ptr view_id = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    return $ Map.lookup view_id ptr_map

get_id :: Ptr CView -> IO ViewId
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
create_view :: ViewId -> String -> Rect.Rect -> Block.ViewConfig
    -> Block.Config -> Fltk ()
create_view view_id window_title rect view_config block_config = do
    MVar.modifyMVar_ view_id_to_ptr $ \ptr_map -> do
        when (view_id `Map.member` ptr_map) $
            throw $ show view_id ++ " already in displayed view list: "
                ++ show (Map.assocs ptr_map)
        viewp <- withCString window_title $ \titlep ->
            with block_config $ \configp -> with view_config $ \view_configp ->
                c_create (i x) (i y) (i w) (i h) titlep configp view_configp
        return $ Map.insert view_id viewp ptr_map
    where
    (x, y, w, h) = (Rect.rx rect, Rect.ry rect, Rect.rw rect, Rect.rh rect)
    i = Util.c_int

foreign import ccall "create"
    c_create :: CInt -> CInt -> CInt -> CInt -> CString -> Ptr Block.Config
        -> Ptr Block.ViewConfig -> IO (Ptr CView)

destroy_view :: ViewId -> Fltk ()
destroy_view view_id = do
    viewp <- get_ptr view_id
    MVar.modifyMVar_ view_id_to_ptr $ \ptr_map -> do
        with_finalizer $ \finalize -> c_destroy viewp finalize
        return $ Map.delete view_id ptr_map
foreign import ccall "destroy"
    c_destroy :: Ptr CView -> FunPtr (FunPtrFinalizer a) -> IO ()

-- ** Set other attributes

set_size :: ViewId -> Rect.Rect -> Fltk ()
set_size view_id rect = do
    viewp <- get_ptr view_id
    c_set_size viewp (i x) (i y) (i w) (i h)
    where
    i = Util.c_int
    (x, y, w, h) = (Rect.rx rect, Rect.ry rect, Rect.rw rect, Rect.rh rect)
foreign import ccall "set_size"
    c_set_size :: Ptr CView -> CInt -> CInt -> CInt -> CInt -> IO ()

set_view_config :: ViewId -> Block.ViewConfig -> Fltk ()
set_view_config view_id config = do
    viewp <- get_ptr view_id
    with config $ \configp -> c_set_view_config viewp configp
foreign import ccall "set_view_config"
    c_set_view_config :: Ptr CView -> Ptr Block.ViewConfig -> IO ()

set_zoom :: ViewId -> Types.Zoom -> Fltk ()
set_zoom view_id zoom = do
    viewp <- get_ptr view_id
    with zoom $ \zoomp -> c_set_zoom viewp zoomp
foreign import ccall "set_zoom"
    c_set_zoom :: Ptr CView -> Ptr Types.Zoom -> IO ()

-- | Set the scroll along the track dimension, in pixels.
set_track_scroll :: ViewId -> Types.Width -> Fltk ()
set_track_scroll view_id offset = do
    viewp <- get_ptr view_id
    c_set_track_scroll viewp (Util.c_int offset)
foreign import ccall "set_track_scroll"
    c_set_track_scroll :: Ptr CView -> CInt -> IO ()

-- | This is called asynchronously by the play updater, so it takes an extra
-- flag to not throw an exception if the ViewId no longer exists.
set_selection :: Bool -> ViewId -> Types.SelNum -> Maybe CSelection -> Fltk ()
set_selection fail_on_view view_id selnum maybe_sel
    | fail_on_view = set =<< get_ptr view_id
    | otherwise = flip when_just set =<< lookup_ptr view_id
    where
    set viewp = maybeWith with maybe_sel $ \selp ->
        c_set_selection viewp (Util.c_int selnum) selp
foreign import ccall "set_selection"
    c_set_selection :: Ptr CView -> CInt -> Ptr CSelection -> IO ()

set_track_selection :: Bool -> ViewId -> Types.SelNum -> TrackNum
    -> Maybe CSelection -> Fltk ()
set_track_selection fail_on_view view_id selnum tracknum maybe_sel
    | fail_on_view = set =<< get_ptr view_id
    | otherwise = flip when_just set =<< lookup_ptr view_id
    where
    set viewp = maybeWith with maybe_sel $ \selp ->
        c_set_track_selection viewp (Util.c_int selnum)
            (Util.c_int tracknum) selp
foreign import ccall "set_track_selection"
    c_set_track_selection :: Ptr CView -> CInt -> CInt -> Ptr CSelection
        -> IO ()

-- | Max number of selections, hardcoded in ui/config.h.
max_selections :: Int
max_selections = (#const Config::max_selections)

bring_to_front :: ViewId -> Fltk ()
bring_to_front view_id = c_bring_to_front =<< get_ptr view_id
foreign import ccall "bring_to_front" c_bring_to_front :: Ptr CView -> IO ()

-- * Block operations

-- These operate on ViewIds too because there is no block/view distinction at
-- this layer.

set_model_config :: ViewId -> Block.Config -> Fltk ()
set_model_config view_id config = do
    viewp <- get_ptr view_id
    with config $ \configp -> c_set_model_config viewp configp
foreign import ccall "set_model_config"
    c_set_model_config :: Ptr CView -> Ptr Block.Config -> IO ()

set_skeleton :: ViewId -> Skeleton.Skeleton -> Fltk ()
set_skeleton view_id skel = do
    viewp <- get_ptr view_id
    with_skeleton skel $ \skelp -> c_set_skeleton viewp skelp
foreign import ccall "set_skeleton"
    c_set_skeleton :: Ptr CView -> Ptr Skeleton.Skeleton -> IO ()

set_title :: ViewId -> String -> Fltk ()
set_title view_id title = do
    viewp <- get_ptr view_id
    withCString title (c_set_title viewp)
foreign import ccall "set_title" c_set_title :: Ptr CView -> CString -> IO ()

set_status :: ViewId -> String -> Fltk ()
set_status view_id status = do
    viewp <- get_ptr view_id
    withCString status (c_set_status viewp)
foreign import ccall "set_status" c_set_status :: Ptr CView -> CString -> IO ()

-- | Set block-local track status.
set_display_track :: ViewId -> TrackNum -> Block.DisplayTrack -> Fltk ()
set_display_track view_id tracknum dtrack = do
    viewp <- get_ptr view_id
    with dtrack $ \dtrackp ->
        c_set_display_track viewp (Util.c_int tracknum) dtrackp
foreign import ccall "set_display_track"
    c_set_display_track :: Ptr CView -> CInt -> Ptr Block.DisplayTrack -> IO ()

-- * Track operations

insert_track :: ViewId -> TrackNum -> Block.Tracklike -> [Track.TrackEvents]
    -> Types.Width -> Fltk ()
insert_track view_id tracknum tracklike merged width = do
    viewp <- get_ptr view_id
    with_tracklike merged tracklike $ \tp mlistp len ->
        c_insert_track viewp (Util.c_int tracknum) tp
            (Util.c_int width) mlistp len

remove_track :: ViewId -> TrackNum -> Fltk ()
remove_track view_id tracknum = do
    viewp <- get_ptr view_id
    with_finalizer $ \finalize ->
        c_remove_track viewp (Util.c_int tracknum) finalize

update_track :: ViewId -> TrackNum -> Block.Tracklike
    -> [Track.TrackEvents] -> ScoreTime -> ScoreTime -> Fltk ()
update_track view_id tracknum tracklike merged start end = do
    viewp <- get_ptr view_id
    with_finalizer $ \finalize ->
        with start $ \startp -> with end $ \endp ->
            with_tracklike merged tracklike $ \tp mlistp len ->
                c_update_track viewp (Util.c_int tracknum) tp
                    mlistp len finalize startp endp

-- | Like 'update_track' except update everywhere.
update_entire_track :: ViewId -> TrackNum -> Block.Tracklike
    -> [Track.TrackEvents] -> Fltk ()
update_entire_track view_id tracknum tracklike merged =
    -- -1 is special cased in c++.
    update_track view_id tracknum tracklike merged (-1) (-1)

foreign import ccall "insert_track"
    c_insert_track :: Ptr CView -> CInt -> Ptr TracklikePtr -> CInt
        -> Ptr Ruler.Marklist -> CInt -> IO ()
foreign import ccall "remove_track"
    c_remove_track :: Ptr CView -> CInt -> FunPtr (FunPtrFinalizer a) -> IO ()
foreign import ccall "update_track"
    c_update_track :: Ptr CView -> CInt -> Ptr TracklikePtr
        -> Ptr Ruler.Marklist -> CInt -> FunPtr (FunPtrFinalizer a)
        -> Ptr ScoreTime -> Ptr ScoreTime -> IO ()

-- | Unlike other Fltk functions, this doesn't throw if the ViewId is not
-- found.  That's because it's called asynchronously when derivation is
-- complete.
set_track_signal :: ViewId -> TrackNum -> Track.TrackSignal -> Fltk ()
set_track_signal view_id tracknum tsig = do
    maybe_viewp <- lookup_ptr view_id
    when_just maybe_viewp $ \viewp -> with tsig $ \tsigp ->
        c_set_track_signal viewp (Util.c_int tracknum) tsigp
foreign import ccall "set_track_signal"
    c_set_track_signal :: Ptr CView -> CInt -> Ptr Track.TrackSignal -> IO ()

-- | When I do anything that will destroy previous callbacks, I have to pass
-- yet another callback which will be used to mark the old callbacks as done,
-- so that the haskell GC knows it can collect the data those callbacks use.
type FunPtrFinalizer a = FunPtr a -> IO ()
foreign import ccall "wrapper"
    c_make_free_fun_ptr :: FunPtrFinalizer a -> IO (FunPtr (FunPtrFinalizer a))

make_free_fun_ptr :: IO (FunPtr (FunPtrFinalizer a))
make_free_fun_ptr = Util.make_fun_ptr "free_fun_ptr" $
    c_make_free_fun_ptr Util.free_fun_ptr

with_finalizer :: (FunPtr (FunPtrFinalizer a) -> IO c) -> IO c
with_finalizer = Exception.bracket make_free_fun_ptr Util.free_fun_ptr

-- | Convert a Tracklike into the set of pointers that c++ knows it as.
-- A set of event lists can be merged into event tracks.
with_tracklike :: [Track.TrackEvents] -> Block.Tracklike
    -> (Ptr TracklikePtr -> Ptr Ruler.Marklist -> CInt -> IO ()) -> IO ()
with_tracklike merged_events tracklike f = case tracklike of
    Block.T track ruler -> RulerC.with_ruler ruler $ \rulerp mlistp len ->
        TrackC.with_track track merged_events $ \trackp ->
            with (TPtr trackp rulerp) $ \tp -> f tp mlistp len
    Block.R ruler -> RulerC.with_ruler ruler $ \rulerp mlistp len ->
        with (RPtr rulerp) $ \tp ->
            f tp mlistp len
    Block.D div -> with div $ \dividerp -> with (DPtr dividerp) $ \tp ->
        f tp nullPtr 0

data TracklikePtr =
    TPtr (Ptr Track.Track) (Ptr Ruler.Ruler)
    | RPtr (Ptr Ruler.Ruler)
    | DPtr (Ptr Block.Divider)

set_track_width :: ViewId -> TrackNum -> Types.Width -> Fltk ()
set_track_width view_id tracknum width = do
    viewp <- get_ptr view_id
    c_set_track_width viewp (Util.c_int tracknum) (Util.c_int width)
foreign import ccall "set_track_width"
    c_set_track_width :: Ptr CView -> CInt -> CInt -> IO ()

set_track_title :: ViewId -> TrackNum -> String -> Fltk ()
set_track_title view_id tracknum title = do
    viewp <- get_ptr view_id
    withCString title (c_set_track_title viewp (Util.c_int tracknum))
foreign import ccall "set_track_title"
    c_set_track_title :: Ptr CView -> CInt -> CString -> IO ()

-- ** debugging

show_children :: ViewId -> IO String
show_children view_id = do
    viewp <- get_ptr view_id
    c_show_children viewp (Util.c_int (-1)) >>= peekCString
foreign import ccall "i_show_children"
    c_show_children :: Ptr CView -> CInt -> IO CString

-- * storable

-- ** tracks

instance Storable Block.Divider where
    sizeOf _ = #size DividerConfig
    alignment _ = #{alignment DividerConfig}
    poke dividerp (Block.Divider color) =
        (#poke DividerConfig, color) dividerp color

instance Storable TracklikePtr where
    sizeOf _ = #size Tracklike
    alignment _ = #{alignment Tracklike}
    poke = poke_tracklike_ptr

poke_tracklike_ptr tp tracklike_ptr = do
    -- Apparently 'with' doesn't zero out the memory it allocates.
    (#poke Tracklike, track) tp nullPtr
    (#poke Tracklike, ruler) tp nullPtr
    (#poke Tracklike, divider) tp nullPtr
    case tracklike_ptr of
        TPtr trackp rulerp -> do
            (#poke Tracklike, track) tp trackp
            (#poke Tracklike, ruler) tp rulerp
        RPtr rulerp -> (#poke Tracklike, ruler) tp rulerp
        DPtr dividerp -> (#poke Tracklike, divider) tp dividerp

-- ** configs

instance Storable Block.Config where
    sizeOf _ = #size BlockModelConfig
    -- TODO alignment figures the alignment is 1, which seems to be crashing
    -- the gc.  See if this fixes it.
    alignment _ = 4 -- #{alignment BlockModelConfig}
    poke = poke_block_model_config

poke_block_model_config configp
    (Block.Config _sel_colors bg (track_box, track_char) (sb_box, sb_char)) = do
        (#poke BlockModelConfig, bg) configp bg
        (#poke BlockModelConfig, track_box) configp track_box
        (#poke BlockModelConfig, sb_box) configp sb_box
        (#poke BlockModelConfig, track_char) configp track_char
        (#poke BlockModelConfig, sb_char) configp sb_char

instance Storable Block.ViewConfig where
    sizeOf _ = #size BlockViewConfig
    alignment _ = #{alignment BlockViewConfig}
    peek = error "no peek for ViewConfig"
    poke = poke_config

poke_config configp (Block.ViewConfig block track skel sb status) = do
    (#poke BlockViewConfig, block_title_height) configp (Util.c_int block)
    (#poke BlockViewConfig, track_title_height) configp (Util.c_int track)
    (#poke BlockViewConfig, skel_height) configp (Util.c_int skel)
    (#poke BlockViewConfig, sb_size) configp (Util.c_int sb)
    (#poke BlockViewConfig, status_size) configp (Util.c_int status)

instance Storable Block.DisplayTrack where
    sizeOf _ = #size DisplayTrack
    alignment _ = #{alignment DisplayTrack}
    peek = error "no peek for DisplayTrack"
    poke = poke_display_track

poke_display_track dtrackp (Block.DisplayTrack _ _ status brightness) = do
    let (statusc, status_color) = maybe ('\NUL', Color.black) id status
    (#poke DisplayTrack, status) dtrackp statusc
    (#poke DisplayTrack, status_color) dtrackp status_color
    (#poke DisplayTrack, event_brightness) dtrackp brightness

-- ** skeleton

instance Storable Skeleton.Skeleton where
    sizeOf _ = #size SkeletonConfig
    alignment _ = #{alignment SkeletonConfig}
    -- | Because I have to dynamically allocate arrays and pass their pointers,
    -- the real work is done by 'poke_skeleton'.
    poke _ _ = return ()

with_skeleton skel f =
    withArrayLen ps $ \len pp -> withArray cs $ \cp -> alloca $ \skelp -> do
        poke_skeleton skelp (Util.c_int len) pp cp
        f skelp
    where
    (parents, children) = unzip (Skeleton.flatten skel)
    -- The -1s are because the fltk set_skeleton doesn't the ruler track, while
    -- of course the tracknums here do.
    -- TODO would it be better to put this in BlockView::set_skeleton?
    (ps, cs) = (map (Util.c_int . subtract 1) parents,
        map (Util.c_int . subtract 1) children)

poke_skeleton :: Ptr Skeleton.Skeleton -> CInt -> Ptr CInt -> Ptr CInt -> IO ()
poke_skeleton skelp len parentsp childrenp = do
    (#poke SkeletonConfig, len) skelp len
    (#poke SkeletonConfig, parents) skelp parentsp
    (#poke SkeletonConfig, children) skelp childrenp

-- ** selection

-- | C++ Selections have a color, but in haskell the color is separated into
-- Block.config_selection_colors.
data CSelection = CSelection Color.Color Types.Selection deriving (Show)

instance Storable CSelection where
    sizeOf _ = #size Selection
    alignment _ = #{alignment Selection}
    peek = error "no peek selection"
    poke = poke_selection

poke_selection selp (CSelection color
        (Types.Selection start_track start_pos cur_track cur_pos)) = do
    (#poke Selection, color) selp color
    (#poke Selection, start_track) selp (Util.c_int start_track)
    (#poke Selection, start_pos) selp start_pos
    (#poke Selection, cur_track) selp (Util.c_int cur_track)
    (#poke Selection, cur_pos) selp cur_pos
