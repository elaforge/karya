-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
{- | This layer gives direct wrapped access to the fltk API.

    It maintains a map from ViewIds to window pointers, which represents the on
    screen state.  All functions here take ViewIds, and will throw an exception
    if the view_id isn't in the pointer map.  C++ exceptions coming from FLTK
    should be converted and thrown as haskell exceptions.

    TODO exceptions are not implemented yet

    There are three methods for sharing data with C++:

    - Note tracks pass a callback, that will return events in the given
    ScoreTime range.  This means the complete map of events exists only on the
    Haskell side, and changing events means passing a new callback to C++.
    This is because events change a lot, and it seems wasteful to copy over
    a whole track of events just because one was added.

    - Rulers have marklists, which are like events but much denser.  They're
    entirely marshalled to a C array so C++ has direct access to the marks.
    Because many tracks will share the same ruler, marklists use a reference
    counting scheme so Haskell can allocate the array once, and then C++
    and Haskell can cooperate on the memory management.

    - Control signals are just copied, and ownership is given to C++.  This
    is because control signals are large but are already dense arrays so they
    can be copied very quickly with memcpy.  They could use a refcounting
    scheme like ruler marklists, but they're rarely shared so it's probably not
    worth it.
-}
module Ui.BlockC (
    -- | lookup_id and CView are only exported for Ui.UiMsgC which is a slight
    -- abstraction breakage.
    lookup_id, CView

    -- * view creation
    , create_view, destroy_view
    -- ** set other attributes
    , set_size
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
    -- ** edit input
    , edit_open, edit_append

    -- ** Track operations
    , insert_track, remove_track, update_track, update_entire_track
    , set_track_signal
    , set_track_title, set_track_title_focus, set_block_title_focus

    -- * debugging
    , show_children, dump
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Typeable as Typeable
import Util.ForeignC
import qualified System.IO.Unsafe as Unsafe

import Util.Control
import qualified Util.Rect as Rect

import qualified Ui.Color as Color
import qualified Ui.Types as Types
import qualified Ui.Util as Util
import Ui.Util (Fltk)

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.RulerC as RulerC
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.TrackC as TrackC

import qualified App.Config as Config
import Types


#include "Ui/c_interface.h"
-- This is from http://haskell.org/haskellwiki/FFI_cook_book.  Is there a
-- better way?  I dunno, but this is clever and looks like it should work.

withText :: Text -> (CString -> IO a) -> IO a
withText = ByteString.useAsCString . Text.Encoding.encodeUtf8

-- * errors

-- Phantom type for block view ptrs.
data CView

-- | Global map of view IDs to their windows.  This is global mutable state
-- because the underlying window system is also global mutable state, and is
-- not well represented by a persistent functional state.
{-# NOINLINE view_id_to_ptr #-}
view_id_to_ptr :: MVar.MVar (Map.Map ViewId (Ptr CView))
view_id_to_ptr = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

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

-- | Nothing indicates that the UI returned a view ptr I didn't know I had.
-- It's rare, but it can happen if I close a window but a msg about it is still in the queue.
lookup_id :: Ptr CView -> IO (Maybe ViewId)
lookup_id viewp = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    return $ fst <$> List.find ((==viewp) . snd) (Map.assocs ptr_map)

-- * view creation

-- | Create an empty block view with the given configs.  Tracks must be
-- inserted separately.
create_view :: ViewId -> Text -> Rect.Rect -> Block.Config -> Fltk ()
create_view view_id window_title rect block_config =
    MVar.modifyMVar_ view_id_to_ptr $ \ptr_map -> do
        when (view_id `Map.member` ptr_map) $
            throw $ show view_id ++ " already in displayed view list: "
                ++ show (Map.assocs ptr_map)
        viewp <- withText window_title $ \titlep ->
            with block_config $ \configp ->
                c_create (i x) (i y) (i w) (i h) titlep configp
        return $ Map.insert view_id viewp ptr_map
    where
    (x, y, w, h) = (Rect.rx rect, Rect.ry rect, Rect.rw rect, Rect.rh rect)
    i = Util.c_int

foreign import ccall "create"
    c_create :: CInt -> CInt -> CInt -> CInt -> CString -> Ptr Block.Config
        -> IO (Ptr CView)

destroy_view :: ViewId -> Fltk ()
destroy_view view_id = do
    viewp <- get_ptr view_id
    MVar.modifyMVar_ view_id_to_ptr $ \ptr_map -> do
        c_destroy viewp
        return $ Map.delete view_id ptr_map
foreign import ccall "destroy" c_destroy :: Ptr CView -> IO ()

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

-- | This is called asynchronously by the play monitor, so it takes an extra
-- flag to not throw an exception if the ViewId no longer exists.
set_selection :: Bool -> ViewId -> Types.SelNum -> Maybe CSelection -> Fltk ()
set_selection fail_on_view view_id selnum maybe_sel
    | fail_on_view = set =<< get_ptr view_id
    | otherwise = flip whenJust set =<< lookup_ptr view_id
    where
    set viewp = maybeWith with maybe_sel $ \selp ->
        c_set_selection viewp (Util.c_int selnum) selp
foreign import ccall "set_selection"
    c_set_selection :: Ptr CView -> CInt -> Ptr CSelection -> IO ()

set_track_selection :: Bool -> ViewId -> Types.SelNum -> TrackNum
    -> Maybe CSelection -> Fltk ()
set_track_selection fail_on_view view_id selnum tracknum maybe_sel
    | fail_on_view = set =<< get_ptr view_id
    | otherwise = flip whenJust set =<< lookup_ptr view_id
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

set_skeleton :: ViewId -> Skeleton.Skeleton
    -> [(Color.Color, [(TrackNum, TrackNum)])] -> [Block.Status] -> Fltk ()
set_skeleton view_id skel integrate_edges statuses = do
    viewp <- get_ptr view_id
    with_skeleton_config (skeleton_edges skel integrate_edges) statuses $
        \configp -> c_set_skeleton viewp configp
foreign import ccall "set_skeleton"
    c_set_skeleton :: Ptr CView -> Ptr SkeletonConfig -> IO ()

set_title :: ViewId -> Text -> Fltk ()
set_title view_id title = do
    viewp <- get_ptr view_id
    withText title (c_set_title viewp)
foreign import ccall "set_title" c_set_title :: Ptr CView -> CString -> IO ()

set_status :: ViewId -> Text -> Color.Color -> Fltk ()
set_status view_id status color = do
    viewp <- get_ptr view_id
    withText status $ \statusp -> with color $ \colorp ->
        c_set_status viewp statusp colorp
foreign import ccall "set_status"
    c_set_status :: Ptr CView -> CString -> Ptr Color.Color -> IO ()

-- | Set block-local track status.
set_display_track :: ViewId -> TrackNum -> Block.DisplayTrack -> Fltk ()
set_display_track view_id tracknum dtrack = do
    viewp <- get_ptr view_id
    with dtrack $ \dtrackp ->
        c_set_display_track viewp (Util.c_int tracknum) dtrackp
foreign import ccall "set_display_track"
    c_set_display_track :: Ptr CView -> CInt -> Ptr Block.DisplayTrack -> IO ()

-- ** edit input

edit_open :: ViewId -> TrackNum -> ScoreTime -> Text -> Maybe (Int, Int)
    -> Fltk ()
edit_open view_id tracknum pos text selection = do
    viewp <- get_ptr view_id
    let (sel_start, sel_end) = fromMaybe (-1, 0) selection
    Util.withText text $ \textp ->
        c_edit_open viewp (Util.c_int tracknum) (ScoreTime.to_cdouble pos)
            textp (Util.c_int sel_start) (Util.c_int sel_end)
foreign import ccall "edit_open"
    c_edit_open :: Ptr CView -> CInt -> CDouble -> CString -> CInt -> CInt
        -> IO ()

edit_append :: [ViewId] -> Text -> Fltk ()
edit_append view_ids text =
    Util.withText text $ \textp -> forM_ view_ids $ \view_id -> do
        viewp <- get_ptr view_id
        c_edit_append viewp textp
foreign import ccall "edit_append"
    c_edit_append :: Ptr CView -> CString -> IO ()

-- * Track operations

insert_track :: ViewId -> TrackNum -> Block.Tracklike -> [Events.Events]
    -> Track.SetStyle -> Types.Width -> Fltk ()
insert_track view_id tracknum tracklike merged set_style width = do
    viewp <- get_ptr view_id
    with_tracklike True merged set_style tracklike $ \tp mlistp len ->
        c_insert_track viewp (Util.c_int tracknum) tp
            (Util.c_int width) mlistp len

remove_track :: ViewId -> TrackNum -> Fltk ()
remove_track view_id tracknum = do
    viewp <- get_ptr view_id
    c_remove_track viewp (Util.c_int tracknum)

update_track :: Bool -- ^ True if the ruler has changed and should be copied
    -- over.  It's a bit of a hack to be a separate flag, but rulers are
    -- updated rarely and copied over entirely for efficiency.
    -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> ScoreTime -> ScoreTime -> Fltk ()
update_track update_ruler view_id tracknum tracklike merged set_style start
        end = do
    viewp <- get_ptr view_id
    with_tracklike update_ruler merged set_style tracklike $ \tp mlistp len ->
        c_update_track viewp (Util.c_int tracknum) tp mlistp len
            (ScoreTime.to_cdouble start) (ScoreTime.to_cdouble end)

-- | Like 'update_track' except update everywhere.
update_entire_track :: Bool -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> Fltk ()
update_entire_track update_ruler view_id tracknum tracklike merged set_style =
    -- -1 is special cased in c++.
    update_track update_ruler view_id tracknum tracklike merged set_style
        (-1) (-1)

foreign import ccall "insert_track"
    c_insert_track :: Ptr CView -> CInt -> Ptr TracklikePtr -> CInt
        -> Ptr (Ptr Ruler.Marklist) -> CInt -> IO ()
foreign import ccall "remove_track"
    c_remove_track :: Ptr CView -> CInt -> IO ()
foreign import ccall "update_track"
    c_update_track :: Ptr CView -> CInt -> Ptr TracklikePtr
        -> Ptr (Ptr Ruler.Marklist) -> CInt -> CDouble -> CDouble -> IO ()

-- | Unlike other Fltk functions, this doesn't throw if the ViewId is not
-- found.  That's because it's called asynchronously when derivation is
-- complete.
set_track_signal :: ViewId -> TrackNum -> Track.TrackSignal -> Fltk ()
set_track_signal view_id tracknum tsig = do
    maybe_viewp <- lookup_ptr view_id
    whenJust maybe_viewp $ \viewp -> with tsig $ \tsigp ->
        c_set_track_signal viewp (Util.c_int tracknum) tsigp
foreign import ccall "set_track_signal"
    c_set_track_signal :: Ptr CView -> CInt -> Ptr Track.TrackSignal -> IO ()

-- | Convert a Tracklike into the set of pointers that c++ knows it as.
-- A set of event lists can be merged into event tracks.
with_tracklike :: Bool -> [Events.Events] -> Track.SetStyle -> Block.Tracklike
    -> (Ptr TracklikePtr -> Ptr (Ptr Ruler.Marklist) -> CInt -> IO ()) -> IO ()
with_tracklike update_ruler merged_events set_style tracklike f =
    case tracklike of
        Block.T track ruler -> with_ruler ruler $ \rulerp mlistp len ->
            TrackC.with_track track set_style merged_events $ \trackp ->
                with (TPtr trackp rulerp) $ \tp -> f tp mlistp len
        Block.R ruler -> RulerC.with_ruler ruler $ \rulerp mlistp len ->
            with (RPtr rulerp) $ \tp -> f tp mlistp len
        Block.D div -> with div $ \dividerp -> with (DPtr dividerp) $ \tp ->
            f tp nullPtr 0
    where
    with_ruler = if update_ruler then RulerC.with_ruler
        else const RulerC.no_ruler

data TracklikePtr =
    TPtr (Ptr Track.Track) (Ptr Ruler.Ruler)
    | RPtr (Ptr Ruler.Ruler)
    | DPtr (Ptr Block.Divider)

set_track_title :: ViewId -> TrackNum -> Text -> Fltk ()
set_track_title view_id tracknum title = do
    viewp <- get_ptr view_id
    withText title (c_set_track_title viewp (Util.c_int tracknum))
foreign import ccall "set_track_title"
    c_set_track_title :: Ptr CView -> CInt -> CString -> IO ()

set_track_title_focus :: ViewId -> TrackNum -> Fltk ()
set_track_title_focus view_id tracknum = do
    viewp <- get_ptr view_id
    c_set_track_title_focus viewp (Util.c_int tracknum)
foreign import ccall "set_track_title_focus"
    c_set_track_title_focus :: Ptr CView -> CInt -> IO ()

set_block_title_focus :: ViewId -> Fltk ()
set_block_title_focus view_id = c_set_block_title_focus =<< get_ptr view_id
foreign import ccall "set_block_title_focus"
    c_set_block_title_focus :: Ptr CView -> IO ()


-- ** debugging

show_children :: ViewId -> IO String
show_children view_id = do
    viewp <- get_ptr view_id
    c_show_children viewp (Util.c_int (-1)) >>= peekCString
foreign import ccall "i_show_children"
    c_show_children :: Ptr CView -> CInt -> IO CString

dump :: IO [(ViewId, String)]
dump = do
    views <- Map.toList <$> MVar.readMVar view_id_to_ptr
    dumps <- mapM (c_dump_view . snd) views
    dumps <- mapM peekCString dumps
    return $ zip (map fst views) dumps
foreign import ccall "dump_view" c_dump_view :: Ptr CView -> IO CString

-- * storable

-- ** tracks

instance CStorable Block.Divider where
    sizeOf _ = #size DividerConfig
    alignment _ = alignment Color.black
    poke dividerp (Block.Divider color) =
        (#poke DividerConfig, color) dividerp color

instance CStorable TracklikePtr where
    sizeOf _ = #size Tracklike
    alignment _ = alignment nullPtr
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

instance CStorable Block.Config where
    sizeOf _ = #size BlockModelConfig
    alignment _ = alignment Color.black
    poke = poke_block_model_config

poke_block_model_config configp
    (Block.Config skel_box track_box sb_box) = do
        (#poke BlockModelConfig, skel_box) configp skel_box
        (#poke BlockModelConfig, track_box) configp track_box
        (#poke BlockModelConfig, sb_box) configp sb_box

instance CStorable Block.Box where
    sizeOf _ = #size BlockBox
    alignment _ = alignment Color.black
    poke boxp (Block.Box color char) = do
        (#poke BlockBox, color) boxp color
        (#poke BlockBox, c) boxp (Util.c_char char)

instance CStorable Block.DisplayTrack where
    sizeOf _ = #size DisplayTrack
    alignment _ = alignment (0 :: CDouble)
    peek = error "DisplayTrack peek unimplemented"
    poke = poke_display_track

poke_display_track dtrackp (Block.DisplayTrack _ width _ status bright) = do
    let (c1 : c2 : _, color) = track_status status
    (#poke DisplayTrack, event_brightness) dtrackp (Util.c_double bright)
    (#poke DisplayTrack, width) dtrackp (Util.c_int width)
    (#poke DisplayTrack, status_color) dtrackp color
    (#poke DisplayTrack, status1) dtrackp (Util.c_char c1)
    (#poke DisplayTrack, status2) dtrackp (Util.c_char c2)

track_status :: Block.Status -> (String, Color.Color)
track_status Nothing = (repeat '\0', Color.black)
track_status (Just (status, color)) = (status ++ repeat '\0', color)

-- ** skeleton

skeleton_edges :: Skeleton.Skeleton -> [(Color.Color, [(TrackNum, TrackNum)])]
    -> [SkeletonEdge]
skeleton_edges skel integrate_edges =
    [edge p c 0 Config.skeleton | (p, c) <- Skeleton.flatten skel]
    ++ [edge p c 0 color | (color, edges) <- integrate_edges, (p, c) <- edges]
    where
    edge p c = SkeletonEdge (p-1) (c-1)
    -- The -1s are because the fltk set_skeleton doesn't count the ruler track,
    -- while of course the tracknums here do.
    -- TODO would it be better to put this in BlockView::set_skeleton?

with_skeleton_config :: [SkeletonEdge] -> [Block.Status]
    -> (Ptr SkeletonConfig -> IO a) -> IO a
with_skeleton_config edges statuses f =
    withArrayLen edges $ \edges_len edgesp ->
    -- The first status is for the ruler track, which is never displayed, since
    -- the skeleton display starts at the first track.
    withArrayLen (map SkeletonStatus (drop 1 statuses)) $ \slen statusesp ->
    alloca $ \skelp -> do
        (#poke SkeletonConfig, edges_len) skelp (Util.c_int edges_len)
        (#poke SkeletonConfig, edges) skelp edgesp
        (#poke SkeletonConfig, statuses_len) skelp (Util.c_int slen)
        (#poke SkeletonConfig, statuses) skelp statusesp
        f skelp

data SkeletonConfig
instance CStorable SkeletonConfig where
    sizeOf _ = #size SkeletonConfig
    alignment _ = alignment nullPtr -- contains pointers and ints
    -- peek and poke intentionally not implemented, due to dynamic allocation
    -- for internal pointers.  Use 'with_skeleton_config' instead.

data SkeletonEdge = SkeletonEdge !TrackNum !TrackNum !Types.Width !Color.Color
    deriving (Show)

instance CStorable SkeletonEdge where
    sizeOf _ = #size SkeletonEdge
    alignment _ = alignment Color.black
    peek _ = error "SkeletonEdge peek unimplemented"
    poke edgep (SkeletonEdge parent child width color) = do
        (#poke SkeletonEdge, parent) edgep (Util.c_int parent)
        (#poke SkeletonEdge, child) edgep (Util.c_int child)
        (#poke SkeletonEdge, width) edgep (Util.c_int width)
        (#poke SkeletonEdge, color) edgep color

newtype SkeletonStatus = SkeletonStatus Block.Status
instance CStorable SkeletonStatus where
    sizeOf _ = #size SkeletonStatus
    alignment _ = alignment Color.black
    peek _ = error "SkeletonStatus peek unimplemented"
    poke edgep (SkeletonStatus status) = do
        let (c1 : c2 : _, color) = track_status status
        (#poke SkeletonStatus, status1) edgep (Util.c_char c1)
        (#poke SkeletonStatus, status2) edgep (Util.c_char c2)
        (#poke SkeletonStatus, color) edgep color

-- ** selection

-- | C++ Selections have a color, but in haskell the color is separated into
-- Block.config_selection_colors.
data CSelection = CSelection Color.Color Types.Selection deriving (Show)

instance CStorable CSelection where
    sizeOf _ = #size Selection
    alignment _ = alignment Color.black
    peek = error "CSelection peek unimplemented"
    poke = poke_selection

poke_selection selp (CSelection color
        (Types.Selection start_track start_pos cur_track cur_pos)) = do
    (#poke Selection, color) selp color
    (#poke Selection, start_track) selp (Util.c_int start_track)
    (#poke Selection, start_pos) selp start_pos
    (#poke Selection, cur_track) selp (Util.c_int cur_track)
    (#poke Selection, cur_pos) selp cur_pos

-- * error

newtype FltkException = FltkException String deriving (Typeable.Typeable)
instance Exception.Exception FltkException
instance Show FltkException where
    show (FltkException msg) = "FltkException: " ++ msg

throw :: String -> IO a
throw = Exception.throwIO . FltkException
