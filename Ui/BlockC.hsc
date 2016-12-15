-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
{-# LANGUAGE CPP #-}
module Ui.BlockC (
    -- * view creation
    create_view, destroy_view, get_view_status
    -- ** set other attributes
    , set_size
    , set_zoom
    , set_track_scroll
    , Selection(..)
    , set_selection
    , bring_to_front

    -- * Block operations
    , set_config, set_skeleton, set_title, set_status
    , set_display_track
    -- ** floating input
    , floating_open, floating_insert

    -- ** Track operations
    , tracks, insert_track, remove_track, update_track, update_entire_track
    , set_track_signal
    , set_track_title, set_track_title_focus, set_block_title_focus

    -- * debugging
    , show_children, dump
) where
-- The double hashes quote them for hsc2hs.  I have to delay the CPP since
-- hsc2hs never has STUB_OUT_FLTK defined.
##ifdef STUB_OUT_FLTK
import Ui.BlockCStub
##else
import qualified Control.Exception as Exception
import qualified Data.Map as Map

import Util.ForeignC
import qualified Util.Rect as Rect

import qualified Ui.Color as Color
import qualified Ui.Types as Types
import qualified Ui.Util as Util
import Ui.Ui (Fltk, fltk)

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.PtrMap as PtrMap
import Ui.PtrMap (CView)
import qualified Ui.Ruler as Ruler
import qualified Ui.RulerC as RulerC
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.TrackC as TrackC

import qualified App.Config as Config
import Types
import Global

#include "Ui/c_interface.h"


-- * view creation

-- | Create an empty block view with the given configs.  Tracks must be
-- inserted separately.
create_view :: ViewId -> Text -> Rect.Rect -> Block.Config -> Fltk ()
create_view view_id window_title rect block_config = fltk $ exc "create_view" $
    PtrMap.modify $ \ptr_map -> do
        when (view_id `Map.member` ptr_map) $
            PtrMap.throw $ show view_id ++ " already in displayed view list: "
                ++ show (Map.assocs ptr_map)
        viewp <- Util.withText window_title $ \titlep ->
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
destroy_view view_id = fltk $ exc "destroy_view" $ do
    viewp <- PtrMap.get view_id
    PtrMap.modify $ \ptr_map -> do
        c_destroy viewp
        return $ Map.delete view_id ptr_map
foreign import ccall "destroy" c_destroy :: Ptr CView -> IO ()

-- | Get various position metrics of the window.
--
-- This is unused because I sync this with UI msgs, but if they prove
-- unreliable I could use this to verify or just replace them.
get_view_status :: ViewId -> Fltk (Rect.Rect, Types.Zoom, Int, Int)
    -- ^ (rect, zoom, time_padding, track_padding)
get_view_status view_id = fltk $ exc "get_view_status" $ do
    viewp <- PtrMap.get view_id
    alloca $ \rectp -> alloca $ \zoomp ->
        alloca $ \timep -> alloca $ \trackp -> do
            c_get_view_status viewp rectp zoomp timep trackp
            (,,,) <$> peek rectp <*> peek zoomp
                <*> (fromIntegral <$> peek timep)
                <*> (fromIntegral <$> peek trackp)
foreign import ccall "get_view_status"
    c_get_view_status :: Ptr CView -> Ptr Rect.Rect -> Ptr Types.Zoom
        -> Ptr CInt -> Ptr CInt -> IO ()

-- ** Set other attributes

set_size :: ViewId -> Rect.Rect -> Fltk ()
set_size view_id rect = fltk $ exc "set_size" $ do
    viewp <- PtrMap.get view_id
    c_set_size viewp (i x) (i y) (i w) (i h)
    where
    i = Util.c_int
    (x, y, w, h) = (Rect.rx rect, Rect.ry rect, Rect.rw rect, Rect.rh rect)
foreign import ccall "set_size"
    c_set_size :: Ptr CView -> CInt -> CInt -> CInt -> CInt -> IO ()

set_zoom :: ViewId -> Types.Zoom -> Fltk ()
set_zoom view_id zoom = fltk $ exc "set_zoom" $ do
    viewp <- PtrMap.get view_id
    with zoom $ \zoomp -> c_set_zoom viewp zoomp
foreign import ccall "set_zoom"
    c_set_zoom :: Ptr CView -> Ptr Types.Zoom -> IO ()

-- | Set the scroll along the track dimension, in pixels.
set_track_scroll :: ViewId -> Types.Width -> Fltk ()
set_track_scroll view_id offset = fltk $ exc "set_track_scroll" $ do
    viewp <- PtrMap.get view_id
    c_set_track_scroll viewp (Util.c_int offset)
foreign import ccall "set_track_scroll"
    c_set_track_scroll :: Ptr CView -> CInt -> IO ()

set_selection :: ViewId -> Sel.Num -> [TrackNum] -> [Selection] -> Fltk ()
set_selection view_id selnum tracknums sels
    | null tracknums = fltk $ return ()
    | otherwise = fltk $ exc "set_selection" $ do
        viewp <- PtrMap.get view_id
        withArrayLenNull sels $ \nsels selsp -> forM_ tracknums $ \tracknum ->
            c_set_selection viewp (Util.c_int selnum) (Util.c_int tracknum)
                selsp (Util.c_int nsels)
foreign import ccall "set_selection"
    c_set_selection :: Ptr CView -> CInt -> CInt -> Ptr Selection -> CInt
        -> IO ()

bring_to_front :: ViewId -> Fltk ()
bring_to_front view_id = fltk $ exc "bring_to_front" $
    c_bring_to_front =<< PtrMap.get view_id
foreign import ccall "bring_to_front" c_bring_to_front :: Ptr CView -> IO ()

-- * Block operations

-- These operate on ViewIds too because there is no block/view distinction at
-- this layer.

set_config :: ViewId -> Block.Config -> Fltk ()
set_config view_id config = fltk $ exc "set_config" $ do
    viewp <- PtrMap.get view_id
    with config $ \configp -> c_set_model_config viewp configp
foreign import ccall "set_config"
    c_set_model_config :: Ptr CView -> Ptr Block.Config -> IO ()

set_skeleton :: ViewId -> Skeleton.Skeleton
    -> [(Color.Color, [(TrackNum, TrackNum)])] -> [Block.Status] -> Fltk ()
set_skeleton view_id skel integrate_edges statuses =
    fltk $ exc "set_skeleton" $ do
        viewp <- PtrMap.get view_id
        with_skeleton_config (skeleton_edges skel integrate_edges) statuses $
            \configp -> c_set_skeleton viewp configp
foreign import ccall "set_skeleton"
    c_set_skeleton :: Ptr CView -> Ptr SkeletonConfig -> IO ()

set_title :: ViewId -> Text -> Fltk ()
set_title view_id title = fltk $ exc "set_title" $ do
    viewp <- PtrMap.get view_id
    Util.withText title (c_set_title viewp)
foreign import ccall "set_title" c_set_title :: Ptr CView -> CString -> IO ()

set_status :: ViewId -> Text -> Color.Color -> Fltk ()
set_status view_id status color = fltk $ exc "set_status" $ do
    viewp <- PtrMap.get view_id
    Util.withText status $ \statusp -> with color $ \colorp ->
        c_set_status viewp statusp colorp
foreign import ccall "set_status"
    c_set_status :: Ptr CView -> CString -> Ptr Color.Color -> IO ()

-- | Set block-local track status.
set_display_track :: ViewId -> TrackNum -> Block.DisplayTrack -> Fltk ()
set_display_track view_id tracknum dtrack = fltk $ exc "set_display_track" $ do
    viewp <- PtrMap.get view_id
    with dtrack $ \dtrackp ->
        c_set_display_track viewp (Util.c_int tracknum) dtrackp
foreign import ccall "set_display_track"
    c_set_display_track :: Ptr CView -> CInt -> Ptr Block.DisplayTrack -> IO ()

-- ** floating input

floating_open :: ViewId -> TrackNum -> ScoreTime -> Text -> (Int, Int)
    -> Fltk ()
floating_open view_id tracknum pos text (sel_start, sel_end) =
    fltk $ exc "floating_open" $ do
        viewp <- PtrMap.get view_id
        Util.withText text $ \textp ->
            c_floating_open viewp (Util.c_int tracknum)
                (ScoreTime.to_cdouble pos) textp (Util.c_int sel_start)
                (Util.c_int sel_end)
foreign import ccall "floating_open"
    c_floating_open :: Ptr CView -> CInt -> CDouble -> CString -> CInt -> CInt
        -> IO ()

floating_insert :: [ViewId] -> Text -> Fltk ()
floating_insert view_ids text = fltk $ exc "floating_insert" $
    Util.withText text $ \textp -> forM_ view_ids $ \view_id -> do
        viewp <- PtrMap.get view_id
        c_floating_insert viewp textp
foreign import ccall "floating_insert"
    c_floating_insert :: Ptr CView -> CString -> IO ()

-- * Track operations

-- | Get the number of tracks on the block.
tracks :: ViewId -> Fltk TrackNum
tracks view_id = fltk $ exc "tracks" $
    fromIntegral <$> (c_tracks =<< PtrMap.get view_id)
foreign import ccall "tracks" c_tracks  :: Ptr CView -> IO CInt

insert_track :: ViewId -> TrackNum -> Block.Tracklike -> [Events.Events]
    -> Track.SetStyle -> Types.Width -> Fltk ()
insert_track view_id tracknum tracklike merged set_style width =
    fltk $ exc "insert_track" $ do
        viewp <- PtrMap.get view_id
        with_tracklike True merged set_style tracklike $ \tp mlistp len ->
            c_insert_track viewp (Util.c_int tracknum) tp
                (Util.c_int width) mlistp len

remove_track :: ViewId -> TrackNum -> Fltk ()
remove_track view_id tracknum = fltk $ exc "remove_track" $ do
    viewp <- PtrMap.get view_id
    c_remove_track viewp (Util.c_int tracknum)

update_track :: Bool -- ^ True if the ruler has changed and should be copied
    -- over.  It's a bit of a hack to be a separate flag, but rulers are
    -- updated rarely and copied over entirely for efficiency.
    -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> ScoreTime -> ScoreTime -> Fltk ()
update_track update_ruler view_id tracknum tracklike merged set_style start
        end = fltk $ exc "update_track" $ do
    viewp <- PtrMap.get view_id
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
set_track_signal view_id tracknum tsig = fltk $ exc "set_track_signal" $ do
    maybe_viewp <- PtrMap.lookup view_id
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
set_track_title view_id tracknum title = fltk $ exc "set_track_title" $ do
        viewp <- PtrMap.get view_id
        Util.withText title (c_set_track_title viewp (Util.c_int tracknum))
foreign import ccall "set_track_title"
    c_set_track_title :: Ptr CView -> CInt -> CString -> IO ()

set_track_title_focus :: ViewId -> TrackNum -> Fltk ()
set_track_title_focus view_id tracknum = fltk $ exc "set_track_title_focus" $ do
    viewp <- PtrMap.get view_id
    c_set_track_title_focus viewp (Util.c_int tracknum)
foreign import ccall "set_track_title_focus"
    c_set_track_title_focus :: Ptr CView -> CInt -> IO ()

set_block_title_focus :: ViewId -> Fltk ()
set_block_title_focus view_id = fltk $ exc "set_block_title_focus " $
    c_set_block_title_focus =<< PtrMap.get view_id
foreign import ccall "set_block_title_focus"
    c_set_block_title_focus :: Ptr CView -> IO ()


-- ** debugging

show_children :: ViewId -> IO String
show_children view_id = exc "show_children" $ do
    viewp <- PtrMap.get view_id
    c_show_children viewp (Util.c_int (-1)) >>= peekCString
foreign import ccall "i_show_children"
    c_show_children :: Ptr CView -> CInt -> IO CString

dump :: IO [(ViewId, String)]
dump = do
    views <- Map.toList <$> PtrMap.get_map
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
    poke tp tracklike_ptr = do
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
    sizeOf _ = #size BlockConfig
    alignment _ = alignment Color.black
    poke configp (Block.Config skel_box track_box sb_box) = do
        (#poke BlockConfig, skel_box) configp skel_box
        (#poke BlockConfig, track_box) configp track_box
        (#poke BlockConfig, sb_box) configp sb_box

instance CStorable Block.Box where
    sizeOf _ = #size BlockBox
    alignment _ = alignment Color.black
    poke boxp (Block.Box color char) = do
        (#poke BlockBox, color) boxp color
        (#poke BlockBox, c) boxp (Util.c_rune char)

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
    (#poke DisplayTrack, status1) dtrackp (Util.c_rune c1)
    (#poke DisplayTrack, status2) dtrackp (Util.c_rune c2)

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
        (#poke SkeletonStatus, status1) edgep (Util.c_rune c1)
        (#poke SkeletonStatus, status2) edgep (Util.c_rune c2)
        (#poke SkeletonStatus, color) edgep color

-- ** selection

-- | This is the low level version of 'Sel.Selection'.  It only applies to
-- a single track, and has an explicit color.
data Selection = Selection {
    sel_color :: !Color.Color
    , sel_start :: !TrackTime
    , sel_cur :: !TrackTime
    , sel_draw_arrow :: !Bool
    }
    deriving (Eq, Ord, Show)

instance CStorable Selection where
    sizeOf _ = #size Selection
    alignment _ = alignment (0 :: TrackTime)
    peek = error "Selection peek unimplemented"
    poke selp (Selection color start cur draw_arrow) = do
        (#poke Selection, color) selp color
        (#poke Selection, start) selp start
        (#poke Selection, cur) selp cur
        (#poke Selection, draw_arrow) selp (Util.c_bool draw_arrow)

-- * util

-- | Annotate thrown exceptions with a provenance.
exc :: String -> IO a -> IO a
exc name action = Exception.catch action $ \(PtrMap.FltkException exc) ->
    Exception.throwIO $ PtrMap.FltkException $ name <> ": " <> exc

##endif
