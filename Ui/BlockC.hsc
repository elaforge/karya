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
    -- * query
    get_screens
    -- * view creation
    , create_view, destroy_view, get_view_status
    -- ** set other attributes
    , set_size
    , set_zoom
    , set_track_scroll
    , Selection(..), SelectionOrientation(..)
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
    , set_waveform, clear_waveforms, gc_waveforms
    , set_track_title, set_track_title_focus, set_block_title_focus

    -- * debugging
    , print_debug, dump
) where
-- The double hashes quote them for hsc2hs.  I have to delay the CPP since
-- hsc2hs never has STUB_OUT_FLTK defined.
##ifdef STUB_OUT_FLTK
import           Ui.BlockCStub
##else
import qualified Control.Exception as Exception
import qualified Data.Map as Map

import qualified Util.FFI as FFI
import qualified Util.Rect as Rect

import qualified Ui.Color as Color
import qualified Ui.Fltk as Fltk
import           Ui.Fltk (Fltk)
import qualified Ui.Types as Types
import qualified Ui.Zoom as Zoom

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.PtrMap as PtrMap
import           Ui.PtrMap (CView)
import qualified Ui.Ruler as Ruler
import qualified Ui.RulerC as RulerC
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.TrackC as TrackC

import qualified App.Config as Config

import           ForeignC
import           Global
import           Types

#include "Ui/c_interface.h"


-- | If true, print out UI calls as they happen.
debug :: Bool
debug = False

-- | Put a fltk action in the Fltk monad.  Since Fltk derives Monad, do syntax
-- will also do that implicitly, but this also annotates IO exceptions, which
-- can happen on a bad ViewId, and perhaps someday from C++ exceptions.
fltk :: Pretty args => String -> args -> IO a -> Fltk a
fltk name args action = do
    when debug $
        liftIO $ putStrLn $ name <> " " <> prettys args
    Fltk.fltk  $ annotate name action

-- | Annotate thrown exceptions with a provenance.
annotate :: String -> IO a -> IO a
annotate name action = Exception.catch action $ \(PtrMap.FltkException exc) ->
    Exception.throwIO $ PtrMap.FltkException $ name <> ": " <> exc

-- * query

get_screens :: IO [Rect.Rect]
get_screens = alloca $ \screenspp -> do
    count <- c_get_screens screenspp
    screensp <- peek screenspp
    screens <- peekArray count screensp
    free screensp
    return screens

foreign import ccall "get_screens"
    c_get_screens :: Ptr (Ptr Rect.Rect) -> IO Int

-- * view creation

-- | Create an empty block view with the given configs.  Tracks must be
-- inserted separately.
create_view :: ViewId -> Text -> Rect.Rect -> Block.Config -> Fltk ()
create_view view_id window_title rect block_config =
    fltk "create_view" window_title $ PtrMap.modify $ \ptr_map -> do
        when (view_id `Map.member` ptr_map) $
            PtrMap.throw $ show view_id ++ " already in displayed view list: "
                ++ show (Map.assocs ptr_map)
        viewp <- FFI.withText window_title $ \titlep ->
            with block_config $ \configp ->
                c_create (i x) (i y) (i w) (i h) titlep configp
        return $ Map.insert view_id viewp ptr_map
    where
    (x, y, w, h) = (Rect.x rect, Rect.y rect, Rect.w rect, Rect.h rect)
    i = FFI.c_int

foreign import ccall "create"
    c_create :: CInt -> CInt -> CInt -> CInt -> CString -> Ptr Block.Config
        -> IO (Ptr CView)

destroy_view :: ViewId -> Fltk ()
destroy_view view_id = fltk "destroy_view" view_id $ do
    viewp <- PtrMap.get view_id
    PtrMap.modify $ \ptr_map -> do
        c_destroy viewp
        return $ Map.delete view_id ptr_map
foreign import ccall "destroy" c_destroy :: Ptr CView -> IO ()

-- | Get various position metrics of the window.
--
-- This is unused because I sync this with UI msgs, but if they prove
-- unreliable I could use this to verify or just replace them.
get_view_status :: ViewId -> Fltk (Rect.Rect, Zoom.Zoom, Int, Int)
    -- ^ (rect, zoom, time_padding, track_padding)
get_view_status view_id = fltk "get_view_status" view_id $ do
    viewp <- PtrMap.get view_id
    alloca $ \rectp -> alloca $ \zoomp ->
        alloca $ \timep -> alloca $ \trackp -> do
            c_get_view_status viewp rectp zoomp timep trackp
            (,,,) <$> peek rectp <*> peek zoomp
                <*> (fromIntegral <$> peek timep)
                <*> (fromIntegral <$> peek trackp)
foreign import ccall "get_view_status"
    c_get_view_status :: Ptr CView -> Ptr Rect.Rect -> Ptr Zoom.Zoom
        -> Ptr CInt -> Ptr CInt -> IO ()

-- ** Set other attributes

set_size :: ViewId -> Rect.Rect -> Fltk ()
set_size view_id rect = fltk "set_size" (view_id, rect) $ do
    viewp <- PtrMap.get view_id
    c_set_size viewp (i x) (i y) (i w) (i h)
    where
    i = FFI.c_int
    (x, y, w, h) = (Rect.x rect, Rect.y rect, Rect.w rect, Rect.h rect)
foreign import ccall "set_size"
    c_set_size :: Ptr CView -> CInt -> CInt -> CInt -> CInt -> IO ()

set_zoom :: ViewId -> Zoom.Zoom -> Fltk ()
set_zoom view_id zoom = fltk "set_zoom" (view_id, zoom) $ do
    viewp <- PtrMap.get view_id
    with zoom $ \zoomp -> c_set_zoom viewp zoomp
foreign import ccall "set_zoom"
    c_set_zoom :: Ptr CView -> Ptr Zoom.Zoom -> IO ()

-- | Set the scroll along the track dimension, in pixels.
set_track_scroll :: ViewId -> Types.Width -> Fltk ()
set_track_scroll view_id offset = fltk "set_track_scroll" (view_id, offset) $ do
    viewp <- PtrMap.get view_id
    c_set_track_scroll viewp (FFI.c_int offset)
foreign import ccall "set_track_scroll"
    c_set_track_scroll :: Ptr CView -> CInt -> IO ()

set_selection :: ViewId -> Sel.Num -> [TrackNum] -> [Selection] -> Fltk ()
set_selection view_id selnum tracknums sels
    | null tracknums = return ()
    | otherwise = fltk "set_selection" (view_id, selnum, tracknums, sels) $ do
        viewp <- PtrMap.get view_id
        withArrayLenNull sels $ \nsels selsp -> forM_ tracknums $ \tracknum ->
            c_set_selection viewp (FFI.c_int selnum) (FFI.c_int tracknum)
                selsp (FFI.c_int nsels)
foreign import ccall "set_selection"
    c_set_selection :: Ptr CView -> CInt -> CInt -> Ptr Selection -> CInt
        -> IO ()

bring_to_front :: ViewId -> Fltk ()
bring_to_front view_id = fltk "bring_to_front" view_id $
    c_bring_to_front =<< PtrMap.get view_id
foreign import ccall "bring_to_front" c_bring_to_front :: Ptr CView -> IO ()

-- * Block operations

-- These operate on ViewIds too because there is no block/view distinction at
-- this layer.

set_config :: ViewId -> Block.Config -> Fltk ()
set_config view_id config = fltk "set_config" view_id $ do
    viewp <- PtrMap.get view_id
    with config $ \configp -> c_set_model_config viewp configp
foreign import ccall "set_config"
    c_set_model_config :: Ptr CView -> Ptr Block.Config -> IO ()

set_skeleton :: ViewId -> Skeleton.Skeleton
    -> [(Color.Color, [(TrackNum, TrackNum)])] -> Fltk ()
set_skeleton view_id skel integrate_edges =
    fltk "set_skeleton" (view_id, skel) $ do
        viewp <- PtrMap.get view_id
        with_skeleton_config (skeleton_edges skel integrate_edges) $
            \configp -> c_set_skeleton viewp configp
foreign import ccall "set_skeleton"
    c_set_skeleton :: Ptr CView -> Ptr SkeletonConfig -> IO ()

set_title :: ViewId -> Text -> Fltk ()
set_title view_id title = fltk "set_title" (view_id, title) $ do
    viewp <- PtrMap.get view_id
    FFI.withText title (c_set_title viewp)
foreign import ccall "set_title" c_set_title :: Ptr CView -> CString -> IO ()

set_status :: ViewId -> Text -> Color.Color -> Fltk ()
set_status view_id status color = fltk "set_status" (view_id, status) $ do
    viewp <- PtrMap.get view_id
    FFI.withText status $ \statusp -> with color $ \colorp ->
        c_set_status viewp statusp colorp
foreign import ccall "set_status"
    c_set_status :: Ptr CView -> CString -> Ptr Color.Color -> IO ()

-- | Set block-local track status.
set_display_track :: ViewId -> TrackNum -> Block.DisplayTrack -> Fltk ()
set_display_track view_id tracknum dtrack =
    fltk "set_display_track" (view_id, tracknum, dtrack) $ do
        viewp <- PtrMap.get view_id
        with dtrack $ \dtrackp ->
            c_set_display_track viewp (FFI.c_int tracknum) dtrackp
foreign import ccall "set_display_track"
    c_set_display_track :: Ptr CView -> CInt -> Ptr Block.DisplayTrack -> IO ()

-- ** floating input

floating_open :: ViewId -> TrackNum -> ScoreTime -> Text -> (Int, Int)
    -> Fltk ()
floating_open view_id tracknum pos text (sel_start, sel_end) =
    fltk "floating_open" (view_id, tracknum) $ do
        viewp <- PtrMap.get view_id
        FFI.withText text $ \textp ->
            c_floating_open viewp (FFI.c_int tracknum)
                (ScoreTime.to_cdouble pos) textp (FFI.c_int sel_start)
                (FFI.c_int sel_end)
foreign import ccall "floating_open"
    c_floating_open :: Ptr CView -> CInt -> CDouble -> CString -> CInt -> CInt
        -> IO ()

floating_insert :: [ViewId] -> Text -> Fltk ()
floating_insert view_ids text = fltk "floating_insert" (view_ids, text) $
    FFI.withText text $ \textp -> forM_ view_ids $ \view_id -> do
        viewp <- PtrMap.get view_id
        c_floating_insert viewp textp
foreign import ccall "floating_insert"
    c_floating_insert :: Ptr CView -> CString -> IO ()

-- * Track operations

-- | Get the number of tracks on the block.
tracks :: ViewId -> Fltk TrackNum
tracks view_id = fltk "tracks" view_id $
    fromIntegral <$> (c_tracks =<< PtrMap.get view_id)
foreign import ccall "tracks" c_tracks  :: Ptr CView -> IO CInt

insert_track :: ViewId -> TrackNum -> Block.Tracklike -> [Events.Events]
    -> Track.SetStyle -> Types.Width -> Fltk ()
insert_track view_id tracknum tracklike merged set_style width =
    fltk "insert_track" (view_id, tracknum) $ do
        viewp <- PtrMap.get view_id
        with_tracklike True merged set_style tracklike $ \tp mlistp len ->
            c_insert_track viewp (FFI.c_int tracknum) tp
                (FFI.c_int width) mlistp len

foreign import ccall "insert_track"
    c_insert_track :: Ptr CView -> CInt -> Ptr TracklikePtr -> CInt
        -> Ptr (Ptr Ruler.Marklist) -> CInt -> IO ()

remove_track :: ViewId -> TrackNum -> Fltk ()
remove_track view_id tracknum = fltk "remove_track" (view_id, tracknum) $ do
    viewp <- PtrMap.get view_id
    c_remove_track viewp (FFI.c_int tracknum)

foreign import ccall "remove_track"
    c_remove_track :: Ptr CView -> CInt -> IO ()

update_track :: Bool -- ^ True if the ruler has changed and should be copied
    -- over.  It's a bit of a hack to be a separate flag, but rulers are
    -- updated rarely and copied over entirely for efficiency.
    -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> ScoreTime -> ScoreTime -> Fltk ()
update_track update_ruler view_id tracknum tracklike merged set_style start
        end = fltk "update_track" (view_id, tracknum) $ do
    viewp <- PtrMap.get view_id
    with_tracklike update_ruler merged set_style tracklike $ \tp mlistp len ->
        c_update_track viewp (FFI.c_int tracknum) tp mlistp len
            (ScoreTime.to_cdouble start) (ScoreTime.to_cdouble end)

-- | Like 'update_track' except update everywhere.
update_entire_track :: Bool -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> Fltk ()
update_entire_track update_ruler view_id tracknum tracklike merged set_style =
    -- -1 is special cased in c++.
    update_track update_ruler view_id tracknum tracklike merged set_style
        (-1) (-1)

foreign import ccall "update_track"
    c_update_track :: Ptr CView -> CInt -> Ptr TracklikePtr
        -> Ptr (Ptr Ruler.Marklist) -> CInt -> CDouble -> CDouble -> IO ()

-- | Unlike other Fltk functions, this doesn't throw if the ViewId is not
-- found.  That's because it's called asynchronously when derivation is
-- complete.
set_track_signal :: ViewId -> TrackNum -> Track.TrackSignal -> Fltk ()
set_track_signal view_id tracknum tsig =
    fltk "set_track_signal" (view_id, tracknum) $
        whenJustM (PtrMap.lookup view_id) $ \viewp ->
        with_signal $ \tsigp ->
            c_set_track_signal viewp (FFI.c_int tracknum) tsigp
    where
    with_signal action
        | Track.ts_signal tsig == mempty = action nullPtr
        | otherwise = with tsig action
foreign import ccall "set_track_signal"
    c_set_track_signal :: Ptr CView -> CInt -> Ptr Track.TrackSignal -> IO ()

set_waveform :: ViewId -> TrackNum -> Track.WaveformChunk -> Fltk ()
set_waveform view_id tracknum
        (Track.WaveformChunk filename chunknum start ratios) =
    fltk "set_waveform" (view_id, tracknum, chunknum) $
        whenJustM (PtrMap.lookup view_id) $ \viewp ->
        withCString filename $ \filenamep ->
        withArrayLen (map FFI.c_double ratios) $ \ratios_len ratiosp ->
            c_set_waveform viewp
                (FFI.c_int tracknum) (FFI.c_int chunknum)
                filenamep (ScoreTime.to_cdouble start)
                ratiosp (FFI.c_int ratios_len)

foreign import ccall "set_waveform"
    c_set_waveform :: Ptr CView -> CInt -> CInt -> CString -> CDouble
        -> Ptr CDouble -> CInt -> IO ()
    -- void set_waveform(BlockWindow *view, int tracknum, int chunknum,
    --     const char *filename, double start, double *ratiosp, int ratios_len)

clear_waveforms :: ViewId -> Fltk ()
clear_waveforms view_id = fltk "clear_waveforms" view_id $
    whenJustM (PtrMap.lookup view_id) c_clear_waveforms
foreign import ccall "clear_waveforms" c_clear_waveforms :: Ptr CView -> IO ()

gc_waveforms :: Fltk ()
gc_waveforms = fltk "gc_waveforms" () $ c_gc_waveforms
foreign import ccall "gc_waveforms" c_gc_waveforms :: IO ()

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
set_track_title view_id tracknum title =
    fltk "set_track_title" (view_id, tracknum, title) $ do
        viewp <- PtrMap.get view_id
        FFI.withText title (c_set_track_title viewp (FFI.c_int tracknum))
foreign import ccall "set_track_title"
    c_set_track_title :: Ptr CView -> CInt -> CString -> IO ()

set_track_title_focus :: ViewId -> TrackNum -> Fltk ()
set_track_title_focus view_id tracknum =
    fltk "set_track_title_focus" (view_id, tracknum) $ do
        viewp <- PtrMap.get view_id
        c_set_track_title_focus viewp (FFI.c_int tracknum)
foreign import ccall "set_track_title_focus"
    c_set_track_title_focus :: Ptr CView -> CInt -> IO ()

set_block_title_focus :: ViewId -> Fltk ()
set_block_title_focus view_id = fltk "set_block_title_focus" view_id $
    c_set_block_title_focus =<< PtrMap.get view_id
foreign import ccall "set_block_title_focus"
    c_set_block_title_focus :: Ptr CView -> IO ()


-- ** debugging

-- | Print debugging info about the UI state.
print_debug :: ViewId -> Fltk ()
print_debug view_id = fltk "print_debug" view_id $ do
    putStrLn $ "debug " <> show view_id
    viewp <- PtrMap.get view_id
    c_print_debug viewp
    putStrLn ""
foreign import ccall "print_debug"
    c_print_debug :: Ptr CView -> IO ()

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
        (#poke BlockBox, c) boxp (FFI.c_rune char)

instance CStorable Block.DisplayTrack where
    sizeOf _ = #size DisplayTrack
    alignment _ = alignment (0 :: CDouble)
    peek = error "DisplayTrack peek unimplemented"
    poke dtrackp (Block.DisplayTrack _ width _ status bright) = do
        (#poke DisplayTrack, event_brightness) dtrackp (FFI.c_double bright)
        (#poke DisplayTrack, width) dtrackp (FFI.c_int width)
        (#poke DisplayTrack, status) dtrackp status

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

with_skeleton_config :: [SkeletonEdge] -> (Ptr SkeletonConfig -> IO a) -> IO a
with_skeleton_config edges f =
    withArrayLen edges $ \edges_len edgesp -> alloca $ \skelp -> do
        (#poke SkeletonConfig, edges_len) skelp (FFI.c_int edges_len)
        (#poke SkeletonConfig, edges) skelp edgesp
        f skelp

data SkeletonConfig
instance CStorable SkeletonConfig where
    sizeOf _ = #size SkeletonConfig
    alignment _ = alignment nullPtr -- contains pointers and ints
    peek = error "SkeletonConfig peek unimplemented"
    poke = error "SkeletonConfig poke unimplemented"
    -- peek and poke intentionally not implemented, due to dynamic allocation
    -- for internal pointers.  Use 'with_skeleton_config' instead.

data SkeletonEdge = SkeletonEdge !TrackNum !TrackNum !Types.Width !Color.Color
    deriving (Show)

instance CStorable SkeletonEdge where
    sizeOf _ = #size SkeletonEdge
    alignment _ = alignment Color.black
    peek _ = error "SkeletonEdge peek unimplemented"
    poke edgep (SkeletonEdge parent child width color) = do
        (#poke SkeletonEdge, parent) edgep (FFI.c_int parent)
        (#poke SkeletonEdge, child) edgep (FFI.c_int child)
        (#poke SkeletonEdge, width) edgep (FFI.c_int width)
        (#poke SkeletonEdge, color) edgep color

instance CStorable Block.Status where
    sizeOf _ = #size SkeletonStatus
    alignment _ = alignment Color.black
    peek _ = error "Block.Status peek unimplemented"
    poke statusp (Block.Status chars color) = do
        let c1 : c2 : _ = chars ++ repeat '\0'
        (#poke SkeletonStatus, c1) statusp (FFI.c_rune c1)
        (#poke SkeletonStatus, c2) statusp (FFI.c_rune c2)
        (#poke SkeletonStatus, color) statusp color

-- ** selection

-- | This is the low level version of 'Sel.Selection'.  It only applies to
-- a single track, and has an explicit color.
data Selection = Selection {
    sel_color :: !Color.Color
    , sel_start :: !TrackTime
    , sel_cur :: !TrackTime
    , sel_orientation :: !SelectionOrientation
    }
    deriving (Eq, Ord, Show)

data SelectionOrientation = None | Negative | Positive | Both
    deriving (Show, Eq, Ord)

instance Pretty Selection where
    pretty (Selection color start cur orientation) =
        "Selection " <> pretty (color, start, cur, orientation)

instance Pretty SelectionOrientation where pretty = showt

instance CStorable Selection where
    sizeOf _ = #size Selection
    alignment _ = alignment (0 :: TrackTime)
    peek = error "Selection peek unimplemented"
    poke selp (Selection color start cur orientation) = do
        (#poke Selection, color) selp color
        (#poke Selection, start) selp start
        (#poke Selection, cur) selp cur
        (#poke Selection, orientation) selp (convert_orientation orientation)

convert_orientation :: SelectionOrientation -> CInt
convert_orientation o = case o of
    None -> #const Selection::SelNone
    Positive -> #const Selection::Positive
    Negative -> #const Selection::Negative
    Both -> #const Selection::Both

##endif
