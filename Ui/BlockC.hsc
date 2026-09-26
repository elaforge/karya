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
    getScreens
    -- * view creation
    , createView, destroyView, getViewStatus
    -- ** set other attributes
    , setSize
    , setZoom
    , setTrackScroll
    , Selection(..), SelectionOrientation(..)
    , setSelection
    , bringToFront

    -- * Block operations
    , setConfig, setSkeleton, setTitle, setStatus
    , setDisplayTrack
    -- ** floating input
    , floatingOpen, floatingInsert

    -- ** Track operations
    , tracks, insertTrack, removeTrack, updateTrack, updateEntireTrack
    , setTrackSignal
    , setWaveform, clearWaveforms, gcWaveforms
    , setTrackTitle, setTrackTitleFocus, setBlockTitleFocus

    -- * debugging
    , printDebug, dump
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
import qualified Ui.Meter.Mark as Mark
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
    Fltk.fltk $ annotate name action

-- | Annotate thrown exceptions with a provenance.
annotate :: String -> IO a -> IO a
annotate name action = Exception.catch action $ \(PtrMap.FltkException exc) ->
    Exception.throwIO $ PtrMap.FltkException $ name <> ": " <> exc

-- * query

getScreens :: IO [Rect.Rect]
getScreens = alloca $ \screenspp -> do
    count <- cGetScreens screenspp
    screensp <- peek screenspp
    screens <- peekArray count screensp
    free screensp
    return screens

foreign import ccall "get_screens"
    cGetScreens :: Ptr (Ptr Rect.Rect) -> IO Int

-- * view creation

-- | Create an empty block view with the given configs.  Tracks must be
-- inserted separately.
createView :: ViewId -> Text -> Rect.Rect -> Block.Config -> Fltk ()
createView viewId windowTitle rect blockConfig =
    fltk "create_view" windowTitle $ PtrMap.modify $ \ptrMap -> do
        when (viewId `Map.member` ptrMap) $
            PtrMap.throw $ show viewId ++ " already in displayed view list: "
                ++ show (Map.assocs ptrMap)
        viewp <- FFI.withText windowTitle $ \titlep ->
            with blockConfig $ \configp ->
                cCreate (i x) (i y) (i w) (i h) titlep configp
        return $ Map.insert viewId viewp ptrMap
    where
    (x, y, w, h) = (Rect.x rect, Rect.y rect, Rect.w rect, Rect.h rect)
    i = FFI.cInt

foreign import ccall "create"
    cCreate :: CInt -> CInt -> CInt -> CInt -> CString -> Ptr Block.Config
        -> IO (Ptr CView)

destroyView :: ViewId -> Fltk ()
destroyView viewId = fltk "destroy_view" viewId $ do
    viewp <- PtrMap.get viewId
    PtrMap.modify $ \ptrMap -> do
        cDestroy viewp
        return $ Map.delete viewId ptrMap
foreign import ccall "destroy" cDestroy :: Ptr CView -> IO ()

-- | Get various position metrics of the window.
--
-- This is unused because I sync this with UI msgs, but if they prove
-- unreliable I could use this to verify or just replace them.
getViewStatus :: ViewId -> Fltk (Rect.Rect, Zoom.Zoom, Int, Int)
    -- ^ (rect, zoom, time_padding, track_padding)
getViewStatus viewId = fltk "get_view_status" viewId $ do
    viewp <- PtrMap.get viewId
    alloca $ \rectp -> alloca $ \zoomp ->
        alloca $ \timep -> alloca $ \trackp -> do
            cGetViewStatus viewp rectp zoomp timep trackp
            (,,,) <$> peek rectp <*> peek zoomp
                <*> (fromIntegral <$> peek timep)
                <*> (fromIntegral <$> peek trackp)
foreign import ccall "get_view_status"
    cGetViewStatus :: Ptr CView -> Ptr Rect.Rect -> Ptr Zoom.Zoom
        -> Ptr CInt -> Ptr CInt -> IO ()

-- ** Set other attributes

setSize :: ViewId -> Rect.Rect -> Fltk ()
setSize viewId rect = fltk "set_size" (viewId, rect) $ do
    viewp <- PtrMap.get viewId
    cSetSize viewp (i x) (i y) (i w) (i h)
    where
    i = FFI.cInt
    (x, y, w, h) = (Rect.x rect, Rect.y rect, Rect.w rect, Rect.h rect)
foreign import ccall "set_size"
    cSetSize :: Ptr CView -> CInt -> CInt -> CInt -> CInt -> IO ()

setZoom :: ViewId -> Zoom.Zoom -> Fltk ()
setZoom viewId zoom = fltk "set_zoom" (viewId, zoom) $ do
    viewp <- PtrMap.get viewId
    with zoom $ \zoomp -> cSetZoom viewp zoomp
foreign import ccall "set_zoom"
    cSetZoom :: Ptr CView -> Ptr Zoom.Zoom -> IO ()

-- | Set the scroll along the track dimension, in pixels.
setTrackScroll :: ViewId -> Types.Width -> Fltk ()
setTrackScroll viewId offset = fltk "set_track_scroll" (viewId, offset) $ do
    viewp <- PtrMap.get viewId
    cSetTrackScroll viewp (FFI.cInt offset)
foreign import ccall "set_track_scroll"
    cSetTrackScroll :: Ptr CView -> CInt -> IO ()

setSelection :: ViewId -> Sel.Num -> [TrackNum] -> [Selection] -> Fltk ()
setSelection viewId selnum tracknums sels
    | null tracknums = return ()
    | otherwise = fltk "set_selection" (viewId, selnum, tracknums, sels) $ do
        viewp <- PtrMap.get viewId
        withArrayLenNull sels $ \nsels selsp -> forM_ tracknums $ \tracknum ->
            cSetSelection viewp (FFI.cInt selnum) (FFI.cInt tracknum)
                selsp (FFI.cInt nsels)
foreign import ccall "set_selection"
    cSetSelection :: Ptr CView -> CInt -> CInt -> Ptr Selection -> CInt
        -> IO ()

bringToFront :: ViewId -> Fltk ()
bringToFront viewId = fltk "bring_to_front" viewId $
    cBringToFront =<< PtrMap.get viewId
foreign import ccall "bring_to_front" cBringToFront :: Ptr CView -> IO ()

-- * Block operations

-- These operate on ViewIds too because there is no block/view distinction at
-- this layer.

setConfig :: ViewId -> Block.Config -> Fltk ()
setConfig viewId config = fltk "set_config" viewId $ do
    viewp <- PtrMap.get viewId
    with config $ \configp -> cSetConfig viewp configp
foreign import ccall "set_config"
    cSetConfig :: Ptr CView -> Ptr Block.Config -> IO ()

setSkeleton :: ViewId -> Skeleton.Skeleton
    -> [(Color.Color, [(TrackNum, TrackNum)])] -> Fltk ()
setSkeleton viewId skel integrateEdges =
    fltk "set_skeleton" (viewId, skel) $ do
        viewp <- PtrMap.get viewId
        withSkeletonConfig (skeletonEdges skel integrateEdges) $
            \configp -> cSetSkeleton viewp configp
foreign import ccall "set_skeleton"
    cSetSkeleton :: Ptr CView -> Ptr SkeletonConfig -> IO ()

setTitle :: ViewId -> Text -> Fltk ()
setTitle viewId title = fltk "set_title" (viewId, title) $ do
    viewp <- PtrMap.get viewId
    FFI.withText title (cSetTitle viewp)
foreign import ccall "set_title" cSetTitle :: Ptr CView -> CString -> IO ()

setStatus :: ViewId -> Text -> Color.Color -> Fltk ()
setStatus viewId status color = fltk "set_status" (viewId, status) $ do
    viewp <- PtrMap.get viewId
    FFI.withText status $ \statusp -> with color $ \colorp ->
        cSetStatus viewp statusp colorp
foreign import ccall "set_status"
    cSetStatus :: Ptr CView -> CString -> Ptr Color.Color -> IO ()

-- | Set block-local track status.
setDisplayTrack :: ViewId -> TrackNum -> Block.DisplayTrack -> Fltk ()
setDisplayTrack viewId tracknum dtrack =
    fltk "set_display_track" (viewId, tracknum, dtrack) $ do
        viewp <- PtrMap.get viewId
        with dtrack $ \dtrackp ->
            cSetDisplayTrack viewp (FFI.cInt tracknum) dtrackp
foreign import ccall "set_display_track"
    cSetDisplayTrack :: Ptr CView -> CInt -> Ptr Block.DisplayTrack -> IO ()

-- ** floating input

floatingOpen :: ViewId -> TrackNum -> ScoreTime -> Text -> (Int, Int)
    -> Fltk ()
floatingOpen viewId tracknum pos text (selStart, selEnd) =
    fltk "floating_open" (viewId, tracknum) $ do
        viewp <- PtrMap.get viewId
        FFI.withText text $ \textp ->
            cFloatingOpen viewp (FFI.cInt tracknum)
                (ScoreTime.toCdouble pos) textp (FFI.cInt selStart)
                (FFI.cInt selEnd)
foreign import ccall "floating_open"
    cFloatingOpen :: Ptr CView -> CInt -> CDouble -> CString -> CInt -> CInt
        -> IO ()

floatingInsert :: [ViewId] -> Text -> Fltk ()
floatingInsert viewIds text = fltk "floating_insert" (viewIds, text) $
    FFI.withText text $ \textp -> forM_ viewIds $ \viewId -> do
        viewp <- PtrMap.get viewId
        cFloatingInsert viewp textp
foreign import ccall "floating_insert"
    cFloatingInsert :: Ptr CView -> CString -> IO ()

-- * Track operations

-- | Get the number of tracks on the block.
tracks :: ViewId -> Fltk TrackNum
tracks viewId = fltk "tracks" viewId $
    fromIntegral <$> (cTracks =<< PtrMap.get viewId)
foreign import ccall "tracks" cTracks  :: Ptr CView -> IO CInt

insertTrack :: ViewId -> TrackNum -> Block.Tracklike -> [Events.Events]
    -> Track.SetStyle -> Types.Width -> Fltk ()
insertTrack viewId tracknum tracklike merged setStyle width =
    fltk "insert_track" (viewId, tracknum) $ do
        viewp <- PtrMap.get viewId
        withTracklike True merged setStyle tracklike $ \tp mlistp len ->
            cInsertTrack viewp (FFI.cInt tracknum) tp
                (FFI.cInt width) mlistp len

foreign import ccall "insert_track"
    cInsertTrack :: Ptr CView -> CInt -> Ptr TracklikePtr -> CInt
        -> Ptr (Ptr Mark.Marklist) -> CInt -> IO ()

removeTrack :: ViewId -> TrackNum -> Fltk ()
removeTrack viewId tracknum = fltk "remove_track" (viewId, tracknum) $ do
    viewp <- PtrMap.get viewId
    cRemoveTrack viewp (FFI.cInt tracknum)

foreign import ccall "remove_track"
    cRemoveTrack :: Ptr CView -> CInt -> IO ()

updateTrack :: Bool -- ^ True if the ruler has changed and should be copied
    -- over.  It's a bit of a hack to be a separate flag, but rulers are
    -- updated rarely and copied over entirely for efficiency.
    -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> ScoreTime -> ScoreTime -> Fltk ()
updateTrack updateRuler viewId tracknum tracklike merged setStyle start
        end = fltk "update_track" (viewId, tracknum) $ do
    viewp <- PtrMap.get viewId
    withTracklike updateRuler merged setStyle tracklike $ \tp mlistp len ->
        cUpdateTrack viewp (FFI.cInt tracknum) tp mlistp len
            (ScoreTime.toCdouble start) (ScoreTime.toCdouble end)

-- | Like 'update_track' except update everywhere.
updateEntireTrack :: Bool -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> Fltk ()
updateEntireTrack updateRuler viewId tracknum tracklike merged setStyle =
    -- -1 is special cased in c++.
    updateTrack updateRuler viewId tracknum tracklike merged setStyle
        (-1) (-1)

foreign import ccall "update_track"
    cUpdateTrack :: Ptr CView -> CInt -> Ptr TracklikePtr
        -> Ptr (Ptr Mark.Marklist) -> CInt -> CDouble -> CDouble -> IO ()

-- | Unlike other Fltk functions, this doesn't throw if the ViewId is not
-- found.  That's because it's called asynchronously when derivation is
-- complete.
setTrackSignal :: ViewId -> TrackNum -> Track.TrackSignal -> Fltk ()
setTrackSignal viewId tracknum tsig =
    fltk "set_track_signal" (viewId, tracknum) $
        whenJustM (PtrMap.lookup viewId) $ \viewp ->
        withSignal $ \tsigp ->
            cSetTrackSignal viewp (FFI.cInt tracknum) tsigp
    where
    withSignal action
        | Track.tsSignal tsig == mempty = action nullPtr
        | otherwise = with tsig action
foreign import ccall "set_track_signal"
    cSetTrackSignal :: Ptr CView -> CInt -> Ptr Track.TrackSignal -> IO ()

setWaveform :: ViewId -> TrackNum -> Track.WaveformChunk -> Fltk ()
setWaveform viewId tracknum
        (Track.WaveformChunk filename chunknum start ratios) =
    fltk "set_waveform" (viewId, tracknum, chunknum) $
        whenJustM (PtrMap.lookup viewId) $ \viewp ->
        withCString filename $ \filenamep ->
        withArrayLen (map FFI.cDouble ratios) $ \ratiosLen ratiosp ->
            cSetWaveform viewp
                (FFI.cInt tracknum) (FFI.cInt chunknum)
                filenamep (ScoreTime.toCdouble start)
                ratiosp (FFI.cInt ratiosLen)

foreign import ccall "set_waveform"
    cSetWaveform :: Ptr CView -> CInt -> CInt -> CString -> CDouble
        -> Ptr CDouble -> CInt -> IO ()
    -- void set_waveform(BlockWindow *view, int tracknum, int chunknum,
    --     const char *filename, double start, double *ratiosp, int ratios_len)

clearWaveforms :: ViewId -> Fltk ()
clearWaveforms viewId = fltk "clear_waveforms" viewId $
    whenJustM (PtrMap.lookup viewId) cClearWaveforms
foreign import ccall "clear_waveforms" cClearWaveforms :: Ptr CView -> IO ()

gcWaveforms :: Fltk ()
gcWaveforms = fltk "gc_waveforms" () $ cGcWaveforms
foreign import ccall "gc_waveforms" cGcWaveforms :: IO ()

-- | Convert a Tracklike into the set of pointers that c++ knows it as.
-- A set of event lists can be merged into event tracks.
withTracklike :: Bool -> [Events.Events] -> Track.SetStyle -> Block.Tracklike
    -> (Ptr TracklikePtr -> Ptr (Ptr Mark.Marklist) -> CInt -> IO ()) -> IO ()
withTracklike updateRuler mergedEvents setStyle tracklike f =
    case tracklike of
        Block.T track ruler -> withRuler ruler $ \rulerp mlistp len ->
            TrackC.withTrack track setStyle mergedEvents $ \trackp ->
                with (TPtr trackp rulerp) $ \tp -> f tp mlistp len
        Block.R ruler -> RulerC.withRuler ruler $ \rulerp mlistp len ->
            with (RPtr rulerp) $ \tp -> f tp mlistp len
        Block.D div -> with div $ \dividerp -> with (DPtr dividerp) $ \tp ->
            f tp nullPtr 0
    where
    withRuler = if updateRuler then RulerC.withRuler
        else const RulerC.noRuler

data TracklikePtr
    = TPtr (Ptr Track.Track) (Ptr Ruler.Ruler)
    | RPtr (Ptr Ruler.Ruler)
    | DPtr (Ptr Block.Divider)

setTrackTitle :: ViewId -> TrackNum -> Text -> Fltk ()
setTrackTitle viewId tracknum title =
    fltk "set_track_title" (viewId, tracknum, title) $ do
        viewp <- PtrMap.get viewId
        FFI.withText title (cSetTrackTitle viewp (FFI.cInt tracknum))
foreign import ccall "set_track_title"
    cSetTrackTitle :: Ptr CView -> CInt -> CString -> IO ()

setTrackTitleFocus :: ViewId -> TrackNum -> Fltk ()
setTrackTitleFocus viewId tracknum =
    fltk "set_track_title_focus" (viewId, tracknum) $ do
        viewp <- PtrMap.get viewId
        cSetTrackTitleFocus viewp (FFI.cInt tracknum)
foreign import ccall "set_track_title_focus"
    cSetTrackTitleFocus :: Ptr CView -> CInt -> IO ()

setBlockTitleFocus :: ViewId -> Fltk ()
setBlockTitleFocus viewId = fltk "set_block_title_focus" viewId $
    cSetBlockTitleFocus =<< PtrMap.get viewId
foreign import ccall "set_block_title_focus"
    cSetBlockTitleFocus :: Ptr CView -> IO ()


-- ** debugging

-- | Print debugging info about the UI state.
printDebug :: ViewId -> Fltk ()
printDebug viewId = fltk "print_debug" viewId $ do
    putStrLn $ "debug " <> show viewId
    viewp <- PtrMap.get viewId
    cPrintDebug viewp
    putStrLn ""
foreign import ccall "print_debug"
    cPrintDebug :: Ptr CView -> IO ()

dump :: IO [(ViewId, String)]
dump = do
    views <- Map.toList <$> PtrMap.getMap
    dumps <- mapM (cDumpView . snd) views
    dumps <- mapM peekCString dumps
    return $ zip (map fst views) dumps
foreign import ccall "dump_view" cDumpView :: Ptr CView -> IO CString

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
    poke tp tracklikePtr = do
        -- Apparently 'with' doesn't zero out the memory it allocates.
        (#poke Tracklike, track) tp nullPtr
        (#poke Tracklike, ruler) tp nullPtr
        (#poke Tracklike, divider) tp nullPtr
        case tracklikePtr of
            TPtr trackp rulerp -> do
                (#poke Tracklike, track) tp trackp
                (#poke Tracklike, ruler) tp rulerp
            RPtr rulerp -> (#poke Tracklike, ruler) tp rulerp
            DPtr dividerp -> (#poke Tracklike, divider) tp dividerp

-- ** configs

instance CStorable Block.Config where
    sizeOf _ = #size BlockConfig
    alignment _ = alignment Color.black
    poke configp (Block.Config skelBox trackBox sbBox skelConfig) = do
        (#poke BlockConfig, skel_box) configp skelBox
        (#poke BlockConfig, track_box) configp trackBox
        (#poke BlockConfig, sb_box) configp sbBox
        (#poke BlockConfig, skeleton_editable) configp $ case skelConfig of
            Block.Explicit -> 1 :: CChar
            Block.Implicit -> 0

instance CStorable Block.Box where
    sizeOf _ = #size BlockBox
    alignment _ = alignment Color.black
    poke boxp (Block.Box color char) = do
        (#poke BlockBox, color) boxp color
        (#poke BlockBox, c) boxp (FFI.cRune char)

instance CStorable Block.DisplayTrack where
    sizeOf _ = #size DisplayTrack
    alignment _ = alignment (0 :: CDouble)
    peek = error "DisplayTrack peek unimplemented"
    poke dtrackp (Block.DisplayTrack _ width _ status bright) = do
        (#poke DisplayTrack, event_brightness) dtrackp (FFI.cDouble bright)
        (#poke DisplayTrack, width) dtrackp (FFI.cInt width)
        (#poke DisplayTrack, status) dtrackp status

-- ** skeleton

skeletonEdges :: Skeleton.Skeleton -> [(Color.Color, [(TrackNum, TrackNum)])]
    -> [SkeletonEdge]
skeletonEdges skel integrateEdges =
    [edge p c 0 Config.skeleton | (p, c) <- Skeleton.flatten skel]
    ++ [edge p c 0 color | (color, edges) <- integrateEdges, (p, c) <- edges]
    where
    edge p c = SkeletonEdge (p-1) (c-1)
    -- The -1s are because the fltk set_skeleton doesn't count the ruler track,
    -- while of course the tracknums here do.
    -- TODO would it be better to put this in BlockView::set_skeleton?

withSkeletonConfig :: [SkeletonEdge] -> (Ptr SkeletonConfig -> IO a) -> IO a
withSkeletonConfig edges f =
    withArrayLen edges $ \edgesLen edgesp -> alloca $ \skelp -> do
        (#poke SkeletonConfig, edges_len) skelp (FFI.cInt edgesLen)
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
        (#poke SkeletonEdge, parent) edgep (FFI.cInt parent)
        (#poke SkeletonEdge, child) edgep (FFI.cInt child)
        (#poke SkeletonEdge, width) edgep (FFI.cInt width)
        (#poke SkeletonEdge, color) edgep color

instance CStorable Block.Status where
    sizeOf _ = #size SkeletonStatus
    alignment _ = alignment Color.black
    peek _ = error "Block.Status peek unimplemented"
    poke statusp (Block.Status chars color) = do
        let c1 : c2 : _ = chars ++ repeat '\0'
        (#poke SkeletonStatus, c1) statusp (FFI.cRune c1)
        (#poke SkeletonStatus, c2) statusp (FFI.cRune c2)
        (#poke SkeletonStatus, color) statusp color

-- ** selection

-- | This is the low level version of 'Sel.Selection'.  It only applies to
-- a single track, and has an explicit color.
data Selection = Selection
    { selColor :: !Color.Color
    , selStart :: !TrackTime
    , selCur :: !TrackTime
    , selOrientation :: !SelectionOrientation
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
        (#poke Selection, orientation) selp (convertOrientation orientation)

convertOrientation :: SelectionOrientation -> CInt
convertOrientation o = case o of
    None -> #const Selection::SelNone
    Positive -> #const Selection::Positive
    Negative -> #const Selection::Negative
    Both -> #const Selection::Both

##endif
