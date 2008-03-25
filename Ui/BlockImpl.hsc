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


BlockAttrs (must be serializable if a block is to be serializable)

key responder
deriver
cached derivation
cached realization
-}
module Ui.BlockImpl where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified System.IO.Unsafe as Unsafe

import qualified Ui.Util as Util
import Ui.Util (Fltk)
import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.TrackImpl as TrackImpl
import qualified Ui.RulerImpl as RulerImpl

#include "c_interface.h"

import Foreign
import Foreign.C

-- * Model creation

-- "Cached" values are stored in MVars since they are mutable and accessed
-- concurrently.  Their only access is via the functions in this module, which
-- should either only read a single MVar at a time, or always read them in
-- their order in Block, and so shouldn't deadlock.
-- TODO: check this
data Block = Block
    { block_p :: ForeignPtr CBlockModel
    , block_config :: MVar.MVar Config
    , block_attrs :: MVar.MVar Attrs
    , block_tracks :: MVar.MVar [(Tracklike, Width)]
    }
-- It's convenient to be able to print things containing blocks, but MVar's
-- aren't showable in a pure function.
instance Show Block where
    show block = "<Block.Block " ++ show (block_p block) ++ ">"
data CBlockModel

data Config = Config
    { config_select_colors :: [Color.Color]
    , config_bg_color :: Color.Color
    , config_track_box_color :: Color.Color
    , config_sb_box_color :: Color.Color
    } deriving (Eq, Show)

create :: Config -> IO Block
create config = do
    blockp <- with config $ \configp ->
        c_block_model_create configp
    blockfp <- newForeignPtr c_block_model_destroy blockp
    config_mv <- MVar.newMVar config
    attrs <- MVar.newMVar []
    tracks <- MVar.newMVar []
    return (Block blockfp config_mv attrs tracks)

-- | Max number of selections, hardcoded in ui/Block.h.
max_selections :: Int
max_selections = fromIntegral (#const Config::max_selections)

instance Storable Config where
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
    return $ Config select bg track_box sb_box

poke_block_model_config configp (Config
        { config_select_colors = select
        , config_bg_color = bg
        , config_track_box_color = track_box
        , config_sb_box_color = sb_box
        })
    = do
        pokeArray ((#ptr BlockModelConfig, select) configp)
            (Util.bounded_list Color.black max_selections select)
        (#poke BlockModelConfig, bg) configp bg
        (#poke BlockModelConfig, track_box) configp track_box
        (#poke BlockModelConfig, sb_box) configp sb_box

foreign import ccall unsafe "block_model_create"
    c_block_model_create :: Ptr Config -> IO (Ptr CBlockModel)
foreign import ccall unsafe "&block_model_destroy"
    c_block_model_destroy :: FunPtr (Ptr CBlockModel -> IO ())


-- * Model modification

get_config :: Block -> IO Config
get_config block = MVar.readMVar (block_config block)
set_config :: Block -> Config -> Fltk ()
set_config block config = do
    MVar.swapMVar (block_config block) config
    withForeignPtr (block_p block) $ \blockp -> with config $ \configp ->
        c_block_model_set_config blockp configp
foreign import ccall unsafe "block_model_set_config"
    c_block_model_set_config :: Ptr CBlockModel -> Ptr Config  -> IO ()

get_title :: Block -> Fltk String
get_title block =
    withForeignPtr (block_p block) c_block_model_get_title >>= peekCString
foreign import ccall unsafe "block_model_get_title"
    c_block_model_get_title :: Ptr CBlockModel -> IO CString

set_title :: Block -> String -> Fltk ()
set_title block s = withForeignPtr (block_p block) $ \blockp ->
    withCString s $ \cstr -> c_block_model_set_title blockp cstr
foreign import ccall unsafe "block_model_set_title"
    c_block_model_set_title :: Ptr CBlockModel -> CString -> IO ()

get_attrs :: Block -> IO Attrs
get_attrs block = MVar.readMVar (block_attrs block)
set_attrs :: Block -> Attrs -> IO ()
-- This is the only way to modify attrs, so the swap race shouldn't happen.
set_attrs block attrs = MVar.swapMVar (block_attrs block) attrs >> return ()

-- ** Track management

-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
-- | Index into the the selection list.
type SelNum = Int

-- Tracks may have a Ruler overlay
data Tracklike = T TrackImpl.Track RulerImpl.Ruler | R RulerImpl.Ruler
    | D Color.Color
    deriving (Show, Eq)

tracks :: Block -> IO TrackNum
tracks block = fmap length (MVar.readMVar (block_tracks block))

track_at :: Block -> TrackNum -> IO (Tracklike, Width)
track_at block at = do
    tracks <- MVar.readMVar (block_tracks block)
    return (Util.at "track_at" tracks at)

insert_track :: Block -> TrackNum -> Tracklike -> Width -> Fltk ()
insert_track block at_ track width = MVar.modifyMVar_ mvar $ \tracks -> do
    let at = Util.in_range "insert_track" 0 (length tracks + 1) at_
        cat = Util.c_int at
        cwidth = Util.c_nat width
    withFP (block_p block) $ \blockp -> case track of
        T track ruler -> withFP (TrackImpl.track_p track) $ \trackp ->
            withFP (RulerImpl.ruler_p ruler) $ \rulerp ->
                c_block_model_insert_event_track blockp cat cwidth trackp rulerp
        R ruler -> withFP (RulerImpl.ruler_p ruler) $ \rulerp ->
            c_block_model_insert_ruler_track blockp cat cwidth rulerp
        D color -> with color $ \colorp ->
            c_block_model_insert_divider blockp cat cwidth colorp
    return $ Util.list_insert tracks at (track, width)
    where
    mvar = block_tracks block
    withFP = withForeignPtr

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

remove_track :: Block -> TrackNum -> Fltk ()
remove_track block at_ = MVar.modifyMVar_ (block_tracks block) $ \tracks -> do
    let at = Util.in_range "remove_track" 0 (length tracks) at_
    withForeignPtr (block_p block) $ \blockp ->
        c_block_model_remove_track blockp (Util.c_int at)
    return (Util.list_remove tracks at)

foreign import ccall unsafe "block_model_remove_track"
    c_block_model_remove_track :: Ptr CBlockModel -> CInt -> IO ()

-- * View creation

data View = View
    { view_p :: Ptr CBlockView
    , view_config :: MVar.MVar ViewConfig
    , view_block :: Block
    }
instance Show View where
    show (View viewp config block) = "<Block.View " ++ show viewp
        ++ " " ++ show block ++ ">"
data CBlockView

instance Util.Widget View where
    show_children view = Util.do_show_children (view_p view)

-- The defaults for newly created blocks and the trackviews automatically
-- created.
data ViewConfig = ViewConfig
    { vconfig_zoom_speed :: Double
    , vconfig_block_title_height :: Int
    , vconfig_track_title_height :: Int
    , vconfig_sb_size :: Int
    , vconfig_ruler_size :: Int
    , vconfig_status_size :: Int
    } deriving (Show)

-- | Zoom offset factor
data Zoom = Zoom TrackPos Double deriving (Show)

-- | A selection may span multiple tracks.
data Selection = Selection
    { sel_start_track :: TrackNum
    , sel_start_pos :: TrackPos
    , sel_tracks :: TrackNum
    , sel_duration :: TrackPos
    } deriving (Show)


-- | Maintain a map of view pointers to their haskell equivalents.
-- This is so I can maintain auxiliary data with a Block but not have to
-- serialize it to c++ and back.  When the UI passes back a view pointer,
-- I can look it up here.
-- They don't need to be weak ptrs because views are explicitly destroyed.
view_ptr_to_view :: MVar.MVar (Map.Map (Ptr CBlockView) View)
view_ptr_to_view = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

create_view :: Rect -> Block -> RulerImpl.Ruler -> ViewConfig -> Fltk View
create_view (Rect (x, y) (w, h)) block ruler config = do
    viewp <- withForeignPtr (block_p block) $ \blockp ->
        withForeignPtr (RulerImpl.ruler_p ruler) $ \rulerp ->
            with config $ \configp ->
                c_block_view_create (i x) (i y) (i w) (i h) blockp rulerp
                    configp
    config_mv <- MVar.newMVar config
    let view = View viewp config_mv block
    MVar.modifyMVar_ view_ptr_to_view (return . Map.insert viewp view)
    return view
    where i = Util.c_int

foreign import ccall unsafe "block_view_create"
    c_block_view_create :: CInt -> CInt -> CInt -> CInt -> Ptr CBlockModel
        -> Ptr RulerImpl.CRulerTrackModel -> Ptr ViewConfig
        -> IO (Ptr CBlockView)

destroy_view view = do
    c_block_view_destroy (view_p view)
    MVar.modifyMVar_ view_ptr_to_view (return . Map.delete (view_p view))
foreign import ccall unsafe "block_view_destroy"
    c_block_view_destroy :: Ptr CBlockView -> IO ()

data Rect = Rect (Int, Int) (Int, Int) deriving (Show, Eq)

resize :: View -> Rect -> Fltk ()
resize view (Rect (x, y) (w, h)) =
    c_block_view_resize (view_p view) (i x) (i y) (i w) (i h)
    where i = Util.c_int
foreign import ccall unsafe "block_view_resize"
    c_block_view_resize :: Ptr CBlockView -> CInt -> CInt -> CInt -> CInt
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

instance Storable ViewConfig where
    sizeOf _ = #size BlockViewConfig
    alignment _ = undefined
    peek = error "no peek for ViewConfig"
    poke = poke_config

poke_config configp (ViewConfig
        { vconfig_zoom_speed = zoom_speed
        , vconfig_block_title_height = block_title_height
        , vconfig_track_title_height = track_title_height
        , vconfig_sb_size = sb_size
        , vconfig_ruler_size = ruler_size
        , vconfig_status_size = status_size
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
