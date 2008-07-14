{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
module Ui.Block where
import Control.Monad
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Foreign

import qualified Data.Generics as Generics
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Id as Id
import qualified Ui.Color as Color
import qualified Ui.Track as Track
import qualified Ui.Ruler as Ruler

-- These would have to be hierarchical names, so if you load
-- another song you don't get ID collisions.

-- | Reference to a Block.  Use this to look up Blocks in the State.
-- Even though the constructor is exported, you should only create them
-- through the 'State.StateT' interface.
newtype BlockId = BlockId Id.Id
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)
-- | Reference to a View, as per 'BlockId'.
newtype ViewId = ViewId Id.Id
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)
-- | Reference to a schema.  Declared here instead of Deriver.Schema to avoid
-- a circular import.
newtype SchemaId = SchemaId Id.Id
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

instance Id.Ident BlockId where
    unpack_id (BlockId a) = a
    id_con _ = "bid"
instance Id.Ident ViewId where
    unpack_id (ViewId a) = a
    id_con _ = "vid"
instance Id.Ident SchemaId where
    unpack_id (SchemaId a) = a
    id_con _ = "sid"

-- * block model

data Block = Block {
    block_title :: String
    , block_config :: Config
    -- The Widths here are the default if a new View is created from this Block.
    , block_track_widths :: [(TracklikeId, Width)]
    , block_schema :: SchemaId
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

block_tracks :: Block -> [TracklikeId]
block_tracks = map fst . block_track_widths

block title config tracks schema_id = Block title config tracks schema_id

-- | Per-block configuration.
data Config = Config {
    config_selection_colors :: [Color]
    , config_bg_color :: Color
    , config_track_box_color :: Color
    , config_sb_box_color :: Color
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

-- Tracks may have a Ruler overlay
data TracklikeId =
    TId Track.TrackId Ruler.RulerId
    | RId Ruler.RulerId
    | DId Divider
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

track_id_of :: TracklikeId -> Maybe Track.TrackId
track_id_of (TId tid _) = Just tid
track_id_of _ = Nothing

track_ids_of = Maybe.catMaybes . map track_id_of

ruler_id_of :: TracklikeId -> Maybe Ruler.RulerId
ruler_id_of (TId _ rid) = Just rid
ruler_id_of (RId rid) = Just rid
ruler_id_of _ = Nothing

ruler_ids_of = Maybe.catMaybes . map ruler_id_of

data Tracklike =
    T Track.Track Ruler.Ruler
    | R Ruler.Ruler
    | D Divider
    deriving (Show)

track_of :: Tracklike -> Maybe Track.Track
track_of (T track _) = Just track
track_of _ = Nothing

tracks_of = Maybe.catMaybes . map track_of

ruler_of :: Tracklike -> Maybe Ruler.Ruler
ruler_of (T _ ruler) = Just ruler
ruler_of (R ruler) = Just ruler
ruler_of _ = Nothing

rulers_of = Maybe.catMaybes . map ruler_of

-- | A divider separating tracks.
-- Defined here in Block since it's so trivial.
data Divider = Divider Color
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

-- * block view

data View = View {
    -- | view_block should never change.
    -- TODO Views that point to a BlockId not in state_blocks should be
    -- destroyed.
    view_block :: BlockId
    , view_rect :: Rect
    , view_config :: ViewConfig
    , view_status :: Map.Map String String

    -- | Scroll and zoom
    , view_track_scroll :: Width
    , view_zoom :: Zoom

    , view_selections :: Map.Map SelNum Selection
    -- | These are the per-view settings for the tracks.  There should be one
    -- corresponding to each TracklikeId in the Block.  The StateT operations
    -- should maintain this invariant.
    , view_tracks :: [TrackView]
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

-- | Construct a View, using default values for most of its fields.
-- Don't construct views using View directly since State.create_view overwrites
-- view_tracks, and maybe more in the future.
view block_id rect zoom config =
    View block_id rect config Map.empty 0 zoom Map.empty []

show_status :: View -> String
show_status = Seq.join " | " . map (\(k, v) -> k ++ ": " ++ v)
    . Map.assocs . view_status

-- | Return how much track is in view.
visible_time_area :: View -> TrackPos
visible_time_area view = pixels_to_track_pos (view_zoom view) height
    where
    ViewConfig { vconfig_block_title_height = blockth
        , vconfig_track_title_height = trackth -- yeth a I have lithp
        , vconfig_sb_size = sb
        , vconfig_status_size = status } = view_config view
    -- TODO
    -- This relies on knowing how the widgets are layed out.  It would be nicer
    -- for UpdateViewResize to explicitly give the pixels in the track view,
    -- and I'd need to make sure a haskell-initiated resize gets reported in an
    -- UpdateViewResize too.
    height = rect_h (view_rect view) - blockth - trackth - sb - status

visible_track_area :: View -> Width
visible_track_area view = width - ruler_width - sb
    where
    ViewConfig { vconfig_sb_size = sb } = view_config view
    ruler_width = Seq.mhead 0 (map track_view_width (view_tracks view))
    width = rect_w (view_rect view)

pixels_to_track_pos :: Zoom -> Int -> TrackPos
pixels_to_track_pos zoom pixels =
    track_pos pixels / track_pos (zoom_factor zoom)

data TrackView = TrackView {
    track_view_width :: Width
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

data Rect = Rect {
    rect_x :: Int
    , rect_y :: Int
    , rect_w :: Int
    , rect_h :: Int
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)
rect_r rect = rect_x rect + rect_w rect
rect_b rect = rect_y rect + rect_h rect

-- | The defaults for newly created blocks and the trackviews automatically
-- created.
data ViewConfig = ViewConfig {
    vconfig_zoom_speed :: Double
    , vconfig_block_title_height :: Int
    , vconfig_track_title_height :: Int
    , vconfig_sb_size :: Int
    , vconfig_status_size :: Int
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

-- | View zoom and time scroll offset.
data Zoom = Zoom {
    zoom_offset :: TrackPos
    , zoom_factor :: Double
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

-- TODO: remove color and put it in BlockC.SelectionC, which gets its color
-- from a BlockConfig list
data Selection = Selection {
    sel_start_track :: TrackNum
    , sel_start_pos :: TrackPos
    , sel_tracks :: TrackNum
    , sel_duration :: TrackPos
    } deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

sel_range :: Selection -> (TrackPos, TrackPos)
sel_range sel = (start, start + sel_duration sel)
    where start = sel_start_pos sel

sel_end :: Selection -> TrackPos
sel_end = snd . sel_range

sel_is_point :: Selection -> Bool
sel_is_point sel = sel_duration sel == TrackPos 0

selection tracknum start tracks dur = Just (Selection tracknum start tracks dur)

-- | A point is a selection with no duration.
point_selection :: TrackNum -> TrackPos -> Maybe Selection
point_selection tracknum pos = Just (Selection tracknum pos 1 (TrackPos 0))

-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
-- | Index into the the selection list.
type SelNum = Int


-- This stuff belongs in BlockC.  However, Ui.State uses map_ids, and making
-- State import BlockC makes practically everything import BlockC, which makes
-- ghci freak out since it wants explicit linking for all FFI-using modules.

-- Phantom type for block view ptrs.
data CView

-- | Global map of view IDs to their windows.  This is global mutable state
-- because the underlying window system is also global mutable state, and is
-- not well represented by a persistent functional state.
view_id_to_ptr :: MVar.MVar (Map.Map ViewId (Foreign.Ptr CView))
view_id_to_ptr = Foreign.unsafePerformIO (MVar.newMVar Map.empty)

-- | Rename view ids.  Throws if the views collide, because that would make
-- things break.
map_ids :: (ViewId -> ViewId) -> IO ()
map_ids f = MVar.modifyMVar_ view_id_to_ptr $ \ptrs -> do
    let new_ptrs = Map.mapKeys f ptrs
    when (Map.size new_ptrs /= Map.size ptrs) $
        -- TODO should actually be BlockC exception type, when I have that
        error $ "keys collided in view_id_to_ptr: "
            ++ show (Map.keys (Map.difference ptrs new_ptrs))
    return new_ptrs
