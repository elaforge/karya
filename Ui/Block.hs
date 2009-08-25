{-# LANGUAGE EmptyDataDecls #-}
module Ui.Block where
import Control.Monad
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Foreign
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.Read as Read

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track

-- | Reference to a Block.  Use this to look up Blocks in the State.
-- Even though the constructor is exported, you should only create them
-- through the 'State.StateT' interface.
newtype BlockId = BlockId Id.Id
    deriving (Eq, Ord)
-- | Reference to a View, as per 'BlockId'.
newtype ViewId = ViewId Id.Id
    deriving (Eq, Ord)
-- | Reference to a schema.  Declared here instead of Deriver.Schema to avoid
-- a circular import.
newtype SchemaId = SchemaId Id.Id
    deriving (Eq, Ord)

instance Show BlockId where show = Id.show_ident
instance Show ViewId where show = Id.show_ident
instance Show SchemaId where show = Id.show_ident

instance Read BlockId where readPrec = Id.read_ident undefined
instance Read ViewId where readPrec = Id.read_ident undefined
instance Read SchemaId where readPrec = Id.read_ident undefined

instance Id.Ident BlockId where
    unpack_id (BlockId a) = a
    cons_name _ = "bid"
    cons = BlockId
instance Id.Ident ViewId where
    unpack_id (ViewId a) = a
    cons_name _ = "vid"
    cons = ViewId
instance Id.Ident SchemaId where
    unpack_id (SchemaId a) = a
    cons_name _ = "sid"
    cons = SchemaId

-- * block model

data Block = Block {
    block_title :: String
    , block_config :: Config
    , block_tracks :: [BlockTrack]
    , block_skeleton :: Skeleton.Skeleton
    , block_schema :: SchemaId
    } deriving (Eq, Show, Read)

block_tracklike_ids :: Block -> [TracklikeId]
block_tracklike_ids = map tracklike_id . block_tracks

block_track_ids :: Block -> [Track.TrackId]
block_track_ids = track_ids_of . block_tracklike_ids

block_ruler_ids :: Block -> [Ruler.RulerId]
block_ruler_ids = ruler_ids_of . block_tracklike_ids

block title config tracks schema_id =
    Block title config tracks Skeleton.empty schema_id

-- | Per-block configuration.
data Config = Config {
    config_selection_colors :: [Color]
    , config_bg_color :: Color
    , config_track_box :: (Color, Char)
    , config_sb_box :: (Color, Char)
    } deriving (Eq, Show, Read)

data BlockTrack = BlockTrack {
    tracklike_id :: TracklikeId
    -- | The real width is in the View, but this width is a default if a new
    -- View is created from this Block.
    , track_width :: Width
    -- | Don't display this track at all.
    , track_hidden :: Bool
    -- | Don't display this track, but render its signal behind another track.
    -- Only makes sense for event tracks.
    , track_merged :: Maybe TrackNum
    -- | UI shows muted indication, deriver should skip this track.  Only makes
    -- sense for event tracks.
    , track_muted :: Bool
    -- | Track is replaced by a divider.  Doesn't make much sense if it already
    -- is one.
    , track_collapsed :: Bool
    } deriving (Eq, Show, Read)

-- | Construct a 'BlockTrack' with defaults.
block_track :: TracklikeId -> Width -> BlockTrack
block_track tracklike_id width =
    BlockTrack tracklike_id width False Nothing False False

modify_id :: BlockTrack -> (TracklikeId -> TracklikeId) -> BlockTrack
modify_id track f = track { tracklike_id = f (tracklike_id track) }

data TracklikeId =
    -- | Tracks may have a Ruler overlay
    TId Track.TrackId Ruler.RulerId
    | RId Ruler.RulerId
    | DId Divider
    deriving (Eq, Show, Read)

track_id_of :: TracklikeId -> Maybe Track.TrackId
track_id_of (TId tid _) = Just tid
track_id_of _ = Nothing

track_ids_of = Maybe.catMaybes . map track_id_of

ruler_id_of :: TracklikeId -> Maybe Ruler.RulerId
ruler_id_of (TId _ rid) = Just rid
ruler_id_of (RId rid) = Just rid
ruler_id_of _ = Nothing

ruler_ids_of = Maybe.catMaybes . map ruler_id_of

set_rid rid (TId tid _) = TId tid rid
set_rid rid (RId _) = RId rid
set_rid _ t = t

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
    deriving (Eq, Ord, Show, Read)

-- * block view

data View = View {
    -- | view_block should never change.
    -- TODO Views that point to a BlockId not in state_blocks should be
    -- destroyed.
    view_block :: BlockId
    , view_rect :: Rect

    -- | These two are derived from view_rect, but cached here so pure code
    -- doesn't have to call to the UI and import BlockC.
    , view_visible_track :: Int
    , view_visible_time :: Int

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
    } deriving (Eq, Ord, Show, Read)

-- | Construct a View, using default values for most of its fields.
-- Don't construct views using View directly since State.create_view overwrites
-- view_tracks, and maybe more in the future.
view block_id rect zoom config =
    -- view_visible_track and view_visible_time are unknown, but will
    -- be filled in when the new view emits its initial resize msg.
    View block_id rect 0 0 config Map.empty 0 zoom Map.empty []

show_status :: View -> String
show_status = Seq.join " | " . map (\(k, v) -> k ++ ": " ++ v)
    . Map.assocs . view_status

-- | Return how much track is in view.
visible_time :: View -> TrackPos
visible_time view =
    pixels_to_track_pos (view_zoom view) (view_visible_time view)

visible_track :: View -> Width
visible_track = view_visible_track

pixels_to_track_pos :: Zoom -> Int -> TrackPos
pixels_to_track_pos zoom pixels =
    track_pos pixels / track_pos (zoom_factor zoom)

data TrackView = TrackView {
    track_view_width :: Width
    } deriving (Eq, Ord, Show, Read)

data Rect = Rect {
    rect_x :: Int
    , rect_y :: Int
    , rect_w :: Int
    , rect_h :: Int
    } deriving (Eq, Ord, Show, Read)
rect_r rect = rect_x rect + rect_w rect
rect_b rect = rect_y rect + rect_h rect

-- | These are defaults for newly created blocks.
data ViewConfig = ViewConfig {
    vconfig_block_title_height :: Int
    , vconfig_track_title_height :: Int
    , vconfig_skel_height :: Int
    , vconfig_sb_size :: Int
    , vconfig_status_size :: Int
    } deriving (Eq, Ord, Show, Read)

-- | View zoom and time scroll offset.
data Zoom = Zoom {
    zoom_offset :: TrackPos
    , zoom_factor :: Double
    } deriving (Eq, Ord, Show, Read)

-- TODO: remove color and put it in BlockC.SelectionC, which gets its color
-- from a BlockConfig list
data Selection = Selection {
    -- | The position the selection was established at.
    sel_start_track :: TrackNum
    , sel_start_pos :: TrackPos

    -- | The position the selection is now at.  The tracks are an inclusive
    -- range, the pos are half-open.  This is because these pairs are meant to
    -- be symmetrical, but the c++ layer only supports half-open pos ranges.
    -- I don't think there's much I can do about this.
    , sel_cur_track :: TrackNum
    , sel_cur_pos :: TrackPos
    } deriving (Eq, Ord, Show, Read)

-- | These constructors return Maybe because that's what set_selection expects.
selection :: TrackNum -> TrackPos -> TrackNum -> TrackPos -> Maybe Selection
selection start_track start_pos cur_track cur_pos =
    Just (Selection start_track start_pos cur_track cur_pos)

-- | A point is a selection with no duration.
point_selection :: TrackNum -> TrackPos -> Maybe Selection
point_selection tracknum pos = selection tracknum pos tracknum pos

sel_is_point :: Selection -> Bool
sel_is_point sel = sel_start_pos sel == sel_cur_pos sel

sel_modify_tracks :: (TrackNum -> TrackNum) -> Selection -> Selection
sel_modify_tracks f sel = sel
    { sel_start_track = f (sel_start_track sel)
    , sel_cur_track = f (sel_cur_track sel)
    }

sel_expand_tracks :: TrackNum -> Selection -> Selection
sel_expand_tracks n sel
    | cur > start = sel { sel_cur_track = cur + n }
    | otherwise = sel { sel_start_track = start + n }
    where
    start = sel_start_track sel
    cur = sel_cur_track sel

-- | Start and end tracks, from small to large.
sel_track_range :: Selection -> (TrackNum, TrackNum)
sel_track_range sel = (min track0 track1, max track0 track1)
    where (track0, track1) = (sel_start_track sel, sel_cur_track sel)

sel_tracknums :: Selection -> [TrackNum]
sel_tracknums sel = let (start, end) = sel_track_range sel in [start..end]

-- | Start and end points, from small to large.
sel_range :: Selection -> (TrackPos, TrackPos)
sel_range sel = (min pos0 pos1, max pos0 pos1)
    where (pos0, pos1) = (sel_start_pos sel, sel_cur_pos sel)

sel_set_duration :: TrackPos -> Selection -> Selection
sel_set_duration dur sel
    | cur > start = sel { sel_cur_pos = start + (max (TrackPos 0) dur) }
    | otherwise = sel { sel_start_pos = cur + (max (TrackPos 0) dur) }
    where
    start = sel_start_pos sel
    cur = sel_cur_pos sel

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
