{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Miscellaneous low level types with few dependencies.
module Ui.Types where
import qualified Numeric
import qualified Text.Read as Read -- for Read instances
import Foreign
import Foreign.C

import qualified Ui.Id as Id
import qualified Ui.Util as Util


#include "c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
-- | Index into the the selection list.
type SelNum = Int

-- * rect

data Rect = Rect {
    rect_x :: Int
    , rect_y :: Int
    , rect_w :: Int
    , rect_h :: Int
    } deriving (Eq, Ord, Show, Read)
rect_r rect = rect_x rect + rect_w rect
rect_b rect = rect_y rect + rect_h rect

instance Storable Rect where
    sizeOf _ = #size Rect
    alignment _ = #{alignment Rect}
    poke = error "Rect poke unimplemented"
    peek rectp = do
        x <- (#peek Rect, x) rectp :: IO CInt
        y <- (#peek Rect, y) rectp :: IO CInt
        w <- (#peek Rect, w) rectp :: IO CInt
        h <- (#peek Rect, h) rectp :: IO CInt
        return $ Rect (i x) (i y) (i w) (i h)
        where i = fromIntegral

-- * zoom

-- | View zoom and time scroll offset.
data Zoom = Zoom {
    zoom_offset :: TrackPos
    , zoom_factor :: Double
    } deriving (Eq, Ord, Show, Read)

instance Storable Zoom where
    sizeOf _ = #size ZoomInfo
    alignment _ = #{alignment ZoomInfo}
    peek zoomp = do
        offset <- (#peek ZoomInfo, offset) zoomp
        factor <- (#peek ZoomInfo, factor) zoomp :: IO CDouble
        return $ Zoom offset (realToFrac factor)
    poke zoomp (Zoom offset factor) = do
        (#poke ZoomInfo, offset) zoomp offset
        (#poke ZoomInfo, factor) zoomp (Util.c_double factor)

-- * trackpos

-- | The position of an Event on a track.  The units are arbitrary, so how
-- many units are in one second depends on the tempo.  TrackPos units
-- can be negative, but blocks only display events at positive TrackPos.
newtype TrackPos = TrackPos Double
    deriving (Num, Enum, Real, Fractional, RealFrac, Eq, Ord, Show, Read)

instance Storable TrackPos where
    sizeOf _ = #size TrackPos
    alignment _ = #{alignment TrackPos}
    peek posp = do
        v <- (#peek TrackPos, _val) posp :: IO CDouble
        return (TrackPos (realToFrac v))
    poke posp (TrackPos pos) =
        (#poke TrackPos, _val) posp (realToFrac pos :: CDouble)

track_pos :: (Real a) => a -> TrackPos
track_pos n = TrackPos (realToFrac n)

pretty_pos :: TrackPos -> String
pretty_pos (TrackPos pos) = Numeric.showFFloat (Just 3) pos ""

-- * ID

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

newtype TrackId = TrackId Id.Id
    deriving (Eq, Ord)

newtype RulerId = RulerId Id.Id
    deriving (Eq, Ord)

instance Show BlockId where show = Id.show_ident
instance Show ViewId where show = Id.show_ident
instance Show SchemaId where show = Id.show_ident
instance Show TrackId where show = Id.show_ident
instance Show RulerId where show = Id.show_ident

instance Read BlockId where readPrec = Id.read_ident undefined
instance Read ViewId where readPrec = Id.read_ident undefined
instance Read SchemaId where readPrec = Id.read_ident undefined
instance Read TrackId where readPrec = Id.read_ident undefined
instance Read RulerId where readPrec = Id.read_ident undefined

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
instance Id.Ident TrackId where
    unpack_id (TrackId a) = a
    cons_name _ = "tid"
    cons = TrackId
instance Id.Ident RulerId where
    unpack_id (RulerId a) = a
    cons_name _ = "rid"
    cons = RulerId


-- * selection

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
