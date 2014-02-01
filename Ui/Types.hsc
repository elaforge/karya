-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Miscellaneous low level types with few dependencies.
module Ui.Types (
    TrackNum, Width, SelNum, MouseButton
    , Zoom(..)
    , zoom_to_pixels, zoom_to_time

    -- * Selection
    , Selection(..), selection, point_selection, sel_is_point
    , sel_modify_tracks, sel_expand_tracks, sel_track_range, sel_tracknums
    , sel_range, sel_set_duration
) where
import Util.Control
import Util.ForeignC
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect

import qualified Ui.ScoreTime as ScoreTime
import Ui.ScoreTime (TrackTime)
import qualified Ui.Util as Util


#include "Ui/c_interface.h"

-- A few type synonyms for more descriptive signatures.

-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
-- | Index into the the selection list.
type SelNum = Int
-- | Mouse button number.
type MouseButton = Int

-- * rect

-- | It's an orphan, but hscs are annoying to work with from ghci, so let's
-- just have it live here for now.
instance CStorable Rect.Rect where
    sizeOf _ = #size IRect
    alignment _ = alignment (0 :: CInt)
    poke = error "Rect poke unimplemented"
    peek rectp = do
        x <- (#peek IRect, x) rectp :: IO CInt
        y <- (#peek IRect, y) rectp :: IO CInt
        w <- (#peek IRect, w) rectp :: IO CInt
        h <- (#peek IRect, h) rectp :: IO CInt
        return $ Rect.xywh (i x) (i y) (i w) (i h)
        where i = fromIntegral

-- * zoom

-- | View zoom and time scroll offset.
data Zoom = Zoom {
    zoom_offset :: TrackTime
    , zoom_factor :: Double
    } deriving (Eq, Ord, Show, Read)

instance Pretty.Pretty Zoom where
    pretty (Zoom offset factor) =
        '+' : pretty offset ++ '*' : Pretty.show_float 1 factor

instance CStorable Zoom where
    sizeOf _ = #size ZoomInfo
    alignment _ = alignment (0 :: CDouble)
    peek zoomp = do
        offset <- (#peek ZoomInfo, offset) zoomp
        factor <- (#peek ZoomInfo, factor) zoomp :: IO CDouble
        return $ Zoom offset (Util.hs_double factor)
    poke zoomp (Zoom offset factor) = do
        (#poke ZoomInfo, offset) zoomp offset
        (#poke ZoomInfo, factor) zoomp (Util.c_double factor)

-- | Convert a position at a given zoom factor to a pixel position.  Doesn't
-- take the zoom offset into account.
zoom_to_pixels :: Zoom -> TrackTime -> Int
zoom_to_pixels zoom pos = Num.d2i $ ScoreTime.to_double pos * zoom_factor zoom

-- | Convert a pixel position to a TrackTime at the given zoom factor.
-- Doesn't take the zoom offset into account.
zoom_to_time :: Zoom -> Int -> TrackTime
zoom_to_time zoom pixels =
    ScoreTime.double (fromIntegral pixels / zoom_factor zoom)


-- * selection

data Selection = Selection {
    -- | The position the selection was established at.
    sel_start_track :: TrackNum
    , sel_start_pos :: TrackTime

    -- | The position the selection is now at.  The tracks are an inclusive
    -- range, the pos are half-open.  This is because these pairs are meant to
    -- be symmetrical, but the c++ layer only supports half-open pos ranges.
    -- I don't think there's much I can do about this.
    , sel_cur_track :: TrackNum
    , sel_cur_pos :: TrackTime
    } deriving (Eq, Ord, Show, Read)

instance Pretty.Pretty Selection where
    pretty (Selection strack spos ctrack cpos) =
        "Selection " <> pretty (strack, spos) <> "--" <> pretty (ctrack, cpos)

selection :: TrackNum -> TrackTime -> TrackNum -> TrackTime -> Selection
selection start_track start_pos cur_track cur_pos =
    Selection start_track start_pos cur_track cur_pos

-- | A point is a selection with no duration.
point_selection :: TrackNum -> TrackTime -> Selection
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
sel_range :: Selection -> (TrackTime, TrackTime)
sel_range sel = (min pos0 pos1, max pos0 pos1)
    where (pos0, pos1) = (sel_start_pos sel, sel_cur_pos sel)

sel_set_duration :: TrackTime -> Selection -> Selection
sel_set_duration dur sel
    | cur > start = sel { sel_cur_pos = start + (max 0 dur) }
    | otherwise = sel { sel_start_pos = cur + (max 0 dur) }
    where
    start = sel_start_pos sel
    cur = sel_cur_pos sel
