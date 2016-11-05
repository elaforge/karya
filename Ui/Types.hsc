-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Miscellaneous low level types with few dependencies.
module Ui.Types (
    TrackNum, Width, MouseButton
    , Zoom(..)
    , zoom_to_pixels, zoom_to_time
) where
import Util.ForeignC
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect

import qualified Ui.ScoreTime as ScoreTime
import Ui.ScoreTime (TrackTime)
import qualified Ui.Util as Util

import Global


#include "Ui/c_interface.h"

-- A few type synonyms for more descriptive signatures.

-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
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
    zoom_offset :: !TrackTime
    , zoom_factor :: !Double
    } deriving (Eq, Ord, Show, Read)

instance Pretty.Pretty Zoom where
    pretty (Zoom offset factor) =
        "+" <> pretty offset <> "*" <> Num.showFloat 1 factor

instance CStorable Zoom where
    sizeOf _ = #size Zoom
    alignment _ = alignment (0 :: CDouble)
    peek zoomp = do
        offset <- (#peek Zoom, offset) zoomp
        factor <- (#peek Zoom, factor) zoomp :: IO CDouble
        return $ Zoom offset (Util.hs_double factor)
    poke zoomp (Zoom offset factor) = do
        (#poke Zoom, offset) zoomp offset
        (#poke Zoom, factor) zoomp (Util.c_double factor)

-- | Convert a position at a given zoom factor to a pixel position.  Doesn't
-- take the zoom offset into account.
zoom_to_pixels :: Zoom -> TrackTime -> Int
zoom_to_pixels zoom pos = Num.d2i $ ScoreTime.to_double pos * zoom_factor zoom

-- | Convert a pixel position to a TrackTime at the given zoom factor.
-- Doesn't take the zoom offset into account.
zoom_to_time :: Zoom -> Int -> TrackTime
zoom_to_time zoom pixels =
    ScoreTime.double (fromIntegral pixels / zoom_factor zoom)
