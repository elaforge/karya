-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'Zoom' type.
module Ui.Zoom (Zoom(..), toPixels, toTime) where
import ForeignC
import qualified Util.FFI as FFI
import qualified Util.Num as Num

import qualified Ui.ScoreTime as ScoreTime
import Ui.ScoreTime (TrackTime)

import Global

#include "Ui/c_interface.h"


-- | View zoom and time scroll offset.
data Zoom = Zoom
    { offset :: !TrackTime
    , factor :: !Double
    } deriving (Eq, Ord, Show, Read)

instance Pretty Zoom where
    pretty (Zoom offset factor) =
        "+" <> pretty offset <> "*" <> Num.showFloat 1 factor

instance CStorable Zoom where
    sizeOf _ = #size Zoom
    alignment _ = alignment (0 :: CDouble)
    peek zoomp = do
        offset <- (#peek Zoom, offset) zoomp
        factor <- (#peek Zoom, factor) zoomp :: IO CDouble
        return $ Zoom offset (FFI.hsDouble factor)
    poke zoomp (Zoom offset factor) = do
        (#poke Zoom, offset) zoomp offset
        (#poke Zoom, factor) zoomp (FFI.cDouble factor)

-- | Convert a position at a given zoom factor to a pixel position.  Doesn't
-- take the zoom offset into account.
toPixels :: Zoom -> TrackTime -> Int
toPixels zoom pos = Num.d2i $ ScoreTime.toDouble pos * factor zoom

-- | Convert a pixel position to a TrackTime at the given zoom factor.
-- Doesn't take the zoom offset into account.
toTime :: Zoom -> Int -> TrackTime
toTime zoom pixels = ScoreTime.fromDouble (fromIntegral pixels / factor zoom)
