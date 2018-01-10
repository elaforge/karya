-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Miscellaneous low level types with few dependencies.
module Ui.Types (TrackNum, Width, MouseButton, Orientation(..), invert) where
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize


-- | Index into a block's tracks.
type TrackNum = Int
-- | Width of a track in pixels.
type Width = Int
-- | Mouse button number.
type MouseButton = Int

-- * orientation

-- | Whether the event is front-weighted or back-weighted.  In the event this
-- is represented with positive or negative duration.
data Orientation = Negative | Positive
    deriving (Eq, Ord, Read, Show, Enum, Bounded)
    -- The Negative to Positive order is important, because that affects the
    -- EventMap's sort order, which functions in "Ui.Events" rely on.
instance Pretty.Pretty Orientation where pretty = Text.pack . show

invert :: Orientation -> Orientation
invert Positive = Negative
invert Negative = Positive

instance Serialize.Serialize Orientation where
    put = Serialize.put_enum
    get = Serialize.get_enum
