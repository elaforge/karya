-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Color where
import Data.Bits
import qualified Data.Text as Text
import ForeignC

import qualified Util.CUtil as CUtil
import qualified Util.Num as Num

import Global


-- | How to highlight an event in the UI.  This can be used to highlight
-- instrumental restrictions, or for analysis.
--
-- TODO This doesn't really belong in this module, but I couldn't think of
-- a better place to put it that wouldn't incur unnecessary dependencies.
data Highlight =
    NoHighlight
    -- | This note is special in some way, perhaps an open string.
    | Notice
    -- | This note might have a problem, e.g. in a difficult range or requires
    -- difficult fingering or the pitch needs to be adjusted.
    | Warning
    -- | This note is probably unplayable, e.g. out of range.
    | Error
    deriving (Bounded, Eq, Enum, Ord, Show)

-- | This is so more serious highlights can override less serious ones.
instance Semigroup Highlight where (<>) = max
instance Monoid Highlight where
    mempty = NoHighlight
    mappend = (<>)

-- r, g, b, alpha, from 0--1
data Color = Color !Double !Double !Double !Double
    deriving (Eq, Ord, Show, Read)

instance Pretty Color where
    pretty (Color r g b a) = "rgba:"
        <> Text.intercalate "/" (map (Num.showFloat 2) [r, g, b, a])

-- | An opaque color with the given r, g, and b.
rgb :: Double -> Double -> Double -> Color
rgb r g b = rgba r g b 1

rgba :: Double -> Double -> Double -> Double -> Color
rgba r g b a = let c = Num.clamp 0 1 in Color (c r) (c g) (c b) (c a)

-- | Make a Color from an RGBA word.
rgba_word :: Word32 -> Color
rgba_word word = rgba (d 24) (d 16) (d 8) (d 0)
    where d = (/0xff) . fromIntegral . (.&. 0xff) . shiftR word

black = rgb 0 0 0
white = rgb 1 1 1
red = rgb 1 0 0
green = rgb 0 1 0
blue = rgb 0 0 1

yellow = rgb 1 1 0
purple = rgb 1 0 1
turquoise = rgb 0 1 1

gray1 = rgb 0.1 0.1 0.1
gray2 = rgb 0.2 0.2 0.2
gray3 = rgb 0.3 0.3 0.3
gray4 = rgb 0.4 0.4 0.4
gray5 = rgb 0.5 0.5 0.5
gray6 = rgb 0.6 0.6 0.6
gray7 = rgb 0.7 0.7 0.7
gray8 = rgb 0.8 0.8 0.8
gray9 = rgb 0.9 0.9 0.9

brightness :: Double -> Color -> Color
brightness d (Color r g b a)
    | d < 1 = rgba (Num.scale 0 r d) (Num.scale 0 g d) (Num.scale 0 b d) a
    | otherwise =
        rgba (Num.scale r 1 (d-1)) (Num.scale g 1 (d-1))
            (Num.scale b 1 (d-1)) a

alpha :: Double -> Color -> Color
alpha a' (Color r g b _a) = rgba r g b a'

-- Storable instance

#include "Ui/c_interface.h"

instance CStorable Color where
    sizeOf _ = #size Color
    alignment _ = alignment (0 :: CDouble)
    peek = peek_color
    poke = poke_color

peek_color colorp = do
    r <- (#peek Color, r) colorp :: IO CUChar
    g <- (#peek Color, g) colorp :: IO CUChar
    b <- (#peek Color, b) colorp :: IO CUChar
    a <- (#peek Color, a) colorp :: IO CUChar
    return $ Color (d r) (d g) (d b) (d a)
    where d uchar = fromIntegral uchar / 255.0

poke_color colorp (Color r g b a) = do
    (#poke Color, r) colorp (c r)
    (#poke Color, g) colorp (c g)
    (#poke Color, b) colorp (c b)
    (#poke Color, a) colorp (c a)
    where c double = CUtil.c_uchar (floor (double*255))
