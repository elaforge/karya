{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Ui.Color where
import Foreign
import Foreign.C
import qualified Data.Generics as Generics

import qualified Ui.Util as Util

-- r, g, b, alpha, from 0--1
data Color = Color Double Double Double Double
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)
-- | An opaque color with the given r, g, and b.
rgb r g b = rgba r g b 1
rgba r g b a = let c = clamp 0 1 in Color (c r) (c g) (c b) (c a)

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

scale :: Double -> Color -> Color
scale d (Color r g b a) = rgba (r*d) (g*d) (b*d) a
lighten d = scale (1+d)
darken d = scale (1-d)

alpha a' (Color r g b _a) = rgba r g b a'

clamp lo hi = max lo . min hi

-- Storable instance

#include "c_interface.h"

instance Storable Color where
    sizeOf _ = #size Color
    alignment _ = undefined
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
    where c double = Util.c_uchar (floor (double*255))
