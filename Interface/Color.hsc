module Interface.Color where
import Foreign
import Foreign.C

import qualified Interface.Util as Util

-- r, g, b, alpha, from 0--1
data Color = Color Double Double Double Double deriving (Eq, Show)

black = Color 0 0 0 0
white = Color 1 1 1 0
red = Color 1 0 0 0
green = Color 0 1 0 0
blue = Color 0 0 1 0


-- Storable instance

#include "c_interface.h"

instance Storable Color where
    sizeOf _ = #size Color
    alignment _ = 1
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
