-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'Rect' type.
module Util.Rect (
    Rect(Rect)
    , Point
    -- * access
    , x, y, w, h, r, b
    , upperLeft, lowerLeft, upperRight, lowerRight

    -- * constructor
    , xywh, empty

    -- * transformation
    , place, resize

    -- * functions
    , distance, intersection, overlaps, touches, pointDistance
    , containsPoint, touchesPoint
) where
import qualified Util.Pretty as Pretty
import qualified Util.FFI as FFI

import           ForeignC


data Rect = Rect { x :: Int, y :: Int, w :: Int, h :: Int }
    deriving (Eq, Ord, Show, Read)

type Point = (Int, Int)

instance Pretty.Pretty Rect where
    format (Rect x y w h) = Pretty.text "Rect" Pretty.<+> Pretty.format (x, y)
        Pretty.<+> Pretty.format (w, h)

-- * access

r, b :: Rect -> Int
r rect = x rect + w rect
b rect = y rect + h rect

upperLeft, lowerLeft, upperRight, lowerRight :: Rect -> Point
upperLeft rect = (x rect, y rect)
lowerLeft rect = (x rect, b rect)
upperRight rect = (r rect, y rect)
lowerRight rect = (r rect, b rect)


-- * constructor

xywh :: Int -> Int -> Int -> Int -> Rect
xywh = Rect

empty :: Rect
empty = Rect 0 0 0 0

-- * transform

place :: Int -> Int -> Rect -> Rect
place x y rect = xywh x y (w rect) (h rect)

resize :: Int -> Int -> Rect -> Rect
resize w h rect = xywh (x rect) (y rect) w h

-- * functions

-- | Distance from a point to a rectangle.
distance :: Point -> Rect -> Double
distance (px, py) (Rect x y w h)
    | x <= px && px < r && y <= py && py < b = 0
    | x <= px && px < r = fromIntegral $
        min (abs (y - py)) (abs (b - py))
    | y <= py && py < b = fromIntegral $
        min (abs (x - px)) (abs (r - px))
    | otherwise = dist (x, y) `min` dist (r, y)
        `min` dist (r, b) `min` dist (x, b)
    where
    r = x + w
    b = y + h
    dist = pointDistance (px, py)

-- | Find the intersection of two rectangles.
intersection :: Rect -> Rect -> Rect
intersection r1 r2 = Rect x_ y_ (max 0 (r_-x_)) (max 0 (b_-y_))
    where
    x_ = max (x r1) (x r2)
    y_ = max (y r1) (y r2)
    r_ = min (r r1) (r r2)
    b_ = min (b r1) (b r2)

overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 = not $
    x r1 >= r r2 || r r1 <= x r2 || y r1 >= b r2 || b r1 <= y r2

-- | This is like 'overlaps', but is also true if the the rectangle touch each
-- other.
touches :: Rect -> Rect -> Bool
touches r1 r2 = not $
    x r1 > r r2 || r r1 < x r2 || y r1 > b r2 || b r1 < y r2

pointDistance :: Point -> Point -> Double
pointDistance (x1, y1) (x2, y2) =
    sqrt $ fromIntegral (x2-x1) ** 2 + fromIntegral (y2-y1) ** 2

containsPoint :: Rect -> Point -> Bool
containsPoint rect (x_, y_) =
    x rect <= x_ && x_ < r rect && y rect <= y_ && y_ < b rect

-- | This is like 'contains_point', but is also true if the the point is on the
-- right or bottom edge of the rectangle.
touchesPoint :: Rect -> Point -> Bool
touchesPoint rect (x_, y_) =
    x rect <= x_ && x_ <= r rect && y rect <= y_ && y_ <= b rect


#include "Ui/c_interface.h"

instance CStorable Rect where
    sizeOf _ = #size IRect
    alignment _ = alignment (0 :: CInt)
    poke p (Rect x y w h) = do
        (#poke IRect, x) p (i x)
        (#poke IRect, y) p (i y)
        (#poke IRect, w) p (i w)
        (#poke IRect, h) p (i h)
        where i = FFI.cInt
    peek p = do
        x <- (#peek IRect, x) p :: IO CInt
        y <- (#peek IRect, y) p :: IO CInt
        w <- (#peek IRect, w) p :: IO CInt
        h <- (#peek IRect, h) p :: IO CInt
        return $ xywh (i x) (i y) (i w) (i h)
        where i = fromIntegral

instance CStorable Point where
    sizeOf _ = #size IPoint
    alignment _ = alignment (0 :: CInt)
    poke p (x, y) = do
        (#poke IPoint, x) p (i x)
        (#poke IPoint, y) p (i y)
        where i = FFI.cInt
    peek p = do
        x <- (#peek IPoint, x) p :: IO CInt
        y <- (#peek IPoint, y) p :: IO CInt
        return (i x, i y)
        where i = fromIntegral
