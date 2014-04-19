-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Rect (
    Rect
    -- * access
    , rx, ry, rw, rh, rr, rb
    , upper_left

    -- * constructor
    , xywh, empty

    -- * transformation
    , place, resize

    -- * functions
    , distance, intersection, overlaps, point_distance, contains
) where
import qualified Util.Pretty as Pretty


data Rect = Rect { rx :: Int, ry :: Int, rw :: Int, rh :: Int }
    deriving (Eq, Ord, Show, Read)

type Point = (Int, Int)

instance Pretty.Pretty Rect where
    format (Rect x y w h) = Pretty.text "Rect" Pretty.<+> Pretty.format (x, y)
        Pretty.<+> Pretty.format (w, h)

-- * access

rr, rb :: Rect -> Int
rr r = rx r + rw r
rb r = ry r + rh r

upper_left :: Rect -> Point
upper_left r = (rx r, ry r)

-- * constructor

xywh :: Int -> Int -> Int -> Int -> Rect
xywh = Rect

empty :: Rect
empty = Rect 0 0 0 0

-- * transform

place :: Int -> Int -> Rect -> Rect
place x y rect = xywh x y (rw rect) (rh rect)

resize :: Int -> Int -> Rect -> Rect
resize w h rect = xywh (rx rect) (ry rect) w h

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
    dist = point_distance (px, py)

-- | Find the intersection of two rectangles.
intersection :: Rect -> Rect -> Rect
intersection r1 r2 = Rect x y (max 0 (r-x)) (max 0 (b-y))
    where
    x = max (rx r1) (rx r2)
    y = max (ry r1) (ry r2)
    r = min (rr r1) (rr r2)
    b = min (rb r1) (rb r2)

overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 = rw r > 0 || rh r > 0
    where r = intersection r1 r2

point_distance :: Point -> Point -> Double
point_distance (x1, y1) (x2, y2) =
    sqrt $ fromIntegral (x2-x1) ** 2 + fromIntegral (y2-y1) ** 2

contains :: Rect -> Point -> Bool
contains r (x, y) = rx r <= x && x < rr r && ry r <= y && y < rb r
