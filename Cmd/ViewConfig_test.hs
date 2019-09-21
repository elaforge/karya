-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ViewConfig_test where
import qualified Data.List as List

import qualified Util.Rect as Rect
import Util.Test
import qualified Ui.Id as Id
import qualified Ui.UiTest as UiTest
import qualified Cmd.ViewConfig as ViewConfig


test_horizontal_tile_rects = do
    let f screen rs = map extract $
            ViewConfig.horizontal_tile_rects (mkrect screen) (map mkrect rs)
        mkrect (x, y, w, h) = Rect.xywh x y w h
        extract r = (Rect.x r, Rect.y r, Rect.w r, Rect.h r)
    equal (f (8, 8, 8, 8) []) []
    equal (f (8, 8, 8, 8) [(1, 1, 2, 1)]) [(8, 8, 2, 1)]
    equal (f (8, 8, 8, 8) [(1, 1, 16, 16)]) [(8, 8, 16, 16)]

    -- No overlap, different widths.
    equal (f (8, 8, 8, 8) [(1, 1, 2, 1), (0, 0, 1, 1)])
        [(8, 8, 2, 1), (10, 8, 1, 1)]
    -- Overlapping.
    equal (f (0, 0, 8, 8) [(0, 0, 6, 1), (1, 0, 6, 1)])
        [(0, 0, 6, 1), (2, 0, 6, 1)]

test_fit_rects = do
    let f = List.sort . map (\(ident, r) -> (Id.ident_name ident, unrect r))
            . ViewConfig.fit_rects (Rect.xywh 0 0 100 100)
            . map (\(ident, r) -> (UiTest.vid ident, rect r))
    equal (f [("a", (4, 4, 1, 1))]) [("a", (0, 0, 1, 1))]
    equal (f [("a", (0, 0, 2, 2)), ("b", (1, 2, 2, 2))])
        [("a", (0, 0, 2, 2)), ("b", (0, 2, 2, 2))]

test_corners_of = do
    let f = ViewConfig.corners_of (Rect.xywh 0 0 100 100)
    equal (f []) [(0, 0)]
    equal (f [Rect.xywh 0 0 1 1]) [(0, 1), (1, 0)]
    --   0 1
    -- 0 *
    -- 1 *
    -- 2
    equal (f [Rect.xywh 0 0 1 1, Rect.xywh 0 1 1 1]) [(0, 2), (1, 0)]

rect :: (Int, Int, Int, Int) -> Rect.Rect
rect (x, y, w, h) = Rect.xywh x y w h

unrect :: Rect.Rect -> (Int, Int, Int, Int)
unrect r = (Rect.x r, Rect.y r, Rect.w r, Rect.h r)
