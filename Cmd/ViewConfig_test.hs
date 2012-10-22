module Cmd.ViewConfig_test where
import qualified Util.Rect as Rect
import Util.Test
import qualified Cmd.ViewConfig as ViewConfig


test_horizontal_tile_rects = do
    let f screen rs = map extract $
            ViewConfig.horizontal_tile_rects (mkrect screen) (map mkrect rs)
        mkrect (x, y, w, h) = Rect.xywh x y w h
        extract r = (Rect.rx r, Rect.ry r, Rect.rw r, Rect.rh r)
    equal (f (8, 8, 8, 8) []) []
    equal (f (8, 8, 8, 8) [(1, 1, 2, 1)]) [(8, 8, 2, 1)]
    equal (f (8, 8, 8, 8) [(1, 1, 16, 16)]) [(8, 8, 16, 16)]

    -- No overlap, different widths.
    equal (f (8, 8, 8, 8) [(1, 1, 2, 1), (0, 0, 1, 1)])
        [(8, 8, 2, 1), (10, 8, 1, 1)]
    -- Overlapping.
    equal (f (0, 0, 8, 8) [(0, 0, 6, 1), (1, 0, 6, 1)])
        [(0, 0, 6, 1), (2, 0, 6, 1)]
