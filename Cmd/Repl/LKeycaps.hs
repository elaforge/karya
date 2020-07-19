-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Cmd.Repl.LKeycaps where
import qualified Util.Rect as Rect
import qualified Cmd.Cmd as Cmd
import qualified Ui.Keycaps as Keycaps
import qualified Ui.KeycapsT as KeycapsT


layout :: Cmd.M m => [(Int, Int, Int, Int)] -> [((Int, Int), Char)] -> m ()
layout rects labels =
    update (Just ((200, 200), Keycaps.make_layout (map rect rects) labels)) []
    where rect (x, y, w, h) = Rect.xywh x y w h

update :: Cmd.M m => Maybe ((Int, Int), KeycapsT.Layout) -> [KeycapsT.Binding]
    -> m ()
update layout bindings = Cmd.modify $ \st -> st
    { Cmd.state_keycaps = Just $
        Cmd.KeycapsUpdate layout (KeycapsT.Bindings bindings)
    }
