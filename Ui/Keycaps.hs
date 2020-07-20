-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Higher level wrapper around KeycapsC.
module Ui.Keycaps where
import qualified Data.Map as Map

import qualified Util.Rect as Rect
import qualified Ui.Color as Color
import qualified Ui.KeycapsT as KeycapsT

import           Global


{-
    Hold layouts for e.g. macbook_layout.
    Then construct KeycapsT.DrawKeycaps and DrawConfig by looking up keycaps in
    a map.

    Util function to read from a file and dynamically create a new one.

    Use Local.KeyLayout, Cmd.KeyLayouts, Cmd.PhysicalKey
    to display physical keyboard.
-}

type Bindings = Map Char (Text, Text)

macbook_rects :: [Rect.Rect]
macbook_rects =
    [ Rect.xywh 10 10 10 10
    ]
    -- 5 rows
    -- hardcoded heights, and separations
    -- just the widths
    -- then apply KeyLayouts


make_layout :: Map Text Rect.Rect -> KeycapsT.Layout
make_layout labels = KeycapsT.Layout
    { lt_size = (w, h)
    , lt_bg_color = Color.white
    , lt_keycap_color = Color.gray8
    , lt_highlight_color = Color.rgb 0.85 0.85 1
    , lt_label_color = Color.gray2
    , lt_binding_color = Color.black
    , lt_labels = labels
    }
    where
    w = 5 + maximum (0 : map Rect.r (Map.elems labels))
    h = 5 + maximum (0 : map Rect.b (Map.elems labels))
