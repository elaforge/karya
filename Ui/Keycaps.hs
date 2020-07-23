-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Higher level wrapper around KeycapsC.
module Ui.Keycaps where
import qualified Data.Map as Map

import qualified Util.Rect as Rect
import qualified Ui.Color as Color
import qualified Ui.Key as Key
import qualified Ui.KeycapsT as KeycapsT

import           Global


mac_labels :: Map Key.Key Rect.Rect
mac_labels = Map.fromList $ concat label_rects ++ arrows
    where
    label_rects = zipWith mkrow ys qwerty_rows
    qwerty_rows =
        [ cs "`1234567890-=" ++ [(30, Key.Delete)]
        , (30, Key.Tab) : cs "qwertyuiop[]\\"
        , (37, Key.KCapsLock) : cs "asdfghjkl;'" ++ [(37, Key.Enter)]
        , (50, Key.ShiftL) : cs "zxcvbnm,./" ++ [(49, Key.ShiftR)]
        , map (w,) [Key.Function, Key.ControlL, Key.AltL] ++
            [ (26, Key.MetaL), (115, Key.Char ' '), (26, Key.MetaR)
            , (w, Key.AltR)
            ]
        ]
        where cs = map ((w,) . Key.Char)
    arrows =
        [ (Key.Left, Rect.xywh (xn 0) y2 w h2)
        , (Key.Up, Rect.xywh (xn 1) ly w h2)
        , (Key.Down, Rect.xywh (xn 1) y2 w h2)
        , (Key.Right, Rect.xywh (xn 2) y2 w h2)
        ]
        where
        xn n = lx + gap + (w+gap) * n
        y2 = ly + h2
    (lx, ly) =
        ( maximum $ map (Rect.r . snd) (last label_rects)
        , Rect.y $ snd $ head $ last label_rects
        )
    mkrow y row =
        [ (key, Rect.xywh x y w h)
        | (x, (w, key)) <- zip xs row
        ]
        where xs = scanl ((+) . (+gap)) gap (map fst row)
    ys = iterate (+ (h+gap)) gap
    w = 20
    h = 20
    h2 = h `div` 2
    gap = 5


make_layout :: Map Key.Key Rect.Rect -> KeycapsT.Layout
make_layout labels = KeycapsT.Layout
    { lt_size = (w, h)
    , lt_bg_color = Color.white
    , lt_keycap_color = Color.gray8
    , lt_highlight_color = Color.rgb 0.85 0.85 1
    , lt_label_color = Color.gray5
    , lt_binding_color = Color.black
    , lt_labels = Map.mapKeys Key.to_mac_label labels
    }
    where
    w = 5 + maximum (0 : map Rect.r (Map.elems labels))
    h = 5 + maximum (0 : map Rect.b (Map.elems labels))

resolve_bindings :: KeycapsT.Layout -> KeycapsT.Bindings -> KeycapsT.RawBindings
resolve_bindings layout bindings =
    KeycapsT.RawBindings $ map make $ Map.toAscList $ KeycapsT.lt_labels layout
    where
    make (label, rect) =
        KeycapsT.RawBinding (second (+offset) (Rect.upper_left rect)) $
            Map.findWithDefault KeycapsT.no_binding label bindings
        where
        offset
            | label `elem` map Key.to_mac_label arrows = 5
            | otherwise = 15
    arrows = [Key.Left, Key.Up, Key.Down, Key.Right]
