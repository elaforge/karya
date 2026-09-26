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


macLabels :: Map Key.Key Rect.Rect
macLabels = Map.fromList $ concat labelRects ++ arrows
    where
    labelRects = zipWith mkrow ys qwertyRows
    qwertyRows =
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
        ( maximum $ map (Rect.r . snd) (last labelRects)
        , Rect.y $ snd $ head $ last labelRects
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

makeLayout :: Map Key.Key Rect.Rect -> KeycapsT.Layout
makeLayout labels = KeycapsT.Layout
    { ltSize = (w, h)
    , ltBgColor = Color.white
    , ltKeycapColor = Color.gray8
    , ltHighlightColor = Color.rgb 0.85 0.85 1
    , ltLabelColor = Color.gray5
    , ltBindingColor = Color.black
    , ltLabels = Map.mapKeys Key.toMacLabel labels
    }
    where
    w = 5 + maximum (0 : map Rect.r (Map.elems labels))
    h = 5 + maximum (0 : map Rect.b (Map.elems labels))

resolveBindings :: KeycapsT.Layout -> KeycapsT.Bindings -> KeycapsT.RawBindings
resolveBindings layout bindings =
    KeycapsT.RawBindings $ map make $ Map.toAscList $ KeycapsT.ltLabels layout
    where
    make (label, rect) =
        KeycapsT.RawBinding (second (+offset) (Rect.upperLeft rect)) $
            Map.findWithDefault KeycapsT.noBinding label bindings
        where
        offset
            | label `elem` map Key.toMacLabel arrows = 5
            | otherwise = 15
    arrows = [Key.Left, Key.Up, Key.Down, Key.Right]
