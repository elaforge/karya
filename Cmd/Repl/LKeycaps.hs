-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Cmd.Repl.LKeycaps where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Rect as Rect
import qualified Cmd.Cmd as Cmd
import qualified Ui.Color as Color
import qualified Ui.KeycapsT as KeycapsT

import           Global


mac_labels :: Map Text Rect.Rect
mac_labels = Map.fromList $ concat label_rects ++ arrows
    where
    arrows =
        [ ("←", Rect.xywh (xn 0) y2 w h2)
        , ("↑", Rect.xywh (xn 1) ly w h2)
        , ("↓", Rect.xywh (xn 1) y2 w h2)
        , ("→", Rect.xywh (xn 2) y2 w h2)
        ]
        where
        xn n = lx + gap + (w+gap) * n
        y2 = ly + h2
    (lx, ly) =
        ( maximum $ map (Rect.r . snd) (last label_rects)
        , Rect.y $ snd $ head $ last label_rects
        )
    label_rects = zipWith mkrow ys qwerty_rows
    mkrow y row =
        [ (label, Rect.xywh x y w h)
        | (x, (w, label)) <- zip xs row
        ]
        where xs = scanl ((+) . (+gap)) gap (map fst row)
    ys = iterate (+ (h+gap)) gap
    qwerty_rows =
        [ cs "`1234567890-=" ++ [(30, "del")]
        , (30, "tab") : cs "qwertyuiop[]\\"
        , (37, "caps") : cs "asdfghjkl;'" ++ [(37, "ret")]
        , (50, "shiftl") : cs "zxcvbnm,./" ++ [(49, "shiftr")]
        , map (w,) ["fn", "ctl", "optl"]
            ++ [(26, "cmdl"), (115, "space"), (26, "cmdr"), (w, "optr")]
        ]
        where cs = map ((w,) . Text.singleton)
    w = 20
    h = 20
    h2 = h `div` 2
    gap = 5

make_bindings :: KeycapsT.Layout -> Map Text (Text, Text) -> [KeycapsT.Binding]
make_bindings layout label_to_doc =
    map make (Map.toAscList (KeycapsT.lt_labels layout))
    where
    make (label, rect) = KeycapsT.Binding
        { b_point = second (+offset) (Rect.upper_left rect)
        , b_text = maybe "" fst doc
        , b_doc = maybe "" snd doc
        }
        where doc = Map.lookup label label_to_doc
    offset = 15

t0 :: Cmd.M m => m ()
t0 = do
    let layout = make_layout mac_labels
    update (Just ((200, 200), layout)) []

set_opts :: Cmd.M m => m ()
set_opts = do
    let layout = make_layout mac_labels
    update Nothing $ make_bindings layout $ Map.fromList
        [ ("q", ("4c", "pitch 4c"))
        , ("j", ("msa", "move selection advance"))
        , ("k", ("msr", "move selection rewind"))
        ]

update :: Cmd.M m => Maybe ((Int, Int), KeycapsT.Layout) -> [KeycapsT.Binding]
    -> m ()
update layout bindings = Cmd.modify $ \st -> st
    { Cmd.state_keycaps = Just $
        Cmd.KeycapsUpdate layout (KeycapsT.Bindings bindings)
    }

make_layout :: Map Text Rect.Rect -> KeycapsT.Layout
make_layout labels = KeycapsT.Layout
    { lt_size = (w, h)
    , lt_bg_color = Color.white
    , lt_keycap_color = Color.gray8
    , lt_highlight_color = Color.rgb 0.85 0.85 1
    , lt_label_color = Color.gray5
    , lt_binding_color = Color.black
    , lt_labels = labels
    }
    where
    w = 5 + maximum (0 : map Rect.r (Map.elems labels))
    h = 5 + maximum (0 : map Rect.b (Map.elems labels))
