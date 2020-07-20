-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Cmd.Repl.LKeycaps where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Cmd.Keymap as Keymap

import qualified Local.KeyLayout
import qualified Ui.Color as Color
import qualified Ui.Key as Key
import qualified Ui.KeycapsT as KeycapsT

import           Global


mac_labels :: Map Key.Key Rect.Rect
mac_labels = Map.fromList $ concat label_rects ++ arrows
    where
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
    label_rects = zipWith mkrow ys qwerty_rows
    mkrow y row =
        [ (key, Rect.xywh x y w h)
        | (x, (w, key)) <- zip xs row
        ]
        where xs = scanl ((+) . (+gap)) gap (map fst row)
    ys = iterate (+ (h+gap)) gap
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
        where
        doc = Map.lookup label label_to_doc
        offset
            | label `elem` map Key.to_mac_label arrows = 7
            | otherwise = 15
    arrows = [Key.Left, Key.Up, Key.Down, Key.Right]

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

set_global :: Cmd.M m => m ()
set_global = do
    let layout = make_layout mac_labels
    update Nothing $ make_bindings layout $ Map.fromList
        [ (key, (abbreviate doc, doc))
        | (key, doc) <- cmds
        ]
    where
cmds =
    map (bimap to_logical (\(Keymap.CmdSpec doc _) -> doc)) $
    Seq.map_maybe_fst key_spec_label $
    Map.toList GlobalKeymap.all_cmd_map

to_logical :: Text -> Text
to_logical label = case Text.unpack label of
    [c] | Just c2 <- KeyLayouts.to_qwerty Local.KeyLayout.layout c ->
        Text.singleton c2
    _ -> label

abbreviate :: Text -> Text
abbreviate = Text.pack . map Text.head . take 3 . Text.words

key_spec_label :: Keymap.KeySpec -> Maybe Text
key_spec_label (Keymap.KeySpec mods bindable) | Set.null mods = case bindable of
    Keymap.Key _repeat key -> Just $ Key.to_mac_label key
    _ -> Nothing
key_spec_label _ = Nothing

update :: Cmd.M m => Maybe ((Int, Int), KeycapsT.Layout) -> [KeycapsT.Binding]
    -> m ()
update layout bindings = Cmd.modify $ \st -> st
    { Cmd.state_keycaps = Just $
        Cmd.KeycapsUpdate layout (KeycapsT.Bindings bindings)
    }

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
