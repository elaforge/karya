-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.ExtractDoc where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Environment as Environment
import qualified Text.Printf as Printf

import qualified Util.Html as Html
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified App.Path as Path
import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap

import qualified Derive.C.All as C.All
import qualified Derive.Scale.All as Scale.All
import qualified Ui.Key as Key

import           Global


main :: IO ()
main = do
    args <- Environment.getArgs
    app_dir <- Path.get_app_dir
    case args of
        ["keymap"] -> Text.IO.putStr keymap_doc
        ["calls"] -> do
            hstate <- Html.get_html_state "../haddock" app_dir
            Text.IO.putStr $ Html.un_html $ CallDoc.doc_html hstate $
                CallDoc.builtins C.All.builtins
        ["scales"] -> do
            hstate <- Html.get_html_state "../haddock" app_dir
            Text.IO.putStr $ Html.un_html $ CallDoc.scales_html hstate $
                CallDoc.scale_docs Scale.All.docs
        _ -> error "usage: extract_doc [ keymap | calls | scales ]"

-- * extract keymap

keymap_doc :: Text
keymap_doc = Text.unlines
    [ "<html> <head> <title> keymaps </title> </head> <body>"
    , html_fmt "global" $ extract GlobalKeymap.all_keymap
    , html_fmt "note track" $ extract NoteTrackKeymap.keymap
    , "</body> </html>"
    ]

type Binds = [(Text, [Cmd.KeySpec])]

extract :: Cmd.Keymap Cmd.CmdId -> Binds
extract = sort . strip . group . Map.toList

-- | Sort by the key's position in qwerty.
sort :: Binds -> Binds
sort = Seq.sort_on (map key . snd)
    where
    key (Cmd.KeySpec mods bindable) = (bindable_key bindable, mods)
    bindable_key k@(Cmd.Key _ (Key.Char c)) =
        (Map.findWithDefault (Map.size key_order + 1) c key_order, k)
    bindable_key k = (Map.size key_order + 2, k)
    key_order = Map.fromList $ zip KeyLayouts.qwerty_unshifted [0,2..]
        ++ zip KeyLayouts.qwerty_shifted [1,3..]

group :: [(Cmd.KeySpec, Cmd.NamedCmd m)] -> [(Text, [Cmd.KeySpec])]
group = map (second (map fst)) . Seq.keyed_group_sort (Cmd.cmd_name . snd)

strip :: Binds -> Binds
strip = map (second strip_keyspecs)

-- | A repeatable key implies the non-repeating key.  Also, a drag implies
-- a click.
strip_keyspecs :: [Cmd.KeySpec] -> [Cmd.KeySpec]
strip_keyspecs = map stripm . strip_drag . strip_repeatable
    where
    strip_drag mods
        | any is_drag mods = filter is_drag mods
        | otherwise = mods
    is_drag (Cmd.KeySpec _ (Cmd.Drag {})) = True
    is_drag _ = False
    strip_repeatable mods
        | any is_repeatable mods = filter is_repeatable mods
        | otherwise = mods
    is_repeatable (Cmd.KeySpec _ (Cmd.Key is_repeat _)) = is_repeat
    is_repeatable _ = False
    stripm (Cmd.KeySpec mods bindable) =
        Cmd.KeySpec (Set.fromList (strip_mods bindable (Set.toList mods)))
            bindable

-- | Strip out redundant modifiers.  E.g. Click and Drag bindings by necessity
-- imply that the mouse button is down, but I don't need to print that out.
strip_mods :: Cmd.Bindable -> [Cmd.Modifier] -> [Cmd.Modifier]
strip_mods bindable mods = case bindable of
    Cmd.Click {} -> stripped
    Cmd.Drag {} -> stripped
    _ -> mods
    where
    stripped = filter (not . is_mouse) mods
    is_mouse (Cmd.MouseMod {}) = True
    is_mouse _ = False

-- * txt fmt

txt_fmt :: Binds -> Text
txt_fmt = Text.unlines . map (uncurry show_binding)

show_binding :: Text -> [Cmd.KeySpec] -> Text
show_binding name keyspecs = Texts.join2 " - " mods name
    where mods = "[" <> Text.intercalate ", " (map pretty keyspecs) <> "]"

-- * html fmt

html_fmt :: Text -> Binds -> Text
html_fmt title binds = columns title 3 (map (uncurry html_binding) binds)

columns :: Text -> Int -> [Text] -> Text
columns title n contents = Text.unlines $
    [ "<table width=100%>"
    , txt $ Printf.printf "<tr> <th colspan=%d> %s </th> </tr>" n (untxt title)
    , "<tr>"
    ] ++ ["<td valign=top>" <> t <> "</td>" | t <- tables]
    ++ ["</tr>", "</table>"]
    where
    size = fromIntegral (length contents) / fromIntegral n
    tables = map fmt_table (chunk (ceiling size) contents)
    fmt_table rows = Text.unlines $ ["<table>"] ++ fmt_rows rows ++ ["</table>"]
    fmt_rows rows =
        [ txt $ Printf.printf "<tr bgcolor=%s> %s </tr>" (color :: String)
            (untxt row)
        | (color, row) <- zip (cycle ["white", "#dddddd"]) rows
        ]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = c : chunk n rest
    where (c, rest) = splitAt n xs

html_binding :: Text -> [Cmd.KeySpec] -> Text
html_binding name keyspecs =
    "<td>" <> mods <> "</td> <td> <em>" <> name <> "</em> </td>"
    where mods = Text.intercalate ", " (map html_keyspec keyspecs)

html_keyspec :: Cmd.KeySpec -> Text
html_keyspec (Cmd.KeySpec mods bindable) =
    Texts.join2 " " (show_mods mods)
        ("<b>" <> Cmd.show_bindable False bindable <> "</b>")
    where show_mods = Text.intercalate " + " . map Cmd.show_mod . Set.toList
