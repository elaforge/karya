-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.ExtractDoc where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text.IO

import qualified System.Environment as Environment
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Key as Key
import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Keymap as Keymap
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap

import qualified Derive.Call.All as Call.All
import qualified Derive.Scale.All as Scale.All


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        ["keymap"] -> putStr keymap_doc
        ["calls"] -> do
            hstate <- CallDoc.get_html_state "../haddock" "."
            Text.IO.putStr $ CallDoc.doc_html hstate $ CallDoc.all_sections
                Call.All.scopes
        ["scales"] -> do
            hstate <- CallDoc.get_html_state "../haddock" "."
            Text.IO.putStr $ CallDoc.scales_html hstate $
                CallDoc.scale_docs $ Map.elems Scale.All.scales
        _ -> error $ "usage: extract_doc [ keymap | calls | scales ]"

-- * extract keymap

keymap_doc :: String
keymap_doc = unlines
    [ "<html> <head> <title> keymaps </title> </head> <body>"
    , html_fmt "global" $ extract GlobalKeymap.all_cmd_map
    , html_fmt "note track" $ extract $ fst $ NoteTrackKeymap.make_keymap
    , "</body> </html>"
    ]

type CmdMap = Keymap.CmdMap (Cmd.CmdT Identity.Identity)
type Binds = [(String, [Keymap.KeySpec])]

extract :: CmdMap -> Binds
extract = sort . strip . group . Map.toList

sort :: Binds -> Binds
sort = Seq.sort_on (map key . snd)
    where
    key (Keymap.KeySpec mods bindable) = (bindable_key bindable, mods)
    bindable_key k@(Keymap.Key _ (Key.Char c)) =
        (Map.findWithDefault (Map.size key_order + 1) c key_order, k)
    bindable_key k = (Map.size key_order + 2, k)
    key_order = Map.fromList $
        zip Keymap.qwerty_lower [0,2..] ++ zip Keymap.qwerty_upper [1,3..]

group :: [(Keymap.KeySpec, Keymap.CmdSpec m)] -> [(String, [Keymap.KeySpec])]
group = map (second (map fst)) . Seq.keyed_group_on (name_of . snd)
    where name_of (Keymap.CmdSpec name _) = name

strip :: Binds -> Binds
strip = map (second strip_keyspecs)

-- | A repeatable key implies the non-repeating key.  Also, a drag implies
-- a click.
strip_keyspecs :: [Keymap.KeySpec] -> [Keymap.KeySpec]
strip_keyspecs = map stripm . strip_drag . strip_repeatable
    where
    strip_drag mods
        | any is_drag mods = filter is_drag mods
        | otherwise = mods
    is_drag (Keymap.KeySpec _ (Keymap.Drag {})) = True
    is_drag _ = False
    strip_repeatable mods
        | any is_repeatable mods = filter is_repeatable mods
        | otherwise = mods
    is_repeatable (Keymap.KeySpec _ (Keymap.Key is_repeat _)) = is_repeat
    is_repeatable _ = False
    stripm (Keymap.KeySpec mods bindable) =
        Keymap.KeySpec (Set.fromList (strip_mods bindable (Set.toList mods)))
            bindable

-- | Strip out redundant modifiers.  E.g. Click and Drag bindings by necessity
-- imply that the mouse button is down, but I don't need to print that out.
strip_mods :: Keymap.Bindable -> [Cmd.Modifier] -> [Cmd.Modifier]
strip_mods bindable mods = case bindable of
    Keymap.Click {} -> stripped
    Keymap.Drag {} -> stripped
    _ -> mods
    where
    stripped = filter (not . is_mouse) mods
    is_mouse (Cmd.MouseMod {}) = True
    is_mouse _ = False

-- * txt fmt

txt_fmt :: Binds -> String
txt_fmt = Seq.join "\n" . map (uncurry show_binding)

show_binding :: String -> [Keymap.KeySpec] -> String
show_binding name keyspecs = Seq.join2 " - " mods name
    where mods = "[" ++ Seq.join ", " (map Pretty.pretty keyspecs) ++ "]"

-- * html fmt

html_fmt :: String -> Binds -> String
html_fmt title binds = columns title 3 (map (uncurry html_binding) binds)

columns :: String -> Int -> [String] -> String
columns title n contents = unlines $
    [ "<table width=100%>"
    , Printf.printf "<tr> <th colspan=%d> %s </th> </tr>" n title
    , "<tr>"
    ] ++ ["<td valign=top>" ++ t ++ "</td>" | t <- tables]
    ++ ["</tr>", "</table>"]
    where
    size = fromIntegral (length contents) / fromIntegral n
    tables = map fmt_table (chunk (ceiling size) contents)
    fmt_table rows = unlines $ ["<table>"] ++ fmt_rows rows ++ ["</table>"]
    fmt_rows rows =
        [ Printf.printf "<tr bgcolor=%s> %s </tr>" (color :: String)
            (row :: String)
        | (color, row) <- zip (cycle ["white", "#dddddd"]) rows
        ]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = c : chunk n rest
    where (c, rest) = splitAt n xs

html_binding :: String -> [Keymap.KeySpec] -> String
html_binding name keyspecs =
    "<td>" ++ mods ++ "</td> <td> <em>" ++ name ++ "</em> </td>"
    where
    mods = Seq.join ", " (map html_keyspec keyspecs)

html_keyspec :: Keymap.KeySpec -> String
html_keyspec (Keymap.KeySpec mods bindable) =
    Seq.join2 " " (show_mods mods)
        ("<b>" ++ Keymap.show_bindable False bindable ++ "</b>")
    where show_mods = Seq.join " + " . map Keymap.show_mod . Set.toList


-- * test

-- test = putStrLn (txt_fmt (extract test_binds))
--
-- test_binds :: CmdMap
-- test_binds = fst $ Keymap.make_cmd_map $ concat
--     [ Keymap.bind_repeatable [Keymap.PrimaryCommand] Key.Down "repeatable"
--         (return ())
--     , Keymap.bind_drag [] 1 "drag" (const (return ()))
--     ]
