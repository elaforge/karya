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
import qualified Util.Lists as Lists
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
    appDir <- Path.getAppDir
    case args of
        ["keymap"] -> Text.IO.putStr keymapDoc
        ["calls"] -> do
            hstate <- Html.getHtmlState "../haddock" appDir
            Text.IO.putStr $ Html.unHtml $ CallDoc.docHtml hstate $
                CallDoc.builtins C.All.builtins
        ["scales"] -> do
            hstate <- Html.getHtmlState "../haddock" appDir
            Text.IO.putStr $ Html.unHtml $ CallDoc.scalesHtml hstate $
                CallDoc.scaleDocs Scale.All.docs
        _ -> error "usage: extract_doc [ keymap | calls | scales ]"

-- * extract keymap

keymapDoc :: Text
keymapDoc = Text.unlines
    [ "<html> <head> <title> keymaps </title> </head> <body>"
    , htmlFmt "global" $ extract GlobalKeymap.allKeymap
    , htmlFmt "note track" $ extract NoteTrackKeymap.keymap
    , "</body> </html>"
    ]

type Binds = [(Text, [Cmd.KeySpec])]

extract :: Cmd.Keymap Cmd.CmdId -> Binds
extract = sort . strip . group . Map.toList

-- | Sort by the key's position in qwerty.
sort :: Binds -> Binds
sort = Lists.sortOn (map key . snd)
    where
    key (Cmd.KeySpec mods bindable) = (bindableKey bindable, mods)
    bindableKey k@(Cmd.Key _ (Key.Char c)) =
        (Map.findWithDefault (Map.size keyOrder + 1) c keyOrder, k)
    bindableKey k = (Map.size keyOrder + 2, k)
    keyOrder = Map.fromList $ zip KeyLayouts.qwertyUnshifted [0,2..]
        ++ zip KeyLayouts.qwertyShifted [1,3..]

group :: [(Cmd.KeySpec, Cmd.NamedCmd m)] -> [(Text, [Cmd.KeySpec])]
group = map (second (map fst)) . Lists.keyedGroupSort (Cmd.cmdName . snd)

strip :: Binds -> Binds
strip = map (second stripKeyspecs)

-- | A repeatable key implies the non-repeating key.  Also, a drag implies
-- a click.
stripKeyspecs :: [Cmd.KeySpec] -> [Cmd.KeySpec]
stripKeyspecs = map stripm . stripDrag . stripRepeatable
    where
    stripDrag mods
        | any isDrag mods = filter isDrag mods
        | otherwise = mods
    isDrag (Cmd.KeySpec _ (Cmd.Drag {})) = True
    isDrag _ = False
    stripRepeatable mods
        | any isRepeatable mods = filter isRepeatable mods
        | otherwise = mods
    isRepeatable (Cmd.KeySpec _ (Cmd.Key isRepeat _)) = isRepeat
    isRepeatable _ = False
    stripm (Cmd.KeySpec mods bindable) =
        Cmd.KeySpec (Set.fromList (stripMods bindable (Set.toList mods)))
            bindable

-- | Strip out redundant modifiers.  E.g. Click and Drag bindings by necessity
-- imply that the mouse button is down, but I don't need to print that out.
stripMods :: Cmd.Bindable -> [Cmd.Modifier] -> [Cmd.Modifier]
stripMods bindable mods = case bindable of
    Cmd.Click {} -> stripped
    Cmd.Drag {} -> stripped
    _ -> mods
    where
    stripped = filter (not . isMouse) mods
    isMouse (Cmd.MouseMod {}) = True
    isMouse _ = False

-- * txt fmt

txtFmt :: Binds -> Text
txtFmt = Text.unlines . map (uncurry showBinding)

showBinding :: Text -> [Cmd.KeySpec] -> Text
showBinding name keyspecs = Texts.join2 " - " mods name
    where mods = "[" <> Text.intercalate ", " (map pretty keyspecs) <> "]"

-- * html fmt

htmlFmt :: Text -> Binds -> Text
htmlFmt title binds = columns title 3 (map (uncurry htmlBinding) binds)

columns :: Text -> Int -> [Text] -> Text
columns title n contents = Text.unlines $
    [ "<table width=100%>"
    , txt $ Printf.printf "<tr> <th colspan=%d> %s </th> </tr>" n (untxt title)
    , "<tr>"
    ] ++ ["<td valign=top>" <> t <> "</td>" | t <- tables]
    ++ ["</tr>", "</table>"]
    where
    size = fromIntegral (length contents) / fromIntegral n
    tables = map fmtTable (chunk (ceiling size) contents)
    fmtTable rows = Text.unlines $ ["<table>"] ++ fmtRows rows ++ ["</table>"]
    fmtRows rows =
        [ txt $ Printf.printf "<tr bgcolor=%s> %s </tr>" (color :: String)
            (untxt row)
        | (color, row) <- zip (cycle ["white", "#dddddd"]) rows
        ]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = c : chunk n rest
    where (c, rest) = splitAt n xs

htmlBinding :: Text -> [Cmd.KeySpec] -> Text
htmlBinding name keyspecs =
    "<td>" <> mods <> "</td> <td> <em>" <> name <> "</em> </td>"
    where mods = Text.intercalate ", " (map htmlKeyspec keyspecs)

htmlKeyspec :: Cmd.KeySpec -> Text
htmlKeyspec (Cmd.KeySpec mods bindable) =
    Texts.join2 " " (showMods mods)
        ("<b>" <> Cmd.showBindable False bindable <> "</b>")
    where showMods = Text.intercalate " + " . map Cmd.showMod . Set.toList
