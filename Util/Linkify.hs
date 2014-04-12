-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Turn single quoted strings into links to haddock docs, depending on
    whether the module can be found or not.

    E.g., @\'Derive.Score.Event\'@ becomes
    @[Derive.Score.Event](build/doc/Derive-Score.html#t:Event)@
    and @\'Derive.Score\'@ becomes
    @[Derive.Score](build/doc/Derive-Score.html)@.
-}
module Util.Linkify (main) where
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Environment as Environment

import Util.Control
import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [haddock_dir] ->
            Text.IO.putStr =<< linkify haddock_dir =<< Text.IO.getContents
        _ -> putStrLn "usage: linkify path/to/haddock/dir <doc/text"

linkify :: FilePath -> Text -> IO Text
linkify haddock_dir text = do
    files <- get_files "."
    return $ TextUtil.mapDelimited False '\''
        (link_quoted files haddock_dir) text

get_files :: FilePath -> IO TextUtil.Files
get_files dir = do
    files <- File.listRecursive (maybe False Char.isUpper . Seq.head) dir
    return $ Set.fromList files

link_quoted :: TextUtil.Files -> FilePath -> Text -> Text
link_quoted files haddock_dir text =
    case TextUtil.haddockUrl files haddock_dir text of
        Nothing -> "'" <> text <> "'"
        Just url -> markdown_link text url

markdown_link :: Text -> String -> Text
markdown_link text url = "[" <> text <> "](" <> Text.pack url <> ")"
