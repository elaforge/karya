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
import qualified System.IO as IO

import qualified Util.File as File
import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Texts as Texts

import           Global


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [haddock_dir, input] ->
            Text.IO.putStr =<< linkify haddock_dir input
                =<< Text.IO.readFile input
        _ -> putStrLn "usage: linkify path/to/haddock/dir doc/text"

linkify :: FilePath -> FilePath -> Text -> IO Text
linkify haddock_dir input_file text = do
    files <- get_files "."
    let (out, logs) = Logger.runId $ Texts.mapDelimitedM False '\''
            (link_quoted files haddock_dir) text
    unless (null logs) $
        Text.IO.hPutStrLn IO.stderr $ "** " <> txt input_file
            <> ": broken link: " <> Text.intercalate ", " logs
    return out

get_files :: FilePath -> IO Texts.Files
get_files dir = do
    files <- File.listRecursive (maybe False Char.isUpper . Lists.head) dir
    return $ Set.fromList files

link_quoted :: Texts.Files -> FilePath -> Text -> Logger.Logger Text Text
link_quoted files haddock_dir text
    | looks_like_link text = case Texts.haddockUrl files haddock_dir text of
        Nothing -> do
            Logger.log text
            return $ "'" <> text <> "'"
        Just url -> return $ markdown_link text url
    | otherwise = return $ "'" <> text <> "'"

looks_like_link :: Text -> Bool
looks_like_link text =
    "." `Text.isInfixOf` text && Char.isUpper (Text.head text)

markdown_link :: Text -> String -> Text
markdown_link text url = "[" <> text <> "](" <> Text.pack url <> ")"
