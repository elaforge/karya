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

import qualified GHC.IO.Encoding as Encoding
import qualified System.Environment as Environment
import qualified System.IO as IO

import qualified Util.Files as Files
import qualified Util.Lists as Lists
import qualified Util.Logger as Logger
import qualified Util.Texts as Texts

import           Global


main :: IO ()
main = do
    Encoding.setLocaleEncoding Encoding.utf8
    args <- Environment.getArgs
    case args of
        [haddockDir, input] ->
            Text.IO.putStr =<< linkify haddockDir input
                =<< Text.IO.readFile input
        _ -> putStrLn "usage: linkify path/to/haddock/dir doc/text"

linkify :: FilePath -> FilePath -> Text -> IO Text
linkify haddockDir inputFile text = do
    files <- getFiles "."
    let (out, logs) = Logger.runId $ Texts.mapDelimitedM False '\''
            (linkQuoted files haddockDir) text
    unless (null logs) $
        Text.IO.hPutStrLn IO.stderr $ "** " <> txt inputFile
            <> ": broken link: " <> Text.intercalate ", " logs
    pure out

getFiles :: FilePath -> IO Texts.Files
getFiles dir = do
    files <- Files.listRecursive (maybe False Char.isUpper . Lists.head) dir
    pure $ Set.fromList files

linkQuoted :: Texts.Files -> FilePath -> Text -> Logger.Logger Text Text
linkQuoted files haddockDir text
    | looksLikeLink text = case Texts.haddockUrl files haddockDir text of
        Nothing -> do
            Logger.log text
            pure $ "'" <> text <> "'"
        Just url -> pure $ markdownLink text url
    | otherwise = pure $ "'" <> text <> "'"

looksLikeLink :: Text -> Bool
looksLikeLink text =
    "." `Text.isInfixOf` text && Char.isUpper (Text.head text)

markdownLink :: Text -> String -> Text
markdownLink text url = "[" <> text <> "](" <> Text.pack url <> ")"
