{- | Turn single quoted strings into links to haddock docs, depending on
    whether the module can be found or not.

    E.g., @\'Derive.Score.Event\'@ becomes
    @[Derive.Score.Event](build/doc/Derive-Score.html#t:Event)@
    and @\'Derive.Score\'@ becomes
    @[Derive.Score](build/doc/Derive-Score.html)@.
-}
module Util.Linkify where
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Seq as Seq


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [haddock_dir] ->
            Text.IO.putStr =<< process haddock_dir =<< Text.IO.getContents
        _ -> putStrLn $ "usage: linkify path/to/haddock/dir <doc/text"

process :: FilePath -> Text -> IO Text
process haddock_dir = map_words $ \word -> case extract_quoted word of
    Just (pre, q, post) -> (pre<>) . (<>post) <$> link_quoted haddock_dir q
    Nothing -> return word

map_words :: (Monad m) => (Text -> m Text) -> Text -> m Text
map_words f = liftM Text.concat . go
    where
    go text
        | Text.null word = return [leading]
        | otherwise = do
            w <- f word
            ws <- go rest
            return $ leading : w : ws
        where
        (leading, post) = Text.break (not . Char.isSpace) text
        (word, rest) = Text.break Char.isSpace post

extract_quoted :: Text -> Maybe (Text, Text, Text)
extract_quoted text = case Text.split (=='\'') text of
    [pre, q, post] -> Just (pre, q, post)
    _ -> Nothing

link_quoted :: FilePath -> Text -> IO Text
link_quoted haddock_dir quoted = do
    maybe_url <- linkify haddock_dir quoted
    return $ case maybe_url of
        Nothing -> "'" <> quoted <> "'"
        Just url -> markdown_link quoted url

markdown_link :: Text -> String -> Text
markdown_link text url = "[" <> text <> "](" <> Text.pack url <> ")"

linkify :: FilePath -> Text -> IO (Maybe String)
linkify haddock_dir text =
    ifM (module_exists components) (return $ Just $ module_link components) $
    ifM (module_exists (Seq.rdrop 1 components)) (return symbol_link) $
        return Nothing
    where
    module_exists path = orM $ map Directory.doesFileExist
        [FilePath.joinPath path ++ ".hs", FilePath.joinPath path ++ ".hsc"]
    symbol_link = case Seq.viewr components of
        (mod, Just sym) -> Just $ module_link mod ++ symbol_anchor sym
        _ -> Nothing
    module_link path = haddock_dir </> Seq.join "-" path ++ ".html"
    components = Seq.split "." (Text.unpack text)

symbol_anchor :: String -> String
symbol_anchor sym = case sym of
    c : _ | Char.isUpper c -> "#t:" ++ sym
    _ -> "#v:" ++ sym
