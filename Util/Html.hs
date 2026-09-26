-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Html (
    Html(..), html, unHtml
    -- ** create
    , tag, tagClass, link, tagAttrs
    , htmlDoc
    , HtmlState, getHtmlState
) where
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import           Data.Text (Text)

import qualified Util.Doc as Doc
import qualified Util.Files as Files
import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts

import qualified App.Path as Path


newtype Html = Html Text
    deriving (Semigroup, Monoid, String.IsString, Pretty.Pretty, Show, Eq, Ord)
    -- TODO doesn't IsString defeat the purpose of using Html in the first
    -- place?

instance Texts.Textlike Html where
    toText (Html t) = t
    fromText = Html

html :: Text -> Html
html = Html . Text.replace "<" "&lt;" . Text.replace ">" "&gt;"
    . Text.replace "&" "&amp;"

unHtml :: Html -> Text
unHtml (Html text) = text

-- ** create

tag :: Text -> Html -> Html
tag name content = tagAttrs name [] (Just content)

tagClass :: Text -> Text -> Html -> Html
tagClass name class_ content =
    tagAttrs name [("class", class_)] (Just content)

link :: Text -> Text -> Html
link text url = tagAttrs "a" [("href", url)] (Just (html text))

tagAttrs :: Text -> [(Text, Text)] -> Maybe Html -> Html
tagAttrs name attrs mbContent = mconcat $
    "<" : html name : attrsText : ">"
    : (case mbContent of
        Nothing -> []
        Just content -> [content, "</", html name, ">"])
    where
    attrsText
        | null attrs = ""
        | otherwise = (" "<>) $ Lists.join " "
            [ html name
                <> if Text.null val then "" else "=\"" <> html val <> "\""
            | (name, val) <- attrs
            ]

-- | Format a Doc to HTML.  Interpret simple markdown-like formatting:
-- single quotes for a reference to function or module haddock, backticks
-- for \<code\>, and newline for \<br\>.
--
-- TODO maybe support leading - for \<ol\>.
htmlDoc :: HtmlState -> Doc.Doc -> Html
htmlDoc (haddockDir, files) (Doc.Doc doc) =
    Html . postproc . unHtml . html $ doc
    where
    -- To keep the Text vs. Html type distinction I'd have to have [Either Text
    -- Html] and make mapDelimited return a list, and I couldn't use
    -- Text.replace.  It's doable, but would be more trouble than it's worth.
    postproc = para . backticks . singleQuotes
    para = Text.replace "\n" "\n<br>"
    backticks = Texts.mapDelimited True '`'
        (\t -> "<code>" <> t <> "</code>")
    singleQuotes = Texts.mapDelimited False '\'' $ \text ->
        case Texts.haddockUrl files haddockDir text of
            Nothing -> "'" <> text <> "'"
            Just url -> unHtml $ link text (Text.pack url)

-- | (haddock_dir, directory_tree)
type HtmlState = (FilePath, Set.Set FilePath)

getHtmlState :: FilePath -> Path.AppDir -> IO HtmlState
getHtmlState haddockDir appDir = do
    files <- getFiles appDir
    -- The eventual output is in build/doc.
    pure (haddockDir, files)
    where
    getFiles (Path.AppDir dir) = do
        files <- Files.listRecursive (maybe False Char.isUpper . Lists.head) dir
        pure $ Set.fromList files
