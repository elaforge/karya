-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Html (
    Html(..), html, un_html
    -- ** create
    , tag, tag_class, link, tag_attrs
    , html_doc
    , HtmlState, get_html_state
) where
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import           Data.Text (Text)

import qualified Util.Doc as Doc
import qualified Util.File as File
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
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

un_html :: Html -> Text
un_html (Html text) = text

-- ** create

tag :: Text -> Html -> Html
tag name content = tag_attrs name [] (Just content)

tag_class :: Text -> Text -> Html -> Html
tag_class name class_ content =
    tag_attrs name [("class", class_)] (Just content)

link :: Text -> Text -> Html
link text url = tag_attrs "a" [("href", url)] (Just (html text))

tag_attrs :: Text -> [(Text, Text)] -> Maybe Html -> Html
tag_attrs name attrs mb_content = mconcat $
    "<" : html name : attrs_text : ">"
    : (case mb_content of
        Nothing -> []
        Just content -> [content, "</", html name, ">"])
    where
    attrs_text
        | null attrs = ""
        | otherwise = (" "<>) $ Seq.join " "
            [ html name
                <> if Text.null val then "" else "=\"" <> html val <> "\""
            | (name, val) <- attrs
            ]

-- | Format a Doc to HTML.  Interpret simple markdown-like formatting:
-- single quotes for a reference to function or module haddock, backticks
-- for \<code\>, and newline for \<br\>.
--
-- TODO maybe support leading - for \<ol\>.
html_doc :: HtmlState -> Doc.Doc -> Html
html_doc (haddock_dir, files) (Doc.Doc doc) =
    Html . postproc . un_html . html $ doc
    where
    -- To keep the Text vs. Html type distinction I'd have to have [Either Text
    -- Html] and make mapDelimited return a list, and I couldn't use
    -- Text.replace.  It's doable, but would be more trouble than it's worth.
    postproc = para . backticks . single_quotes
    para = Text.replace "\n" "\n<br>"
    backticks = Texts.mapDelimited True '`'
        (\t -> "<code>" <> t <> "</code>")
    single_quotes = Texts.mapDelimited False '\'' $ \text ->
        case Texts.haddockUrl files haddock_dir text of
            Nothing -> "'" <> text <> "'"
            Just url -> un_html $ link text (Text.pack url)

-- | (haddock_dir, directory_tree)
type HtmlState = (FilePath, Set.Set FilePath)

get_html_state :: FilePath -> Path.AppDir -> IO HtmlState
get_html_state haddock_dir app_dir = do
    files <- get_files app_dir
    -- The eventual output is in build/doc.
    return (haddock_dir, files)
    where
    get_files (Path.AppDir dir) = do
        files <- File.listRecursive (maybe False Char.isUpper . Seq.head) dir
        return $ Set.fromList files
