-- | Utilities for the 'Doc' type.
module Util.Doc (
    Doc(..), pretty, literal, commas
    -- * HTML
    , Html(..), html, un_html
    -- ** create
    , tag, tag_class, link, tag_attrs
    , html_doc
    , HtmlState, get_html_state
) where
import qualified Data.Char as Char
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Util.File as File
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Util.TextUtil as TextUtil


-- | This is for documentation text.  It can contain some simple markdown-like
-- formatting, which may be either be printed directly, or formatted via
-- 'html_doc'.
newtype Doc = Doc Text
    deriving (Eq, Ord, Show, Pretty.Pretty, Monoid, String.IsString,
        Serialize.Serialize)

instance TextUtil.Textlike Doc where
    toText (Doc t) = t
    fromText = Doc

-- | This probably doesn't belong here, but it's useful in the same contexts as
-- 'doc'.
pretty :: Pretty.Pretty a => a -> Doc
pretty = literal . Pretty.pretty

literal :: Text -> Doc
literal text = Doc $ "`" <> text <> "`"

commas :: [Doc] -> Doc
commas = TextUtil.join ", "

-- * HTML

newtype Html = Html Text
    deriving (Monoid, String.IsString, Show, Eq, Ord)
    -- TODO doesn't IsString defeat the purpose of using Html in the first
    -- place?

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
tag_attrs name attrs maybe_content =
    "<" <> html name <> attrs_text <> ">" <> content_text
    where
    content_text = case maybe_content of
        Nothing -> ""
        Just content -> content <> "</" <> html name <> ">"
    attrs_text
        | null attrs = ""
        | otherwise = (" "<>) $ Seq.join " "
            [html name <> "=\"" <> html val <> "\"" | (name, val) <- attrs]

-- | Format a Doc to HTML.  Interpret simple markdown-like formatting:
-- single quotes for a reference to function or module haddock, backticks
-- for \<code\>, and newline for \<br\>.
--
-- TODO maybe support leading - for \<ol\>.
html_doc :: HtmlState -> Doc -> Html
html_doc (haddock_dir, files) (Doc doc) = Html . postproc . un_html . html $ doc
    where
    -- To keep the Text vs. Html type distinction I'd have to have [Either Text
    -- Html] and make mapDelimited return a list, and I couldn't use
    -- Text.replace.  It's doable, but would be more trouble than it's worth.
    postproc = para . backticks . single_quotes
    para = Text.replace "\n" "\n<br>"
    backticks = TextUtil.mapDelimited True '`'
        (\t -> "<code>" <> t <> "</code>")
    single_quotes = TextUtil.mapDelimited False '\'' $ \text ->
        case TextUtil.haddockUrl files haddock_dir text of
            Nothing -> "'" <> text <> "'"
            Just url -> un_html $ link text (Text.pack url)

-- | (haddock_dir, directory_tree)
type HtmlState = (FilePath, Set.Set FilePath)

get_html_state :: FilePath -> FilePath -> IO HtmlState
get_html_state haddock_dir app_dir = do
    files <- get_files app_dir
    -- The eventual output is in build/doc.
    return (haddock_dir, files)
    where
    get_files dir = do
        files <- File.listRecursive (maybe False Char.isUpper . Seq.head) dir
        return $ Set.fromList files
