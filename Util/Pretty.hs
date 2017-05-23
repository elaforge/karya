-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
{- | Like Show, but designed to be easy to read rather than unambiguous and
    complete.
-}
module Util.Pretty (
    module Util.Format
    , Pretty, pretty, format, formatList
    , prettys
    , formatted, pprint
    , char

    -- * formatting
    , textList, formattedList, delimitedList, record, recordTitle
    , constructor
    -- * misc
    , readWord
) where
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Dynamic as Dynamic
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy
import qualified Data.Time as Time
import qualified Data.Time.Calendar as Calendar
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Word as Word

import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.Format as Format
import Util.Format
       (Doc, (</>), (<//>), (<+/>), (<+>), text, string, render, withIndent,
        indent_, indent, indentLine, wrapWords)
import qualified Util.Num as Num
import qualified Util.Seq as Seq


defaultWidth :: Int
defaultWidth = 75

-- | Format values in an eye-pleasing way.  Unlike Show, this isn't intended
-- to produce any kind of valid syntax, or even preserve information.
class Pretty a where
    {-# MINIMAL pretty | format #-}
    pretty :: a -> Text
    pretty = Lazy.toStrict . Format.renderFlat . format
    format :: a -> Doc
    format = text . pretty
    formatList :: [a] -> Doc
    formatList = formattedList '[' ']'

prettys :: Pretty a => a -> String
prettys = Text.unpack . pretty

-- | Render a Pretty value to the default width.
formatted :: Pretty a => a -> Text
formatted = Lazy.toStrict . render "    " defaultWidth . format

pprint :: Pretty a => a -> IO ()
pprint = Text.IO.putStr . formatted

instance Pretty Doc where format = id

showt :: Show a => a -> Text
showt = Text.pack . show

char :: Char -> Doc
char = text . Text.singleton

-- * standard types

instance Pretty a => Pretty [a] where format = formatList
instance Pretty Char where
    format c = text $ "'" <> Text.singleton c <> "'"
    formatList = format . Text.pack

instance Pretty (a -> b) where pretty _ = "<function>"
instance Pretty () where pretty () = "()"
instance Pretty Bool where pretty = showt
instance Pretty Int where pretty = showt
instance Pretty Integer where pretty = showt
instance Pretty Word.Word8 where pretty = showt
instance Pretty Word.Word16 where pretty = showt
instance Pretty Word.Word32 where pretty = showt
instance Pretty Word.Word64 where pretty = showt
instance Pretty Double where pretty = Num.showFloat 3
instance Pretty Float where pretty = Num.showFloat 3

instance (Integral a, Pretty a) => Pretty (Ratio.Ratio a) where
    pretty r
        | r == 0 = "0"
        | frac == 0 = pretty whole
        | whole == 0 = ratio frac
        | otherwise = pretty whole <> "+" <> ratio frac
        where
        whole :: Integer
        (whole, frac) = properFraction r
        ratio r =
            pretty (Ratio.numerator r) <> "/" <> pretty (Ratio.denominator r)

instance Pretty a => Pretty (Maybe a) where
    format Nothing = text "Nothing"
    format (Just a) = format a

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    format (Left a) = text "Left" <+> format a
    format (Right b) = text "Right" <+> format b

instance (Pretty a, Pretty b) => Pretty (a, b) where
    format (a, b) = formattedList '(' ')' [format a, format b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    format (a, b, c) = formattedList '(' ')' [format a, format b, format c]
instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
    format (a, b, c, d) = formattedList '(' ')'
        [format a, format b, format c, format d]
instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) =>
        Pretty (a, b, c, d, e) where
    format (a, b, c, d, e) = formattedList '(' ')'
        [format a, format b, format c, format d, format e]
instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) =>
        Pretty (a, b, c, d, e, f) where
    format (a, b, c, d, e, f) = formattedList '(' ')'
        [format a, format b, format c, format d, format e, format f]

instance Pretty Time.UTCTime where pretty = showt
instance Pretty Dynamic.Dynamic where pretty = showt
instance Pretty Calendar.Day where pretty = showt

-- ** containers

instance Pretty a => Pretty (Set.Set a) where
    format = formattedList '{' '}' . Set.toList

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    format = formattedList '{' '}' . map pair . Map.toList
        where pair (k, v) = format k </> (":" <+> format v)

instance Pretty v => Pretty (IntMap.IntMap v) where
    format = formattedList '{' '}' . map pair . IntMap.toList
        where pair (k, v) = format k </> (":" <+> format v)

instance Pretty a => Pretty (Tree.Tree a) where
    format (Tree.Node val children) =
        "Node" <> indent_ ("(" <> format val <> ")" <+/> format children)

-- ** text

instance Pretty ByteString.ByteString where
    format = format . Encoding.decodeUtf8

instance Pretty Text where
    format t = "\"" <> text t <> "\""

instance Pretty Lazy.Text where
    format = format . Lazy.toStrict

-- * hackage types

instance Pretty a => Pretty (NonEmpty.NonEmpty a) where
    format = formatList . NonEmpty.toList

instance (Unboxed.Unbox a, Pretty a) => Pretty (Unboxed.Vector a) where
    format = formattedList '<' '>' . Unboxed.toList
instance (Pretty a) => Pretty (Vector.Vector a) where
    format = formattedList '<' '>' . Vector.toList
instance (Storable.Storable a, Pretty a) => Pretty (Storable.Vector a) where
    format = formattedList '<' '>' . Storable.toList

-- * local types

instance (Pretty a, Pretty b) => Pretty (Seq.Paired a b) where
    format (Seq.First a) = "First" <+> format a
    format (Seq.Second b) = "Second" <+> format b
    format (Seq.Both a b) = "Both" <+> format a <+> format b


-- * formatters

-- | A list of strings, but without quotes around them.
textList :: [Text] -> Doc
textList = delimitedList False '[' ']' . map text

formattedList :: Pretty a => Char -> Char -> [a] -> Doc
formattedList left right = delimitedList False left right . map format

record :: Doc -> [(Text, Doc)] -> Doc
record title fields =
    title <> indent_ (delimitedList True '{' '}' (map field fields))
    where field (name, val) = text name <+> "=" <+/> val
    -- The "name = val" is already indented due to delimitedList, so if it
    -- wraps it will already be at one level of indentation.

recordTitle :: Text -> [(Text, Doc)] -> Doc
recordTitle = record . text

constructor :: Text -> [Doc] -> Doc
constructor name [] = text name
constructor name fields =
    text name <> indent_ (wrapWords $ map (surround '(' ')') fields)
    where surround left right x = char left <> x <> char right
    -- TODO only surround ()s if it has spaces in it

-- | Format a comma-separated list.  Try to put it on one line, but break
-- before commas if that's not possible.
delimitedList :: Bool -- ^ Always spaces around the delimiters.  Otherwise,
    -- they only get spaces if the list wraps.
    -> Char -> Char -> [Doc] -> Doc
delimitedList spacedDelimiter leftc rightc xs = case xs of
    [] -> left <-> right
    x : xs -> Format.shortForm
        (left <-> x <> mconcat (map (","<+>) xs) <-> right) $
        (left <+> withIndent x) </> Format.wrap (map element xs)
            <//> right <> "\n"
    where
    element x = "," <+> withIndent x
    (<->) = if spacedDelimiter then (<+>) else (<>)
    left = text $ Text.singleton leftc
    right = text $ Text.singleton rightc



-- * Read

-- These don't really belong here, but this module has to do with reading and
-- showing, and as long as only have a few Read utilities I might as well put
-- them here.

-- | Read a space separated word.
readWord :: Read.ReadPrec String
readWord = Read.lift $ do
    ReadP.skipSpaces
    w <- ReadP.many1 (ReadP.satisfy (not . Char.isSpace))
    ReadP.skipSpaces
    return w
