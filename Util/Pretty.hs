-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Like Show, but designed to be easy to read rather than unambiguous and
    complete.
-}
module Util.Pretty (
    module Util.Format4
    , Pretty(..), pretty
    , formatted, formatteds, pprint
    , char

    -- * formatting
    , textList, formattedList, delimitedList, record, recordTitle
    , constructor
    -- * misc
    , showFloat, showFloat0, readWord
) where
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Monoid (mempty, mconcat, (<>))
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.RealFloat as Builder.RealFloat
import qualified Data.Time as Time
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Word as Word

import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.Format4 as Format
import Util.Format4 (Doc, (</>), (<//>), (<+/>), (<+>), text, render, indented)
import qualified Util.Seq as Seq


defaultWidth :: Int
defaultWidth = 75

-- | Format values in an eye-pleasing way.  Unlike Show, this isn't intended
-- to produce any kind of valid syntax, or even preserve information.
class Pretty a where
    {-# MINIMAL prettyt | format #-}
    prettyt :: a -> Text
    prettyt = Lazy.toStrict . Format.renderFlat . format
    format :: a -> Doc
    format = text . prettyt
    formatList :: [a] -> Doc
    formatList = formattedList '[' ']'

pretty :: Pretty a => a -> String
pretty = Text.unpack . prettyt

-- | Render a Pretty value to the default width.
formatted :: Pretty a => a -> Text
formatted = Lazy.toStrict . render "    " defaultWidth . format

formatteds :: Pretty a => a -> String
formatteds = Text.unpack . formatted

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

instance Pretty (a -> b) where prettyt _ = "<function>"
instance Pretty () where prettyt () = "()"
instance Pretty Bool where prettyt = showt
instance Pretty Int where prettyt = showt
instance Pretty Integer where prettyt = showt
instance Pretty Word.Word8 where prettyt = showt
instance Pretty Word.Word16 where prettyt = showt
instance Pretty Word.Word32 where prettyt = showt
instance Pretty Word.Word64 where prettyt = showt
instance Pretty Double where prettyt = showFloat 3
instance Pretty Float where prettyt = showFloat 3

instance (Integral a, Pretty a) => Pretty (Ratio.Ratio a) where
    prettyt r = prettyt (Ratio.numerator r) <> "/" <> prettyt (Ratio.denominator r)

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

instance Pretty Time.UTCTime where prettyt = showt

-- ** containers

instance Pretty a => Pretty (Set.Set a) where
    format = formattedList '{' '}' . Set.toList

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    format = formattedList '{' '}' . map pair . Map.toList
        where pair (k, v) = format k </> indented (":" <+> format v)

instance Pretty a => Pretty (Tree.Tree a) where
    format (Tree.Node val children) =
        "Node" <+/> indented ("(" <> format val <> ")" <+/> format children)

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
textList = formattedList '[' ']' . map text

formattedList :: Pretty a => Char -> Char -> [a] -> Doc
formattedList left right = delimitedList False left right . map format

record :: Doc -> [(Text, Doc)] -> Doc
record title fields =
    title <+/> indented (delimitedList True '{' '}' (map field fields))
    where field (name, val) = text name <+> "=" <+/> indented val

recordTitle :: Text -> [(Text, Doc)] -> Doc
recordTitle = record . text

constructor :: Text -> [Doc] -> Doc
constructor name [] = text name
constructor name fields =
    text name <+/> indented (wrapWords $ map (surround '(' ')') fields)
    where surround left right x = char left <> x <> char right
    -- TODO only surround ()s if it has spaces in it

delimitedList :: Bool -> Char -> Char -> [Doc] -> Doc
delimitedList spacedDelimiter leftc rightc xs = case xs of
    [] -> left <-> right
    [x] -> left <-> x <-> right
    x : xs -> Format.shortForm
        (left <-> x <> mconcat (map (","<+>) xs) <-> right)
        ((left <+> x) </> wrap (map (","<+>) xs) <> "\n" <> right <> "\n")
    where
    (<->) = if spacedDelimiter then (<+>) else (<>)
    left = text $ Text.singleton leftc
    right = text $ Text.singleton rightc

wrapWords :: [Doc] -> Doc
wrapWords = List.foldl' (<+/>) mempty

wrap :: [Doc] -> Doc
wrap = List.foldl' (</>) mempty

-- * misc

-- | Display a float with the given precision, dropping trailing and leading
-- zeros.  Haskell requires a 0 before the decimal point, so this produces
-- non-Haskell numbers.
showFloat :: RealFloat a => Int -> a -> Text
showFloat precision = drop0 . showFloat0 precision
    where
    drop0 t
        | t == "0" = "0"
        | Just rest <- Text.stripPrefix "-0." t = "-." <> rest
        | Just rest <- Text.stripPrefix "0." t = Text.cons '.' rest
        | otherwise = t

-- | Like 'showFloat', but use a leading 0, so haskell can parse it.
showFloat0 :: RealFloat a => Int -> a -> Text
showFloat0 precision =
    clean . Lazy.toStrict . Builder.toLazyText
    . Builder.RealFloat.formatRealFloat Builder.RealFloat.Fixed
        (Just precision)
    where
    clean
        | precision > 0 = Text.dropWhileEnd (=='.') . Text.dropWhileEnd (=='0')
        | otherwise = id

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
