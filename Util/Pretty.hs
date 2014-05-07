-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP, OverloadedStrings #-}
{- | Like Show, but designed to be easy to read rather than unambiguous and
    complete.
-}
module Util.Pretty (
    Pretty(..), Doc
    , prettyt
    , formatted, pprint, render, render_compact
    -- * re-exported
    , PP.char, PP.text, PP.fsep, PP.fcat, PP.nest
    , (PP.<+>)

    -- * formatting
    , format_commas, text_list, comma_list, record
    , constructor
    -- * misc
    , show_float, show_float0, read_word
) where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Time as Time
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Word as Word

import qualified Numeric
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<+>), Doc)
import qualified Text.Read as Read

import qualified Util.Seq as Seq
import qualified Util.Then as Then


default_width :: Int
default_width = 75

-- | Format values in an eye-pleasing way.  Unlike Show, this isn't intended
-- to produce any kind of valid syntax, or even preserve information.
class Pretty a where
    pretty :: a -> String
    pretty = render_compact . format
    format :: a -> Doc
    format = PP.text . pretty
    -- | Everyone's favorite list hack from show, so pretty on String is
    -- treated specially.  Don't implement this.
    format_list :: [a] -> Doc
    format_list = format_commas '[' ']'

prettyt :: Pretty a => a -> Text
prettyt = Text.pack . pretty

instance Pretty Doc where format = id
instance (Pretty a) => Pretty [a] where format = format_list
instance (Pretty a) => Pretty (NonEmpty.NonEmpty a) where
    format = format_list . NonEmpty.toList
instance Pretty Char where
    format = PP.quotes . PP.char
    format_list = PP.doubleQuotes . PP.text

instance Pretty Int where format = PP.int
instance Pretty Integer where format = PP.integer
instance Pretty Word.Word8 where pretty = show
instance Pretty Word.Word16 where pretty = show
instance Pretty Word.Word32 where pretty = show
instance Pretty Word.Word64 where pretty = show
instance Pretty Double where pretty = show_float 3
instance Pretty Float where pretty = show_float 3
instance Pretty Bool where pretty = show

instance Pretty a => Pretty (DList.DList a) where
    format = format . DList.toList

instance (Integral a, Pretty a) => Pretty (Ratio.Ratio a) where
    pretty r = pretty (Ratio.numerator r) <> "/" <> pretty (Ratio.denominator r)

instance Pretty (a -> b) where
    pretty _ = "<function>"

instance (Unboxed.Unbox a, Pretty a) => Pretty (Unboxed.Vector a) where
    format = format_commas '<' '>' . Unboxed.toList
instance (Pretty a) => Pretty (Vector.Vector a) where
    format = format_commas '<' '>' . Vector.toList
instance (Storable.Storable a, Pretty a) => Pretty (Storable.Vector a) where
    format = format_commas '<' '>' . Storable.toList

instance (Pretty a) => Pretty (Maybe a) where
    format Nothing = PP.text "Nothing"
    format (Just a) = format a

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    format (Left a) = PP.text "Left" <+> format a
    format (Right b) = PP.text "Right" <+> format b

instance (Pretty a) => Pretty (Set.Set a) where
    format = format_commas '{' '}' . Set.toList

instance Pretty () where pretty () = "()"
instance (Pretty a, Pretty b) => Pretty (a, b) where
    format (a, b) = comma_list Never '(' ')' [format a, format b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    format (a, b, c) = comma_list Never '(' ')' [format a, format b, format c]
instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
    format (a, b, c, d) = comma_list Never '(' ')'
        [format a, format b, format c, format d]
instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) =>
        Pretty (a, b, c, d, e) where
    format (a, b, c, d, e) = comma_list Never '(' ')'
        [format a, format b, format c, format d, format e]
instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) =>
        Pretty (a, b, c, d, e, f) where
    format (a, b, c, d, e, f) = comma_list Never '(' ')'
        [format a, format b, format c, format d, format e, format f]

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    format = format_commas '{' '}' . map mpair . Map.assocs
        where mpair (k, v) = PP.fcat [format k, PP.text ":" <+> format v]

instance Pretty ByteString.ByteString where
    format = PP.doubleQuotes . PP.text . UTF8.toString

instance Pretty Text where
    format = PP.doubleQuotes . PP.text . Text.unpack

instance Pretty Text.Lazy.Text where
    format = PP.doubleQuotes . PP.text . Text.Lazy.unpack

instance (Pretty a) => Pretty (Tree.Tree a) where
    format (Tree.Node val children) =
        "Node" <+> PP.fsep ["(" <> format val <> ")", format children]

instance (Pretty a, Pretty b) => Pretty (Seq.Paired a b) where
    format (Seq.First a) = "First" <+> format a
    format (Seq.Second b) = "Second" <+> format b
    -- format (Seq.Both a b) = "Both" <+> format a <+> format b
    format (Seq.Both a b) = record "Both"
        [ ("first", format a)
        , ("second", format b)
        ]

instance Pretty Time.UTCTime where pretty = show

formatted :: (Pretty a) => a -> String
formatted = (++"\n") . render default_width . format

pprint :: (Pretty a) => a -> IO ()
pprint = putStr . formatted

render :: Int -> Doc -> String
render width = PP.renderStyle (PP.Style PP.PageMode width 1)

render_compact :: Doc -> String
render_compact = unwrap . render 1000
    where unwrap = Seq.join " " . map (dropWhile (==' ')) . lines
    -- OneLineMode is tempting, but it turns the newlines into spaces.

format_commas :: (Pretty a) => Char -> Char -> [a] -> Doc
format_commas left right = comma_list Sometimes left right . map format

-- | A list of strings, but without quotes around them.
text_list :: [String] -> Doc
text_list = format_commas '[' ']' . map PP.text

data Spaces = Always | Sometimes | Never deriving (Show, Eq)

comma_list :: Spaces -> Char -> Char -> [Doc] -> Doc
comma_list spaces leftc rightc xs = case xs of
        [] -> left <> space <> right
        [x] -> left <> space <> x <> space <> right
        x : xs -> let inner = left `sep` x : map (PP.comma <+>) xs
            in fsep [PP.fcat inner, right]
    where
    space = if spaces == Always then PP.space else PP.empty
    sep = if spaces == Never then (<>) else (<+>)
    fsep = if spaces == Never then PP.fcat else PP.fsep
    left = PP.char leftc
    right = PP.char rightc

record :: Doc -> [(String, Doc)] -> Doc
record title fields =
    PP.fsep [title, PP.nest 2 (comma_list Always '{' '}' (map field fields))]
    where
    field (name, val) = fsep' [PP.text name, PP.equals, val]
    fsep' :: [Doc] -> Doc
    fsep' [] = PP.empty
    fsep' (d:ds) = PP.nest 2 (PP.fsep (PP.nest (-2) d:ds))

constructor :: String -> [Doc] -> Doc
constructor name [] = PP.text name
constructor name fields = PP.text name
    <+> PP.fsep (map (surround '(' ')') fields)

surround :: Char -> Char -> Doc -> Doc
surround left right x = PP.char left <> x <> PP.char right


-- * misc

-- | Display a float with the given precision, dropping trailing
-- and leading zeros.  Haskell requires a 0 before the decimal point, so
-- this produces unparseable strings.
show_float :: (RealFloat a) => Int -> a -> String
show_float precision = drop0 . show_float0 precision
    where
    drop0 "0" = "0"
    drop0 ('-':'0':'.':s) = '-':'.':s
    drop0 ('0':'.':s) = '.':s
    drop0 s = s

-- | Like 'show_float', but use a leading 0, so haskell can parse it.
show_float0 :: (RealFloat a) => Int -> a -> String
show_float0 precision f = clean $ Numeric.showFFloat Nothing f ""
    where
    clean = Seq.rdrop_while (=='.')
        . (Then.takeWhile (/='.') $ Then.take 1 $
            \rest -> Seq.rdrop_while (=='0') (take precision rest))

-- * Read

-- These don't really belong here, but this module has to do with reading and
-- showing, and as long as only have a few Read utilities I might as well put
-- them here.

-- | Read a space separated word.
read_word :: Read.ReadPrec String
read_word = Read.lift $ do
    ReadP.skipSpaces
    w <- ReadP.many1 (ReadP.satisfy (not . Char.isSpace))
    ReadP.skipSpaces
    return w
