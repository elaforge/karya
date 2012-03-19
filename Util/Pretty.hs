{- | Like Show, but designed to be easy to read rather than unambiguous and
    complete.
-}
module Util.Pretty (
    Pretty(..)
    , record, sep_by
    , formatted, render, render_compact
    , module Text.PrettyPrint.ANSI.Leijen
    -- * misc
    , lines, show_float
    -- * Read
    , read_word
) where
import Prelude hiding (lines)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Word as Word

import qualified Numeric
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen
       (Doc, (<+>), (</>), text, brackets)
import qualified Text.Read as Read

import qualified Util.Seq as Seq
import qualified Util.Then as Then


width :: Int
width = 79

-- | Format values in an eye-pleasing way.  Unlike Show, this isn't intended
-- to produce any kind of valid syntax, or even preserve information.
class Show a => Pretty a where
    pretty :: a -> String
    pretty = render_compact . format
    format :: a -> Doc
    format = PP.text . pretty

    format_list :: [a] -> Doc
    format_list = PP.brackets . commas

instance Pretty Int where format = PP.int
instance Pretty Integer where format = PP.integer
instance Pretty Word.Word8 where pretty = show
instance Pretty Word.Word16 where pretty = show
instance Pretty Word.Word32 where pretty = show
instance Pretty Word.Word64 where pretty = show
instance Pretty Double where pretty = show_float 3
instance Pretty Float where pretty = show_float 3

instance Pretty Char where
    format = PP.char
    format_list = PP.dquotes . PP.string

instance Pretty ByteString.ByteString where
    format = format . UTF8.toString

formatted :: (Pretty a) => a -> String
formatted = render . format

render, render_compact :: Doc -> String
render doc = PP.displayS (PP.renderPretty 0.75 width doc) ""
render_compact doc = replace $ PP.displayS (PP.renderCompact doc) ""
    where replace = map $ \c -> if c == '\n' then ' ' else c

commas :: (Pretty a) => [a] -> Doc
commas = sep_by PP.comma

sep_by :: (Pretty a) => Doc -> [a] -> Doc
sep_by sep = PP.align . PP.fillSep . PP.punctuate sep . fmap format

record :: [(String, Doc)] -> Doc
record = (\doc -> PP.lbrace <+> doc <+> PP.rbrace) . commas
    . map (\(label, doc) -> PP.text label <+> PP.equals <+> doc)
    -- . map (\(label, doc) -> PP.nest 4 (PP.text label <+> PP.equals </> doc))
    -- TODO not sure how to do this
    -- if I put a </> after the equals and wrap in a 'nest', I can get breaks
    -- after long labels, but short labels look really ugly because it dosen't
    -- know to prefer not to.  Sincle labels are less likely to be long,
    -- I don't allow breaking after equals.

instance Pretty Doc where
    format = id

instance (Pretty a) => Pretty [a] where
    format = format_list

instance (Unboxed.Unbox a, Pretty a) => Pretty (Unboxed.Vector a) where
    format = PP.angles . commas . Unboxed.toList
instance (Pretty a) => Pretty (Vector.Vector a) where
    format = PP.angles . commas . Vector.toList

instance (Pretty a) => Pretty (Maybe a) where
    format Nothing = PP.text "Nothing"
    format (Just a) = format a

instance (Pretty a) => Pretty (Set.Set a) where
    format = PP.braces . commas . Set.toList

instance (Pretty a, Pretty b) => Pretty (a, b) where
    format (a, b) = PP.parens $ commas [format a, format b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    format (a, b, c) = PP.parens $ commas [format a, format b, format c]

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    format = PP.braces . commas . map format . Map.assocs


-- * misc

-- | Pretty up a list with a line for each element.
lines :: (Pretty a) => [a] -> String
lines = List.unlines . map pretty

-- | Display a float with the given precision, dropping trailing
-- and leading zeros.  Haskell requires a 0 before the decimal point, so
-- this produces unparseable strings.
show_float :: (RealFloat a) => Int -> a -> String
show_float precision f = clean $ Numeric.showFFloat Nothing f ""
    where
    clean = drop0 . Seq.rdrop_while (=='.')
        . (Then.takeWhile (/='.') $ Then.take 1 $
            \rest -> Seq.rdrop_while (=='0') (take precision rest))
    drop0 "0" = "0"
    drop0 ('-':'0':'.':s) = '-':'.':s
    drop0 ('0':'.':s) = '.':s
    drop0 s = s

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
