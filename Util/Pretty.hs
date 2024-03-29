-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{- | Like Show, but designed to be easy to read rather than unambiguous and
    complete.
-}
module Util.Pretty (
    module Util.Format
    , Pretty, pretty, format, formatList
    , prettys
    , formatted, pprint
    , char
    , improperRatio, fraction

    -- * generic derivation
    , formatG, formatG_, formatGCamel, formatGPrefix

    -- * formatting
    , textList, formattedList, delimitedList, record, recordTitle
    , formatMap
    , constructor
    -- * standalone
    , duration
    , bytes
    -- * misc
    , readWord
) where
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Dynamic as Dynamic
import qualified Data.Int as Int
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Text (Text)
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

import qualified Foreign
import qualified Foreign.C as C
import qualified GHC.Generics as Generics
import           GHC.Generics ((:*:)((:*:)))
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.Format as Format
import           Util.Format
    (Doc, indent, indentLine, indent_, render, string, text, withIndent,
     wrapWords, (<+/>), (<+>), (<//>), (</>))
import qualified Util.Lists as Lists
import qualified Util.Num as Num


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


data GConfig = GConfig {
    _modifyField :: String -> String
    }

gconfig :: GConfig
gconfig = GConfig id

dropPrefix :: String -> GConfig
dropPrefix prefix = gconfig
    { _modifyField = \field ->
        Maybe.fromMaybe field (List.stripPrefix prefix field)
    }

dropCamel :: GConfig
dropCamel = gconfig
    { _modifyField = lower . dropWhile Char.isDigit . dropWhile Char.isLower }
    where
    lower (c:cs) = Char.toLower c : cs
    lower [] = "<no-capital>"

dropUnderscore :: GConfig
dropUnderscore =
    gconfig { _modifyField = dropWhile (=='_') . dropWhile (/='_') }

class PrettyG f where
    prettyG :: GConfig -> f a -> [(Text, Doc)]

formatG :: (PrettyG (Generics.Rep a), Generics.Generic a) => a -> Doc
formatG = formatGWith gconfig

formatG_ :: (PrettyG (Generics.Rep a), Generics.Generic a) => a -> Doc
formatG_ = formatGWith dropUnderscore

formatGCamel :: (PrettyG (Generics.Rep a), Generics.Generic a) => a -> Doc
formatGCamel = formatGWith dropCamel

formatGPrefix :: (PrettyG (Generics.Rep a), Generics.Generic a)
    => String -> a -> Doc
formatGPrefix prefix = formatGWith (dropPrefix prefix)

-- | Generic derivation for Pretty.  This works on single-constructor types,
-- records and positional.
formatGWith :: (PrettyG (Generics.Rep a), Generics.Generic a) => GConfig -> a
    -> Doc
formatGWith config a = case prettyG config (Generics.from a) of
    [("", doc)] -> doc
    -- TODO what causes this?
    fields -> record "??" fields

instance (PrettyG f) => PrettyG (Generics.M1 Generics.D d f) where
    prettyG config (Generics.M1 x) = prettyG config x

instance (PrettyG f, Generics.Constructor c) =>
        PrettyG (Generics.M1 Generics.C c f) where
    prettyG config c@(Generics.M1 x)
        | Generics.conIsRecord c =
            [("", record (text (Text.pack name)) (prettyG config x))]
        | otherwise =
            [("", constructor (Text.pack name) (map snd (prettyG config x)))]
        where name = Generics.conName c

instance (PrettyG f, Generics.Selector s) =>
        PrettyG (Generics.M1 Generics.S s f) where
    prettyG config it@(Generics.M1 x) =
        zip (repeat (Text.pack (_modifyField config (Generics.selName it))))
            (map snd (prettyG config x))

instance Pretty a => PrettyG (Generics.K1 t a) where
    prettyG _config (Generics.K1 x) = [("", format x)]

-- instance PrettyG Generics.U1 where
--     prettyG Generics.U1 = []
instance (PrettyG f, PrettyG g) => PrettyG (f :*: g) where
    prettyG config (xs :*: ys) = prettyG config xs ++ prettyG config ys
-- instance (PrettyG f, PrettyG g) => PrettyG (f :+: g) where
--     prettyG (Generics.L1 x) = prettyG x
--     prettyG (Generics.R1 x) = prettyG x

-- * standard types

instance Pretty a => Pretty [a] where format = formatList
instance Pretty Char where
    format c = text $ "'" <> Text.singleton c <> "'"
    formatList = format . Text.pack

instance Pretty (a -> b) where pretty _ = "<function>"
instance Pretty () where pretty () = "()"
instance Pretty Bool where pretty = showt
instance Pretty Int where pretty = showt
instance Pretty Int.Int8 where pretty = showt
instance Pretty Int.Int16 where pretty = showt
instance Pretty Int.Int32 where pretty = showt
instance Pretty Int.Int64 where pretty = showt
instance Pretty Integer where pretty = showt
instance Pretty Word.Word8 where pretty = showt
instance Pretty Word.Word16 where pretty = showt
instance Pretty Word.Word32 where pretty = showt
instance Pretty Word.Word64 where pretty = showt
instance Pretty Double where pretty = Num.showFloatP False 3
instance Pretty Float where pretty = Num.showFloatP False 3

instance Pretty C.CChar where pretty = showt
instance Pretty C.CInt where pretty = showt
instance Pretty C.CFloat where pretty (C.CFloat a) = pretty a

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

-- | The default Pretty instance for Ratio reduces the fraction to k+n/d, which
-- is not always right.  This formats an improper fraction.
improperRatio :: (Eq a, Num a, Pretty a) => Ratio.Ratio a -> Text
improperRatio r
    | denom == 1 = pretty num
    | otherwise = pretty num <> "/" <> pretty denom
    where
    (num, denom) = (Ratio.numerator r, Ratio.denominator r)

-- | If it looks like a low fraction, display it thus, rather than as
-- a decimal.  This is useful because e.g. meters in three will have lots of
-- repeating decimals.  I also use fractions for power of two denominators
-- which are just fine in decimal, but the fraction still takes up less space.
fraction :: (RealFrac a, Pretty a) => Bool -> a
    -- ^ If true, try an ASCII fraction if there are no unicode ones, otherwise
    -- always use decimal.
    -> Text
fraction asciiFraction d
    | d == 0 = "0"
    | Just frac <- Map.lookup ratio fractions = int_s <> frac
    | asciiFraction && Ratio.denominator ratio <= 12 =
        int_s <> "+" <> pretty ratio
    | otherwise = pretty d
    where
    (int, frac) = properFraction d
    int_s = if int == 0 then "" else Text.pack (show int)
    ratio = Ratio.approxRational frac 0.0001
    fractions = Map.fromList
        [ (0 / 1, "")
        , (1 / 4, "¼"), (1 / 2, "½"), (3 / 4, "¾")
        , (1 / 3, "⅓"), (2 / 3, "⅔")
        , (1 / 5, "⅕"), (2 / 5, "⅖"), (3 / 5, "⅗"), (4 / 5, "⅘")
        , (1 / 6, "⅙"), (5 / 6, "⅚")
        , (1 / 8, "⅛"), (3 / 8, "⅜"), (5 / 8, "⅝"), (7 / 8, "⅞")
        ]

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

-- ** containers

instance Pretty a => Pretty (Set.Set a) where
    format = formattedList '{' '}' . Set.toList

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    format = formatMap . map (\(k, v) -> (format k, format v)) . Map.toList

instance Pretty v => Pretty (IntMap.IntMap v) where
    format = formattedList '{' '}' . map pair . IntMap.toList
        where pair (k, v) = format k </> (":" <+> format v)

instance Pretty a => Pretty (Tree.Tree a) where
    format (Tree.Node val children) =
        "Node" <> indent_ ("(" <> format val <> ")" <+/> format children)

-- ** other base types

instance Pretty Time.UTCTime where pretty = showt
instance Pretty Time.NominalDiffTime where
    pretty s = pretty (realToFrac s :: Double) <> "s"
instance Pretty Dynamic.Dynamic where pretty = showt
instance Pretty Calendar.Day where pretty = showt

instance Pretty (Foreign.Ptr a) where pretty = showt
instance Pretty (Foreign.ForeignPtr a) where pretty = showt

-- ** text

instance Pretty ByteString.ByteString where
    format bs = case Encoding.decodeUtf8' bs of
        -- If it's binary, quote like a string.  Unfortunately, show will add
        -- extra "s which format will then add again.
        Left _ -> format $ Lists.dropEnd 1 $ drop 1 $ show bs
        Right txt -> format txt

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

instance (Pretty a, Pretty b) => Pretty (Lists.Paired a b) where
    format (Lists.First a) = "First" <+> format a
    format (Lists.Second b) = "Second" <+> format b
    format (Lists.Both a b) = "Both" <+> format a <+> format b


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

formatMap :: [(Doc, Doc)] -> Doc
formatMap = formattedList '{' '}' . map pair
    where pair (k, v) = k </> (":" <+> v)

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


-- * standalone

duration :: Time.NominalDiffTime -> Text
duration secs0 = Text.unwords $ concat
    [ [showt days <> "d" | days > 0]
    , [showt hours <> "h" | hours > 0]
    , [showt mins <> "m" | mins > 0]
    , [pretty secs3]
    ]
    where
    (days, secs1) = Num.fDivMod secs0 (60*60*24)
    (hours, secs2) = Num.fDivMod secs1 (60*60)
    (mins, secs3) = Num.fDivMod secs2 60

bytes :: Int -> Int -> Text
bytes precision bs = case List.find ((>=1) . fst) sizes of
    Just (v, s) -> Num.showFloat precision v <> s
    Nothing -> showt bs <> "b"
    where
    sizes = [(gb, "gb"), (mb, "mb"), (kb, "kb")]
    kb = fromIntegral bs / 1024
    mb = kb / 1024
    gb = mb / 1024

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
