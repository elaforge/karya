-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NondecreasingIndentation #-}
{- | Parsing utilities for Text, using Attoparsec.

    This module also exports some basic combinators.  The idea is that modules
    that want to do a bit of parsing should be able to import this and need not
    import the underlying parsing library, which should make it easier to
    switch parsing libraries in the future if I want to.  Of course the parsers
    may return a different type (ByteString vs. Text) so callers will still
    need a little modification to switch libraries.
-}
module Util.ParseText (module Util.ParseText, many) where
import Control.Applicative (many)
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text ((<?>))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read

import Global


type Parser a = A.Parser a

-- | Parse all the text, and annotate the error with the char number.  For
-- single-line input.
parse1 :: Parser a -> Text -> Either Text a
parse1 p text = first fmt $ parse_all p text
    where
    fmt (rest, msg) = "parse error: " <> error_context col text <> ": " <> msg
        where col = infer_column text rest

-- | Parse all of the text, and annotate the error with line number and column.
parse :: Parser a -> Text -> Either Error a
parse p text = case parse_all p text of
    Right val -> Right val
    Left (rest, msg) -> Left $ Error loc msg
        where loc = infer_location text rest

data Error = Error {
    _position :: Maybe (Text, (Row, Column))
    , _message ::Text
    } deriving (Eq, Show)
type Row = Int
type Column = Int

prefix :: Text -> Error -> Error
prefix pref err = err { _message = pref <> _message err }

offset :: (Row, Column) -> Error -> Error
offset (row, col) err =
    err { _position = second (\(r, c) -> (r+row, c+col)) <$> _position err }

message :: Text -> Error
message = Error Nothing

show_error :: Error -> Text
show_error (Error Nothing msg) = msg
show_error (Error (Just (line, (row, column))) msg) = mconcat
    [ showt row, ":", showt column, ": ", msg, " in line "
    , error_context (Just column) line
    ]

error_context :: Maybe Int -> Text -> Text
error_context Nothing expr = "\"" <> expr <> "\""
error_context (Just i) expr = "\"" <> pre <> char <> post <> "\""
    where
    (pre, post) = (Text.take i expr, Text.drop i expr)
    -- This just has to look distinctively not like ascii and stick out a bit.
    char = "⎣"

parse_all :: A.Parser a -> Text -> Either (Text, Text) a
parse_all p text = go (A.parse p text)
    where
    -- The msg can be really unclear, like "string", so put quotes on to
    -- at least tell it's trying to be an error.
    go (A.Fail rest contexts msg) = Left (rest, "'" <> txt msg <> "'" <> c)
        where
        c = if null contexts then ""
            else " [" <> Text.intercalate ", " (map txt contexts) <> "]"
    go (A.Partial cont) = go (cont "")
    go (A.Done rest val)
        | Text.null rest = Right val
        | otherwise = Left (rest, "expected eof")

infer_location :: Text -> Text -> Maybe (Text, (Int, Int))
    -- ^ (line, row from 1, column from 1)
infer_location text rest = infer =<< infer_column text rest
    where
    infer i = extract i <$> List.find (\(_, end, _) -> end > i)
        (zip3 sums (drop 1 sums) (zip [1..] lines))
    extract i (start, _, (row, line)) = (line, (row, i - start + 1))
    sums = scanl (+) 0 (map ((+1) . Text.length) lines)
    lines = Text.lines text

infer_column :: Text -> Text -> Maybe Int
infer_column text rest
    | rest `Text.isSuffixOf` text = Just $ Text.length text - Text.length rest
    | otherwise = Nothing

-- * casual parsing

maybe_parse :: Parser a -> Text -> Maybe a
maybe_parse parser text = either (const Nothing) Just (parse_all parser text)

maybe_parse_string :: Parser a -> String -> Maybe a
maybe_parse_string parser = maybe_parse parser . txt

float :: Text -> Maybe Double
float = maybe_parse p_float

int :: Text -> Maybe Int
int = maybe_parse p_int

nat :: Text -> Maybe Int
nat = maybe_parse p_nat

-- * combinators

between :: Parser x -> Parser y -> Parser a -> Parser a
between open close p = open *> p <* close

optional :: Parser a -> Parser (Maybe a)
optional p = A.option Nothing (Just <$> p)

-- * parsers

-- | Convert a parser into a lexeme parser by skipping whitespace afterwards.
lexeme :: Parser a -> Parser a
lexeme p = p <* A.skipSpace

p_float :: Parser Double
p_float = do
    sign <- A.option 1 (A.char '-' >> return (-1))
    val <- p_unsigned_float
    return (val * sign)
    <?> "float"

p_unsigned_float :: Parser Double
p_unsigned_float = do
    i <- A.takeWhile is_digit
    f <- A.option "" (A.char '.' >> A.takeWhile1 is_digit)
    if Text.null i && Text.null f then mzero else do
    case (dec i, dec f) of
        (Just i', Just f') -> return $ fromIntegral i'
            + fromIntegral f' / fromIntegral (10 ^ Text.length f)
        _ -> mzero
    <?> "unsigned float"
    where
    dec :: Text -> Maybe Int
    dec s
        | Text.null s = Just 0
        | otherwise = case Text.Read.decimal s of
            Right (d, rest) | Text.null rest -> Just d
            _ -> Nothing

p_int :: Parser Int
p_int = do
    sign <- A.option '+' (A.satisfy (\c -> c == '+' || c == '-'))
    val <- p_nat
    return $ (if sign == '-' then -1 else 1) * val

p_nat :: Parser Int
p_nat = do
    i <- A.takeWhile1 is_digit
    case Text.Read.decimal i of
        Right (d, _) -> return d
        Left _ -> mzero

is_digit :: Char -> Bool
is_digit c = c >= '0' && c <= '9'

-- | A word of non-space chars.
p_word :: Parser Text
p_word = A.takeWhile1 (/= ' ')
