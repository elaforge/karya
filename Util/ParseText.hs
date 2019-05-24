-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
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
parse :: Parser a -> Text -> Either Text a
parse p text = case parse_all p text of
    Right val -> Right val
    Left (rest, msg) -> Left $
        "parse error: char " <> maybe "?" showt ((+1) <$> col) <> " of "
            <> show_expr col text <> ": " <> msg
        where col = infer_column text rest

-- | Parse all of the text, and annotate the error with line number and column.
parse_lines :: Int -> Parser a -> Text -> Either Text a
parse_lines start_line p text = case parse_all p text of
    Right val -> Right val
    Left (rest, msg) -> Left $ err <> ": " <> msg <> " in line "
        <> maybe "" (\(line, _, column) -> show_expr (Just column) line) loc
        where
        loc = infer_line text rest
        err = case loc of
            Nothing -> ""
            Just (_, lineno, column) ->
                showt (start_line + lineno) <> ":" <> showt (column + 1)

show_expr :: Maybe Int -> Text -> Text
show_expr Nothing expr = "\"" <> expr <> "\""
show_expr (Just i) expr = "\"" <> pre <> "»" <> post <> "\""
    where (pre, post) = (Text.take i expr, Text.drop i expr)

parse_all :: A.Parser a -> Text -> Either (Text, Text) a
parse_all p text = go (A.parse p text)
    where
    go (A.Fail rest contexts msg) = Left (rest, txt msg <> c)
        where
        c = if null contexts then ""
            else " [" <> Text.intercalate ", " (map txt contexts) <> "]"
    go (A.Partial cont) = go (cont "")
    go (A.Done rest val)
        | Text.null rest = Right val
        | otherwise = Left (rest, "expected eof")

infer_line :: Text -> Text -> Maybe (Text, Int, Int)
    -- ^ (line, lineno from 0, column from 0)
infer_line text rest = infer =<< infer_column text rest
    where
    infer i = extract i <$> List.find (\(_, end, _) -> end > i)
        (zip3 sums (drop 1 sums) (zip [0..] lines))
    extract i (start, _, (lineno, line)) = (line, lineno, i - start)
    sums = scanl (+) 0 (map ((+1) . Text.length) lines)
    lines = Text.lines text

infer_column :: Text -> Text -> Maybe Int
infer_column text rest
    | rest `Text.isSuffixOf` text = Just $ Text.length text - Text.length rest
    | otherwise = Nothing

-- * casual parsing

maybe_read :: Read a => String -> Maybe a
maybe_read str = case reads str of
    (a, "") : _ -> Just a
    _ -> Nothing

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

between :: Parser _a -> Parser _b -> Parser a -> Parser a
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
