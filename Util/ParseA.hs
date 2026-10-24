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
module Util.ParseA (module Util.ParseA, many) where
import           Control.Applicative (many)
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read

import qualified Util.ParseM as ParseM

import           Global


type Parser a = A.Parser a

-- | Parse all the text, and annotate the error with the char number.  For
-- single-line input.
parse1 :: Parser a -> Text -> Either Text a
parse1 p text = first fmt $ parseAll p text
    where
    fmt (rest, msg) = "parse error: " <> shortContext col text <> ": " <> msg
        where col = inferColumn text rest

-- | Parse all of the text, and annotate the error with line number and column.
parse :: Parser a -> Text -> Either ParseM.Error a
parse p text = case parseAll p text of
    Right val -> Right val
    Left (rest, msg) -> Left $ case inferLocation text rest of
        Nothing -> ParseM.message msg
        Just (line, position) -> ParseM.Error
            { _filename = Nothing
            , _diagnostic = ParseM.Diagnostic (Just line) (Just position) 1 msg
            , _extraDiagnostics = []
            }

type Row = Int
type Column = Int

-- ** shortContext

shortContext :: Maybe Int -> Text -> Text
shortContext Nothing expr = "\"" <> expr <> "\""
shortContext (Just i) expr = "\"" <> pre <> char <> post <> "\""
    where
    (pre, post) = (Text.take i expr, Text.drop i expr)
    -- This just has to look distinctively not like ascii and stick out a bit.
    char = "⎣"

parseAll :: A.Parser a -> Text -> Either (Text, Text) a
parseAll p text = go (A.parse p text)
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

inferLocation :: Text -> Text -> Maybe (Text, (Row, Column))
inferLocation text rest = infer <$> inferColumn text rest
    where
    infer i = (line, (length parts, Text.length (last parts) + 1))
        where
        (before, after) = Text.splitAt i text
        parts = Text.splitOn "\n" before
        line = last parts <> Text.takeWhile (/= '\n') after

inferColumn :: Text -> Text -> Maybe Int
inferColumn text rest
    | rest `Text.isSuffixOf` text = Just $ Text.length text - Text.length rest
    | otherwise = Nothing

-- * casual parsing

maybeParse :: Parser a -> Text -> Maybe a
maybeParse parser text = either (const Nothing) Just (parseAll parser text)

maybeParseString :: Parser a -> String -> Maybe a
maybeParseString parser = maybeParse parser . txt

float :: Text -> Maybe Double
float = maybeParse pFloat

int :: Text -> Maybe Int
int = maybeParse pInt

nat :: Text -> Maybe Int
nat = maybeParse pNat

-- * combinators

between :: Parser x -> Parser y -> Parser a -> Parser a
between open close p = open *> p <* close

optional :: Parser a -> Parser (Maybe a)
optional p = A.option Nothing (Just <$> p)

-- * parsers

-- | Convert a parser into a lexeme parser by skipping whitespace afterwards.
lexeme :: Parser a -> Parser a
lexeme p = p <* A.skipSpace

pFloat :: Parser Double
pFloat = do
    sign <- A.option 1 (A.char '-' >> pure (-1))
    val <- pUnsignedFloat
    pure (val * sign)
    <?> "float"

pUnsignedFloat :: Parser Double
pUnsignedFloat = do
    i <- A.takeWhile isDigit
    f <- A.option "" (A.char '.' >> A.takeWhile1 isDigit)
    if Text.null i && Text.null f then mzero else do
    case (dec i, dec f) of
        (Just i', Just f') -> pure $ fromIntegral i'
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

pInt :: Parser Int
pInt = do
    sign <- A.option '+' (A.satisfy (\c -> c == '+' || c == '-'))
    val <- pNat
    pure $ (if sign == '-' then -1 else 1) * val

pNat :: Parser Int
pNat = do
    i <- A.takeWhile1 isDigit
    case Text.Read.decimal i of
        Right (d, _) -> pure d
        Left _ -> mzero

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- | A word of non-space chars.
pWord :: Parser Text
pWord = A.takeWhile1 (/= ' ')
