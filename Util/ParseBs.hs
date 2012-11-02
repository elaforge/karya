{-# LANGUAGE OverloadedStrings #-}
{- | Parsing utilities for ByteStrings, using Attoparsec.

    This module also exports some basic combinators.  The idea is that modules
    that want to do a bit of parsing should be able to import this and need not
    import the underlying parsing library, which should make it easier to
    switch parsing libraries in the future if I want to.  Of course the parsers
    may return a different type (ByteString vs. Text) so callers will still
    need a little modification to switch libraries.
-}
module Util.ParseBs (module Util.ParseBs, many) where
import Control.Applicative (many)
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec as Attoparsec
import qualified Data.Attoparsec.Char8 as A
import Data.Attoparsec ((<?>))

import Util.Control
import qualified Util.Seq as Seq


type Parser a = A.Parser a

parse_all :: Parser a -> B.ByteString -> Either String a
parse_all p text = go (A.parse p text)
    where
    go (Attoparsec.Fail rest contexts msg) =
        Left $ err rest ++ msg ++ " [" ++ Seq.join ", " contexts ++ "]"
    go (Attoparsec.Partial cont) = go (cont "")
    go (Attoparsec.Done rest val)
        | B.null rest = Right val
        | otherwise = Left $ err rest ++ "expected eof"
    err rest = "parse error on byte " ++ column rest ++ " of " ++ show text
        ++ ": "
    column t
        | t `B.isSuffixOf` text = show $ B.length text - B.length t + 1
        | otherwise = "?"

-- * casual parsing

maybe_read :: (Read a) => String -> Maybe a
maybe_read str = case reads str of
    (a, "") : _ -> Just a
    _ -> Nothing

maybe_parse :: Parser a -> B.ByteString -> Maybe a
maybe_parse parser text = either (const Nothing) Just (parse_all parser text)

maybe_parse_string :: Parser a -> String -> Maybe a
maybe_parse_string parser = maybe_parse parser . B.pack

float :: String -> Maybe Double
float = maybe_parse_string p_float

int :: String -> Maybe Int
int = maybe_parse_string p_int

-- * combinators

between :: Parser _a -> Parser _b -> Parser a -> Parser a
between open close p = open >> p <* close

optional :: Parser a -> Parser (Maybe a)
optional p = A.option Nothing (Just <$> p)

-- ** re-export

char :: Char -> Parser Char
char = A.char

spaces :: Parser ()
spaces = A.skipSpace

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
    i <- A.takeWhile A.isDigit
    f <- A.option "" (A.char '.' >> A.takeWhile1 A.isDigit)
    if B.null i && B.null f then mzero else do
    case (dec i, dec f) of
        (Just i', Just f') -> return $ fromIntegral i'
            + fromIntegral f' / fromIntegral (10 ^ B.length f)
        _ -> mzero
    <?> "unsigned float"
    where
    dec :: B.ByteString -> Maybe Int
    dec s
        | B.null s = Just 0
        | otherwise = case B.readInt s of
            Just (d, rest) | B.null rest -> Just d
            _ -> Nothing

p_int :: Parser Int
p_int = do
    sign <- A.option '+' (A.satisfy (\c -> c == '+' || c == '-'))
    val <- p_nat
    return $ (if sign == '-' then -1 else 1) * val

p_nat :: Parser Int
p_nat = do
    i <- A.takeWhile1 A.isDigit
    case B.readInt i of
        Just (d, _) -> return d
        Nothing -> mzero


-- | A word of non-space chars.
p_word :: Parser B.ByteString
p_word = A.takeWhile1 (/= ' ')
