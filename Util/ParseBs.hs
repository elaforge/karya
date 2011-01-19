{-# LANGUAGE OverloadedStrings #-}
-- | Parsing utilities for ByteStrings, using Attoparsec.
module Util.ParseBs where
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.Char8 as A
import Data.Attoparsec ((<?>))

import Util.Control
import qualified Util.Seq as Seq


parse_all :: A.Parser a -> B.ByteString -> Either String a
parse_all p text = go (A.parse p text)
    where
    go (A.Fail rest contexts msg) =
        Left $ err rest ++ msg ++ " [" ++ Seq.join ", " contexts ++ "]"
    go (A.Partial cont) = go (cont "")
    go (A.Done rest val)
        | B.null rest = Right val
        | otherwise = Left $ err rest ++ "expected eof"
    err rest = "parse error on byte " ++ column rest ++ " of " ++ show text
        ++ ": "
    column t
        | t `B.isSuffixOf` text = show $ B.length text - B.length t + 1
        | otherwise = "?"

-- * combinators

between :: A.Parser _a -> A.Parser _b -> A.Parser a -> A.Parser a
between open close p = open >> p <* close

optional :: A.Parser a -> A.Parser (Maybe a)
optional p = A.option Nothing (Just <$> p)

-- * parsers

-- | Convert a parser into a lexeme parser by skipping whitespace afterwards.
lexeme :: A.Parser a -> A.Parser a
lexeme p = p <* A.skipWhile A.isSpace

p_float :: A.Parser Double
p_float = do
    sign <- A.option 1 (A.char '-' >> return (-1))
    val <- p_unsigned_float
    return (val * sign)
    <?> "float"

p_unsigned_float :: A.Parser Double
p_unsigned_float = do
    i <- A.takeWhile A.isDigit
    f <- A.option "" (A.char '.' >> A.takeWhile1 A.isDigit)
    if (B.null i && B.null f) then mzero else do
    case (dec i, dec f) of
        (Just i', Just f') -> return $ fromIntegral i'
            + fromIntegral f' / fromIntegral (10 ^ (B.length f))
        _ -> mzero
    <?> "unsigned float"
    where
    dec :: B.ByteString -> Maybe Int
    dec s
        | B.null s = Just 0
        | otherwise = case B.readInt s of
            Just (d, rest) | B.null rest -> Just d
            _ -> Nothing
