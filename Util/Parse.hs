-- | Like "Util.ParseBs", but for parsec, not attoparsec.  I only use
-- attoparsec when performance matters, because its error msgs are crummy.
module Util.Parse where
import qualified Text.Parsec.Error as Error
import qualified Numeric
import qualified Text.Parsec as P
import Text.Parsec ((<?>))

import Util.Control
import qualified Util.Seq as Seq


type Parser st a = P.Parsec String st a

parse :: Parser () a -> String -> Either String a
parse parser input = either (Left . format1 input) Right $
    P.parse (parser <* P.eof) "" input

format1 :: String -> P.ParseError -> String
format1 input err
    | line == 1 = "col " ++ show col ++ ": " ++ msg
    | otherwise = show line ++ ":" ++ show col ++ ": " ++ msg
    where
    msg = Seq.join "; " (show_error err) ++ ": " ++ show input
    pos = P.errorPos err
    (line, col) = (P.sourceLine pos, P.sourceColumn pos)

show_error :: P.ParseError -> [String]
show_error = filter (not.null) . lines
    . Error.showErrorMessages "or" "unknown parse error"
        "expecting" "unexpected" "end of input"
    . Error.errorMessages

-- | Natural number including 0.
p_nat :: Parser st Int
p_nat = do
    i <- P.many1 P.digit
    case Numeric.readDec i of
        (n, _) : _ -> return n
        _ -> mzero -- this should never happen

-- | Natural number without 0.
p_positive :: Parser st Int
p_positive = do
    n <- p_nat
    if n == 0 then mzero else return n

p_float :: Parser st Double
p_float = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    val <- p_unsigned_float
    return (val * sign)
    <?> "float"

p_unsigned_float :: Parser st Double
p_unsigned_float = do
    i <- P.many P.digit
    f <- P.option "" (P.char '.' >> P.many1 P.digit)
    if null i && null f then mzero else do
    case (dec i, dec f) of
        (Just i', Just f') -> return $ fromIntegral i'
            + fromIntegral f' / fromIntegral (10 ^ length f)
        _ -> mzero
    <?> "unsigned float"
    where
    dec :: String -> Maybe Int
    dec s
        | null s = Just 0
        | otherwise = case Numeric.readDec s of
            (d, "") : _ -> Just d
            _ -> Nothing
