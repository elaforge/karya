-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Like "Util.ParseBs", but for parsec, not attoparsec.  I only use
-- attoparsec when performance matters, because its error msgs are crummy.
module Util.Parse where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Numeric
import qualified Text.Parsec as P
import Text.Parsec ((<?>))
import qualified Text.Parsec.Error as Error
import Text.Parsec.Text ()

import qualified Util.File as File
import Global


-- * parsec

type Parser st a = P.Parsec Text st a

parse :: Parser () a -> Text -> Either Text a
parse parser input = either (Left . format1 input) Right $
    P.parse (parser <* P.eof) "" input

parse_maybe :: Parser () a -> Text -> Maybe a
parse_maybe p = either (const Nothing) Just . parse p

-- | Try to parse a file, or return a default value if the file doesn't exist.
file :: a -> Parser st a -> st -> FilePath -> IO (Either P.ParseError a)
file deflt parser state fname = do
    maybe_text <- File.ignoreEnoent (Text.IO.readFile fname)
    return $ maybe (Right deflt) (P.runParser parser state fname) maybe_text

-- | Format a ParseError assuming the input is just one line.
format1 :: Text -> P.ParseError -> Text
format1 input err
    | line == 1 = "col " <> showt col <> ": " <> msg
    | otherwise = showt line <> ":" <> showt col <> ": " <> msg
    where
    msg = Text.intercalate "; " (show_error err) <> ": " <> showt input
    pos = P.errorPos err
    (line, col) = (P.sourceLine pos, P.sourceColumn pos)

show_error :: P.ParseError -> [Text]
show_error = filter (not . Text.null) . Text.lines . txt
    . Error.showErrorMessages "or" "unknown parse error"
        "expecting" "unexpected" "end of input"
    . Error.errorMessages

-- | Like 'P.string', but for text.  Apparently there's no way to do this
-- without converting to String first.
text :: Text -> Parser st ()
text txt = P.string (untxt txt) >> return ()

p_int :: Parser st Int
p_int = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    (*sign) <$> p_nat
    <?> "int"

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
