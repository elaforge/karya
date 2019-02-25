-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Generic parsing utils.
module Util.Parse where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Void as Void

import qualified Numeric
import qualified Text.Megaparsec as P
import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec.Char as P

import qualified Util.File as File

import           Global

-- * parsec

type Parser a = ParserT Identity.Identity a
type ParserT m a = P.ParsecT Void.Void Text m a
type ParserS s a = ParserT (State.StateT s Identity.Identity) a

parse :: Parser a -> Text -> Either Text a
parse p = Identity.runIdentity . parseM "" p

parseM :: Monad m => FilePath -> ParserT m a -> Text -> m (Either Text a)
parseM fname p = fmap (first (txt . P.errorBundlePretty)) . P.runParserT p fname

parseS :: state -> FilePath -> ParserS state a -> Text -> Either Text a
parseS state fname p = fst . flip State.runState state . parseM fname p

parse_maybe :: Parser a -> Text -> Maybe a
parse_maybe p = either (const Nothing) Just . parse p

-- | Try to parse a file, or return a default value if the file doesn't exist.
file :: a -> ParserS st a -> st -> FilePath -> IO (Either Text a)
file deflt parser state fname = do
    mb_text <- File.ignoreEnoent (Text.IO.readFile fname)
    return $ maybe (Right deflt) (parseS state fname parser) mb_text

-- -- | Format a ParseError assuming the input is just one line.
-- format1 :: Text -> P.ParseError -> Text
-- format1 input err
--     | line == 1 = "col " <> showt col <> ": " <> msg
--     | otherwise = showt line <> ":" <> showt col <> ": " <> msg
--     where
--     msg = Text.intercalate "; " (show_error err) <> ": " <> showt input
--     pos = P.errorPos err
--     (line, col) = (P.sourceLine pos, P.sourceColumn pos)
--
-- show_error :: P.ParseError -> [Text]
-- show_error = filter (not . Text.null) . Text.lines . txt
--     . Error.showErrorMessages "or" "unknown parse error"
--         "expecting" "unexpected" "end of input"
--     . Error.errorMessages

p_int :: ParserT m Int
p_int = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    (*sign) <$> p_nat
    <?> "int"

-- | Natural number including 0.
p_nat :: ParserT m Int
p_nat = do
    i <- P.takeWhile1P Nothing is_digit
    case Numeric.readDec (untxt i) of
        (n, _) : _ -> return n
        _ -> mzero -- this should never happen
    <?> "nat"

-- | Natural number without 0.
p_positive :: ParserT m Int
p_positive = do
    n <- p_nat
    if n == 0 then mzero else return n

p_float :: ParserT m Double
p_float = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    val <- p_unsigned_float
    return (val * sign)
    <?> "float"

p_unsigned_float :: ParserT m Double
p_unsigned_float = do
    i <- P.takeWhileP Nothing is_digit
    f <- P.option "" (P.char '.' >> P.takeWhile1P Nothing is_digit)
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
        | otherwise = case Numeric.readDec (untxt s) of
            (d, "") : _ -> Just d
            _ -> Nothing

is_digit :: Char -> Bool
is_digit c = '0' <= c && c <= '9'
