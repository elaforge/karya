-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Generic parsing utils for megaparsec.
module Util.ParseM where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Void as Void

import qualified Numeric
import qualified Text.Megaparsec as P
import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec.Char as P

import qualified Util.Exceptions as Exceptions

import           Global


-- * parsec

type Parser a = ParserT Identity.Identity a
type ParserT m a = P.ParsecT Void.Void Text m a
type ParserS s a = ParserT (State.StateT s Identity.Identity) a

parse :: Parser a -> Text -> Either Text a
parse p = Identity.runIdentity . parseM "" p

parseMaybe :: Parser a -> Text -> Maybe a
parseMaybe p = either (const Nothing) Just . parse p

parseM :: Monad m => FilePath -> ParserT m a -> Text -> m (Either Text a)
parseM fname p =
    fmap (first (txt . P.errorBundlePretty)) . P.runParserT (p <* P.eof) fname

parseS :: state -> FilePath -> ParserS state a -> Text -> Either Text a
parseS state fname p = fst . flip State.runState state . parseM fname p

parseE :: FilePath -> Parser a -> Text
    -> Either (P.ParseErrorBundle Text Void.Void) a
parseE fname p = Identity.runIdentity . P.runParserT (p <* P.eof) fname

type Offset = Int

failAt :: Offset -> String -> Parser a
failAt offset err =
    P.parseError $ P.FancyError offset $ Set.singleton (P.ErrorFail err)

getOffset :: Parser Offset
getOffset = P.getOffset

offsetToSourcePos :: Offset -> Parser P.SourcePos
offsetToSourcePos offset = do
  state <- P.getParserState
  P.setOffset offset
  pos <- P.getSourcePos
  P.setParserState state
  pure pos

-- | Try to parse a file, or return a default value if the file doesn't exist.
file :: a -> ParserS st a -> st -> FilePath -> IO (Either Text a)
file deflt parser state fname = do
    mbText <- Exceptions.ignoreEnoent (Text.IO.readFile fname)
    pure $ maybe (Right deflt) (parseS state fname parser) mbText

pInt :: ParserT m Int
pInt = do
    sign <- P.option 1 (P.char '-' >> pure (-1))
    (*sign) <$> pNat
    <?> "int"

-- | Natural number including 0.
pNat :: ParserT m Int
pNat = do
    i <- P.takeWhile1P Nothing isDigit
    case Numeric.readDec (untxt i) of
        (n, _) : _ -> pure n
        _ -> mzero -- this should never happen
    <?> "nat"

-- | Natural number without 0.
pPositive :: ParserT m Int
pPositive = do
    n <- pNat
    if n == 0 then mzero else pure n

pFloat :: ParserT m Double
pFloat = do
    sign <- P.option 1 (P.char '-' >> pure (-1))
    val <- pUnsignedFloat
    pure (val * sign)
    <?> "float"

pUnsignedFloat :: ParserT m Double
pUnsignedFloat = do
    i <- P.takeWhileP Nothing isDigit
    f <- P.option "" (P.char '.' >> P.takeWhile1P Nothing isDigit)
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
        | otherwise = case Numeric.readDec (untxt s) of
            (d, "") : _ -> Just d
            _ -> Nothing

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'


-- * attoparse

-- | Convert an attoparsec parser to a megaparsec one.
attoparse :: forall a. String -> A.Parser a -> Parser a
attoparse msg p = do
    input <- P.getInput
    go (Text.length input) $ A.parse p input
    where
    go pre = \case
        A.Done rest val -> do
            P.setInput rest
            offset <- P.getOffset
            P.setOffset (offset + (pre - Text.length rest))
            pure val
        A.Fail _rest _contexts _msg ->
            -- The attoparsec msg is so useless I just ignore it and use my
            -- own.  E.g. it may be "Failed reading: takeWhile1".
            fail $ "expected " <> msg
        A.Partial cont -> go pre (cont "")
