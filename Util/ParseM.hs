-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Generic parsing utils for megaparsec.
module Util.ParseM (
    -- * parse
    Parser, ParserT, ParserS
    , parse
    , parseMaybe
    , parseM, parseS, parseE
    , file
    -- * basic parsing
    , Offset
    , failAt
    , getOffset
    , offsetToSourcePos
    , pInt, pNat, pPositive, pFloat, pUnsignedFloat
    -- * attoparse
    , attoparse
    -- * errors
    , Error(..)
    , Diagnostic(..)
    , Row, Column
    , fromMegaparsec
    , prefix
    , addRow
    , message
    , setFilename
    , showError
) where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Void as Void

import qualified Numeric
import qualified Text.Megaparsec as P
import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec.Char as P

import qualified Util.Exceptions as Exceptions
import qualified Util.Texts as Texts

import           Global


-- * parse

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

-- | Try to parse a file, or return a default value if the file doesn't exist.
file :: a -> ParserS st a -> st -> FilePath -> IO (Either Text a)
file deflt parser state fname = do
    mbText <- Exceptions.ignoreEnoent (Text.IO.readFile fname)
    pure $ maybe (Right deflt) (parseS state fname parser) mbText

-- * basic parsing

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

-- * errors

-- | A local simplification of P.ParseErrorBundle.  I use it because I also
-- emit non-parse errors, and I want them to be reported in the same way.
data Error = Error
    { _filename :: Maybe FilePath
    -- TODO This is only a list because some rare megaparsec condition I don't
    -- use can emit multiple errors.  Maybe I can just have one and crash or
    -- smash them together for multiple?
    , _diagnostic :: Diagnostic
    , _extraDiagnostics :: [Diagnostic]
    } deriving (Eq, Show)

data Diagnostic = Diagnostic
    { _line :: Maybe Text -- ^ line where the error occurred
    , _position :: Maybe (Row, Column)
    , _length :: Int -- ^ length of caret highlight
    , _message :: Text
    } deriving (Eq, Show)

-- | 1-based line number.
type Row = Int
-- | 1-based column number.
type Column = Int

-- | Convert Megaparsec diagnostics into the common error representation.
fromMegaparsec :: P.ParseErrorBundle Text Void.Void -> Error
fromMegaparsec bundle = case located of
    (_filename, diagnostic) : rest -> Error
        { _filename
        , _diagnostic = diagnostic
        , _extraDiagnostics = map snd rest
        }
    -- TODO because didn't use NonEmpty.mapAccumL?
    [] -> message "Megaparsec returned an empty error bundle"
    where
    located = snd $ List.mapAccumL convert (P.bundlePosState bundle)
        (NonEmpty.toList (P.bundleErrors bundle))
    convert posState0 err =
        ( posState1
        , ( if sourceName == "" then Nothing else Just sourceName
          , Diagnostic
                { _line = txt <$> line
                , _position = Just
                    ( P.unPos (P.sourceLine sourcePos)
                    , P.unPos (P.sourceColumn sourcePos)
                    )
                , _length = errorLength err
                , _message = Text.stripEnd $ txt $ P.parseErrorTextPretty err
                }
          )
        )
        where
        (line, posState1) = P.reachOffset (P.errorOffset err) posState0
        sourcePos = P.pstateSourcePos posState1
        sourceName = P.sourceName sourcePos
    errorLength (P.TrivialError _ unexpected _) =
        maybe 1 errorItemLength unexpected
    errorLength (P.FancyError _ errors) =
        maximum (1 : map errorFancyLength (Set.toList errors))
    errorItemLength (P.Tokens tokens) = NonEmpty.length tokens
    errorItemLength _ = 1
    errorFancyLength (P.ErrorCustom component) = P.errorComponentLen component
    errorFancyLength _ = 1

prefix :: Text -> Error -> Error
prefix prefix err = err { _diagnostic = add (_diagnostic err) }
    where add diag = diag { _message = prefix <> _message diag }

-- | Add a row offset.
addRow :: Row -> Error -> Error
addRow row err = err { _diagnostic = shift (_diagnostic err) }
    where shift diag = diag { _position = first (+row) <$> _position diag }

message :: Text -> Error
message msg = Error Nothing (Diagnostic Nothing Nothing 1 msg) []

-- | Set the filename unless the error already carries one.
setFilename :: FilePath -> Error -> Error
setFilename filename err = err { _filename = _filename err <|> Just filename }

showError :: Error -> Text
showError (Error mbFilename diagnostic diagnostics) =
    Text.intercalate "\n" $
        map (formatDiagnostic mbFilename) (diagnostic : diagnostics)

formatDiagnostic :: Maybe FilePath -> Diagnostic -> Text
formatDiagnostic mbFilename (Diagnostic mbLine position length_ msg) =
    case position of
        Nothing -> Texts.join2 ": " filename msg
        Just (row, column) -> Text.intercalate "\n" $
            Text.intercalate ":" [filename, showt row, showt column, msg]
            : maybe [] (formatLine row column) mbLine
    where
    filename = maybe "<unknown>" txt mbFilename
    formatLine row column sourceLine =
        [ showt row <> " | " <> sourceLine
        , padding <> "| " <> Text.replicate (column - 1) " "
            <> Text.replicate pointerLength "^"
        ]
        where
        padding = Text.replicate (length (show row) + 1) " "
        shift = max 0 (column - 1)
        lineLength = Text.length sourceLine
        pointerLength
            | shift + length_ > lineLength = max 1 (lineLength - shift + 1)
            | otherwise = length_
