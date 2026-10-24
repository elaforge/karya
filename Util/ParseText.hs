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
import           Control.Applicative (many)
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Text ((<?>))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Data.Void as Void

import qualified Text.Megaparsec as P

import qualified Util.Texts as Texts

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
parse :: Parser a -> Text -> Either Error a
parse p text = case parseAll p text of
    Right val -> Right val
    Left (rest, msg) -> Left $ case inferLocation text rest of
        Nothing -> message msg
        Just (line, position) ->
            Error Nothing (Diagnostic (Just line) (Just position) 1 msg) []

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

type Row = Int
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
    Text.intercalate "\n\n" $
        map (formatDiagnostic mbFilename) (diagnostic : diagnostics)

formatDiagnostic :: Maybe FilePath -> Diagnostic -> Text
formatDiagnostic mbFilename (Diagnostic line position length_ msg) =
    case position of
        Nothing -> Texts.join2 ": " (maybe "" txt mbFilename) msg
        Just (row, column) -> mconcat
            [ maybe "" ((<> ":") . txt) mbFilename
            , showt row, ":", showt column, ":\n"
            , maybe "" (formatLine row column) line
            , msg
            ]
    where
    formatLine row column sourceLine = mconcat
        [ padding, "|\n"
        , showt row, " | ", sourceLine, "\n"
        , padding, "| "
        , Text.replicate (max 0 (column - 1)) " "
        , Text.replicate pointerLength "^"
        , "\n"
        ]
        where
        padding = Text.replicate (length (show row) + 1) " "
        shift = max 0 (column - 1)
        lineLength = Text.length sourceLine
        pointerLength
            | shift + length_ > lineLength = max 1 (lineLength - shift + 1)
            | otherwise = length_

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
