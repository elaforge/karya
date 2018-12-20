-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TScore.Parse where
import qualified Control.Monad.Combinators as P
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Void as Void

import qualified Numeric
import qualified Text.Megaparsec as P
import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec.Char as P

import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global


data Config = Config {
    -- If true, "a" is parsed as "a/", if false, it's parsed as "/a".
    _default_call :: Bool
    } deriving (Eq, Show)

-- * parse

pparse :: Parser a -> Text -> Either String a
pparse p = first P.errorBundlePretty . P.parse (p <* P.eof) ""

type Parser a = P.Parsec Void.Void Text a

class Element a where
    parse :: Parser a
    unparse :: a -> Text

instance Element T.Score where
    parse = p_whitespace False *> (T.Score <$> P.many (lexeme parse))
    unparse (T.Score toplevels) = Text.unlines (map unparse toplevels)

instance Element T.Toplevel where
    parse = T.ToplevelDirective <$> parse <|> T.BlockDefinition <$> parse
    unparse (T.ToplevelDirective a) = unparse a
    unparse (T.BlockDefinition a) = unparse a

-- > ns/block = %directive "block title" [ ... ]
instance Element T.Block where
    parse = do
        bid <- lexeme parse
        keyword "="
        directives <- P.many (lexeme parse)
        title <- P.option "" (lexeme p_string)
        tracks <- parse
        return $ T.Block bid directives title tracks
    unparse (T.Block bid directives title tracks) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [unparse bid, "="]
            , map unparse directives
            , [if Text.null title then "" else un_string title]
            , [unparse tracks]
            ]

instance Element Id.BlockId where
    parse = do
        a <- P.takeWhile1P Nothing Id.is_id_char
        mb <- P.optional $ P.try $
            P.char '/' *> (P.takeWhile1P Nothing Id.is_id_char)
        let bid = maybe (Id.id default_namespace a)
                (\b -> Id.id (Id.namespace a) b) mb
        maybe (fail $ "invalid BlockId: " <> prettys bid) return (Id.make bid)
        <?> "BlockId"
    unparse = Id.show_short default_namespace . Id.unpack_id

default_namespace :: Id.Namespace
default_namespace = Id.namespace "ns"

instance Element T.Tracks where
    parse = fmap T.Tracks $
        keyword "[" *> P.sepBy1 parse (keyword "//") <* keyword "]"
    unparse (T.Tracks tracks) = Text.unwords $
        "[" : List.intersperse "//" (map unparse tracks) ++ ["]"]

instance Element T.Track where
    parse = T.Track
        <$> (P.option "" (lexeme p_string)) <*> P.some (lexeme parse)
    unparse (T.Track title tokens) =
        (if Text.null title then "" else un_string title <> " ")
        <> Text.unwords (map unparse tokens)

instance Element T.Directive where
    parse = do
        P.char '%'
        T.Directive
            <$> not_in "= \n"
            <*> P.optional (P.char '=' *> not_in " \n")
        where
        not_in :: [Char] -> Parser Text
        not_in cs = P.takeWhile1P Nothing (`notElem` cs)
    unparse (T.Directive lhs rhs) = "%" <> lhs <> maybe "" ("="<>) rhs

instance Element T.Token where
    parse = T.TBarline <$> parse <|> T.TNote <$> parse <|> T.TRest <$> parse
    unparse (T.TBarline bar) = unparse bar
    unparse (T.TNote note) = unparse note
    unparse (T.TRest rest) = unparse rest

-- ** barline

instance Element T.Barline where
    parse = T.Barline <$>
        (Text.length <$> P.takeWhile1P Nothing (=='|')
            <|> (P.char ';' *> pure 0))
    unparse (T.Barline 0) = ";"
    unparse (T.Barline n) = Text.replicate n "|"

-- ** Note

-- | Parse a note with a letter pitch.
--
-- > a a2 call/a2
-- > a2.
-- > a~ a2~
-- > "call with spaces"/
instance Element T.Note where
    parse = do
        call <- P.optional $ P.try $ parse <* P.char '/'
        pitch <- parse
        dur <- parse
        let note = T.Note (fromMaybe (T.Call "") call) pitch dur
        -- If I allow "" as a note, I can't get P.many of them.
        guard (note /= empty_note)
        return note
    unparse (T.Note call pitch dur) = mconcat
        [ if call == T.Call "" then "" else unparse call <> "/"
        , unparse pitch
        , unparse dur
        ]

empty_note :: T.Note
empty_note = T.Note (T.Call "") (T.Pitch (T.Relative 0) "")
    (T.Duration Nothing 0 False)

-- |
-- > word-without-slash
-- > "word with spaces"
-- > "with embedded "() quote"
instance Element T.Call where
    parse = (<?> "call") $ fmap T.Call $
        p_string <|> P.takeWhile1P Nothing (`notElem` [' ', '/'])
    unparse (T.Call call)
        | Text.any (`elem` [' ', '/']) call = "\"" <> call <> "\""
        | otherwise = call

p_string :: Parser Text
p_string = fmap mconcat $ P.between (P.char '"') (P.char '"') $
    P.many (P.try (P.string "\"(") <|> (Text.singleton <$> P.satisfy (/='"')))

un_string :: Text -> Text
un_string str = "\"" <> str <> "\""

instance Element T.Rest where
    -- TODO I could possibly forbid ~ tie for rests, but I don't see why
    parse = T.Rest <$> (P.char '_' *> parse)
    unparse (T.Rest dur) = "_" <> unparse dur

-- ** Pitch

instance Element T.Pitch where
    parse = T.Pitch <$> parse <*> (P.takeWhileP Nothing Char.isLetter)
        <?> "pitch"
    unparse (T.Pitch octave call) = unparse octave <> call

instance Element T.Octave where
    parse = T.Absolute <$> p_int <|> T.Relative <$> p_relative <?> "octave"
        where
        p_relative = Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhileP Nothing (`elem` (",'" :: String))
    unparse (T.Absolute oct) = showt oct
    unparse (T.Relative n)
        | n >= 0 = Text.replicate n "'"
        | otherwise = Text.replicate (-n) ","

-- ** Duration

instance Element T.Duration where
    parse = T.Duration
        <$> P.optional p_nat
        <*> (Text.length <$> P.takeWhileP Nothing (=='.'))
        <*> P.option False (P.char '~' *> pure True)
        <?> "duration"
    unparse (T.Duration dur dots tie) = mconcat
        [ maybe "" showt dur
        , Text.replicate dots "."
        , if tie then "~" else ""
        ]

-- ** util

p_whitespace :: Bool -> Parser ()
p_whitespace required = do
    if required then P.eof <|> P.space1 else P.space
    P.option () $ do
        P.string "--"
        P.takeWhileP Nothing (/='\n')
        P.option () (void $ P.char '\n')

p_space :: Parser ()
p_space = void $ P.takeWhile1P Nothing (==' ')

lexeme :: Parser a -> Parser a
lexeme = (<* p_whitespace False)

keyword :: Text -> Parser ()
keyword str = void $ lexeme (P.string str)

-- ** from Util.Parse, but with megaparsec

p_int :: Parser Int
p_int = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    (*sign) <$> p_nat
    <?> "int"

-- | Natural number including 0.
p_nat :: Parser Int
p_nat = do
    i <- P.takeWhile1P Nothing (\c -> '0' <= c && c <= '9')
    case Numeric.readDec (untxt i) of
        (n, _) : _ -> return n
        _ -> mzero -- this should never happen
    <?> "nat"
