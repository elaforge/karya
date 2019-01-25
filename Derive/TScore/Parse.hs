-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- |

    %scale=sargam
    root = [melody/0 melody]
    melody = %dur=mult [
        ">inst1" | a1 | b2 c |
        //
        ">inst2" | p1 | m |
    ]
-}
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

import qualified Util.TextUtil as TextUtil
import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global


-- * parse

parse_score :: Text -> Either String T.Score
parse_score = parse_text parse

parse_text :: Parser a -> Text -> Either String a
parse_text p = first P.errorBundlePretty . P.parse (p <* P.eof) ""

type Parser a = P.Parsec Void.Void Text a

get_pos :: Parser T.Pos
get_pos = T.Pos . P.stateOffset <$> P.getParserState

class Element a where
    parse :: Parser a
    unparse :: a -> Text

instance Element a => Element (T.Pos, a) where
    parse = (,) <$> get_pos <*> parse
    unparse = unparse . snd

instance Element T.Score where
    parse = p_whitespace *> (T.Score <$> P.many (lexeme parse))
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
default_namespace = Id.namespace "tscore"

instance Element T.Tracks where
    parse = fmap T.Tracks $
        keyword "[" *> P.sepBy1 (lexeme parse) (keyword "//") <* keyword "]"
    unparse (T.Tracks tracks) = Text.unwords $
        "[" : List.intersperse "//" (map unparse tracks) ++ ["]"]

instance Element T.Track where
    parse = T.Track
        <$> P.option "" (lexeme p_title) <*> P.some (lexeme parse)
        where
        p_title =
            P.char '>' *> ((">"<>) <$> P.takeWhileP Nothing Id.is_id_char)
            <|> p_space_title
        p_space_title = do
            t <- p_string
            unless (">" `Text.isPrefixOf` t) $
                fail $ "note track should start with >: " <> untxt t
            return t
    unparse (T.Track title tokens) =
        TextUtil.join2 un_title (Text.unwords (map unparse tokens))
        where
        un_title
            | " " `Text.isInfixOf` title = un_string title
            | otherwise = title

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

instance Element (T.Token T.Pitch T.NDuration T.Duration) where
    parse = T.TBarline <$> get_pos <*> parse
        <|> T.TRest <$> get_pos <*> parse
        <|> T.TNote <$> get_pos <*> parse
    unparse (T.TBarline _ bar) = unparse bar
    unparse (T.TNote _ note) = unparse note
    unparse (T.TRest _ rest) = unparse rest

instance Pretty (T.Token T.Pitch T.NDuration T.Duration) where pretty = unparse

p_tokens :: Parser [T.Token T.Pitch T.NDuration T.Duration]
p_tokens = P.some (lexeme parse)

-- ** barline

instance Element T.Barline where
    parse = T.Barline <$>
        (Text.length <$> P.takeWhile1P Nothing (=='|')
            <|> (P.char ';' *> pure 0))
    unparse (T.Barline 0) = ";"
    unparse (T.Barline n) = Text.replicate n "|"

instance Pretty T.Barline where pretty = unparse

-- ** Note

-- | Parse a note with a letter pitch.
--
-- > <call><oct><pitch><dur><dots><tie>
-- > call/  4    s      4    .     ~
-- >        ,    s      :1
--
-- oct is optional, but oct without pitch is not so useful, so pitch >=1 char.
--
-- > a a2 call/a2
-- > a2.
-- > a~ a2~
-- > "call with spaces"/
instance Element (T.Note T.Pitch T.NDuration) where
    parse = do
        call <- P.optional $ P.try $ parse <* P.char '/'
        -- I need a try, because if it starts with a number it could be
        -- an octave, or a duration.
        pitch <- P.option empty_pitch $ P.try parse
        dur <- parse
        let note = T.Note
                { note_call = fromMaybe (T.Call "") call
                , note_pitch = pitch
                , note_zero_duration = False -- TODO
                , note_duration = dur
                , note_pos = T.Pos 0
                }
        -- If I allow "" as a note, I can't get P.many of them.
        guard (note /= empty_note)
        pos <- get_pos
        return $ note { T.note_pos = pos }
    unparse (T.Note call pitch _zero_dur dur _pos) = mconcat
        [ if call == T.Call "" then "" else unparse call <> "/"
        , unparse pitch
        , unparse dur
        ]

empty_note :: T.Note T.Pitch T.NDuration
empty_note = T.Note
    { note_call = T.Call ""
    , note_pitch = empty_pitch
    , note_zero_duration = False
    , note_duration = empty_duration
    , note_pos = T.Pos 0
    }

empty_duration :: T.NDuration
empty_duration = T.NDuration $ T.Duration
    { dur_int1 = Nothing
    , dur_int2 = Nothing
    , dur_dots = 0
    , dur_tie = False
    }

empty_pitch :: T.Pitch
empty_pitch = T.Pitch (T.Relative 0) ""

-- |
-- > plain-word
-- > "ns/block"
-- > "word with spaces"
-- > "with embedded "() quote"
instance Element T.Call where
    parse = (<?> "call") $ fmap T.Call $
        p_string <|> P.takeWhile1P Nothing call_char
    unparse (T.Call call)
        | Text.any (`elem` [' ', '/']) call = "\"" <> call <> "\""
        | otherwise = call

p_string :: Parser Text
p_string = fmap mconcat $ P.between (P.char '"') (P.char '"') $
    P.many $ P.try (P.string "\"(") <|> (Text.singleton <$> P.satisfy (/='"'))

un_string :: Text -> Text
un_string str = "\"" <> str <> "\""

instance Element (T.Rest T.Duration) where
    -- I could possibly forbid ~ tie for rests, but I don't see why
    parse = T.Rest <$> (P.char '_' *> parse)
    unparse (T.Rest dur) = "_" <> unparse dur

call_char :: Char -> Bool
call_char c = c `notElem` [' ', '/']

pitch_char :: Char -> Bool
pitch_char c = c `notElem` exclude && not (Char.isDigit c)
    where
    -- This breaks modularity because I have to just know all the syntax that
    -- could come after, which is Duration, end of Tracks, Rest.  But the more
    -- I can get into Pitch, the more I can get into Call without needing ""s.
    exclude :: [Char]
    exclude =
        [ ' ', '\n', '\t'
        , '~', '.' -- pitch is followed by T.NDuration
        , ':' -- T.Duration
        , '/' -- Don't mistake the T.Tracks separator for a note.
        , ']' -- end of T.Tracks
        ]

-- ** Pitch

instance Element T.Pitch where
    parse = T.Pitch <$> parse <*> P.takeWhile1P Nothing pitch_char <?> "pitch"
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

instance Element T.NDuration where
    parse = P.char '0' *> pure T.CallDuration <|> T.NDuration <$> parse
    unparse T.CallDuration = "0"
    unparse (T.NDuration a) = unparse a

instance Element T.Duration where
    parse = T.Duration
        <$> P.optional p_nat
        <*> P.optional (P.char ':' *> p_nat)
        <*> (Text.length <$> P.takeWhileP Nothing (=='.'))
        <*> P.option False (P.char '~' *> pure True)
        <?> "duration"
    unparse (T.Duration int1 int2 dots tie) = mconcat
        [ maybe "" showt int1
        , maybe "" ((":"<>) . showt) int2
        , Text.replicate dots "."
        , if tie then "~" else ""
        ]

-- ** util

p_whitespace :: Parser ()
p_whitespace = void $ P.many $ P.space1 <|> p_comment
    where
    p_comment = do
        P.string "--"
        P.takeWhileP Nothing (/='\n')
        P.option () (void $ P.char '\n')

p_space :: Parser ()
p_space = void $ P.takeWhile1P Nothing (==' ')

lexeme :: Parser a -> Parser a
lexeme = (<* p_whitespace)

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
