-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Parse the tscore language:

    > %scale=sargam
    > root = %default-call [melody/0 melody]
    > melody = %dur=mult [
    >     ">inst1" | a1 | b2 [ c~ c // e f ]/ |
    >     //
    >     ">inst2" | p1 | m |
    > ]
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

import qualified Util.Control as Control
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global


-- | Parsing config.  Parsed 'T.Directive's can affect further parsing, which
-- is not very nice, but convenient for concise notation.
data Config = Config {
    -- | If true, notes with no call get the pitch as their call.
    _default_call :: !Bool
    } deriving (Show, Eq)

default_config :: Config
default_config = Config False

-- * parse

parse_score :: Text -> Either String T.Score
parse_score = parse_text (parse default_config)

parse_text :: Parser a -> Text -> Either String a
parse_text p = first P.errorBundlePretty . P.parse (p <* P.eof) ""

type Parser a = P.Parsec Void.Void Text a

get_pos :: Parser T.Pos
get_pos = T.Pos . P.stateOffset <$> P.getParserState

class Element a where
    parse :: Config -> Parser a
    unparse :: Config -> a -> Text

instance Element a => Element (T.Pos, a) where
    parse config = (,) <$> get_pos <*> parse config
    unparse config = unparse config . snd

instance Element T.Score where
    parse config = fmap T.Score $ do
        p_whitespace
        Control.loop1 config $ \loop config ->
            P.optional (lexeme (parse config)) >>= \case
                Nothing -> return []
                Just toplevel -> (toplevel:) <$>
                    loop (update_config (directives_of (snd toplevel)) config)
    unparse config (T.Score toplevels) =
        Text.unlines $ snd $ List.mapAccumL un config toplevels
        where
        un config toplevel =
            ( update_config (directives_of (snd toplevel)) config
            , unparse config toplevel
            )

update_config :: [T.Directive] -> Config -> Config
update_config directives config
    | default_call_set directives = config { _default_call = True }
    | otherwise = config

default_call_set :: [T.Directive] -> Bool
default_call_set directives =
    maybe False (maybe True (/="f")) $
    Seq.last [val | T.Directive name val <- directives, name == default_call]

default_call :: Text
default_call = "default-call"

directives_of :: T.Toplevel -> [T.Directive]
directives_of (T.ToplevelDirective d) = [d]
directives_of _ = []

instance Element T.Toplevel where
    parse config = T.ToplevelDirective <$> parse config
        <|> T.BlockDefinition <$> parse config
    unparse config (T.ToplevelDirective a) = unparse config a
    unparse config (T.BlockDefinition a) = unparse config a

-- > ns/block = %directive "block title" [ ... ]
instance Element (T.Block T.Call) where
    parse config = do
        bid <- lexeme (parse config)
        keyword "="
        directives <- P.many (lexeme (parse config))
        title <- P.option "" (lexeme p_string)
        tracks <- parse (update_config directives config)
        return $ T.Block bid directives title tracks
    unparse config (T.Block bid directives title tracks) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [unparse config bid, "="]
            , map (unparse config) directives
            , [if Text.null title then "" else un_string title]
            , [unparse (update_config directives config) tracks]
            ]

instance Element Id.BlockId where
    parse _ = do
        a <- P.takeWhile1P Nothing Id.is_id_char
        mb <- P.optional $ P.try $
            P.char '/' *> (P.takeWhile1P Nothing Id.is_id_char)
        let bid = maybe (Id.id default_namespace a)
                (\b -> Id.id (Id.namespace a) b) mb
        maybe (fail $ "invalid BlockId: " <> prettys bid) return (Id.make bid)
        <?> "BlockId"
    unparse _ = Id.show_short default_namespace . Id.unpack_id

default_namespace :: Id.Namespace
default_namespace = Id.namespace "tscore"

instance Element (T.Tracks T.Call) where
    parse config = fmap T.Tracks $
        keyword "[" *> P.sepBy1 (lexeme (parse config)) (keyword "//")
        <* keyword "]"
    unparse config (T.Tracks tracks) = Text.unwords $
        "[" : List.intersperse "//" (map (unparse config) tracks) ++ ["]"]

instance Element (T.Track T.Call) where
    parse config = T.Track
        <$> P.option "" (lexeme p_title) <*> P.some (lexeme (parse config))
        where
        p_title =
            P.char '>' *> ((">"<>) <$> P.takeWhileP Nothing Id.is_id_char)
            <|> p_space_title
        p_space_title = do
            t <- p_string
            unless (">" `Text.isPrefixOf` t) $
                fail $ "note track should start with >: " <> untxt t
            return t
    unparse config (T.Track title tokens) =
        TextUtil.join2 un_title (Text.unwords (map (unparse config) tokens))
        where
        un_title
            | " " `Text.isInfixOf` title = un_string title
            | otherwise = title

instance Element T.Directive where
    parse _ = do
        P.char '%'
        T.Directive
            <$> not_in "= \n"
            <*> P.optional (P.char '=' *> not_in " \n")
        where
        not_in :: [Char] -> Parser Text
        not_in cs = P.takeWhile1P Nothing (`notElem` cs)
    unparse _ (T.Directive lhs rhs) = "%" <> lhs <> maybe "" ("="<>) rhs

instance Element (T.Token T.Call T.Pitch T.NDuration T.Duration) where
    parse config = T.TBarline <$> get_pos <*> parse config
        <|> T.TRest <$> get_pos <*> parse config
        <|> T.TNote <$> get_pos <*> parse config
    unparse config (T.TBarline _ bar) = unparse config bar
    unparse config (T.TNote _ note) = unparse config note
    unparse config (T.TRest _ rest) = unparse config rest

instance Pretty (T.Token T.Call T.Pitch T.NDuration T.Duration) where
    pretty = unparse default_config

-- ** barline

instance Element T.Barline where
    parse _ = T.Barline <$>
        (Text.length <$> P.takeWhile1P Nothing (=='|')
            <|> (P.char ';' *> pure 0))
    unparse _ (T.Barline 0) = ";"
    unparse _ (T.Barline n) = Text.replicate n "|"

instance Pretty T.Barline where pretty = unparse default_config

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
instance Element (T.Note T.Call T.Pitch T.NDuration) where
    parse config
        -- I need this P.try because / is an empty note and I need to backtrack
        -- when it fails, to parse //.
        | _default_call config = P.try $ do
            call <- P.optional $ parse config
            (pitch, dur) <- P.option (empty_pitch, empty_duration) $ do
                P.char '/'
                (,) <$> P.option empty_pitch (P.try (parse config))
                    <*> parse config
            make_note call pitch dur
        | otherwise = do
            call <- P.optional $ P.try $ parse config <* P.char '/'
            -- I need a try, because if it starts with a number it could be
            -- an octave, or a duration.
            pitch <- P.option empty_pitch $ P.try (parse config)
            dur <- parse config
            make_note call pitch dur
        where
        make_note call pitch dur = do
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
    unparse config (T.Note call pitch _zero_dur dur _pos)
        | _default_call config =
            unparse config call
            <> if pitch == empty_pitch && dur == empty_duration then ""
                else "/" <> unparse config pitch <> unparse config dur
        | otherwise = mconcat
            [ if call == T.Call "" then "" else unparse config call <> "/"
            , unparse config pitch
            , unparse config dur
            ]

empty_note :: T.Note T.Call T.Pitch T.NDuration
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
-- > [sub // block]
-- > plain-word[sub // block]
-- > "spaces word"[sub]
instance Element T.Call where
    parse config =
        uncurry T.SubBlock <$> P.try (p_subblock config)
        <|> T.Call <$> p_string
        <|> T.Call <$> P.takeWhile1P Nothing call_char
        <?> "call"
    unparse _ (T.Call call)
        | Text.any (`elem` [' ', '/']) call = "\"" <> call <> "\""
        | otherwise = call
    unparse config (T.SubBlock prefix tracks) =
        (if Text.null prefix then "" else unparse config (T.Call prefix))
        <> unparse config tracks

instance Pretty T.Call where pretty = unparse default_config

p_subblock :: Config -> Parser (Text, T.Tracks T.Call)
p_subblock config = (,)
    <$> (p_string <|> P.takeWhile1P Nothing call_char <|> pure "")
    <*> parse config

p_string :: Parser Text
p_string = fmap mconcat $ P.between (P.char '"') (P.char '"') $
    P.many $ P.try (P.string "\"(") <|> (Text.singleton <$> P.satisfy (/='"'))

un_string :: Text -> Text
un_string str = "\"" <> str <> "\""

instance Element (T.Rest T.Duration) where
    -- I could possibly forbid ~ tie for rests, but I don't see why
    parse config = T.Rest <$> (P.char '_' *> parse config)
    unparse config (T.Rest dur) = "_" <> unparse config dur

call_char :: Char -> Bool
call_char = (`notElem` exclude)
    where
    exclude =
        [ ' ', '/'
        , '['
        , ']' -- so a Note inside a SubBlock doesn't eat the ]
        ]

pitch_char :: Char -> Bool
pitch_char c = c `notElem` exclude && not (Char.isDigit c)
    where
    -- This breaks modularity because I have to just know all the syntax that
    -- could come after, which is Duration, end of Tracks, Rest.  But the more
    -- I can get into Pitch, the more I can get into Call without needing ""s.
    exclude =
        [ ' ', '\n', '\t'
        , '~', '.' -- pitch is followed by T.NDuration
        , ':' -- T.Duration
        , '/' -- Don't mistake the T.Tracks separator for a note.
        , ']' -- end of T.Tracks
        ]

-- ** Pitch

instance Element T.Pitch where
    parse config = T.Pitch <$>
        parse config <*> P.takeWhile1P Nothing pitch_char <?> "pitch"
    unparse config (T.Pitch octave call) = unparse config octave <> call

instance Pretty T.Pitch where pretty = unparse default_config

instance Element T.Octave where
    parse _ = T.Absolute <$> p_int <|> T.Relative <$> p_relative <?> "octave"
        where
        p_relative = Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhileP Nothing (`elem` (",'" :: String))
    unparse _ (T.Absolute oct) = showt oct
    unparse _ (T.Relative n)
        | n >= 0 = Text.replicate n "'"
        | otherwise = Text.replicate (-n) ","

instance Pretty T.Octave where pretty = unparse default_config

-- ** Duration

instance Element T.NDuration where
    parse config =
        P.char '0' *> pure T.CallDuration <|> T.NDuration <$> parse config
    unparse _ T.CallDuration = "0"
    unparse config (T.NDuration a) = unparse config a

instance Element T.Duration where
    parse _ = T.Duration
        <$> P.optional p_nat
        <*> P.optional (P.char ':' *> p_nat)
        <*> (Text.length <$> P.takeWhileP Nothing (=='.'))
        <*> P.option False (P.char '~' *> pure True)
        <?> "duration"
    unparse _ (T.Duration int1 int2 dots tie) = mconcat
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
