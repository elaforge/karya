{-# LANGUAGE CPP #-}
-- | TrackLang parsers using ByteStrings and Attoparsec.
--
-- This is faster than Parsec + Strings, but cannot handle non-ascii text.
-- I think symbols will be handled with Symbol notation, which is ascii, so
-- this shouldn't be a big deal.  Unfortunately the speed difference is not
-- that large either.
--
-- Attoparsec has support for Text now, but it's still fastest when working
-- with ByteStrings (TODO according to profiling at the time, should recheck
-- this someday).  This module uses UTF8 internally, so unicode should be
-- preserved, but most of the parsers insist on a restrictive character set.
-- Special characters should probably be written with backticks anyway.
module Derive.ParseBs (
    from_string, to_string, from_text, to_text
    , parse_expr
    , parse_control_title
    , parse_val, parse_attrs, parse_num, parse_call
    , lex1, lex

    -- * expand macros
    , expand_macros
#ifdef TESTING
    , p_equal
#endif
) where
import Prelude hiding (lex)
import qualified Control.Applicative as A (many)
import Data.Attoparsec ((<?>))
import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding as Encoding

import Util.Control hiding (Text)
import qualified Util.ParseBs as Parse
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Util

import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


type Text = Event.Text

from_string :: String -> Text
from_string = UTF8.fromString

to_string :: Text -> String
to_string = UTF8.toString

from_text :: Text.Text -> Text
from_text = Text.Encoding.encodeUtf8

to_text :: Text -> Text.Text
to_text = Ui.Util.decodeUtf8

parse_expr :: Text -> Either String TrackLang.Expr
parse_expr = parse p_pipeline

-- | Parse a control track title.  The first expression in the composition is
-- parsed simply as a list of values, not a Call.  Control track titles don't
-- follow the normal calling process but pattern match directly on vals.
parse_control_title :: Text.Text
    -> Either String ([TrackLang.Val], [TrackLang.Call])
parse_control_title = Parse.parse_all p_control_title . from_text

-- | Parse a single Val.  This takes a String since it's used with Notes and
-- Symbols, which are still Strings.
parse_val :: Text.Text -> Either String TrackLang.Val
parse_val = Parse.parse_all (lexeme p_val) . from_text

-- | Parse attributes in the form +a+b.
parse_attrs :: String -> Either String Score.Attributes
parse_attrs = parse (A.option '+' (A.char '+') *> p_attrs) . from_string

-- | Parse a number or hex code, without a type suffix.
parse_num :: Text.Text -> Either String Signal.Y
parse_num = Parse.parse_all (lexeme (p_hex <|> p_untyped_num)) . from_text

-- | Extract only the call part of the text.
parse_call :: Text -> Maybe Text.Text
parse_call text = case parse_expr text of
    Right expr -> case NonEmpty.last expr of
        TrackLang.Call (TrackLang.Symbol call) _ -> Just call
    _ -> Nothing

parse :: A.Parser a -> Text -> Either String a
parse p = Parse.parse_all (spaces >> p)

-- * lex

-- | Lex out a single expression.  This isn't really a traditional lex, because
-- it will extract a whole parenthesized expression instead of a token.
lex1 :: Text -> (Text, Text)
lex1 text = case parse ((,) <$> p_lex1 <*> A.takeWhile (const True)) text of
    Right ((), rest) -> (B.take (B.length text - B.length rest) text, rest)
    Left _ -> (text, "")

lex :: Text -> [Text]
lex text
    | B.null pre = []
    | B.null post = [pre]
    | otherwise = pre : lex post
    where (pre, post) = lex1 text

-- | Attoparsec doesn't keep track of byte position, and always backtracks.
-- I think this means I can't reuse 'p_term'.
p_lex1 :: A.Parser ()
p_lex1 = (str <|> parens <|> word) >> spaces
    where
    str = p_single_string >> return ()
    parens = do
        A.char '('
        A.many $ parens <|> str <|> (A.takeWhile1 content_char >> return ())
        A.char ')'
        return ()
    word = A.skipWhile (\c -> c /= '(' && is_word_char c)
    content_char c = c /= '(' && c /= ')' && c /= '\''

-- * expand macros

-- | Map the identifiers after a \"\@\" through the given function.  Used
-- to implement ID macros for the REPL.
expand_macros :: (String -> String) -> String -> Either String String
expand_macros replacement str
    | '@' `notElem` str = Right str
    | otherwise = Parse.parse_all (to_string <$> p_macros replacement) text
    where text = from_string str

p_macros :: (String -> String) -> A.Parser Text
p_macros replacement = do
    chunks <- A.many1 $ p_macro replace <|> p_chunk <|> p_hs_string
    return $ mconcat chunks
    where
    p_chunk = A.takeWhile1 (\c -> c /= '"' && c /= '@')
    replace = from_string . replacement . to_string

p_macro :: (Text -> Text) -> A.Parser Text
p_macro replacement = do
    A.char '@'
    replacement <$> A.takeWhile1 (\c -> Id.is_strict_id_char c || c == '/')

p_hs_string :: A.Parser Text
p_hs_string = fmap (\s -> "\"" <> s <> "\"") $
    Parse.between (A.char '"') (A.char '"') $ mconcat <$> A.many chunk
    where
    chunk = (A.char '\\' >> B.cons '\\' <$> A.take 1)
        <|> A.takeWhile1 (\c -> c /= '"' && c /= '\\')

-- * toplevel parsers

p_control_title :: A.Parser ([TrackLang.Val], [TrackLang.Call])
p_control_title = do
    vals <- A.many (lexeme p_val)
    expr <- A.option [] (p_pipe >> NonEmpty.toList <$> p_pipeline)
    return (vals, expr)

p_pipeline :: A.Parser TrackLang.Expr
p_pipeline = do
    -- It definitely matches at least one, because p_null_call always matches.
    c : cs <- A.sepBy1 p_expr p_pipe
    return $ c :| cs

p_expr :: A.Parser TrackLang.Call
p_expr = A.try p_equal <|> A.try (p_call True) <|> p_null_call

p_pipe :: A.Parser ()
p_pipe = void $ lexeme (A.char '|')

p_equal :: A.Parser TrackLang.Call
p_equal = do
    a1 <- p_val
    spaces1
    A.char '='
    -- This ensures that "a =b" is not interpreted as an equal expression.
    spaces1
    a2 <- p_term
    return $ TrackLang.Call TrackLang.c_equal [TrackLang.Literal a1, a2]

p_call :: Bool -> A.Parser TrackLang.Call
p_call toplevel =
    TrackLang.Call <$> lexeme (p_call_symbol toplevel) <*> A.many p_term

p_null_call :: A.Parser TrackLang.Call
p_null_call = return (TrackLang.Call (TrackLang.Symbol "") []) <?> "null call"

-- | Any word in call position is considered a Symbol.  This means that
-- you can have calls like @4@ and @>@, which are useful names for notes or
-- ornaments.
p_call_symbol :: Bool -- ^ A call at the top level can allow a ).
    -> A.Parser TrackLang.Symbol
p_call_symbol toplevel = TrackLang.Symbol . to_text <$>
    (if toplevel then A.takeWhile1 (/=' ') else p_word)

p_term :: A.Parser TrackLang.Term
p_term = lexeme $
    TrackLang.Literal <$> p_val <|> TrackLang.ValCall <$> p_sub_call

p_sub_call :: A.Parser TrackLang.Call
p_sub_call = Parse.between (A.char '(') (A.char ')') (p_call False)

p_val :: A.Parser TrackLang.Val
p_val =
    TrackLang.VInstrument <$> p_instrument
    -- RelativeAttrs and Num can both start with a '-', but an RelativeAttrs
    -- has to have a letter afterwards, while a Num is a '.' or digit, so
    -- they're not ambiguous.
    <|> TrackLang.VRelativeAttrs <$> A.try p_rel_attrs
    <|> TrackLang.VNum . Score.untyped <$> p_hex
    <|> TrackLang.VNum <$> p_num
    <|> TrackLang.VSymbol <$> p_string
    <|> TrackLang.VControl <$> p_control
    <|> TrackLang.VPitchControl <$> p_pitch_control
    <|> TrackLang.VScaleId <$> p_scale_id
    <|> (A.char '_' >> return TrackLang.VNotGiven)
    <|> TrackLang.VSymbol <$> p_symbol

p_num :: A.Parser Score.TypedVal
p_num = do
    num <- p_untyped_num
    suffix <- A.option "" ((:"") <$> A.letter_ascii)
    case Score.code_to_type suffix of
        Nothing -> fail $ "p_num expected suffix in [cdsr]: " ++ show suffix
        Just typ -> return $ Score.Typed typ num

p_untyped_num :: A.Parser Signal.Y
p_untyped_num = Parse.p_float

p_hex :: A.Parser Signal.Y
p_hex = do
    A.string prefix
    let higit c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'
    c1 <- A.satisfy higit
    c2 <- A.satisfy higit
    return $ fromIntegral (parse_hex c1 c2) / 0xff
    where prefix = Encoding.encodeUtf8 ShowVal.hex_prefix

parse_hex :: Char -> Char -> Int
parse_hex c1 c2 = higit c1 * 16 + higit c2
    where
    higit c
        | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
        | otherwise = fromEnum c - fromEnum 'a' + 10

-- | A string is anything between single quotes.  A single quote itself is
-- represented by two single quotes in a row.
p_string :: A.Parser TrackLang.Symbol
p_string = TrackLang.Symbol . to_text <$> p_single_string

p_single_string :: A.Parser Text
p_single_string = do
    chunks <- A.many1 $
        Parse.between (A.char '\'') (A.char '\'') (A.takeTill (=='\''))
    return $ B.intercalate "'" chunks

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
p_rel_attrs :: A.Parser TrackLang.RelativeAttrs
p_rel_attrs =
    TrackLang.Add <$> (A.char '+' *> p_attrs)
    <|> TrackLang.Remove <$> (A.char '-' *> p_attrs)
    <|> TrackLang.Set <$> (A.char '=' *> p_attrs)
    <?> "relative attr"

p_attrs :: A.Parser Score.Attributes
p_attrs =
    Score.attrs . map to_text <$> A.sepBy1 (p_identifier "+") (A.char '+')
    <|> A.char '-' *> return mempty

p_control :: A.Parser TrackLang.ValControl
p_control = do
    A.char '%'
    control <- Score.Control . to_text <$> A.option "" (p_identifier ",")
    deflt <- Parse.optional (A.char ',' >> p_num)
    return $ case deflt of
        Nothing -> TrackLang.LiteralControl control
        Just val -> TrackLang.DefaultedControl control val
    <?> "control"

p_pitch_control :: A.Parser TrackLang.PitchControl
p_pitch_control = do
    A.char '#'
    control <- Score.Control . to_text <$> A.option "" (p_identifier ",")
    deflt <- Parse.optional (A.char ',' >> p_word)
    return $ case deflt of
        Nothing -> TrackLang.LiteralControl control
        Just val ->
            TrackLang.DefaultedControl control $
                TrackLang.Note (Pitch.Note (to_text val)) []
    <?> "pitch control"

p_scale_id :: A.Parser Pitch.ScaleId
p_scale_id = do
    A.char '*'
    Pitch.ScaleId . to_string <$> A.option "" (p_identifier "")
    <?> "scale id"

p_instrument :: A.Parser Score.Instrument
p_instrument = A.char '>' >> (Score.Instrument . to_text) <$> p_null_word
    <?> "instrument"

-- | Symbols can have anything in them but they have to start with a letter.
-- This means special literals can start with wacky characters and not be
-- ambiguous.
--
-- They can also start with a *.  This is a special hack to support *scale
-- syntax in pitch track titles, but who knows, maybe it'll be useful in other
-- places too.
p_symbol :: A.Parser TrackLang.Symbol
p_symbol = do
    c <- A.satisfy (\c -> A.isAlpha_ascii c || c == '*')
    rest <- p_null_word
    return $ TrackLang.Symbol $ Text.cons c (to_text rest)

-- | Identifiers are somewhat more strict than usual.  They must be lowercase,
-- and the only non-letter allowed is hyphen.  This means words must be
-- separated with hyphens, and leaves me free to give special meanings to
-- underscores or caps if I want.
--
-- @until@ gives additional chars that stop parsing, for idents that are
-- embedded in another lexeme.
p_identifier :: String -> A.Parser Text
p_identifier until = do
    -- TODO attoparsec docs say it's faster to do the check manually, profile
    -- and see if it makes a difference.
    ident <- A.takeWhile1 (A.notInClass (until ++ " |=)"))
    -- This forces identifiers to be separated with spaces, except with the |
    -- operator.  Otherwise @sym>inst@ is parsed as a call @sym >inst@, which
    -- seems like something I don't want to support.
    unless (is_strict_id ident) $
        fail $ "invalid chars in identifier; only [a-z0-9-] are accepted: "
            ++ show ident
    return ident

-- | ByteString versions of the ones in "Ui.Id".
is_strict_id :: Text -> Bool
is_strict_id s = not (B.null s) && Id.ascii_lower (B.head s)
    && B.all Id.is_strict_id_char s

p_word, p_null_word :: A.Parser Text
p_word = A.takeWhile1 is_word_char
p_null_word = A.takeWhile is_word_char

is_word_char :: Char -> Bool
is_word_char c = c /= ' ' && c /= ')'
    -- I need to exclude ')' because otherwise I can't tell where the symbol
    -- ends in a subcall like '(a)'.  But '(' is just fine, though it would
    -- look weird in a subcall: '(()'.

lexeme :: A.Parser a -> A.Parser a
lexeme p = p <* spaces

spaces :: A.Parser ()
spaces = do
    A.skipWhile A.isSpace
    comment <- A.option "" (A.string "--")
    unless (B.null comment) $
        A.skipWhile (const True)

spaces1 :: A.Parser ()
spaces1 = A.satisfy A.isSpace >> spaces
