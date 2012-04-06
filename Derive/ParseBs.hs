{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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
    from_string, to_string, ParseExpr
    , parse_expr, parse_num_expr, parse_control_title
    , parse_val

    -- * expand macros
    , expand_macros
#ifdef TESTING
    , p_equal
#endif
) where
import qualified Control.Applicative as A (many)
import Data.Attoparsec ((<?>))
import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8

import Util.Control
import qualified Util.ParseBs as Parse
import qualified Ui.Id as Id
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Pitch as Pitch


type Text = B.ByteString

from_string :: String -> Text
from_string = UTF8.fromString

to_string :: Text -> String
to_string = UTF8.toString

-- | The returned Expr is never null.
type ParseExpr = Text -> Either String TrackLang.Expr

parse_expr, parse_num_expr :: ParseExpr
parse_expr = parse p_pipeline
parse_num_expr = parse p_num_pipeline

-- | Parse a control track title.  The first expression in the composition is
-- parsed simply as a list of values, not a Call.  Control track titles don't
-- follow the normal calling process but pattern match directly on vals.
parse_control_title :: String -> Either String ([TrackLang.Val], TrackLang.Expr)
parse_control_title =
    Parse.parse_all p_control_title . strip_comment . from_string

-- | Parse a single Val.  This takes a String since it's used with Notes and
-- Symbols, which are still Strings.
parse_val :: String -> Either String TrackLang.Val
parse_val = Parse.parse_all (Parse.lexeme p_val) . from_string

parse :: A.Parser a -> Text -> Either String a
parse p text = Parse.parse_all p (strip_comment text)

strip_comment :: Text -> Text
strip_comment = fst . B.breakSubstring "--"


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
p_macro replacement = A.char '@' >> replacement <$> p_id

p_hs_string :: A.Parser Text
p_hs_string = fmap (\s -> "\"" <> s <> "\"") $
    Parse.between (A.char '"') (A.char '"') $ mconcat <$> A.many chunk
    where
    chunk = (A.char '\\' >> B.cons '\\' <$> A.take 1)
        <|> A.takeWhile1 (\c -> c /= '"' && c /= '\\')

-- * toplevel parsers

p_control_title :: A.Parser ([TrackLang.Val], TrackLang.Expr)
p_control_title = do
    vals <- A.many (Parse.lexeme p_val)
    expr <- A.option [] (p_pipe >> p_pipeline)
    return (vals, expr)

p_pipeline :: A.Parser TrackLang.Expr
p_pipeline = A.sepBy p_expr p_pipe

p_expr :: A.Parser TrackLang.Call
p_expr = A.try p_equal <|> A.try p_call <|> p_null_call

p_num_pipeline :: A.Parser TrackLang.Expr
p_num_pipeline = A.sepBy p_num_expr p_pipe

p_pipe :: A.Parser ()
p_pipe = Parse.lexeme (A.char '|') >> return ()

-- | This is just like a 'p_expr', except that a leading number is treated
-- as a null call with a number argument rather than a call to that number.
-- This saves parsing the number once as a symbol and again as a number.
p_num_expr :: A.Parser TrackLang.Call
p_num_expr = A.try p_equal <|> p_num_call <|> A.try p_call <|> p_null_call

p_equal :: A.Parser TrackLang.Call
p_equal = do
    a1 <- p_val
    spaces
    A.char '='
    -- This ensures that "a =b" is not interpreted as an equal expression.
    spaces
    a2 <- p_term
    return $ TrackLang.Call TrackLang.c_equal [TrackLang.Literal a1, a2]
    where
    spaces = A.satisfy A.isSpace >> A.skipWhile A.isSpace

p_call :: A.Parser TrackLang.Call
p_call = TrackLang.Call <$> Parse.lexeme p_call_symbol <*> A.many p_term

p_null_call :: A.Parser TrackLang.Call
p_null_call = return (TrackLang.Call (TrackLang.Symbol "") []) <?> "null call"

p_num_call :: A.Parser TrackLang.Call
p_num_call = do
    num <- TrackLang.VNum <$> p_num
    rest <- A.many p_term
    return $ TrackLang.Call (TrackLang.Symbol "")
        (TrackLang.Literal num : rest)

-- | Any word in call position is considered a Symbol.  This means that
-- you can have calls like @4@ and @>@, which are useful names for notes or
-- ornaments.
p_call_symbol :: A.Parser TrackLang.Symbol
p_call_symbol = TrackLang.Symbol . to_string <$> p_word

p_term :: A.Parser TrackLang.Term
p_term = Parse.lexeme $
    TrackLang.Literal <$> p_val <|> TrackLang.ValCall <$> p_sub_call

p_sub_call :: A.Parser TrackLang.Call
p_sub_call = Parse.between (A.char '(') (A.char ')') p_call

p_val :: A.Parser TrackLang.Val
p_val =
    TrackLang.VInstrument <$> p_instrument
    -- RelativeAttr and Num can both start with a '-', but an RelativeAttr has
    -- to have a letter afterwards, while a Num is a '.' or digit, so they're
    -- not ambiguous.
    <|> TrackLang.VRelativeAttr <$> A.try p_rel_attr
    <|> TrackLang.VNum <$> p_num
    <|> (TrackLang.VString . to_string) <$> p_string
    <|> TrackLang.VControl <$> p_control
    <|> TrackLang.VPitchControl <$> p_pitch_control
    <|> TrackLang.VScaleId <$> p_scale_id
    <|> (A.char '_' >> return TrackLang.VNotGiven)
    <|> TrackLang.VSymbol <$> p_symbol

p_num :: A.Parser Score.TypedVal
p_num = do
    num <- Parse.p_float
    suffix <- A.option "" ((:"") <$> A.letter_ascii)
    case Score.code_to_type suffix of
        Nothing ->
            fail $ "p_num expected suffix in [cdsr]: " ++ show suffix
        Just typ -> return $ Score.Typed typ num

p_string :: A.Parser Text
p_string = p_single_string <?> "string"

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
p_rel_attr :: A.Parser TrackLang.RelativeAttr
p_rel_attr = do
    let as m c = A.char c >> return m
    mode <- A.choice
        [as TrackLang.Add '+', as TrackLang.Remove '-', as TrackLang.Set '=']
    attr <- case mode of
        TrackLang.Set -> (A.char '-' >> return "") <|> p_identifier ""
        _ -> p_identifier ""
    return $ TrackLang.RelativeAttr
        (if B.null attr then TrackLang.Clear else mode, to_string attr)
    <?> "relative attr"

p_control :: A.Parser TrackLang.ValControl
p_control = do
    A.char '%'
    control <- Score.Control . to_string <$> A.option "" (p_identifier ",")
    deflt <- Parse.optional (A.char ',' >> p_num)
    return $ case deflt of
        Nothing -> TrackLang.LiteralControl control
        Just val -> TrackLang.DefaultedControl control val
    <?> "control"

p_pitch_control :: A.Parser TrackLang.PitchControl
p_pitch_control = do
    A.char '#'
    control <- Score.Control . to_string <$> A.option "" (p_identifier ",")
    deflt <- Parse.optional (A.char ',' >> p_word)
    return $ case deflt of
        Nothing -> TrackLang.LiteralControl control
        Just val ->
            TrackLang.DefaultedControl control
                (TrackLang.Note (Pitch.Note (to_string val)) [])
    <?> "pitch control"

p_scale_id :: A.Parser Pitch.ScaleId
p_scale_id = do
    A.char '*'
    Pitch.ScaleId . to_string <$> A.option "" (p_identifier "")
    <?> "scale id"

p_instrument :: A.Parser Score.Instrument
p_instrument = A.char '>' >> (Score.Instrument . to_string) <$> p_null_word
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
    return $ TrackLang.Symbol (c : to_string rest)

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
    ident <- A.takeWhile1 (A.notInClass (until ++ " |="))
    -- This forces identifiers to be separated with spaces, except with the |
    -- operator.  Otherwise @sym>inst@ is parsed as a call @sym >inst@, which
    -- seems like something I don't want to support.
    unless (is_strict_id ident) $
        fail $ "invalid chars in identifier; only [a-z0-9-] are accepted: "
            ++ show ident
    return ident

-- | Much like 'p_identifier', but for BlockId, RulerId, etc. which are
-- more permissive.
p_id :: A.Parser Text
p_id = do
    ident <- A.takeWhile1 (A.notInClass " |=")
    unless (is_id ident) $
        fail $ "invalid chars in identifier; only [a-z0-9`.-] are accepted: "
            ++ show ident
    return ident

-- | ByteString versions of the ones in "Ui.Id".
is_strict_id :: Text -> Bool
is_strict_id s = not (B.null s) && Id.ascii_lower (B.head s)
    && B.all Id.is_strict_id_char s

is_id :: Text -> Bool
is_id s = not (B.null s) && B.all Id.is_id_char s

p_single_string :: A.Parser Text
p_single_string = do
    chunks <- A.many1 $
        Parse.between (A.char '\'') (A.char '\'') (A.takeTill (=='\''))
    return $ B.intercalate "'" chunks

p_word, p_null_word :: A.Parser Text
p_word = A.takeWhile1 _word_char
p_null_word = A.takeWhile _word_char
_word_char c = c /= ' ' && c /= '(' && c /= ')'
