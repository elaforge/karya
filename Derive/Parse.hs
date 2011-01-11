module Derive.Parse where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<?>))

import Util.Control
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch

import qualified Derive.TrackLang as TrackLang


-- | The only operator is @|@, so a list of lists suffices for an AST.
type Expr = [Call]
data Call = Call TrackLang.CallId [Term] deriving (Eq, Show)
data Term = ValCall Call | Literal TrackLang.Val deriving (Eq, Show)

-- | Convenient constructor for Call.  Not to be confused with 'call0'--calln.
--
-- TODO I should be able to have different types of vals, but I think I need an
-- existential wrapper for that, or an infix operator.
call :: String -> Call
call sym = Call (TrackLang.Symbol sym) []

val_call :: String -> Term
val_call sym = ValCall (Call (TrackLang.Symbol sym) [])

-- | The returned Expr is never null.
parse :: String -> Either String Expr
parse text = Parse.parse_all p_pipeline (strip_comment text)

-- | Parse a control track title.  The first expression in the composition is
-- parsed simply as a list of values, not a Call.  Control track titles don't
-- follow the normal calling process but pattern match directly on vals.
parse_control_title :: String -> Either String ([TrackLang.Val], Expr)
parse_control_title text = Parse.parse_all p_control_title (strip_comment text)

p_control_title :: P.Parser ([TrackLang.Val], Expr)
p_control_title = do
    vals <- P.many (Parse.lexeme p_val)
    expr <- P.option [] (Parse.symbol "|" >> p_pipeline)
    return (vals, expr)

parse_val :: String -> Either String TrackLang.Val
parse_val = Parse.parse_all (Parse.lexeme p_val)

p_pipeline :: P.Parser Expr
p_pipeline = P.sepBy p_expr (Parse.symbol "|")

p_expr, p_equal :: P.Parser Call
p_expr = P.try p_equal <|> P.try p_call <|> p_null_call
p_equal = do
    a1 <- p_val
    P.skipMany1 P.space
    P.char '='
    -- This ensures that "a =b" is not interpreted as an equal expression.
    P.skipMany1 P.space
    a2 <- p_term
    return $ Call TrackLang.c_equal [Literal a1, a2]

p_call :: P.Parser Call
p_call = Call <$> Parse.lexeme p_call_symbol <*> P.many p_term

p_null_call :: P.Parser Call
p_null_call = return (Call (TrackLang.Symbol "") []) <?> "null call"

-- | Any word in call position is considered a Symbol.  This means that
-- you can have calls like @4@ and @>@, which are useful names for notes or
-- ornaments.
p_call_symbol :: P.Parser TrackLang.Symbol
p_call_symbol = TrackLang.Symbol <$> p_word

strip_comment :: String -> String
strip_comment = fst . Seq.break_tails ("--" `List.isPrefixOf`)

p_term :: P.Parser Term
p_term = Parse.lexeme $ Literal <$> p_val <|> ValCall <$> p_sub_call

p_sub_call :: P.Parser Call
p_sub_call = P.between (P.char '(') (P.char ')') p_call

p_val :: P.Parser TrackLang.Val
p_val =
    TrackLang.VInstrument <$> p_instrument
    -- RelativeAttr and Num can both start with a '-', but an RelativeAttr has
    -- to have a letter afterwards, while a Num is a '.' or digit, so they're
    -- not ambiguous.
    <|> TrackLang.VRelativeAttr <$> P.try p_rel_attr
    <|> TrackLang.VNum <$> p_num
    <|> TrackLang.VString <$> p_string
    <|> TrackLang.VControl <$> p_control
    <|> TrackLang.VPitchControl <$> p_pitch_control
    <|> TrackLang.VScaleId <$> p_scale_id
    <|> (P.char '_' >> return TrackLang.VNotGiven)
    <|> TrackLang.VSymbol <$> p_symbol

p_num :: P.Parser Double
p_num = Parse.p_float

p_string :: P.Parser String
p_string = p_single_string <?> "string"

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
p_rel_attr :: P.Parser TrackLang.RelativeAttr
p_rel_attr = do
    let as m c = const m <$> P.char c
    mode <- P.choice
        [as TrackLang.Add '+', as TrackLang.Remove '-', as TrackLang.Set '=']
    attr <- case mode of
        TrackLang.Set -> (P.char '-' >> return "") <|> p_ident ""
        _ -> p_ident ""
    return $ TrackLang.RelativeAttr
        (if null attr then TrackLang.Clear else mode, attr)
    <?> "relative attr"

p_control :: P.Parser TrackLang.Control
p_control = do
    P.char '%'
    control <- Score.Control <$> p_ident ","
    deflt <- Parse.optional (P.char ',' >> Parse.p_float)
    return $ case deflt of
        Nothing -> TrackLang.Control control
        Just val -> TrackLang.DefaultedControl control val
    <?> "control"

p_pitch_control :: P.Parser TrackLang.PitchControl
p_pitch_control = do
    P.char '#'
    control <- Score.Control <$> P.option "" (p_ident ",")
    deflt <- Parse.optional (P.char ',' >> p_word)
    return $ case deflt of
        Nothing -> TrackLang.Control control
        Just val -> TrackLang.DefaultedControl control (Pitch.Note val)
    <?> "pitch control"

p_scale_id :: P.Parser Pitch.ScaleId
p_scale_id = do
    P.char '*'
    Pitch.ScaleId <$> P.option "" (p_ident "")
    <?> "scale id"

p_instrument :: P.Parser Score.Instrument
p_instrument = P.char '>' >> Score.Instrument <$> p_null_word
    <?> "instrument"

-- | Symbols can have anything in them but they have to start with a letter.
-- This means special literals can start with wacky characters and not be
-- ambiguous.
--
-- They can also start with a *.  This is a special hack to support *scale
-- syntax in pitch track titles, but who knows, maybe it'll be useful in other
-- places too.
p_symbol :: P.Parser TrackLang.Symbol
p_symbol = do
    c <- P.letter <|> P.char '*'
    rest <- p_null_word
    return $ TrackLang.Symbol (c : rest)

-- | Identifiers are somewhat more strict than usual.  They must be lowercase,
-- and the only non-letter allowed is hyphen.  This means words must be
-- separated with hyphens, and leaves me free to give special meanings to
-- underscores or caps if I want.
--
-- @until@ gives additional chars that stop parsing, for idents that are
-- embedded in another lexeme.
p_ident :: String -> P.Parser String
p_ident until = do
    initial <- P.lower
    -- This forces identifiers to be separated with spaces, except with the |
    -- operator.  Otherwise @sym>inst@ is parsed as a call @sym >inst@, which
    -- seems like something I don't want to support.
    rest <- P.many (P.noneOf (" |=" ++ until))
    let ident = initial : rest
    when (not (valid_identifier ident)) $
        P.unexpected $ "chars in identifier; only [a-z0-9-] are accepted: "
            ++ show ident
    return ident

valid_identifier :: String -> Bool
valid_identifier = all $ \c -> Char.isLower c || Char.isDigit c || c == '-'

p_single_string :: P.Parser String
p_single_string = P.between (P.char '\'') (P.char '\'') $
    P.many (P.noneOf "'" <|> (P.try (P.string "''") >> return '\''))

p_word, p_null_word :: P.Parser String
p_word = P.many1 (P.noneOf " ()")
p_null_word = P.many (P.noneOf " ()")
