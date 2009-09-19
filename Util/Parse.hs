-- | Parsing utilities.
module Util.Parse where
import qualified Numeric
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Error as Parsec.Error
import Text.ParserCombinators.Parsec ((<?>))

import qualified Util.Seq as Seq


parse :: P.CharParser () a -> String -> Either String a
parse parser text = case P.parse parser "" text of
    Left err -> Left (format_error text err)
    Right val -> Right val

maybe_parse :: P.CharParser () a -> String -> Maybe a
maybe_parse parser text = either (const Nothing) Just (parse parser text)

format_error :: String -> Parsec.Error.ParseError -> String
format_error text err = "parse error on char "
    ++ show (P.sourceColumn (P.errorPos err)) ++ " of " ++ show text ++ ": "
    ++ Seq.replace "\n" "; " (show_error_msgs (Parsec.Error.errorMessages err))

-- | Contrary to its documentation, showErrorMessages takes a set of strings
-- for translation, which makes it hard to use.
show_error_msgs :: [Parsec.Error.Message] -> String
show_error_msgs = Parsec.Error.showErrorMessages
    "or" "unknown parse error" "expecting" "unexpected" "end of input"

-- | Convert a parser into a lexeme parser by skipping whitespace afterwards.
lexeme :: P.CharParser st a -> P.CharParser st a
lexeme p = p >>= \v -> P.skipMany P.space >> return v

p_rest :: P.GenParser tok st t -> P.GenParser tok st (t, [tok])
p_rest p = do
    val <- p
    rest <- P.getInput
    return (val, rest)


-- | A word of non-space chars, and eat the following spaces.
p_word :: P.CharParser st String
p_word = do
    w <- P.many1 (P.noneOf " ")
    P.spaces
    return w

-- | Parse a possibly signed integer.
p_int :: P.CharParser st Integer
p_int = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    i <- P.many P.digit
    case Numeric.readDec i of
        (n, _):_ -> return (n*sign)
        _ -> P.pzero -- this should never happen
    <?> "signed int"

p_nat :: P.CharParser st Integer
p_nat = do
    i <- P.many P.digit
    case Numeric.readDec i of
        (n, _):_ -> return n
        _ -> P.pzero -- this should never happen
    <?> "natural int"

p_float :: P.CharParser st Double
p_float = do
    sign <- P.option 1 (P.char '-' >> return (-1))
    i <- P.many P.digit
    f <- P.option "" (P.char '.' >> P.many1 P.digit)
    case (i, f) of
        ("", "") -> P.pzero
        _ -> case read_float (i ++ "." ++ f) of
            [] -> P.pzero
            ((val, _rest):_) -> return (val * sign)
    <?> "float"

-- * non-parsec

float :: (RealFrac a) => String -> Maybe a
float = complete_parse . read_float

int :: (Integral a) => String -> Maybe a
int = complete_parse . read_int

-- | Read numbers of the form \"[+-]?\d*\.\d*\".  So unlike haskell, +.3 is
-- valid.
read_float :: (RealFrac a) => String -> [(a, String)]
read_float s = [(sign*i, rest) | (i, rest) <- Numeric.readFloat (add0 num)]
    where (sign, num) = read_sign s

read_int :: Integral a => String -> [(a, String)]
read_int s = [(sign*i, rest) | (i, rest) <- Numeric.readDec num]
    where (sign, num) = read_sign s

read_sign :: Num a => String -> (a, String)
read_sign s = case s of
    '+':rest -> (1, rest)
    '-':rest -> (-1, rest)
    rest -> (1, rest)

add0 ('.':s) = '0' : '.' : s
add0 s = s

complete_parse :: [(a, String)] -> Maybe a
complete_parse results = case results of
    [] -> Nothing
    ((val, ""):_) -> Just val
    _ -> Nothing
