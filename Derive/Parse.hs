{- | Utilities to help parse event text.
-}
module Derive.Parse where
import Control.Monad
import qualified Data.Char as Char
import qualified Numeric

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Error as Parsec.Error
import Text.ParserCombinators.Parsec ((<?>))

import qualified Util.Seq as Seq
import qualified Derive.Derive as Derive


-- * split up args

{-
-- Split a score Event into (Event, [Arg]).
lex_event :: Score.Event -> (Score.Event, [String])
lex_event event = (event, split_args (Score.event_text event))

split_args :: String -> [String]
split_args s
    | null word = []
    | otherwise = word : split_args rest
    where
    (word, rest) = break Char.isSpace (dropWhile Char.isSpace s)
-}

-- * parse values

{-
warn_float :: (Monad m) => String -> String -> Derive.DeriveT m Double
warn_float fail_str s = do
    (val, rest) <- case read_float s of
            [] -> Derive.throw $
                fail_str ++ " can't parse float from " ++ show s
            (parse:_) -> return parse
    unless (null rest) $
        Derive.warn $ fail_str ++ " has trailing junk: " ++ show rest
    return val
-}

-- | Parse the text of an event with the given parser @p@.
parse :: (Monad m) => P.CharParser () a -> String -> Derive.DeriveT m a
parse p text = do
    (val, rest) <- case P.parse (p_rest p) "" text of
        Left err -> Derive.throw $
            "parse error on char "
            ++ show (P.sourceColumn (P.errorPos err))
            ++ " of " ++ show text ++ ": "
            ++ Seq.replace "\n" " "
                (show_error_msgs (Parsec.Error.errorMessages err))
        Right val -> return val
    unless (null rest) $
        Derive.warn $ "trailing junk: " ++ show rest
    return val

-- | Convert a parser into a lexeme parser by skipping whitespace afterwards.
lexeme :: P.CharParser st a -> P.CharParser st a
lexeme p = p >>= \v -> P.skipMany P.space >> return v

-- Contrary to its documentation, showErrorMessages takes a set of strings
-- for translation, which makes it hard to use.
show_error_msgs = Parsec.Error.showErrorMessages
    "or" "unknown parse error" "expecting" "unexpected" "end of input"

p_rest :: P.GenParser tok st t -> P.GenParser tok st (t, [tok])
p_rest p = do
    val <- p
    rest <- P.getInput
    return (val, rest)

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

parse_float :: (RealFrac a) => String -> (Maybe a, String)
parse_float s = case read_float s of
    [] -> (Nothing, s)
    ((val, rest):_) -> (Just val, rest)

read_float :: (RealFrac a) => String -> [(a, String)]
read_float ('-':s) = map (\(i, s) -> (-i, s)) (Numeric.readFloat (add0 s))
read_float s = Numeric.readFloat (add0 s)

add0 ('.':s) = '0' : '.' : s
add0 s = s

{-
require_args caller n args
    | length args /= n = Derive.throw_event $
        caller ++ " expected " ++ show n ++ " args, but got " ++ show args
    | otherwise = return args
-}
