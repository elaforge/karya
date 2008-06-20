-- | Simple macro substitution.  Add SrcPos (Just (line, lineno)) args to
-- certain functions.
--
-- I don't use cpp because it doesn't like dots in symbols.
import Prelude hiding (lex)
import qualified Data.List as List
import qualified Data.Char as Char
import qualified System.Environment
import qualified Text.Regex as Regex

{-
-- TODO eventually do a real parser that replaces tokens with dots, but not
-- things in quotes

import qualified Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadP ((+++))
import qualified Text.Read.Lex as Lex

t0 = "hello there"
t1 = "Foo.bar there"

lex = last . ReadP.readP_to_S lex_p

lex_p = do
    c <- ReadP.satisfy $ \c -> c == '_' || Char.isAlpha c
    cs <- ReadP.many idchar_p
    return (c:cs)
idchar_p = ReadP.satisfy $ \c -> c == '.' || c == '_' || Char.isAlpha c
-}

-- These are substituted everywhere.
global_macros = map (\s -> (s, s++"_srcpos"))
    [ "Log.debug", "Log.notice", "Log.warn", "Log.error"
    , "Log.debug_stack", "Log.notice_stack", "Log.warn_stack", "Log.error_stack"
    ]

-- These are only substituted in test modules.
test_macros = map (\s -> (s, s++"_srcpos"))
    ["equal", "io_human", "throws"]

main = do
    [orig_fn, src_fn, dest_fn] <- System.Environment.getArgs
    input <- readFile src_fn
    let subs = if "_test.hs" `List.isSuffixOf` orig_fn
            then test_subs else global_subs
        line_pragma = "{-# LINE 1 \"" ++ orig_fn ++ "\" #-}"
        subs' = map ($orig_fn) subs

        func_names = annotate_func_names
        annotate lines = zip3 [1..] (annotate_func_names lines) lines

    writeFile dest_fn ((unlines . (line_pragma:)
        . map (process_line subs') . annotate . lines) input)

enumerate = zip [1..]

-- It would be more efficient to use ByteString and the new regexes, but they
-- seem really complicated and don't appear to support substitution.
global_subs = map make_sub global_macros
test_subs = map make_sub (global_macros ++ test_macros)

process_line subs (i, func_name, line) =
    foldl (.) id (map ($ (i, func_name)) subs) line

make_sub (src, dest) fn (i, func_name) line =
    -- Hack: to avoid substituting Something.equal, insist on spaces
    -- surrounding the src string.
    Regex.subRegex reg line
        (" " ++ dest ++ " (" ++ show (Just (fn, func_name, i)) ++ ") ")
    where reg = Regex.mkRegex (" " ++ src ++ " ")


annotate_func_names lines = tail (scanl scan Nothing lines)

scan last_func line = case func_name line of
    Nothing -> last_func
    Just x -> Just x

func_name line = case Regex.matchRegex reg line of
        Nothing -> Nothing
        Just [fname] -> Just fname
    where
    reg = Regex.mkRegex "^([a-z0-9_][A-Za-z0-9_]*) .*="
