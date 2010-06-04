-- | Simple macro substitution.
-- Add SrcPos (Just (line, Just func_name, lineno)) args to certain functions.
--
-- I don't use cpp because it doesn't like dots in symbols.
--
-- TODO this screws up \\ string continuation
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified System.Environment
import qualified System.FilePath as FilePath

import qualified Util.Regex as Regex
import qualified Util.Seq as Seq

import Util.Test


-- | module_name, sym
data Macro = SrcposMacro (Maybe String) String
    deriving (Show)

-- These are substituted everywhere.
global_macros :: [Macro]
global_macros = map (SrcposMacro (Just "Util.Log"))
    [ "msg", "debug", "notice", "warn", "error", "timer"
    , "debug_stack", "notice_stack", "warn_stack", "error_stack"
    ]
    ++ map (SrcposMacro (Just "Derive.Derive"))
    [ "throw", "warn"
    ]

-- These are only substituted in test modules.
test_macros :: [Macro]
test_macros = map (SrcposMacro Nothing)
    [ "equal", "strings_like", "left_like", "io_human", "throws", "check"
    , "check_msg"]


main = do
    args <- System.Environment.getArgs
    [orig_fn, src_fn, dest_fn] <- if length args == 3 then return args
        else error $ "usage: hspp <orig_fn> <src_fn> <dest_fn>"
    input <- readFile src_fn
    let macros = if "_test.hs" `List.isSuffixOf` orig_fn
            then test_macros else global_macros
    writeFile dest_fn (process orig_fn macros input)

process :: FilePath -> [Macro] -> String -> String
process fn macros = unlines . (line_pragma:)
    . map (\(i, func, line) -> replace_ids (make_replace fn i func macros) line)
    . annotate . smart_lines
    where line_pragma = "{-# LINE 1 \"" ++ fn ++ "\" #-}"

make_replace :: FilePath -> Integer -> Maybe String -> [Macro]
    -> (String -> String)
make_replace fn lineno func_name macros tok =
    case find_macro func_name (fn_to_module fn) tok macros of
        Nothing -> tok
        Just repl -> "(" ++ repl ++ " (" ++ srcpos ++ "))"
    where srcpos = make_srcpos fn func_name lineno

find_macro :: Maybe String -> String -> String -> [Macro] -> Maybe String
find_macro func_name mod token macros
    | any matches macros = Just (token ++ "_srcpos")
    | otherwise = Nothing
    where
    matches (SrcposMacro Nothing sym) = unqualified_match sym
    matches (SrcposMacro (Just macro_mod) sym)
        | mod == macro_mod = unqualified_match sym
        | otherwise = token == mod_to_token macro_mod sym
    -- Match an unqualified symbol, but not inside the function itself, other
    -- wise this would replace the function definition!
    unqualified_match sym = Just token /= func_name && token == sym
    mod_to_token mod sym
        | null qualification = sym
        | otherwise = qualification ++ "." ++ sym
        where qualification = Seq.mlast "" id (Seq.split "." mod)

fn_to_module :: FilePath -> String
fn_to_module = map repl . FilePath.dropExtension
    where repl c = if c == '/' then '.' else c


make_srcpos :: FilePath -> Maybe String -> Integer -> String
make_srcpos fn func_name lineno = show (Just (fn, func_name, lineno))

replace_ids :: (String -> String) -> String -> String
replace_ids _ "" = ""
replace_ids replace contents = spaces ++ tok2 ++ replace_ids replace after_tok
    where
    (spaces, rest) = span Char.isSpace contents
    (tok, after_tok) = case lex_dot rest of
        (tok, rest) : _ -> (tok, rest)
        _ -> ("", "")
    tok2 = replace tok


-- | Annotate lines with their line numbers and func names.
annotate :: [String] -> [(Integer, Maybe String, String)]
annotate lines = zip3 [1..] (annotate_func_names lines) lines

annotate_func_names lines = tail (scanl scan Nothing lines)

scan last_func line = case func_name line of
    Nothing -> last_func
    Just x -> Just x

func_name :: String -> Maybe String
func_name line = case Regex.find_groups reg line of
        [] -> Nothing
        (_, [fname]) : _ -> Just fname
        groups -> error $ "unexpected groups: " ++ show groups
    where reg = Regex.make "^([a-z0-9_][A-Za-z0-9_]*) .*="


-- * parsing

-- | A version of 'lex' that understands qualified names.
lex_dot :: String -> [(String, String)]
lex_dot s = case lex s of
    [(tok1, '.':rest1)] ->
        [(tok1 ++ "." ++ tok2, rest2) | (tok2, rest2) <- lex_dot rest1]
    val -> val

-- | A version of 'lines' that understands string literal backslash
-- continuation.  Actually, it just checks for trailing and leading
-- backslashes, it doesn't check to make sure they're in a string literal.
smart_lines :: String -> [String]
smart_lines "" = []
smart_lines text = line : smart_lines rest
    where
    (line, rest) = break_line text
    break_line ('\\' : '\n' : rest)
        | "\\" `List.isPrefixOf` Seq.lstrip rest =
            let (pre, post) = break_line rest in ('\\' : '\n' : pre, post)
        | otherwise = ("\\", rest)
    break_line ('\n' : rest) = ("", rest)
    break_line (c : rest) = let (pre, post) = break_line rest in (c : pre, post)
    break_line "" = ("", "")


-- * tests

test_find_macro = do
    let f mod token macro_mod = Maybe.isJust $
            find_macro (Just "qq") mod token [SrcposMacro macro_mod "f"]
    -- If module isn't set, look for 'f'.
    check (not (f "A.B" "B.f" Nothing))
    check (f "A.B" "f" Nothing)

    -- In another module, look for 'B.f'
    check (f "Q" "B.f" (Just "A.B"))
    check (not (f "Q" "f" (Just "A.B")))

    -- In same module, look for 'f'
    check (f "A.B" "f" (Just "A.B"))
    check (not (f "A.B" "B.f" (Just "A.B")))

test_smart_lines = do
    let f = smart_lines
    equal (f "1\n2") ["1", "2"]
    equal (f "1 \"hello\\\n  \\there\" f\n2\n")
        ["1 \"hello\\\n  \\there\" f", "2"]
