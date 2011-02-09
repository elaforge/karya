-- | Simple macro substitution.
-- Add SrcPos (Just (line, Just func_name, lineno)) args to certain functions.
--
-- I don't use cpp because it doesn't like dots in symbols.
--
-- I need to replace only function calls, but can't know what those are without
-- actually parsing the source.  I think replacement of qualified names is
-- pretty safe since those don't really occur otherwise, but replacing
-- unqualified names in the same module is tricky.
--
-- Known problems:
--
-- - \"where\" in the module comment will make macros in the module export list
-- be replaced
--
-- - mangles comments and replaces macros in them, but I don't really care
-- about that
--
-- - Since annotations are per-line, @x = x@ won't replace @x@, but that's
-- a silly definition anyway.
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
    [ "msg", "initialized_msg"
    , "debug", "notice", "warn", "error", "timer"
    , "debug_stack", "notice_stack", "warn_stack", "error_stack"
    ]
    ++ map (SrcposMacro (Just "Derive.Derive"))
    [ "throw", "throw_error", "throw_arg_error"
    ]

-- These are only substituted in test modules.
test_macros :: [Macro]
test_macros = map (SrcposMacro Nothing)
    [ "equal", "strings_like", "left_like", "io_human", "throws", "check"
    , "check_msg"]


main :: IO ()
main = do
    args <- System.Environment.getArgs
    [orig_fn, src_fn, dest_fn] <- if length args == 3 then return args
        else error $ "usage: hspp <orig_fn> <src_fn> <dest_fn>"
    input <- readFile src_fn
    let macros = if "_test.hs" `List.isSuffixOf` orig_fn
            then test_macros else global_macros
    writeFile dest_fn (process (clean_fn orig_fn) macros input)

clean_fn :: FilePath -> FilePath
clean_fn fn
    -- ghc tends to pass filenames with leading ./
    | "./" `List.isPrefixOf` fn = drop 2 fn
    | otherwise = fn

process :: FilePath -> [Macro] -> String -> String
process fn macros = unlines . (line_pragma:)
    . map (\(line, annot) -> replace_ids (replace macros fn annot) line)
    . annotate . smart_lines
    where line_pragma = "{-# LINE 1 \"" ++ fn ++ "\" #-}"

-- | Replace a token with the version applied to the srcpos.
--
-- This is tricky because when I am in other modules, I want to replace only
-- the qualified function call \"A.f\".  If I am replacing a function in the
-- same module as it is defined, I want to replace non-qualified, but must
-- be careful to only replace function calls.  Not definitions, not export
-- lists!
replace :: [Macro] -> FilePath -> Annotation -> String -> String
replace macros fn annot tok
    | a_declaration annot || not (a_post_header annot) = tok
    | otherwise = case find_macro macros func_name (fn_to_module fn) tok of
        Nothing -> tok
        Just repl -> "(" ++ repl ++ " (" ++ srcpos ++ "))"
    where
    func_name = a_func_name annot
    srcpos = make_srcpos fn (a_func_name annot) (a_line annot)

find_macro :: [Macro] -> Maybe String -> String -> String -> Maybe String
find_macro macros func_name mod token
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

make_srcpos :: FilePath -> Maybe String -> Int -> String
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


data Annotation = Annotation {
    a_line :: Int
    -- | If function name if this token is inside a function.
    , a_func_name :: Maybe String
    -- | True if left of @::@ or in an import list.
    , a_declaration :: Bool
    -- | Line is after the module's @where@
    , a_post_header :: Bool
    } deriving (Show)

annotate :: [String] -> [(String, Annotation)]
annotate lines =
    zip lines (tail (scanl scan_line (Annotation 0 Nothing False False) lines))

scan_line :: Annotation -> String -> Annotation
scan_line old_annot line = Annotation
    (a_line old_annot + 1)
    (maybe (a_func_name old_annot) Just (line_to_func line))
    (Regex.matches declaration line || "import " `List.isPrefixOf` line)
    -- fooled by 'where' in comment or inside another word
    -- doesn't work at all for modules without a name or imports
    (a_post_header old_annot || "where" `List.isInfixOf` line
        || "import" `List.isPrefixOf` line)

line_to_func :: String -> Maybe String
line_to_func line = case Regex.find_groups definition line of
        [] -> Nothing
        (_, [fname]) : _ -> Just fname
        groups -> error $ "unexpected groups: " ++ show groups

definition = Regex.make "^([a-z0-9_][A-Za-z0-9_]*) .*="
declaration = Regex.make "^[a-z0-9_][A-Za-z0-9_, ]* *::"


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
            find_macro [SrcposMacro macro_mod "f"] (Just "qq") mod token
    -- If module isn't set, look for 'f'.
    check (not (f "A.B" "B.f" Nothing))
    check (f "A.B" "f" Nothing)

    -- In another module, look for 'B.f'
    check (f "Q" "B.f" (Just "A.B"))
    check (not (f "Q" "f" (Just "A.B")))

    -- In same module, look for 'f'
    check (f "A.B" "f" (Just "A.B"))
    check (not (f "A.B" "B.f" (Just "A.B")))

test_annotate = do
    pprint $ annotate ["module X (", "foo", ") where",
        "foo = bar", "bar, buz :: Baz", "bar a b = a b", "x = 10"]

test_smart_lines = do
    let f = smart_lines
    equal (f "1\n2") ["1", "2"]
    equal (f "1 \"hello\\\n  \\there\" f\n2\n")
        ["1 \"hello\\\n  \\there\" f", "2"]
