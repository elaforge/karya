{- | Simple macro substitution.
    Add SrcPos (Just (line, Just func_name, lineno)) args to certain
    functions.

    I don't use cpp because it doesn't like dots in symbols.

    I need to replace only function calls, but can't know what those are
    without actually parsing the source.  I think replacement of qualified
    names is pretty safe since those don't really occur otherwise, but
    replacing unqualified names in the same module is tricky.

    Known problems:

    - \"where\" in the module comment will make macros in the module export
    list be replaced

    - mangles comments and replaces macros in them, but I don't really care
    about that

    - Since annotations are per-line, @x = x@ won't replace @x@, but that's
    a silly definition anyway.
-}
module Util.Hspp where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Environment
import qualified System.FilePath as FilePath

import qualified Util.Regex as Regex
import qualified Util.Seq as Seq


data Macro = SrcposMacro {
    -- | If Just, in this module the symbol will be matched unqualified, since
    -- that is how it will be called.  For functions which are always called
    -- unqualified, this is Nothing.
    m_home_module :: Maybe String
    -- | A list of possible qualifications that may prefix the symbol.
    -- Normally this is the C of \"A.B.C\", but some symbols may be imported
    -- under multiple names.
    , m_qualifications :: [String]
    -- | The symbol itself, which will get a _srcpos suffix and be supplied
    -- a SrcPos argument.
    , m_sym :: String
    } deriving (Show)

-- These are substituted everywhere.
global_macros :: [Macro]
global_macros = map (SrcposMacro (Just "Util.Log") ["Log"])
    [ "msg", "initialized_msg"
    , "debug", "notice", "warn", "error", "timer"
    , "debug_stack", "notice_stack", "warn_stack", "error_stack"
    ]
    ++ map (SrcposMacro (Just "Derive.Deriver.Internal")
            ["Derive", "Internal"])
    [ "throw", "throw_error", "throw_arg_error"
    ]

-- These are only substituted in test modules.
test_macros :: [Macro]
test_macros = map (SrcposMacro Nothing [])
    [ "equal", "equalf", "io_equal", "strings_like", "check_right", "left_like"
    , "io_human", "throws", "check" , "check_msg"
    ]


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
    matches (SrcposMacro Nothing _ sym) = unqualified_match sym
    matches (SrcposMacro (Just macro_mod) quals sym)
        | mod == macro_mod = unqualified_match sym
        | otherwise = any (token==) (qualified_symbol quals sym)
    -- Match an unqualified symbol, but not inside the function itself, other
    -- wise this would replace the function definition!
    unqualified_match sym = Just token /= func_name && token == sym
    qualified_symbol quals sym = [q ++ "." ++ sym | q <- quals]

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

definition, declaration :: Regex.Regex
definition = Regex.make "^([a-z0-9_][A-Za-z0-9_]*) .*="
declaration = Regex.make "^[a-z0-9_][A-Za-z0-9_, ]* *::"


-- * parsing

-- | A version of 'lex' that understands qualified names and template splices.
lex_dot :: String -> [(String, String)]
lex_dot s
    -- Half-ass template haskell splice detection.  Otherwise, the '' and '
    -- quotes will fail to lex.
    | "$(" `List.isPrefixOf` Seq.lstrip s =
        let (pre, post) = matching 0 (Seq.lstrip s)
        in [(pre, post)]
    | otherwise = case lex s of
        [(tok1, '.':rest1)] ->
            [(tok1 ++ "." ++ tok2, rest2) | (tok2, rest2) <- lex_dot rest1]
        val -> val

-- | Find an opening paren and break after its matching close paren.
matching :: Int -> String -> (String, String)
matching _ [] = ("", "")
matching n (c:cs) = (c:pre, post)
    where
    (pre, post) = case c of
        '(' -> matching (n+1) cs
        ')' | n <= 1 -> ("", cs)
            | otherwise -> matching (n-1) cs
        _ -> matching n cs

-- | A version of 'lines' that understands string literal backslash
-- continuation.  Actually, it just checks for trailing and leading
-- backslashes, it doesn't check to make sure they're in a string literal.
smart_lines :: String -> [String]
smart_lines "" = []
smart_lines text = line : smart_lines rest
    where
    (line, rest) = break_line text
    break_line ('\\' : '\n' : rest)
        | "\\" `List.isPrefixOf` dropWhile Char.isSpace rest =
            let (pre, post) = break_line rest in ('\\' : '\n' : pre, post)
        | otherwise = ("\\", rest)
    break_line ('\n' : rest) = ("", rest)
    break_line (c : rest) = let (pre, post) = break_line rest
        in (c : pre, post)
    break_line "" = ("", "")
