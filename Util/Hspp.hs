-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
import Control.Monad
import qualified Data.Char as Char
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified System.Environment
import qualified System.FilePath as FilePath

import qualified Util.Regex as Regex


data Macro = Macro {
    -- | If Just, in this module the symbol will be matched unqualified, since
    -- that is how it will be called.  For functions which are always called
    -- unqualified, this is Nothing.
    m_home_module :: Maybe ModuleName
    -- | A list of possible qualifications that may prefix the symbol.
    -- Normally this is the C of \"A.B.C\", but some symbols may be imported
    -- under multiple names.
    , m_qualifications :: [String]
    -- | The symbol itself, which will get a 'srcpos_suffix' and be supplied
    -- a SrcPos argument.
    , m_sym :: String
    } deriving (Show)

type ModuleName = String
-- | The X in @import .. as X@.
type Qualification = String

-- These are substituted everywhere.
global_macros :: [Macro]
global_macros = map (Macro (Just "Util.Log") ["Log"])
    [ "msg", "initialized_msg"
    , "debug", "notice", "warn", "error", "timer"
    , "debug_stack", "notice_stack", "warn_stack", "error_stack"
    ]
    ++ map (Macro (Just "Derive.Deriver.Internal") ["Derive", "Internal"])
    [ "throw", "throw_error", "throw_arg_error"
    ]

-- These are only substituted in test modules.
test_macros :: [Macro]
test_macros = map (Macro Nothing [])
    [ "equal", "equalf", "io_equal", "strings_like", "check_right", "left_like"
    , "io_human", "throws", "check"
    ]

-- | Append this to replaced symbols, along with the extra SrcPos argument.
srcpos_suffix :: String
srcpos_suffix = "_srcpos"


main :: IO ()
main = do
    args <- System.Environment.getArgs
    [orig_fn, src_fn, dest_fn] <- if length args == 3 then return args
        else error "usage: hspp <orig_fn> <src_fn> <dest_fn>"
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
    | a_declaration annot || not (a_after_module_where annot) = tok
    | any (macro_matches func_name (fn_to_module fn) tok) macros =
        "(" <> tok <> srcpos_suffix <> " (" <> srcpos <> "))"
    | otherwise = tok
    where
    func_name = a_func_name annot
    srcpos = make_srcpos fn (a_func_name annot) (a_line annot)

macro_matches :: Maybe String -> String -> String -> Macro -> Bool
macro_matches func_name mod token macro = case macro of
    Macro Nothing _ sym -> unqualified_match sym
    Macro (Just macro_mod) quals sym
        | mod == macro_mod -> unqualified_match sym
        | otherwise -> any (token==) (qualified_symbol quals sym)
    where
    qualified_symbol quals sym = [q ++ "." ++ sym | q <- quals]
    -- Match an unqualified symbol, but not inside the function itself, other
    -- wise this would replace the function definition!
    unqualified_match sym = Just token /= func_name && token == sym

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
    -- | Line Number.
    a_line :: !Int
    -- | Set to the function name if this token is inside a function.
    , a_func_name :: !(Maybe String)
    -- | True if left of @::@ or in an import list.
    , a_declaration :: !Bool
    -- | Line is after the module's @where@
    , a_after_module_where :: !Bool
    } deriving (Show)

annotate :: [String] -> [(String, Annotation)]
annotate lines =
    zip lines (tail (scanl scan_line (Annotation 0 Nothing False False) lines))

scan_line :: Annotation -> String -> Annotation
scan_line prev line = Annotation
    { a_line = a_line prev + 1
    , a_func_name = line_to_func line `mplus` a_func_name prev
    , a_declaration =
        Regex.matches declaration line || "import " `List.isPrefixOf` line
    -- fooled by 'where' in comment or inside another word
    -- doesn't work at all for modules without a name or imports
    , a_after_module_where =
        a_after_module_where prev || "where" `List.isInfixOf` line
        || "import" `List.isPrefixOf` line
    }

line_to_func :: String -> Maybe String
line_to_func line = case Regex.groups definition line of
    [] -> Nothing
    (_, [fname]) : _ -> Just fname
    groups -> error $ "unexpected groups: " ++ show groups

definition, declaration :: Regex.Regex
definition = Regex.compileUnsafe "definition" "^([a-z_][A-Za-z0-9_']*) .*="
declaration = Regex.compileUnsafe "declaration" "^[a-z_][A-Za-z0-9_', ]* *::"


-- * parsing

-- | A version of 'lex' that understands qualified names and template splices.
lex_dot :: String -> [(String, String)]
lex_dot s
    -- Half-ass template haskell splice detection.  Otherwise, the '' and '
    -- quotes will fail to lex.
    | "$(" `List.isPrefixOf` lstrip s =
        let (pre, post) = matching 0 (lstrip s)
        in [(pre, post)]
    | otherwise = case lex s of
        [(tok1, '.':rest1)] ->
            [(tok1 ++ "." ++ tok2, rest2) | (tok2, rest2) <- lex_dot rest1]
        val -> val

lstrip :: String -> String
lstrip = dropWhile Char.isSpace

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
