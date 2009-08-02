-- | Simple macro substitution.
-- Add SrcPos (Just (line, Just func_name, lineno)) args to certain functions.
--
-- I don't use cpp because it doesn't like dots in symbols.
import qualified Data.List as List
import qualified Data.Char as Char
import qualified System.Environment
import qualified Text.Regex as Regex


-- These are substituted everywhere.
global_subs = map (\s -> (s, s++"_srcpos"))
    [ "Log.msg", "Log.debug", "Log.notice", "Log.warn", "Log.error", "Log.timer"
    , "Log.debug_stack", "Log.notice_stack", "Log.warn_stack", "Log.error_stack"
    , "Derive.throw", "Derive.warn"
    ]

-- These are only substituted in test modules.
test_subs = map (\s -> (s, s++"_srcpos"))
    ["equal", "strings_like", "io_human", "throws", "check", "check_msg"]

main = do
    args <- System.Environment.getArgs
    [orig_fn, src_fn, dest_fn] <- if length args == 3 then return args
        else error $ "usage: hspp <orig_fn> <src_fn> <dest_fn>"
    input <- readFile src_fn
    let subs = if "_test.hs" `List.isSuffixOf` orig_fn
            then test_subs else global_subs

    writeFile dest_fn (process orig_fn subs input)


process fn subs = unlines . (line_pragma:)
    . map (\(i, func, line) -> replace_ids (make_replace fn i func subs) line)
    . annotate . lines
    where line_pragma = "{-# LINE 1 \"" ++ fn ++ "\" #-}"

annotate :: [String] -> [(Integer, Maybe String, String)]
annotate lines = zip3 [1..] (annotate_func_names lines) lines

make_replace :: FilePath -> Integer -> Maybe String -> [(String, String)]
    -> String -> String
make_replace fn lineno func_name subs tok = case lookup tok subs of
    Nothing -> tok
    Just repl -> repl ++ " (" ++ make_srcpos fn func_name lineno ++ ")"

make_srcpos :: FilePath -> Maybe String -> Integer -> String
make_srcpos fn func_name lineno = show (Just (fn, func_name, lineno))


lex_dot s = case lex s of
    [(tok1, '.':rest1)] ->
        [(tok1 ++ "." ++ tok2, rest2) | (tok2, rest2) <- lex_dot rest1]
    val -> val

replace_ids :: (String -> String) -> String -> String
replace_ids _ "" = ""
replace_ids replace contents = spaces ++ tok2 ++ replace_ids replace after_tok
    where
    (spaces, rest) = span Char.isSpace contents
    (tok, after_tok) = case lex_dot rest of
        (tok, rest) : _ -> (tok, rest)
        _ -> ("", "")
    tok2 = replace tok

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
        Just groups -> error $ "unexpected groups: " ++ show groups
    where
    reg = Regex.mkRegex "^([a-z0-9_][A-Za-z0-9_]*) .*="
