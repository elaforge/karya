-- | Simple macro substitution.  Add SrcPos (Just (line, lineno)) args to
-- certain functions.
--
-- I don't use cpp because it doesn't like dots in symbols.
module Main where
import qualified Data.List as List
import qualified System.Environment
import qualified Text.Regex as Regex

-- These are substituted everywhere.
global_macros = map (\s -> (s, s++"_srcpos"))
    ["Log.debug", "Log.notice", "Log.warn", "Log.error"]

-- These are only substituted in test modules.
test_macros = map (\s -> (s, s++"_srcpos"))
    ["equal", "io_human"]

main = do
    [orig_fn, src_fn, dest_fn] <- System.Environment.getArgs
    input <- readFile src_fn
    let subs = if "_test.hs" `List.isSuffixOf` orig_fn
            then test_subs else global_subs
        line_pragma = "{-# LINE 1 \"" ++ orig_fn ++ "\" #-}"
    let subs' = map ($orig_fn) subs
    writeFile dest_fn ((unlines . (line_pragma:)
        . map (process_line subs') . enumerate . lines) input)

enumerate = zip [1..]

-- It would be more efficient to use ByteString and the new regexes, but they
-- seem really complicated and don't appear to support substitution.
global_subs = map make_sub global_macros
test_subs = map make_sub (global_macros ++ test_macros)

process_line subs (i, line) = foldl (.) id (map ($i) subs) line

make_sub (src, dest) fn i line =
    Regex.subRegex reg line (dest ++ " (" ++ show (Just (fn, i)) ++ ")")
    where reg = Regex.mkRegex ("[[:<:]]" ++ src ++ "[[:>:]]")
