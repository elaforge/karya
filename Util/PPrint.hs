-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE PackageImports #-}
{- | This is based on gleb.alexeev\@gmail.com's ipprint package on hackage.

    I'm not just using it directly because I want to pass custom formatting
    flags because my terminal is 80 chars wide, not the 137-whatever default.
-}
module Util.PPrint (
    pprint, pshow
    , format, format_str
    -- * util
    , record, list
) where
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Language.Haskell.Parser as Parser
import qualified Language.Haskell.Pretty as Pretty
import qualified Text.PrettyPrint as PrettyPrint
import qualified Text.Printf as Printf

import qualified Util.Strings as Strings

import           "haskell-src" Language.Haskell.Syntax


-- * showable

pprint :: Show a => a -> IO ()
pprint = putStr . pshow

-- | Pretty show.
pshow :: Show a => a -> String
pshow = format . show

-- * String

-- | Pretty up a string containing a parseable haskell value.
format :: String -> String
format = parse format_parsed

-- | Pretty up haskell value, unless it's a string, in which case return it
-- directly.
--
-- Previously I needed this in the REPL since it didn't have a way to say text
-- should be unformatted.  I don't need it any more, but it doesn't seem to be
-- hurting so I'll leave it here for now.
format_str :: String -> String
format_str = parse format_nonstr
    where
    format_nonstr m = Maybe.fromMaybe (format_parsed m) (is_str m)
    is_str (HsModule _ _ _ _ [HsPatBind _ _ (HsUnGuardedRhs rhs) _]) =
        case rhs of
            HsLit (HsString s) -> Just s
            _ -> Nothing
    is_str _ = Nothing

-- * util

-- | pprint does ok for records, but it doesn't work so well if I want to print
-- part of the record, or change the types.  A real record system for haskell
-- would probably fix this.
record :: [(String, String)] -> String
record = concatMap $ \(k, v) ->
    let s = Strings.strip v in Printf.printf "%s:%s\n" k
            (if '\n' `elem` s then '\n' : indent_lines s else ' ' : s)

indent_lines = Strings.rstrip . unlines . map (indent++) . lines
indent = "  "

list :: [String] -> String
list xs = concatMap (\(i, x) -> Printf.printf "%d. %s\n" i x)
    (zip [0 :: Int ..] xs)


-- * implementation

parse :: (HsModule -> String) -> String -> String
parse format s = case Parser.parseModule ("value = " ++ s) of
    Parser.ParseOk m -> format m
    -- The formatted version appends a newline, so the unformatted one should
    -- too.
    Parser.ParseFailed _ _  -> s ++ "\n"

format_parsed :: HsModule -> String
format_parsed = strip_boilerplate . pprint_mode

strip_boilerplate :: String -> String
strip_boilerplate = dedent . ("    "++) . strip_match "value="
    . dropWhile (/='\n') -- Strip module line and "value =".
    -- Prefix 4 spaces since this is how much will have been stripped from
    -- the first line, namely " = ", and make this line up vertically with the
    -- following lines.  If it fit on one line, it'll be "value = " which is
    -- not 4 characters but it doesn't matter because there's no following
    -- line.

strip_match :: String -> String -> String
strip_match pattern str = go pattern str
    where
    go "" s = strip s
    go _ "" = ""
    go (p:ps) s = case strip s of
        c : cs | p == c -> go ps cs
        _ -> str
    strip = dropWhile Char.isSpace

pprint_mode :: Pretty.Pretty a => a -> String
pprint_mode = Pretty.prettyPrintStyleMode pp_style Pretty.defaultMode
    where
    pp_style = PrettyPrint.style
        { PrettyPrint.ribbonsPerLine = 1, PrettyPrint.lineLength = 80 }

dedent :: String -> String
dedent s = unlines $ map (drop indent) slines
    where
    indent = minimum $ 80 : map (length . takeWhile Char.isSpace) slines
    slines = lines s
