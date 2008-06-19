{- | This is stolen from gleb.alexeev\@gmail.com's ipprint package on hackage.
I'm not just using it directly because I want to pass custom formatting flags
because my terminal is 80 chars wide.
-}
module Util.PPrint where
import qualified Data.Char as Char

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Text.Read
import qualified Text.PrettyPrint as PrettyPrint


pprint :: Show a => a -> IO ()
pprint = putStrLn . pshow

is_str (HsModule _ _ _ _ [HsPatBind _ _ (HsUnGuardedRhs rhs) _]) = case rhs of
    HsLit (HsString s) -> Just s
    HsCon (Special HsUnitCon) -> Just ""
    _ -> Nothing
is_str _ = Nothing

pp_style = PrettyPrint.style
    { PrettyPrint.ribbonsPerLine = 1, PrettyPrint.lineLength = 80 }

pprint_mode = prettyPrintStyleMode pp_style defaultMode

-- Yay for copy and paste.  TODO clean this up a bit.

-- | Pretty show.
pshow :: Show a => a -> String
pshow v = dedent $ case parseModule ("value = " ++ s) of
        ParseOk m -> tidy (pprint_mode m)
        ParseFailed _ _   -> s
    where
    s = show v
    tidy x = case readPrec_to_S skipBoring 0 x of
        [((), tail)] -> "   " ++ tail
        _            -> s

-- | Pretty print the given value, unless it's a string, in which case return
-- it unchanged.
str_pshow :: Show a => a -> String
str_pshow v = dedent $ case parseModule ("value = " ++ s) of
        ParseOk m -> case is_str m of
            Just s -> s
            Nothing -> tidy (pprint_mode m)
        ParseFailed _ _   -> s
    where
    s = show v
    tidy x = case readPrec_to_S skipBoring 0 x of
        [((), tail)] -> "   " ++ tail
        _            -> s

skipBoring :: ReadPrec ()
skipBoring =
    do { Ident "value" <- lexP; Punc  "=" <- lexP; return () } <++
        do { lexP; skipBoring }


dedent s = unlines $ map (drop indent) slines
    where
    indent = minimum $ 80 : map (length . takeWhile Char.isSpace) slines
    slines = lines s
