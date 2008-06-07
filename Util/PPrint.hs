module Util.PPrint where
import qualified Data.Char as Char

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Text.Read

-- gleb.alexeev@gmail.com's ipprint package from hackage
import qualified IPPrint


pshow :: Show a => a -> String
pshow = dedent . IPPrint.pshow
pprint :: Show a => a -> IO ()
pprint = putStrLn . pshow

is_str (HsModule _ _ _ _ [HsPatBind _ _ (HsUnGuardedRhs rhs) _]) = case rhs of
    HsLit (HsString s) -> Just s
    HsCon (Special HsUnitCon) -> Just ""
    _ -> Nothing
is_str _ = Nothing

-- | Pretty print the given value, unless it's a string, in which case return
-- it unchanged.
str_pshow :: Show a => a -> String
str_pshow v = dedent $ case parseModule ("value = " ++ s) of
        ParseOk m -> case is_str m of
            Just s -> s
            Nothing -> tidy $ prettyPrint m
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
