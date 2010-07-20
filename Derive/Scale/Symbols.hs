-- | Scales can keep their symbols with them in the 'Pitch.Scale.scale_symbols'
-- field, but there are a number of symbols that are common between a number of
-- scales.  If I define them once here I can avoid loading them redundantly.
module Derive.Scale.Symbols where

import qualified Ui.Symbol as Symbol
import qualified Perform.Pitch as Pitch


symbols :: [Symbol.Symbol]
symbols = dotted_numbers

-- * dotted numbers

dotted_number :: (Int, Int) -> Pitch.Note
dotted_number (num, oct)
    | oct == 0 = Pitch.Note nums
    | oct < 0 = Pitch.Note $ "`" ++ nums ++ replicate (abs oct) '.' ++ "`"
    | otherwise = Pitch.Note $ "`" ++ nums ++ replicate oct '^' ++ "`"
    where nums = show num

dot :: String
dot = "•" -- unicode \x2022

dot_above :: String -> Symbol.Symbol
dot_above s = Symbol.Symbol (s ++ "^") (Just (0.6, 1.4))
    [Symbol.glyph s, Symbol.Glyph dot Nothing 0 (0.2, -0.6)]

dot2_above :: String -> Symbol.Symbol
dot2_above s = Symbol.Symbol (s ++ "^^") (Just (0.6, 1.4))
    [Symbol.glyph s,
        Symbol.Glyph dot Nothing 0 (0, -0.6),
        Symbol.Glyph dot Nothing 0 (0.4, -0.6)]

dot_below :: String -> Symbol.Symbol
dot_below s = Symbol.Symbol (s ++ ".") (Just (0.6, 1.2))
    [Symbol.Glyph s Nothing 0 (0, -0.2),
        Symbol.Glyph dot Nothing 0 (0.2, 0.5)]

dot2_below :: String -> Symbol.Symbol
dot2_below s = Symbol.Symbol (s ++ "..") (Just (0.6, 1.2))
    [Symbol.Glyph s Nothing 0 (0, -0.2)
        Symbol.Glyph dot Nothing 0 (0, 0.5),
        Symbol.Glyph dot Nothing 0 (0.4, 0.5)]

dotted_numbers :: [Symbol.Symbol]
dotted_numbers = map dot_above cs ++ map dot2_above
        cs ++ map dot_below cs ++ map dot2_below cs
    where
    -- If some scale wants higher numbers, they are easy to add.
    cs = map show [1..12]
