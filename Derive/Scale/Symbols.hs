-- | Scales can keep their symbols with them in the 'Pitch.Scale.scale_symbols'
-- field, but there are a number of symbols that are common between a number of
-- scales.  If I define them once here I can avoid loading them redundantly.
module Derive.Scale.Symbols where

import qualified Ui.Symbol as Symbol
import qualified Perform.Pitch as Pitch


symbols :: [Symbol.Symbol]
symbols = dotted_numbers ++ staff_symbols ++ gongchepu

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
dot_above s = Symbol.Symbol (s ++ "^") True
    [Symbol.glyph s, Symbol.glyph_at dot (0.5, -0.3)]

dot2_above :: String -> Symbol.Symbol
dot2_above s = Symbol.Symbol (s ++ "^^") True
    [Symbol.glyph s,
        Symbol.glyph_at dot (-0.3, -0.3),
        Symbol.glyph_at dot (0.5, -0.3)]

dot_below :: String -> Symbol.Symbol
dot_below s = Symbol.Symbol (s ++ ".") True
    [Symbol.glyph s, Symbol.glyph_at dot (0.5, 0.3)]

dot2_below :: String -> Symbol.Symbol
dot2_below s = Symbol.Symbol (s ++ "..") True
    [Symbol.glyph s,
        Symbol.glyph_at dot (-0.3, 0.3),
        Symbol.glyph_at dot (0.5, 0.3)]

dotted_numbers :: [Symbol.Symbol]
dotted_numbers = map dot_above cs ++ map dot2_above
        cs ++ map dot_below cs ++ map dot2_below cs
    where
    -- If some scale wants higher numbers, they are easy to add.
    cs = map show [0..9]


-- * staff notation

staff_symbols :: [Symbol.Symbol]
staff_symbols =
    [ Symbol.Symbol "sharp" False [g "\xe10e" 1]
    , Symbol.Symbol "sharp2" False [g "\xe125" 2]
    , Symbol.Symbol "flat" False [g "\xe11a" 2]
    , Symbol.Symbol "flat2" False [g "\xe123" 2]
    ]
    where g str size = Symbol.Glyph str (Just "Emmentaler 11") size (0, 0)


-- * 工尺譜 gongchepu for 南管

gongchepu :: [Symbol.Symbol]
gongchepu =
    -- TODO fix names later
    [ Symbol.simple "si" "士"
    , Symbol.simple "e" "下"
    , Symbol.simple "che" "ㄨ" -- bopo \x3128 ㄨ
    , Symbol.simple "gong" "工"
    , Symbol.simple "liu" "六"

    -- 琵琶手法
    , Symbol.simple "kou" "口"
    , Symbol.simple "up" "㇀" -- \x31c0
    , Symbol.simple "L" "㇄" -- \x31c4
    -- \x2020 at least until I can make a better one
    , Symbol.Symbol "dagger" True [Symbol.Glyph "†" Nothing 4 (0, 0)]
    , Symbol.Symbol "wu1" True
        [ g wu (0, 0)
        , g dian (-0.5, 0)
        ]
    , Symbol.Symbol "wu666" True
        [ g wu (0, 0)
        , g dian (-0.5, 0)
        , g dian (0, 0.5)
        ]
    , Symbol.Symbol "wu866" True
        [ g wu (0, 0)
        , g dian (-0.5, 0.2)
        , g dian (-0.5, -0.2)
        ]

    -- meter
    , Symbol.simple "ling" "○"
    , Symbol.simple "dian" dian
    ]
    where
    g str = Symbol.Glyph str Nothing 0
    dian = "、"
    wu = "ㄨ"
