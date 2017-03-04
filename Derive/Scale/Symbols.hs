-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Scales can keep their symbols with them in the 'Pitch.Scale.scale_symbols'
-- field, but there are a number of symbols that are common between a number of
-- scales.  If I define them once here I can avoid loading them redundantly.
module Derive.Scale.Symbols where
import qualified Data.Text as Text

import qualified Ui.Symbol as Symbol
import qualified Perform.Pitch as Pitch
import Global


symbols :: [Symbol.Symbol]
symbols = dotted_numbers <> staff_symbols <> 工尺譜

-- * dotted numbers

dotted_number :: Int -> Int -> Pitch.Note
dotted_number num oct
    | oct == 0 = Pitch.Note nums
    | oct < 0 = Pitch.Note $ "`" <> nums <> Text.replicate (abs oct) "." <> "`"
    | otherwise = Pitch.Note $ "`" <> nums <> Text.replicate oct "^" <> "`"
    where nums = showt num

dot :: Symbol.Glyph
dot = Symbol.glyph "•" -- unicode \x2022

dot_above :: Text -> Symbol.Symbol
dot_above s = Symbol.Symbol (s <> "^") True
    [Symbol.glyph s, Symbol.glyph_at 0 (0.5, -0.3) dot]

dot2_above :: Text -> Symbol.Symbol
dot2_above s = Symbol.Symbol (s <> "^^") True
    [Symbol.glyph s,
        Symbol.glyph_at 0 (-0.3, -0.3) dot,
        Symbol.glyph_at 0 (0.5, -0.3) dot]

dot_below :: Text -> Symbol.Symbol
dot_below s = Symbol.Symbol (s <> ".") True
    [Symbol.glyph s, Symbol.glyph_at 0 (0.5, 0.3) dot]

dot2_below :: Text -> Symbol.Symbol
dot2_below s = Symbol.Symbol (s <> "..") True
    [ Symbol.glyph s
    , Symbol.glyph_at 0 (-0.3, 0.3) dot
    , Symbol.glyph_at 0 (0.5, 0.3) dot
    ]

dotted_numbers :: [Symbol.Symbol]
dotted_numbers = map dot_above cs <> map dot2_above
        cs <> map dot_below cs <> map dot2_below cs
    where
    -- If some scale wants higher numbers, they are easy to add.
    cs = map showt [0..9]


-- * staff notation

staff_symbols :: [Symbol.Symbol]
staff_symbols =
    [ Symbol.symbol "#" [g "\x266f" 1]
    -- bravura: e263
    , Symbol.symbol "##" [g "\x1d12a" 10]
    , Symbol.symbol "b" [g "\x266d" 1]
    -- bravura: e264
    , Symbol.symbol "bb" [g "\x1d12b" 4]
    , Symbol.symbol "n" [g "\x266e" 1]
    ]
    where g str size = (Symbol.glyph str) { Symbol.glyph_size = size }


-- * 南管的工尺譜

工尺譜 :: [Symbol.Symbol]
工尺譜 =
    -- TODO fix names later
    [ Symbol.simple "si" "士"
    , Symbol.simple "e" "下"
    , Symbol.simple "che" "ㄨ" -- bopo \x3128 ㄨ
    , Symbol.simple "gong" "工"
    , Symbol.simple "liu" "六"

    -- 琵琶手法
    , Symbol.simple "kou" "口"
    , Symbol.simple "upstroke" "㇀" -- \x31c0
    , Symbol.simple "L" "㇄" -- \x31c4
    -- \x2020 at least until I can make a better one
    , placed "dagger" [(Symbol.glyph "†") { Symbol.glyph_size = 4 }]
    , placed "wu1"
        [ glyph_at wu (0, 0)
        , glyph_at dian (-0.5, 0)
        ]
    , placed "wu666"
        [ glyph_at wu (0, 0)
        , glyph_at dian (-0.5, 0)
        , glyph_at dian (0, 0.5)
        ]
    , placed "wu866"
        [ glyph_at wu (0, 0)
        , glyph_at dian (-0.5, 0.2)
        , glyph_at dian (-0.5, -0.2)
        ]

    -- meter
    , Symbol.simple "ling" "○"
    , Symbol.simple "dian" dian
    ]
    where
    placed name glyphs = Symbol.Symbol name True glyphs
    glyph_at str align = (Symbol.glyph str) { Symbol.glyph_align = align }
    dian = "、"
    wu = "ㄨ"
