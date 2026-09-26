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
symbols = dottedNumbers <> staffSymbols <> 工尺譜

-- * dotted numbers

dottedNumber :: Int -> Int -> Pitch.Note
dottedNumber num oct
    | oct == 0 = Pitch.Note nums
    | oct < 0 = Pitch.Note $ "`" <> nums <> Text.replicate (abs oct) "." <> "`"
    | otherwise = Pitch.Note $ "`" <> nums <> Text.replicate oct "^" <> "`"
    where nums = showt num

dot :: Symbol.Glyph
dot = Symbol.glyph "•" -- unicode \x2022

dotAbove :: Text -> Symbol.Symbol
dotAbove s = Symbol.Symbol (s <> "^") True
    [Symbol.glyph s, Symbol.glyphAt 0 (0.5, -0.3) dot]

dot2Above :: Text -> Symbol.Symbol
dot2Above s = Symbol.Symbol (s <> "^^") True
    [Symbol.glyph s,
        Symbol.glyphAt 0 (-0.3, -0.3) dot,
        Symbol.glyphAt 0 (0.5, -0.3) dot]

dotBelow :: Text -> Symbol.Symbol
dotBelow s = Symbol.Symbol (s <> ".") True
    [Symbol.glyph s, Symbol.glyphAt 0 (0.5, 0.3) dot]

dot2Below :: Text -> Symbol.Symbol
dot2Below s = Symbol.Symbol (s <> "..") True
    [ Symbol.glyph s
    , Symbol.glyphAt 0 (-0.3, 0.3) dot
    , Symbol.glyphAt 0 (0.5, 0.3) dot
    ]

dottedNumbers :: [Symbol.Symbol]
dottedNumbers = map dotAbove cs <> map dot2Above
        cs <> map dotBelow cs <> map dot2Below cs
    where
    -- If some scale wants higher numbers, they are easy to add.
    cs = map showt [0..9]


-- * staff notation

staffSymbols :: [Symbol.Symbol]
staffSymbols =
    [ Symbol.symbol "#" [g "\x266f" 1]
    -- bravura: e263
    , Symbol.symbol "##" [g "\x1d12a" 10]
    , Symbol.symbol "b" [g "\x266d" 1]
    -- bravura: e264
    , Symbol.symbol "bb" [g "\x1d12b" 4]
    , Symbol.symbol "n" [g "\x266e" 1]
    ]
    where g str size = (Symbol.glyph str) { Symbol.glyphSize = size }


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
    , placed "dagger" [(Symbol.glyph "†") { Symbol.glyphSize = 4 }]
    , placed "wu1"
        [ glyphAt wu (0, 0)
        , glyphAt dian (-0.5, 0)
        ]
    , placed "wu666"
        [ glyphAt wu (0, 0)
        , glyphAt dian (-0.5, 0)
        , glyphAt dian (0, 0.5)
        ]
    , placed "wu866"
        [ glyphAt wu (0, 0)
        , glyphAt dian (-0.5, 0.2)
        , glyphAt dian (-0.5, -0.2)
        ]

    -- meter
    , Symbol.simple "ling" "○"
    , Symbol.simple "dian" dian
    ]
    where
    placed name glyphs = Symbol.Symbol name True glyphs
    glyphAt str align = (Symbol.glyph str) { Symbol.glyphAlign = align }
    dian = "、"
    wu = "ㄨ"
