-- | Symbols for instrument-specific calls.
module Derive.Instrument.Symbols where
import qualified Ui.Symbol as Symbol


symbols :: [Symbol.Symbol]
symbols = hang ++ kendang

-- | Hang drum notation.
hang :: [Symbol.Symbol]
hang =
    [ Symbol.simple "zhong1" "中"
    , Symbol.simple "pang2" "旁"
    , Symbol.simple "da3" "打"
    , Symbol.simple "zhi3" "指"
    , Symbol.simple "shou3" "手"
    ]

-- | Balinese kendang notation.
kendang :: [Symbol.Symbol]
kendang =
    [ Symbol.simple "O+" "⨁" -- "\10753" -- circled plus operator
    ]
