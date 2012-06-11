-- | Symbols for the hang drum.
module Derive.Instrument.Symbols where

import qualified Ui.Symbol as Symbol


symbols :: [Symbol.Symbol]
symbols = hang ++ kendang

-- * hang

hang :: [Symbol.Symbol]
hang =
    [ Symbol.simple "zhong1" "中"
    , Symbol.simple "pang2" "旁"
    , Symbol.simple "da3" "打"
    , Symbol.simple "zhi3" "指"
    , Symbol.simple "shou3" "手"
    ]

-- * kendang

kendang :: [Symbol.Symbol]
kendang =
    [ Symbol.simple "circle+" "⨁" -- "\10753" -- circled plus operator
    ]
