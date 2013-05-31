-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
