-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Symbols for instrument-specific calls.
module Derive.Instrument.Symbols where
import qualified Ui.Symbol as Symbol


symbols :: [Symbol.Symbol]
symbols = kendang

-- | Balinese kendang notation.
kendang :: [Symbol.Symbol]
kendang =
    [ Symbol.simple "O+" "⨁" -- "\10753" -- circled plus operator
    ]
