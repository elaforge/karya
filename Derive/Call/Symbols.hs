-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Symbols used in calls.
module Derive.Call.Symbols where
import qualified Ui.Symbol as Symbol
import qualified App.Config as Config


symbols :: [Symbol.Symbol]
symbols = misc_symbols ++ staff_symbols

misc_symbols :: [Symbol.Symbol]
misc_symbols =
    [ Symbol.Symbol "0x" True
        [Symbol.glyph_at (-2) (0, -0.4) (Symbol.glyph "x")]
    ]

staff_symbols :: [Symbol.Symbol]
staff_symbols =
    [ symbol "tr" "\xe14d"
    , symbol "mordent" "\xe160"
    , symbol "rmordent" "\xe161"
    , symbol "mordent2" "\xe162"
    , symbol "rmordent2" "\xe163"
    , Symbol.symbol "arp-up"
        [arp, Symbol.glyph_at 8 (-0.14, -0.25) arp_arrow_up]
    , Symbol.symbol "arp-down"
        [arp, Symbol.glyph_at 8 (-0.14, 0.5) arp_arrow_down]
    , symbol "ped" "\xe181"
    ]
    where
    symbol name str = Symbol.Symbol name False [glyph str]
    glyph str = Symbol.Glyph str (Just Config.emmentaler) 4 (0, 0) 0
    arp = (glyph "\xe162") { Symbol.glyph_rotate = 90 }
    arp_arrow_down = glyph "\xe15d"
    arp_arrow_up = glyph "\xe15e"
