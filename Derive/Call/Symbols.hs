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
    [ symbol "tr" "\xe17a"
    , symbol "mordent" "\xe18c"
    , symbol "rmordent" "\xe18d"
    , symbol "mordent2" "\xe18e"
    , symbol "rmordent2" "\xe18f"
    , Symbol.symbol "arp-up"
        [arp, Symbol.glyph_at 8 (-0.14, -0.25) arp_arrow_up]
    , Symbol.symbol "arp-down"
        [arp, Symbol.glyph_at 8 (-0.14, 0.5) arp_arrow_down]
    , symbol "ped" "\xe1b7"
    ]
    where
    symbol name str = Symbol.Symbol name False [glyph str]
    glyph str = Symbol.Glyph str (Just Config.emmentaler) 4 (0, 0) 0
    arp = (glyph "\xe18e") { Symbol.glyph_rotate = 90 }
    arp_arrow_up = glyph "\xe18a"
    arp_arrow_down = glyph "\xe189"
