-- | Symbols used in calls.
module Derive.Call.Symbols where
import qualified Ui.Symbol as Symbol


symbols :: [Symbol.Symbol]
symbols = staff_symbols

staff_symbols :: [Symbol.Symbol]
staff_symbols =
    [ symbol "tr" "\xe17a"
    , symbol "mordent" "\xe18c"
    , symbol "rmordent" "\xe18d"
    , symbol "mordent2" "\xe18e"
    , symbol "rmordent2" "\xe18f"
    , Symbol.symbol "arp-up" [arp, glyph_at 8 (-0.14, -0.62) arp_arrow_up]
    , Symbol.symbol "arp-down" [arp, glyph_at 8 (-0.14, 0.25) arp_arrow_down]
    ]
    where
    symbol name str = Symbol.Symbol name False [glyph str]
    glyph str = Symbol.Glyph str (Just "Emmentaler 11") 4 (0, 0) 0
    glyph_at size align g =
        g { Symbol.glyph_size = size, Symbol.glyph_align = align }

    arp = (glyph "\xe18e") { Symbol.glyph_rotate = 90 }
    arp_arrow_up = glyph "\xe18a"
    arp_arrow_down = glyph "\xe189"
