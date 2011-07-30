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
    ]
    where
    symbol name str = Symbol.Symbol name False [glyph str]
    glyph str = Symbol.Glyph str (Just "Emmentaler 11") 4 (0, 0)
