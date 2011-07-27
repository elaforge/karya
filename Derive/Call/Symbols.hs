-- | Symbols used in calls.
module Derive.Call.Symbols where
import qualified Ui.Symbol as Symbol


symbols :: [Symbol.Symbol]
symbols = staff_symbols

staff_symbols :: [Symbol.Symbol]
staff_symbols =
    [ Symbol.Symbol "tr" False [g "\xe17a" 2]
    , Symbol.Symbol "mordent" False [g "\xe18c" 2]
    , Symbol.Symbol "rmordent" False [g "\xe18d" 2]
    , Symbol.Symbol "mordent2" False [g "\xe18e" 2]
    , Symbol.Symbol "rmordent2" False [g "\xe18f" 2]
    ]
    where g str size = Symbol.Glyph str (Just "Emmentaler 11") size (0, 0)
