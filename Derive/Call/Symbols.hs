-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Symbols used in calls.
module Derive.Call.Symbols where
import qualified Ui.Symbol as Symbol
import qualified App.Config as Config
import Global


symbols :: [Symbol.Symbol]
symbols = misc_symbols ++ staff_symbols

misc_symbols :: [Symbol.Symbol]
misc_symbols =
    [ Symbol.Symbol "0x" True
        [Symbol.glyph_at (-2) (0, -0.4) (Symbol.glyph "x")]
    , simple "up" "↑"
    , simple "^" "↑"
    , simple "right" "→"
    , simple "left" "←"
    , simple "down" "↓"
    , simple "_" "↓"
    ]

simple :: Text -> Text -> Symbol.Symbol
simple name text = Symbol.symbol name [Symbol.glyph text]

-- | See <http://www.smufl.org/version/latest/>
--
-- TODO if I do more of these, I could load glyphnames.json to address these by
-- name.  I probably don't want to load every single glyph as-is though.
staff_symbols :: [Symbol.Symbol]
staff_symbols =
    [ symbol "tr" "\xe566"
    , symbol "mordent" "\xe56c" -- ornamentMordent
    , symbol "rmordent" "\xe56d" -- ornamentMordentInverted
    -- There is also "\xe634", but it's too tall and skinny.
    , Symbol.symbol "arp-up"
        [ arp_glyph 90 (0, -0.25) "\xeaad" -- wiggleArpeggiatoUpArrow
        , arp_glyph 90 (0, 0) "\xeaa9" -- wiggleArpeggiatoUp
        ]
    , Symbol.symbol "arp-down" -- Or "\xe635".
        [ arp_glyph (-90) (0, 0.25) "\xeaae" -- wiggleArpeggiatoDownArrow
        , arp_glyph (-90) (0, 0) "\xeaaa" -- wiggleArpeggiatoDown
        ]
    , symbol "ped" "\xe650" -- keyboardPedalPed
    , symbol "ped-up" "\xe655" -- keyboardPedalUp
    ]
    where
    arp_glyph rotate align str = (glyph str)
        { Symbol.glyph_align = align
        , Symbol.glyph_rotate = rotate
        }
    glyph str = Symbol.Glyph str (Just Config.bravura) 8 (0, 0) 0
    symbol name str = Symbol.Symbol
        { name = name, absolute_y = False, glyphs = [glyph str] }
