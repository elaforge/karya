-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'get' for 'Cmd.Keymap.physical_key'.  This would be in
-- "Local.Config" except that would cause a circular import.
module Local.KeyLayout (get) where
import qualified Data.Map as Map

import qualified Cmd.KeyLayouts as KeyLayouts


get :: Char -> Maybe Char
get = flip Map.lookup layout

layout :: Map.Map Char Char
layout = Map.fromList (zip KeyLayouts.qwerty dvorak)

-- | Not just dvorak, but my slightly modified version.
dvorak :: [Char]
dvorak = concat
    [ "1234567890-="
    , "',.pyfgcrl[]\\"
    , "aoeuidhtns/"
    , ";qjkxbmwvz"

    , "!@#$%^&*()_+"
    , "\"<>PYFGCRL{}|"
    , "AOEUIDHTNS?"
    , ":QJKXBMWVZ"
    ]
