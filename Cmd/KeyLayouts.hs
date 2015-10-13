-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This defines some key layouts.  "Local.KeyLayout" is expected to export
-- a function @get :: Char -> Maybe Char@ which will map a key to its
-- corresponding key in USA qwerty, which will be called by
-- 'Cmd.Keymap.physical_key'.  So if your key layout is already qwerty, it's
-- just @id@.  Otherwise it would be e.g.
-- @flip Map.lookup (Map.fromList (zip KeyLayouts.qwerty KeyLayouts.dvorak))@.
module Cmd.KeyLayouts where


qwerty, qwerty_lower, qwerty_upper :: [Char]
qwerty = qwerty_lower ++ qwerty_upper
qwerty_lower = concat
    [ "1234567890-="
    , "qwertyuiop[]\\"
    , "asdfghjkl;'"
    , "zxcvbnm,./"
    ]
qwerty_upper = concat
    [ "!@#$%^&*()_+"
    , "QWERTYUIOP{}|"
    , "ASDFGHJKL:\""
    , "ZXCVBNM<>?"
    ]

dvorak :: [Char]
dvorak = concat
    [ "1234567890[]"
    , "',.pyfgcrl/=\\"
    , "aoeuidhtns-"
    , ";qjkxbmwvz"

    , "!@#$%^&*(){}"
    , "\"<>PYFGCRL?+|"
    , "AOEUIDHTNS_"
    , ":QJKXBMWVZ"
    ]
