-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This defines some key layouts.  "Local.KeyLayout" is expected to export
-- @layout :: KeyLayouts.Layout@.  If your key layout is already qwerty, just
-- use @KeyLayouts.qwerty@.
module Cmd.KeyLayouts (
    Layout, layout, to_unshifted, from_qwerty
    , qwerty, dvorak
    , qwerty_unshifted, qwerty_shifted
) where
import qualified Data.Map as Map

import qualified Util.Seq as Seq


data Layout = Layout {
    -- | Map from the shifted key to the unshifted one.
    map_to_unshifted :: Map.Map Char Char
    -- | Map from the layout to qwerty.
    , map_from_qwerty :: Map.Map Char Char
    } deriving (Show)

to_unshifted :: Layout -> Char -> Maybe Char
to_unshifted layout c = Map.lookup c (map_to_unshifted layout)

from_qwerty :: Layout -> Char -> Maybe Char
from_qwerty layout c = Map.lookup c (map_from_qwerty layout)

layout :: String -> [Char] -> [Char] -> Layout
layout name unshifted shifted
    | length unshifted /= length shifted =
        error $ prefix ++ "(unshifted, shifted) not the same length: "
            ++ show (Seq.zip_padded unshifted shifted)
    | length unshifted /= length qwerty_unshifted =
        error $ prefix ++ "size should be " ++ show (length qwerty_unshifted)
            ++ " but is " ++ show (length unshifted)
    | otherwise = Layout
        { map_to_unshifted = Map.fromList $ zip shifted unshifted
        , map_from_qwerty = Map.fromList $
            zip (qwerty_unshifted ++ qwerty_shifted) (unshifted ++ shifted)
        }
    where prefix = "KeyLayouts.layout " ++ show name ++ ": "

qwerty_unshifted, qwerty_shifted :: [Char]
qwerty_unshifted = concat
    [ "`1234567890-="
    , "qwertyuiop[]\\"
    , "asdfghjkl;'"
    , "zxcvbnm,./"
    ]
qwerty_shifted = concat
    [ "~!@#$%^&*()_+"
    , "QWERTYUIOP{}|"
    , "ASDFGHJKL:\""
    , "ZXCVBNM<>?"
    ]

qwerty :: Layout
qwerty = layout "qwerty" qwerty_unshifted qwerty_shifted

dvorak :: Layout
dvorak = layout "dvorak"
    (concat
        [ "1234567890[]"
        , "',.pyfgcrl/=\\"
        , "aoeuidhtns-"
        , ";qjkxbmwvz"
        ])
    (concat
        [ "!@#$%^&*(){}"
        , "\"<>PYFGCRL?+|"
        , "AOEUIDHTNS_"
        , ":QJKXBMWVZ"
        ])
