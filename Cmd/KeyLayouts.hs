-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This defines some key layouts.  "Local.KeyLayout" is expected to export
-- @layout :: KeyLayouts.Layout@.  If your key layout is already qwerty, just
-- use @KeyLayouts.qwerty@.
--
-- Ultimately this is necessary because some keys are mapped based on their
-- physical location.
module Cmd.KeyLayouts (
    Layout, layout, to_unshifted, to_shifted, from_qwerty, to_qwerty
    , qwerty, dvorak
    , qwerty_unshifted, qwerty_shifted
    , qwerty_unshifted_rows, qwerty_shifted_rows
) where
import qualified Data.Map as Map

import qualified Util.Maps as Maps
import qualified Util.Seq as Seq

import           Global


data Layout = Layout {
    -- | Map from the shifted key to the unshifted one.
    map_to_unshifted :: Map Char Char
    , map_to_shifted :: Map Char Char
    -- | Map from the layout to qwerty.
    , map_from_qwerty :: Map Char Char
    , map_to_qwerty :: Map Char Char
    } deriving (Show)

to_unshifted :: Layout -> Char -> Maybe Char
to_unshifted layout c = Map.lookup c (map_to_unshifted layout)

to_shifted :: Layout -> Char -> Maybe Char
to_shifted layout c = Map.lookup c (map_to_shifted layout)

from_qwerty :: Layout -> Char -> Maybe Char
from_qwerty layout c = Map.lookup c (map_from_qwerty layout)

to_qwerty :: Layout -> Char -> Maybe Char
to_qwerty layout c = Map.lookup c (map_to_qwerty layout)

layout :: String -> [Char] -> [Char] -> Layout
layout name unshifted shifted
    | length unshifted /= length shifted =
        errorStack $ prefix <> "(unshifted, shifted) not the same length: "
            <> showt (Seq.zip_padded unshifted shifted)
    | length unshifted /= length qwerty_unshifted =
        errorStack $ prefix <> "size should be "
            <> showt (length qwerty_unshifted) <> " but is "
            <> showt (length unshifted)
    | otherwise = Layout
        { map_to_unshifted = Map.fromList $ zip shifted unshifted
        , map_to_shifted = Map.fromList $ zip unshifted shifted
        , map_from_qwerty = from_qwerty
        , map_to_qwerty = Maps.invert from_qwerty
        }
    where
    from_qwerty = Map.fromList $
        zip (qwerty_unshifted ++ qwerty_shifted) (unshifted ++ shifted)
    prefix = showt name <> ": "

qwerty_unshifted, qwerty_shifted :: [Char]
qwerty_unshifted = concat qwerty_unshifted_rows
qwerty_shifted = concat qwerty_shifted_rows

qwerty_unshifted_rows, qwerty_shifted_rows :: [[Char]]
qwerty_unshifted_rows =
    [ "`1234567890-="
    , "qwertyuiop[]\\"
    , "asdfghjkl;'"
    , "zxcvbnm,./"
    ]
qwerty_shifted_rows =
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
        [ "`1234567890[]"
        , "',.pyfgcrl/=\\"
        , "aoeuidhtns-"
        , ";qjkxbmwvz"
        ])
    (concat
        [ "~!@#$%^&*(){}"
        , "\"<>PYFGCRL?+|"
        , "AOEUIDHTNS_"
        , ":QJKXBMWVZ"
        ])
