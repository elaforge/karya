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
    Layout, layout, toUnshifted, toShifted, fromQwerty, toQwerty
    , qwerty, dvorak
    , qwertyUnshifted, qwertyShifted
    , qwertyUnshiftedRows, qwertyShiftedRows
) where
import qualified Data.Map as Map

import qualified Util.Maps as Maps
import qualified Util.Lists as Lists

import           Global


data Layout = Layout
    -- | Map from the shifted key to the unshifted one.
    { mapToUnshifted :: Map Char Char
    , mapToShifted :: Map Char Char
    -- | Map from the layout to qwerty.
    , mapFromQwerty :: Map Char Char
    , mapToQwerty :: Map Char Char
    } deriving (Show)

toUnshifted :: Layout -> Char -> Maybe Char
toUnshifted layout c = Map.lookup c (mapToUnshifted layout)

toShifted :: Layout -> Char -> Maybe Char
toShifted layout c = Map.lookup c (mapToShifted layout)

fromQwerty :: Layout -> Char -> Maybe Char
fromQwerty layout c = Map.lookup c (mapFromQwerty layout)

toQwerty :: Layout -> Char -> Maybe Char
toQwerty layout c = Map.lookup c (mapToQwerty layout)

layout :: String -> [Char] -> [Char] -> Layout
layout name unshifted shifted
    | length unshifted /= length shifted =
        errorStack $ prefix <> "(unshifted, shifted) not the same length: "
            <> showt (Lists.zipPadded unshifted shifted)
    | length unshifted /= length qwertyUnshifted =
        errorStack $ prefix <> "size should be "
            <> showt (length qwertyUnshifted) <> " but is "
            <> showt (length unshifted)
    | otherwise = Layout
        { mapToUnshifted = Map.fromList $ zip shifted unshifted
        , mapToShifted = Map.fromList $ zip unshifted shifted
        , mapFromQwerty = fromQwerty
        , mapToQwerty = Maps.invert fromQwerty
        }
    where
    fromQwerty = Map.fromList $
        zip (qwertyUnshifted ++ qwertyShifted) (unshifted ++ shifted)
    prefix = showt name <> ": "

qwertyUnshifted, qwertyShifted :: [Char]
qwertyUnshifted = concat qwertyUnshiftedRows
qwertyShifted = concat qwertyShiftedRows

qwertyUnshiftedRows, qwertyShiftedRows :: [[Char]]
qwertyUnshiftedRows =
    [ "`1234567890-="
    , "qwertyuiop[]\\"
    , "asdfghjkl;'"
    , "zxcvbnm,./"
    ]
qwertyShiftedRows =
    [ "~!@#$%^&*()_+"
    , "QWERTYUIOP{}|"
    , "ASDFGHJKL:\""
    , "ZXCVBNM<>?"
    ]

qwerty :: Layout
qwerty = layout "qwerty" qwertyUnshifted qwertyShifted

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
