-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Each event has a display style.
--
-- Actually, to be consistent with Block and Track shouldn't this be called
-- EventConfig?
module Ui.Style (
    Style(..), Font(..), FontFace(..), StyleId(..)
) where
import qualified Data.List as List
import qualified Data.Word as Word

import qualified Ui.Color as Color
import qualified Util.FFI as FFI
import qualified Util.Num as Num
import qualified Util.Serialize as Serialize

import           ForeignC


data Style = Style
    { styleFont :: Font
    , styleFace :: [FontFace]
    , styleSize :: Int
    , styleTextColor :: Color.Color
    , styleEventColor :: Color.Color
    } deriving (Eq, Show)

data Font = Helvetica | Times | Courier
    deriving (Eq, Show)
data FontFace = Bold | Italic
    deriving (Eq, Show)

-- | To save space, event styles are explicitly shared by storing them in
-- a table.
newtype StyleId = StyleId Word.Word8
    deriving (Serialize.Serialize, Eq, Ord, Show)


-- * storable

#include "Ui/c_interface.h"

instance CStorable Style where
    sizeOf _ = #size EventStyle
    alignment = alignment . styleTextColor
    peek = error "EventStyle peek unimplemented"
    poke stylep (Style font face size textColor eventColor) = do
        (#poke EventStyle, font) stylep
            (FFI.cInt (fontCode font + faceCode face))
        (#poke EventStyle, size) stylep (FFI.cNat size)
        (#poke EventStyle, text_color) stylep textColor
        (#poke EventStyle, event_color) stylep eventColor

fontCode :: Font -> Int
fontCode font = case font of
    Helvetica -> #const FL_HELVETICA
    Times -> #const FL_TIMES
    Courier -> #const FL_COURIER

faceCode :: [FontFace] -> Int
faceCode = Num.sum . map code . List.nub
    where
    code face = case face of
        Bold -> #const FL_BOLD
        Italic -> #const FL_ITALIC
