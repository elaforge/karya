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
import qualified Util.CUtil as CUtil
import qualified Util.Num as Num
import qualified Util.Serialize as Serialize

import           ForeignC


data Style = Style {
    style_font :: Font
    , style_face :: [FontFace]
    , style_size :: Int
    , style_text_color :: Color.Color
    , style_event_color :: Color.Color
    } deriving (Eq, Show, Read)

data Font = Helvetica | Times | Courier
    deriving (Eq, Show, Read)
data FontFace = Bold | Italic
    deriving (Eq, Show, Read)

-- | To save space, event styles are explicitly shared by storing them in
-- a table.
newtype StyleId = StyleId Word.Word8
    deriving (Serialize.Serialize, Eq, Show, Read)


-- * storable

#include "Ui/c_interface.h"

instance CStorable Style where
    sizeOf _ = #size EventStyle
    alignment = alignment . style_text_color
    peek = error "EventStyle peek unimplemented"
    poke stylep (Style font face size text_color event_color) = do
        (#poke EventStyle, font) stylep
            (CUtil.c_int (font_code font + face_code face))
        (#poke EventStyle, size) stylep (CUtil.c_nat size)
        (#poke EventStyle, text_color) stylep text_color
        (#poke EventStyle, event_color) stylep event_color

font_code :: Font -> Int
font_code font = case font of
    Helvetica -> #const FL_HELVETICA
    Times -> #const FL_TIMES
    Courier -> #const FL_COURIER

face_code :: [FontFace] -> Int
face_code = Num.sum . map code . List.nub
    where
    code face = case face of
        Bold -> #const FL_BOLD
        Italic -> #const FL_ITALIC
