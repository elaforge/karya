module Ui.Font (
    EventStyle(..), Font(..), FontFace(..)
) where
import qualified Data.List as List
import qualified Ui.Color as Color
import Foreign

import qualified Ui.Util as Util


#include "c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


data EventStyle = EventStyle {
    style_font :: Font
    , style_face :: [FontFace]
    , style_size :: Int
    , style_color :: Color.Color
    } deriving (Eq, Show, Read)

data Font = Helvetica | Times | Courier
    deriving (Eq, Show, Read)
data FontFace = Bold | Italic
    deriving (Eq, Show, Read)

instance Storable EventStyle where
    sizeOf _ = #size EventStyle
    alignment _ = #{alignment EventStyle}
    peek = error "EventStyle peek unimplemented"
    poke = poke_text_style

poke_text_style stylep (EventStyle font face size color) = do
    (#poke EventStyle, font) stylep
        (Util.c_int (font_code font + face_code face))
    (#poke EventStyle, size) stylep (Util.c_nat size)
    (#poke EventStyle, color) stylep color

font_code font = case font of
    Helvetica -> #const FL_HELVETICA
    Times -> #const FL_TIMES
    Courier -> #const FL_COURIER

face_code = sum . map code . List.nub
    where
    code face = case face of
        Bold -> #const FL_BOLD
        Italic -> #const FL_ITALIC
