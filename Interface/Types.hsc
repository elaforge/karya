{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-
The basic types that the interface modules use.
-}
module Interface.Types (
    UI -- this should move to Interface.Ui or something
    , Attrs
    -- * trackpos
    , TrackPos(..)

    -- * fonts
    , TextStyle(..), Font(..), FontFace(..)
) where
import qualified Data.List as List
import Foreign

import qualified Interface.Util as Util
import qualified Interface.Color as Color

-- TODO:
-- implement Ui such that you can only run Ui actions in the monad
-- returned by initialize.
type UI = IO

-- | Used to associate auxiliary values with UI objects.
type Attrs = [(String, String)]

-- * trackpos

-- | The position of an Event on a track.  One of these is normally a second.
-- The type of the value here should be kept in sync with the type of the c++
-- TrackPos value.
newtype TrackPos = TrackPos Double deriving (Eq, Ord, Show)

#include "c_interface.h"
instance Storable TrackPos where
    sizeOf _ = #size TrackPos
    alignment _ = undefined
    peek posp = (#peek TrackPos, _val) posp >>= return . TrackPos
    poke posp (TrackPos pos) = (#poke TrackPos, _val) posp pos


-- * fonts

data TextStyle = TextStyle
    { style_font :: Font
    , style_face :: [FontFace]
    , style_size :: Int
    , style_color :: Color.Color
    } deriving (Eq, Show)

data Font = Helvetica | Times | Courier deriving (Eq, Show)
data FontFace = Bold | Italic deriving (Eq, Show)

instance Storable TextStyle where
    sizeOf _ = #size TextStyle
    alignment _ = undefined
    peek = undefined
    poke = poke_text_style

poke_text_style stylep (TextStyle font face size color) = do
    (#poke TextStyle, font) stylep
        (Util.c_int (font_code font + face_code face))
    (#poke TextStyle, size) stylep (Util.c_nat size)
    (#poke TextStyle, color) stylep color

font_code font = case font of
    Helvetica -> #const FL_HELVETICA
    Times -> #const FL_TIMES
    Courier -> #const FL_COURIER

face_code = sum . map code . List.nub
    where
    code face = case face of
        Bold -> #const FL_BOLD
        Italic -> #const FL_ITALIC
