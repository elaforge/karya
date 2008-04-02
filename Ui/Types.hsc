{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-
The basic types that the interface modules use.
-}
module Ui.Types (
    UI -- this should move to Ui.Ui or something
    -- From Ui.Color, but it's convenient to have the type in scope.
    , Color
    , Attrs
    -- * trackpos
    , TrackPos(..)

    -- * fonts
    , TextStyle(..), Font(..), FontFace(..)
) where
import qualified Data.List as List
import Foreign
import Foreign.C

import qualified Ui.Util as Util
import qualified Ui.Color
import Ui.Color (Color)

-- TODO:
-- implement UI such that you can only run Ui actions in the monad
-- returned by initialize.

-- | You can query and modify the UI in this monad.
-- It's just a synonym for IO right now, but all UI actions (i.e. functions
-- from Block, Track, and Ruler) shoud be in this monad.
type UI = IO

-- | Used to associate auxiliary values with UI objects.
type Attrs = [(String, String)]

-- * trackpos

-- | The position of an Event on a track.  The units are arbitrary, so how
-- many units are in one second depends on the tempo.  TrackPos units
-- can be negative, but once they get to the UI they will be clamped to be
-- within 0--ULONG_MAX.
newtype TrackPos = TrackPos Integer deriving (Num, Eq, Ord, Show)

#include "c_interface.h"
instance Storable TrackPos where
    sizeOf _ = #size TrackPos
    alignment _ = undefined
    peek posp = do
        v <- (#peek TrackPos, _val) posp :: IO CLLong
        return (TrackPos (fromIntegral v))
    poke posp (TrackPos pos) = (#poke TrackPos, _val) posp cpos
        where
        cpos :: CLLong
        cpos = fromIntegral
            (Util.bounded 0 (fromIntegral (maxBound::CLLong)) pos)


-- * fonts

data TextStyle = TextStyle
    { style_font :: Font
    , style_face :: [FontFace]
    , style_size :: Int
    , style_color :: Color
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
