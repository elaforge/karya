{-
A ruler is also a Track, and can be mixed with them freely.  Rulers have Marks
at various positions.  Marks have width, color, translucency, and an optional
name.  They also have a display at zoom value.  Marks are only displayed if
the zoom level is >= the display at zoom.
-}

module Interface.Ruler where
import Interface.Types

create :: [Mark] -> Ruler
create marks = Ruler marks

data Ruler = Ruler [Mark] deriving (Show)
type T = Ruler

-- | Ruler with nothing in it.
empty = create []

data Mark = Mark
    { mark_width :: Int
    , mark_color :: Color
    , mark_name :: String
    , mark_name_zoom_level :: Double
    , zoom_level :: Double
    } deriving (Show)
