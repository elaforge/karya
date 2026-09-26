-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Types used by "Ui.KeycapsC".  It's a separate module to avoid the FFI
-- dependency when possible.
module Ui.KeycapsT where
import qualified Util.Rect as Rect
import qualified Ui.Color as Color

import           Global


data Layout = Layout
    { ltSize :: (Int, Int) -- ^ width and height for the window
    , ltBgColor :: Color.Color
    , ltKeycapColor :: Color.Color
    , ltHighlightColor :: Color.Color
    , ltLabelColor :: Color.Color
    , ltBindingColor :: Color.Color
    , ltLabels :: Map Keycap Rect.Rect
    } deriving (Show)

type Keycap = Text
-- | Binding text that shows up on the keycap.
type KeyDoc = Text
-- | Longer binding text that shows up on mouseover.
type Doc = Text

type Bindings = Map Keycap Binding

data Binding = Binding
    { bColor :: Maybe Color.Color
    , bText :: KeyDoc
    , bDoc :: Doc
    } deriving (Show)

instance Pretty Binding where
    pretty (Binding mbColor text doc) =
        text <> "(" <> doc <> ")[" <> pretty mbColor <> "]"

-- | Since Bindings have to have the same indices of 'lt_labels', I need dummy
-- ones to fill out the array.
noBinding :: Binding
noBinding = Binding
    { bColor = Nothing
    , bText = ""
    , bDoc = ""
    }

-- | Low level type, for sending to fltk.
newtype RawBindings = RawBindings [RawBinding]
    deriving (Show)
data RawBinding = RawBinding Rect.Point Binding
    deriving (Show)

-- | Phantom type for ptr to the window object.
data CWindow
