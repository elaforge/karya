-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Types used by "Ui.KeycapsC".  It's a separate module to avoid the FFI
-- dependency when possible.
module Ui.KeycapsT where
import qualified Util.Rect as Rect
import qualified Ui.Color as Color

import           Global


data Layout = Layout {
    lt_size :: (Int, Int) -- ^ width and height for the window
    , lt_bg_color :: Color.Color
    , lt_keycap_color :: Color.Color
    , lt_highlight_color :: Color.Color
    , lt_label_color :: Color.Color
    , lt_binding_color :: Color.Color
    , lt_labels :: Map Keycap Rect.Rect
    } deriving (Show)

type Keycap = Text
type KeyDoc = Text
type Doc = Text

newtype Bindings = Bindings [Binding]
    deriving (Show)

data Binding = Binding {
    b_point :: Rect.Point
    -- | Short text to draw.
    , b_text :: KeyDoc
    -- | Longer text that shows up in tooltip or the gutter on mouseover.
    , b_doc :: Doc
    , b_color :: Maybe Color.Color
    } deriving (Show)

-- | Phantom type for ptr to the window object.
data CWindow
