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
-- | Binding text that shows up on the keycap.
type KeyDoc = Text
-- | Longer binding text that shows up on mouseover.
type Doc = Text

type Bindings = Map Keycap Binding

data Binding = Binding {
    b_color :: Maybe Color.Color
    , b_text :: KeyDoc
    , b_doc :: Doc
    } deriving (Show)

-- | Since Bindings have to have the same indices of 'lt_labels', I need dummy
-- ones to fill out the array.
no_binding :: Binding
no_binding = Binding
    { b_color = Nothing
    , b_text = ""
    , b_doc = ""
    }

-- | Low level type, for sending to fltk.
newtype RawBindings = RawBindings [RawBinding]
    deriving (Show)
data RawBinding = RawBinding Rect.Point Binding
    deriving (Show)

-- | Phantom type for ptr to the window object.
data CWindow
