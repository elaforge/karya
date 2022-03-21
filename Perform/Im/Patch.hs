-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Describe an Im 'Patch', from the sequencer's point of view.
module Perform.Im.Patch (
    Patch(..), patch
    , controls, attribute_map, elements
    , AttributeMap, make_attribute_map
) where
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

import           Global


data Patch = Patch {
    -- | Map supported controls to documentation.
    -- TODO maybe I need a separate one for pitch controls.
    patch_controls :: !(Map Control.Control Text)
    , patch_attribute_map :: !AttributeMap
    , patch_elements :: !(Set Note.Element)
    } deriving (Show)

patch :: Patch
patch = Patch
    { patch_controls = mempty
    , patch_attribute_map = Common.AttributeMap []
    , patch_elements = mempty
    }

controls = Lens.lens patch_controls
    (\f r -> r { patch_controls = f (patch_controls r) })
attribute_map = Lens.lens patch_attribute_map
    (\f r -> r { patch_attribute_map = f (patch_attribute_map r) })
elements = Lens.lens patch_elements
    (\f r -> r { patch_elements = f (patch_elements r) })

instance Pretty Patch where
    format (Patch controls attr_map elements) = Pretty.record "Patch"
        [ ("controls", Pretty.format controls)
        , ("attribute_map", Pretty.format attr_map)
        , ("elements", Pretty.format elements)
        ]

-- | Since the synth understands Attributes directly, this is just a list of
-- supported Attributes along with their priority.
type AttributeMap = Common.AttributeMap ()

make_attribute_map :: [Attrs.Attributes] -> AttributeMap
make_attribute_map = Common.attribute_map . map (\a -> (a, ()))
