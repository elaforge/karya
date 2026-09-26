-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
-- | Describe an Im 'Patch', from the sequencer's point of view.
module Perform.Im.Patch (
    Patch(..), patch
    , controls, attributeMap, elements
    , AttributeMap, makeAttributeMap
) where
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

import           Global


data Patch = Patch
    -- | Map supported controls to documentation.
    -- TODO maybe I need a separate one for pitch controls.
    { patchControls :: Map Control.Control Text
    , patchAttributeMap :: AttributeMap
    , patchElements :: Set Note.Element
    } deriving (Show)

patch :: Patch
patch = Patch
    { patchControls = mempty
    , patchAttributeMap = Common.AttributeMap []
    , patchElements = mempty
    }

controls = Lens.lens patchControls (\r a -> r { patchControls = a })
attributeMap = Lens.lens patchAttributeMap
    (\r a -> r { patchAttributeMap = a })
elements = Lens.lens patchElements (\r a -> r { patchElements = a })

instance Pretty Patch where
    format (Patch controls attrMap elements) = Pretty.record "Patch"
        [ ("controls", Pretty.format controls)
        , ("attribute_map", Pretty.format attrMap)
        , ("elements", Pretty.format elements)
        ]

-- | Since the synth understands Attributes directly, this is just a list of
-- supported Attributes along with their priority.
type AttributeMap = Common.AttributeMap ()

makeAttributeMap :: [Attrs.Attributes] -> AttributeMap
makeAttributeMap = Common.attributeMap . map (\a -> (a, ()))
