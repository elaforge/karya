-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Describe an Im 'Patch', from the sequencer's point of view.
module Perform.Im.Patch where
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Synth.Shared.Control as Control

import Global


data Patch = Patch {
    -- | Map supported controls to documentation.
    -- TODO maybe I need a separate one for pitch controls.
    patch_controls :: !(Map Control.Control Text)
    , patch_attribute_map :: !AttributeMap
    , patch_flags :: !(Set Flag)
    } deriving (Show)

patch :: Patch
patch = Patch
    { patch_controls = mempty
    , patch_attribute_map = Common.AttributeMap []
    , patch_flags = mempty
    }

add_flag :: Flag -> Patch -> Patch
add_flag flag patch =
    patch { patch_flags = Set.insert flag (patch_flags patch) }

has_flag :: Patch -> Flag -> Bool
has_flag patch flag = Set.member flag (patch_flags patch)

instance Pretty Patch where
    format (Patch controls attr_map flags) = Pretty.record "Patch"
        [ ("controls", Pretty.format controls)
        , ("attribute_map", Pretty.format attr_map)
        , ("flags", Pretty.format flags)
        ]

-- | Since the synth understands Attributes directly, this is just a list of
-- supported Attributes along with their priority.
type AttributeMap = Common.AttributeMap ()

attribute_map :: [Attrs.Attributes] -> AttributeMap
attribute_map = Common.attribute_map . map (\a -> (a, ()))

data Flag =
    -- | Patch doesn't pay attention to duration, e.g. percussion.  The UI can
    -- use this to create zero duration events for this instrument.
    Triggered
    deriving (Eq, Ord, Show)

instance Pretty Flag where pretty = showt
