-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Describe an Im 'Patch', from the sequencer's point of view.
module Perform.Im.Patch where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import Global


data Patch = Patch {
    -- | This is a unique key to address this patch on the synthesizer.
    patch_name :: InstTypes.Name
    -- | Map supported controls to documentation.
    , patch_controls :: !(Map.Map ScoreTypes.Control Text)
    , patch_attribute_map :: !AttributeMap
    , patch_flags :: !(Set.Set Flag)
    } deriving (Show)

patch :: InstTypes.Name -> Patch
patch name = Patch
    { patch_name = name
    , patch_controls = mempty
    , patch_attribute_map = Common.AttributeMap []
    , patch_flags = mempty
    }

instance Pretty.Pretty Patch where
    format (Patch name controls attr_map flags) = Pretty.record "Patch"
        [ ("name", Pretty.format name)
        , ("controls", Pretty.format controls)
        , ("attribute_map", Pretty.format attr_map)
        , ("flags", Pretty.format flags)
        ]

-- | Since the synth understands Attributes directly, this is just a list of
-- support Attributes along with their priority.
type AttributeMap = Common.AttributeMap ScoreTypes.Attributes

attribute_map :: [ScoreTypes.Attributes] -> AttributeMap
attribute_map =
    Common.sort_attribute_map . Common.AttributeMap . map (\a -> (a, a))

data Flag =
    -- | Patch doesn't pay attention to duration, e.g. percussion.  The UI can
    -- use this to create zero duration events for this instrument.
    Triggered
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Flag where pretty = showt
