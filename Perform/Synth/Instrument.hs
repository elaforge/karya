-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Describe a Synth 'Instrument', from the sequencer's point of view.
module Perform.Synth.Instrument where
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Instrument.Common as Common
import Global


data Instrument = Instrument {
    inst_controls :: !(Set.Set ScoreTypes.Control)
    , inst_attribute_map :: !AttributeMap
    , inst_flags :: !(Set.Set Flag)
    } deriving (Show)

empty :: Instrument
empty = Instrument mempty (Common.AttributeMap []) mempty

instance Pretty.Pretty Instrument where
    format (Instrument controls attr_map flags) =
        Pretty.record "Instrument"
            [ ("controls", Pretty.format controls)
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
