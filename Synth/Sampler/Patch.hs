-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch (
    module Synth.Sampler.Patch
) where
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Derive.Attrs as Attrs
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Perform.Im.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Types as Types
import Global


data Patch = Patch {
    -- | Sample file names are relative to this.
    sampleDirectory :: !FilePath
    , karyaCommon :: !(Common.Common Cmd.InstrumentCode)
    -- | Paths are relative to 'sampleDirectory'.
    , samples :: !(Map FilePath Sample)
    } deriving (Show)

-- Putting code here means that the sampler has to link in a large portion of
-- the sequencer code, when it doesn't need to run it.  To avoid this I'd have
-- to maintain a separate DB in the sequencer that matches up by name.  For the
-- moment, linking in the extra code doesn't seem like a problem.

instrument :: FilePath -> [(FilePath, Sample)] -> Patch
instrument dir samples = Patch
    { sampleDirectory = dir
    , karyaCommon = Common.common Cmd.empty_code
    , samples = Map.fromList samples
    }

instance Pretty Patch where
    format (Patch dir common samples) = Pretty.record "Patch"
        [ ("sampleDirectory", Pretty.format dir)
        , ("karyaCommon", Pretty.format common)
        , ("samples", Pretty.format samples)
        ]

-- | Sample in an Patch.
data Sample = Sample {
    pitch :: !(Maybe Pitch.NoteNumber)
    -- | Select Samples whose attribute match the Note's attribute.
    , attributes :: !Types.Attributes
    } deriving (Show)

sample :: Sample
sample = Sample Nothing mempty

pitchedSample :: Pitch.NoteNumber -> Sample
pitchedSample pitch = Sample (Just pitch) mempty

instance Pretty Sample where
    format (Sample pitch attrs) = Pretty.constructor "Sample"
        [Pretty.format pitch, Pretty.format attrs]


-- * makeInst

makeInst :: Patch -> Inst.Inst Cmd.InstrumentCode
makeInst patch = Inst.Inst
    { inst_backend = Inst.Im $ inferPatch (Map.elems (samples patch))
    , inst_common = karyaCommon patch
    }

-- | This doesn't allow you to specify priority, but is sufficient for simple
-- instruments.
inferPatch :: [Sample] -> Patch.Patch
inferPatch samples = Patch.Patch
    { patch_controls = Map.fromList $ concat
        [ [(c Control.pitch, "Pitch signal.")
            | any (Maybe.isJust . pitch) samples]
        ]
    , patch_attribute_map = Patch.attribute_map $ map convertAttributes $
        Seq.unique $ map attributes samples
    , patch_flags = Set.fromList $
        if all ((==Nothing) . pitch) samples then [Patch.Triggered] else []
    }
    where c (Control.Control a) = ScoreTypes.Control a

convertAttributes :: Types.Attributes -> Attrs.Attributes
convertAttributes (Types.Attributes attrs) = Attrs.from_set attrs
