-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Synth.Sampler.Patch (
    module Synth.Sampler.Patch, Attributes, attr
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Derive.ScoreTypes (Attributes, attr)
import qualified Perform.Pitch as Pitch
import qualified Perform.Im.Patch as Patch
import Global


data Patch = Patch {
    -- | Sample file names are relative to this.
    sampleDirectory :: !FilePath
    , karyaPatch :: !Patch.Patch
    -- | Paths are relative to 'sampleDirectory'.
    , samples :: !(Map.Map FilePath Sample)
    } deriving (Show)

instrument :: FilePath -> [(FilePath, Sample)] -> Patch
instrument dir samples = Patch
    { sampleDirectory = dir
    , karyaPatch = inferPatch (map snd samples)
    , samples = Map.fromList samples
    }

instance Pretty.Pretty Patch where
    format (Patch dir inst samples) = Pretty.record "Patch"
        [ ("sampleDirectory", Pretty.format dir)
        , ("karyaPatch", Pretty.format inst)
        , ("samples", Pretty.format samples)
        ]

-- | Unique identifier for an instrument.
type Name = Text

-- | Sample in an Patch.
data Sample = Sample {
    pitch :: !(Maybe Pitch.NoteNumber)
    -- | Select Samples whose attribute match the Note's attribute.
    , attributes :: !Attributes
    } deriving (Show)

sample :: Sample
sample = Sample Nothing mempty

pitchedSample :: Pitch.NoteNumber -> Sample
pitchedSample pitch = Sample (Just pitch) mempty

instance Pretty.Pretty Sample where
    format (Sample pitch attrs) = Pretty.constructor "Sample"
        [Pretty.format pitch, Pretty.format attrs]

-- | This doesn't allow you to specify priority, but is sufficient for simple
-- instruments.
inferPatch :: [Sample] -> Patch.Patch
inferPatch samples = Patch.empty
    { Patch.patch_attribute_map =
        Patch.attribute_map $ Seq.unique $ map attributes samples
    , Patch.patch_flags = Set.fromList $
        if all ((==Nothing) . pitch) samples then [Patch.Triggered] else []
    }
