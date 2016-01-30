-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Synth.Sampler.Instrument (
    module Synth.Sampler.Instrument, Attributes, attr
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Derive.ScoreTypes (Attributes, attr)
import qualified Perform.Pitch as Pitch
import qualified Perform.Synth.Instrument as Instrument
import Global


data Instrument = Instrument {
    -- | Sample file names are relative to this.
    sampleDirectory :: !FilePath
    , karyaInstrument :: Instrument.Instrument
    -- | Paths are relative to 'sampleDirectory'.
    , samples :: !(Map.Map FilePath Sample)
    } deriving (Show)

instrument :: FilePath -> [(FilePath, Sample)] -> Instrument
instrument dir samples = Instrument
    { sampleDirectory = dir
    , karyaInstrument = inferInstrument (map snd samples)
    , samples = Map.fromList samples
    }

instance Pretty.Pretty Instrument where
    format (Instrument dir inst samples) = Pretty.record "Instrument"
        [ ("sampleDirectory", Pretty.format dir)
        , ("karyaInstrument", Pretty.format inst)
        , ("samples", Pretty.format samples)
        ]

-- | Unique identifier for an instrument.
type Name = Text

-- | Sample in an Instrument.
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
inferInstrument :: [Sample] -> Instrument.Instrument
inferInstrument samples = Instrument.empty
    { Instrument.inst_attributes =
        Instrument.attribute_map $ Seq.unique $ map attributes samples
    , Instrument.inst_flags = Set.fromList $
        if all ((==Nothing) . pitch) samples then [Instrument.Triggered] else []
    }
