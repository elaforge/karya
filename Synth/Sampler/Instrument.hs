-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Synth.Sampler.Instrument where
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.String as String

import qualified Util.Serialize as Serialize
import qualified Perform.Pitch as Pitch
import Global


data Instrument = Instrument {
    -- | Sample file names are relative to this.
    sampleDirectory :: !FilePath
    -- | Paths are relative to 'sampleDirectory'.
    , samples :: !(Map.Map FilePath Sample)
    } deriving (Show)

instrument :: FilePath -> [(FilePath, Sample)] -> Instrument
instrument dir samples = Instrument dir (Map.fromList samples)

-- | Unique identifier for an instrument.
type Name = Text

-- | Sample in an Instrument.
data Sample = Sample {
    pitch :: !(Maybe Pitch.NoteNumber)
    -- | Select Samples whose attribute match the Note's attribute.
    , attribute :: !Attribute
    } deriving (Show)

sample :: Sample
sample = Sample Nothing ""

pitchedSample :: Pitch.NoteNumber -> Sample
pitchedSample pitch = Sample (Just pitch) ""

-- | A string that should select a particular way of playing, e.g. pizz or
-- arco.  This should probably be called Articulation, but is called Attribute
-- for consistency with Karya.
newtype Attribute = Attribute Text
    deriving (Eq, Ord, Show, String.IsString, Aeson.ToJSON, Aeson.FromJSON,
        Serialize.Serialize)
