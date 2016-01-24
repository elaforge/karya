{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Instrument where
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.String as String

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
    pitch :: !(Maybe NoteNumber)
    -- | Select Samples whose attribute match the Note's attribute.
    , attribute :: !Attribute
    } deriving (Show)

sample :: Sample
sample = Sample Nothing ""

pitchedSample :: NoteNumber -> Sample
pitchedSample pitch = Sample (Just pitch) ""

-- | A string that should select a particular way of playing, e.g. pizz or
-- arco.  This should probably be called Articulation, but is called Attribute
-- for consistency with Karya.
newtype Attribute = Attribute Text
    deriving (Eq, Ord, Show, String.IsString, Aeson.ToJSON, Aeson.FromJSON)

-- | This is equal tempered scale notes with the same definition as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz, and is -1c.  Middle C (4c) is
-- NoteNumber 60.
type NoteNumber = Double

type Hz = Double


-- TODO copy pasted from Perform.Pitch :(

nnToHz :: NoteNumber -> Hz
nnToHz nn = exp (nn * _hzScale + _hzOffset)

-- | Constants to calculate equal tempered conversions.
_hzScale, _hzOffset :: Hz
_hzScale = log 2 / 12
_hzOffset = log a_hz - (a_nn * _hzScale)
    where
    -- NoteNumber is defined with these values.  Ultimately it's because midi
    -- synthesizers are by default defined with these values.
    a_hz = 440
    a_nn = 69
