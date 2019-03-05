-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Im-specific controls.  Generally they are per-instrument, but like
-- "Derive.Controls", it's useful to have a standard vocabulary.
module Synth.Shared.Control where
import qualified Data.Map as Map
import qualified Data.String as String

import qualified Util.Serialize as Serialize

import Global


-- | Unlike 'Derive.ScoreTypes.Control', pitch is just another control.
newtype Control = Control Text
    deriving (Eq, Ord, Show, String.IsString, Serialize.Serialize)

instance Pretty Control where pretty (Control c) = c

-- | This should come from 'Derive.Controls.dynamic'.  Unlike score-level dyn,
-- whose meaning is abstract, this should be more or less on a dB scale, where
-- 0 corresponds to 'minimumDb'.
--
-- It can be more closely defined here because im instruments have direct
-- control over the sound they produce, while other backends, like MIDI, are up
-- to the whims of the synthesizer's interpretation.
dynamic :: Control
dynamic = "dyn" -- TODO unify with Controls.dynamic

-- | This is similar to 'dynamic', but this always maps to physical volume,
-- for the case where an instrument distinguishes between dynamic level and
-- volume level.  The score-level equivalent is 'Derive.Controls.vol' which
-- corresponds to MIDI cc7 in the MIDI backend.
volume :: Control
volume = "vol"

-- | A 'dynamic' of 1 maps to 0dB of attenuation, and 0 maps to this level.
-- Humans maybe have a limit around 120dB, but 96 is a theoretical maximum for
-- 16 bit audio, which is likely what this turns into anyway.
--
-- My manual experiment shows that -96 on a normalized sample is just about
-- inaudible.
minimumDb :: Double
minimumDb = -96

-- | -1 means pan left, 1 means pan right.  Unlike other controls, the range is
-- -1 to 1 instead of 0 to 1.  This is so adding signals is the same as adding
-- pan, and a 0 signal has no effect.
pan :: Control
pan = "pan"

-- | Pitch in NoteNumbers.
pitch :: Control
pitch = "pitch"

-- | Used by some synths to mark note start and end times.  This should be used
-- internally, not exposed.
gate :: Control
gate = "gate"

-- | An integral variation, for "random" variant samples.
variation :: Control
variation = "variation"

-- | Amount of mutedness, for instruments that support variable amount of mute.
mute :: Control
mute = "mute"

-- * Supported

type Supported = Map Control Text

supportPitch :: Supported
supportPitch = Map.singleton pitch "Pitch signal."

supportDyn :: Supported
supportDyn = Map.singleton dynamic $
    "Dynamic signal.  dB scale, where 0 is " <> pretty minimumDb <> " dB."

supportVariation :: Supported
supportVariation = Map.singleton
    variation "Random integer, to choose between variant samples."
