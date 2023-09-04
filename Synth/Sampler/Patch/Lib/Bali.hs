-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for Balinese instruments.
module Synth.Sampler.Patch.Lib.Bali where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Text.Read as Read

import qualified Util.Num as Num
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


data Pitch = Pitch !Pitch.Octave !PitchClass
    deriving (Eq, Ord, Show)

instance Pretty Pitch where
    pretty (Pitch oct pc) = showt oct <> Util.showtLower pc

instance Enum Pitch where
    toEnum n = let (oct, pc) = n `divMod` 5 in Pitch oct (toEnum pc)
    fromEnum (Pitch oct pc) = oct * 5 + fromEnum pc

data PitchClass = I | O | E | U | A
    deriving (Eq, Ord, Show, Enum, Bounded, Read)

parsePitch :: String -> Maybe Pitch
parsePitch [oct, pc] = Pitch <$> Num.readDigit oct
    <*> Read.readMaybe (Char.toUpper pc : "")
parsePitch _ = Nothing


-- * convert

supportVariableMute :: Im.Patch.Patch -> Im.Patch.Patch
supportVariableMute patch = patch
    { Im.Patch.patch_controls = Im.Patch.patch_controls patch <>
        Map.singleton Control.mute
            "Variable amount of mute. Simulated with a shortened envelope."
    }

variableMuteEnv :: Util.DynVal -> Note.Note -> Signal.Signal
variableMuteEnv dynVal note
    | mute > 0 = Util.triggerRelease dynVal release note
    | otherwise = Util.sustainRelease dynVal dampTime note
    where
    mute = RealTime.seconds $ Note.initial0 Control.mute note
    release = uncurry Num.scale variableMuteRange (1-mute)
    -- Duration of shortest to longest mute.
    variableMuteRange = (0.25, 1)
    -- Time to damp a note to 0.
    dampTime = 0.35
