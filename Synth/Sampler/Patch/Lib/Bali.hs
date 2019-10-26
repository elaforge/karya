-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for Balinese instruments.
module Synth.Sampler.Patch.Lib.Bali where
import qualified Data.Char as Char
import qualified Text.Read as Read

import qualified Util.Num as Num
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch.Lib.Util as Util

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
