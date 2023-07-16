-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeFamilies #-}
-- | 'Affine' and instances.
-- module Util.Affine (Affine(..), (.-^)) where
module Util.Affine where
import qualified Data.Time as Time


-- | Simplified version of AffineSpace from the vector-space package.
--
-- Num gives access to negate and (+) and (*).  It's probably technically too
-- much because it also has fromInteger, but I don't think I need the whole
-- AdditiveGroup etc. alternative numeric hierarchy.
class Num (Diff p) => Affine p where
    type Diff p
    (.-.) :: p -> p -> Diff p
    (.+^) :: p -> Diff p -> p

(.-^) :: Affine p => p -> Diff p -> p
p .-^ diff = p .+^ (-diff)

-- * base types

instance Affine Time.UTCTime where
    type Diff Time.UTCTime = Time.NominalDiffTime
    (.-.) = Time.diffUTCTime
    (.+^) = flip Time.addUTCTime

-- Match (+) and (-).
infixl 6 .+^
infixl 6 .-^
infixl 6 .-.

-- I could put in the basic numeric types, but why?

-- * pitch

-- | Absolute chromatic steps.
newtype Chromatic = Chromatic Int
    deriving (Show, Eq, Ord)
-- | Relative chromatic steps, modulo pc_per_octave.
newtype ChromaticSteps = ChromaticSteps Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

instance Affine Chromatic where
    type Diff Chromatic = ChromaticSteps
    Chromatic a .-. Chromatic b = ChromaticSteps (a - b)
    Chromatic p .+^ ChromaticSteps v = Chromatic (p + v)

newtype Diatonic = Diatonic Int
    deriving (Show, Eq, Ord)
newtype DiatonicSteps = DiatonicSteps Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

instance Affine Diatonic where
    type Diff Diatonic = DiatonicSteps
    Diatonic a .-. Diatonic b = DiatonicSteps (a - b)
    Diatonic p .+^ DiatonicSteps v = Diatonic (p + v)

-- I'd like to put these in RealFrac so I can get round, floor, properFraction,
-- but that also gives realToFrac, which could then coerce between absolute and
-- relative, which defeats the purpose of keeping them separate.  Also it would
-- force me to put FChromatic and FDiatonic into Num, which would let me add
-- them, which also defeats the purpose.

newtype FChromatic = FChromatic Double
    deriving (Show, Eq, Ord)
newtype FChromaticSteps = FChromaticSteps Double
    deriving (Show, Eq, Ord, Num)

instance Affine FChromatic where
    type Diff FChromatic = FChromaticSteps
    FChromatic a .-. FChromatic b = FChromaticSteps (a - b)
    FChromatic p .+^ FChromaticSteps v = FChromatic (p + v)

newtype FDiatonic = FDiatonic Double
    deriving (Show, Eq, Ord)
newtype FDiatonicSteps = FDiatonicSteps Double
    deriving (Show, Eq, Ord, Num)

instance Affine FDiatonic where
    type Diff FDiatonic = FDiatonicSteps
    FDiatonic a .-. FDiatonic b = FDiatonicSteps (a - b)
    FDiatonic p .+^ FDiatonicSteps v = FDiatonic (p + v)
