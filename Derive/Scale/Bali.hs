-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support types for Balinese scales.  Unlike BaliScales, this should be
-- independent of the scale framework and have light dependencies.
module Derive.Scale.Bali where
import qualified Util.Seq as Seq
import qualified Perform.Pitch as Pitch


-- | Pitch for saih pitu.  Pemero and penyorog are Es and As respectively.
data Pitch = I | O | E | Es | U | A | As
    deriving (Eq, Ord, Enum, Show, Bounded)

-- | Extend a scale downwards and upwards, assuming the extended octaves
-- are exactly 2:1.  The input should have at least an octave's worth of
-- pitches.
extend_scale :: Pitch.PitchClass -> Pitch.Pitch -- ^ extend down to here
    -> Pitch.Pitch -- ^ extend up to here
    -> Pitch.Pitch -- ^ from this original starting point
    -> [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend_scale per_octave low high start nns =
    reverse (take to_low down) ++ nns ++ take to_high up
    where
    to_low = Pitch.diff_pc per_octave start low
    -- 'high' is inclusive, so +1.
    to_high = Pitch.diff_pc per_octave high start - length nns + 1
    down =
        [ nn - 12 * fromIntegral oct
        | oct <- [1..], nn <- reverse (take per_octave nns)
        ]
    up =
        [ nn + 12 * fromIntegral oct
        | oct <- [1..], nn <- Seq.rtake per_octave nns
        ]
