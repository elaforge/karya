-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Scale is actually defined in "Derive.Deriver.Monad" to avoid circular
-- imports.  But you should refer to it from here.
--
-- The difference between this and "Derive.Scale.Scales" is that this is
-- intended for using scales, while Scales is intended for implementing them.
module Derive.Scale (module Derive.Derive, module Derive.Scale) where
import Util.Control
import qualified Data.Vector.Unboxed as Vector

import Derive.Derive (Scale(..), Transposition(..), ScaleError(..), Layout)
import qualified Derive.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch


type PitchNn = PitchSignal.PitchConfig
    -> Either PitchSignal.PitchError Pitch.NoteNumber
type PitchNote = PitchSignal.PitchConfig
    -> Either PitchSignal.PitchError Pitch.Note

layout :: [Pitch.Semi] -> Layout
layout = Vector.fromList

no_octaves :: Layout
no_octaves = Vector.empty

diatonic_layout :: Pitch.PitchClass -> Layout
diatonic_layout per_oct = layout $ replicate per_oct 1

-- | Number of chromatic steps in an octave.  Nothing if this scale doesn't
-- have octaves.
semis_per_octave :: Layout -> Pitch.Semi
semis_per_octave = Vector.sum

semis_at_pc :: Layout -> Pitch.PitchClass -> Pitch.Semi
semis_at_pc layout pc = case pc_per_octave layout of
    Nothing -> pc
    Just per_oct -> oct * Vector.sum layout + Vector.sum (Vector.take i layout)
        where (oct, i) = pc `divMod` per_oct

-- | Number of diatonic steps in an octave.  Nothing if this scale doesn't have
-- octaves.  This is the same as 'semis_per_octave' for scales without
-- a diatonic\/chromatic distinction.
pc_per_octave :: Layout -> Maybe Pitch.PitchClass
pc_per_octave layout
    | Vector.null layout = Nothing
    | otherwise = Just $ Vector.length layout

diatonic_difference :: Layout -> Pitch.Pitch -> Pitch.Pitch
    -> Pitch.PitchClass
diatonic_difference layout (Pitch.Pitch oct1 (Pitch.Degree pc1 _))
        (Pitch.Pitch oct2 (Pitch.Degree pc2 _)) =
    oct_diff + (pc1 - pc2)
    where oct_diff = maybe 0 (* (oct1-oct2)) (pc_per_octave layout)

chromatic_difference :: Layout -> Pitch.Pitch -> Pitch.Pitch -> Pitch.Semi
chromatic_difference layout (Pitch.Pitch oct1 (Pitch.Degree pc1 acc1))
        (Pitch.Pitch oct2 (Pitch.Degree pc2 acc2)) =
    oct_diff + (semis_at_pc layout pc1 - semis_at_pc layout pc2) + (acc1 - acc2)
    where oct_diff = semis_per_octave layout * (oct1 - oct2)

transpose :: Transposition -> Scale -> Maybe Pitch.Key -> Pitch.Octave
    -> Pitch.Step -> Pitch.Note -> Either ScaleError Pitch.Note
transpose transposition scale key octaves steps =
    scale_show scale key <=< scale_transpose scale transposition key steps
        . Pitch.add_octave octaves <=< scale_read scale key
