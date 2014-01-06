-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Scale is actually defined in "Derive.Deriver.Monad" to avoid circular
-- imports.  But you should refer to it from here.
module Derive.Scale (Scale(..), ScaleError(..), module Derive.Scale) where
import qualified Data.Vector.Unboxed as Vector

import Derive.Derive (Scale(..), ScaleError(..), Layout)
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Theory as Theory

import qualified Perform.Pitch as Pitch


type PitchNn = PitchSignal.PitchConfig
    -> Either PitchSignal.PitchError Pitch.NoteNumber
type PitchNote = PitchSignal.PitchConfig
    -> Either PitchSignal.PitchError Pitch.Note

layout :: [Theory.Semi] -> Layout
layout = Vector.fromList

no_octaves :: Layout
no_octaves = Vector.empty

diatonic_layout :: Theory.PitchClass -> Layout
diatonic_layout per_oct = layout $ replicate per_oct 1

-- | Number of chromatic steps in an octave.  Nothing if this scale doesn't
-- have octaves.
semis_per_octave :: Layout -> Theory.Semi
semis_per_octave = Vector.sum

semis_at_pc :: Layout -> Theory.PitchClass -> Theory.Semi
semis_at_pc layout pc = case pc_per_octave layout of
    Nothing -> pc
    Just per_oct -> oct * Vector.sum layout + Vector.sum (Vector.take i layout)
        where (oct, i) = pc `divMod` per_oct

-- | Number of diatonic steps in an octave.  Nothing if this scale doesn't have
-- octaves.  This is the same as 'semis_per_octave' for scales without
-- a diatonic\/chromatic distinction.
pc_per_octave :: Layout -> Maybe Theory.PitchClass
pc_per_octave layout
    | Vector.null layout = Nothing
    | otherwise = Just $ Vector.length layout

diatonic_difference :: Layout -> Theory.Pitch -> Theory.Pitch
    -> Theory.PitchClass
diatonic_difference layout (Theory.Pitch oct1 (Theory.Note pc1 _))
        (Theory.Pitch oct2 (Theory.Note pc2 _)) =
    oct_diff + (pc1 - pc2)
    where oct_diff = maybe 0 (* (oct1-oct2)) (pc_per_octave layout)

chromatic_difference :: Layout -> Theory.Pitch -> Theory.Pitch -> Theory.Semi
chromatic_difference layout (Theory.Pitch oct1 (Theory.Note pc1 acc1))
        (Theory.Pitch oct2 (Theory.Note pc2 acc2)) =
    oct_diff + (semis_at_pc layout pc1 - semis_at_pc layout pc2) + (acc1 - acc2)
    where oct_diff = semis_per_octave layout * (oct1 - oct2)
