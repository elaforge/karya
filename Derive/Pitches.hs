-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for 'PitchSignal.Signal's and 'PitchSignal.Pitch's.
--
-- Functions here can't go into "Derive.PitchSignal" itself due to circular
-- imports---PitchSignal is a low level module imported by other low level
-- modules like "Derive.Score".
module Derive.Pitches where
import qualified Util.Num as Num
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale

import qualified Perform.Pitch as Pitch
import Global


scale :: Scale.Scale -> PitchSignal.Scale
scale scale =
    PitchSignal.Scale (Scale.scale_id scale) (Scale.scale_transposers scale)

-- | A pitch interpolated a certain distance between two other pitches.
interpolated :: PitchSignal.Pitch -> PitchSignal.Pitch -> Double
    -> PitchSignal.Pitch
interpolated low high dist =
    PitchSignal.pitch (PitchSignal.pitch_scale low) nn note mempty
    where
    nn config = do
        low_nn <- PitchSignal.pitch_nn $ PitchSignal.coerce $
            PitchSignal.config config low
        high_nn <- PitchSignal.pitch_nn $ PitchSignal.coerce $
            PitchSignal.config config high
        return $ Num.scale low_nn high_nn (Pitch.NoteNumber dist)
    note = PitchSignal.pitch_eval_note $ PitchSignal.coerce $
        if dist < 1 then low else high

-- | Transpose a pitch.
transpose :: Pitch.Transpose -> PitchSignal.RawPitch a -> PitchSignal.RawPitch a
transpose t = PitchSignal.add_control control val
    where (val, control) = Controls.transpose_control t

transpose_d :: Pitch.Step -> PitchSignal.RawPitch a -> PitchSignal.RawPitch a
transpose_d = transpose . Pitch.Diatonic . fromIntegral

transpose_c :: Pitch.Step -> PitchSignal.RawPitch a -> PitchSignal.RawPitch a
transpose_c = transpose . Pitch.Chromatic . fromIntegral

-- | Convert a Pitch to a NoteNumber, throwing an exception if the pitch
-- failed.
pitch_nn :: PitchSignal.Transposed -> Derive.Deriver Pitch.NoteNumber
pitch_nn = either (Derive.throw . ("evaluating pitch: " <>) . prettys)
    return . PitchSignal.pitch_nn

-- | Like 'pitch_nn', but return the Note.
pitch_note :: PitchSignal.Transposed -> Derive.Deriver Pitch.Note
pitch_note = either (Derive.throw . ("evaluating pitch: " <>) . prettys)
    return . PitchSignal.pitch_note

-- | Create a Pitch that only emits the given NoteNumber, and doesn't respond
-- to transposition.
nn_pitch :: Pitch.NoteNumber -> PitchSignal.Pitch
nn_pitch nn = PitchSignal.pitch PitchSignal.no_scale
    (const (Right nn)) (const $ Right $ Pitch.Note $ pretty nn)
    mempty
