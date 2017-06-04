-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for 'PSignal.PSignal's and 'PSignal.Pitch's.
--
-- Functions here can't go into "Derive.PSignal" itself due to circular
-- imports---PSignal is a low level module imported by other low level
-- modules like "Derive.Score".
module Derive.Pitches where
import qualified Util.Num as Num
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale

import qualified Perform.Pitch as Pitch
import Global


scale :: Scale.Scale -> PSignal.Scale
scale scale =
    PSignal.Scale (Scale.scale_id scale) (Scale.scale_transposers scale)

-- | A pitch interpolated a certain distance between two other pitches.
interpolated :: PSignal.Pitch -> PSignal.Pitch -> Double -> PSignal.Pitch
interpolated low high dist =
    PSignal.pitch (PSignal.pitch_scale low) nn note mempty
    where
    nn config = do
        low_nn <- PSignal.pitch_nn $ PSignal.coerce $
            PSignal.config config low
        high_nn <- PSignal.pitch_nn $ PSignal.coerce $
            PSignal.config config high
        return $ Num.scale low_nn high_nn (Pitch.NoteNumber dist)
    note = PSignal.pitch_eval_note $ PSignal.coerce $
        if dist < 1 then low else high

-- | Transpose a pitch.
transpose :: Pitch.Transpose -> PSignal.RawPitch a -> PSignal.RawPitch a
transpose t = PSignal.add_control control val
    where (val, control) = Controls.transpose_control t

transpose_nn :: Pitch.NoteNumber -> PSignal.RawPitch a -> PSignal.RawPitch a
transpose_nn = transpose . Pitch.Nn . realToFrac

transpose_d :: Pitch.Step -> PSignal.RawPitch a -> PSignal.RawPitch a
transpose_d = transpose . Pitch.Diatonic . fromIntegral

transpose_c :: Pitch.Step -> PSignal.RawPitch a -> PSignal.RawPitch a
transpose_c = transpose . Pitch.Chromatic . fromIntegral

-- | Convert a Pitch to a NoteNumber, throwing an exception if the pitch
-- failed.
pitch_nn :: PSignal.Transposed -> Derive.Deriver Pitch.NoteNumber
pitch_nn = either (Derive.throw . ("evaluating pitch nn: " <>) . pretty)
    return . PSignal.pitch_nn

-- | Like 'pitch_nn', but return the Note.
pitch_note :: PSignal.Transposed -> Derive.Deriver Pitch.Note
pitch_note = either (Derive.throw . ("evaluating symbolic pitch: " <>) . pretty)
    return . PSignal.pitch_note

-- | Modify a pitch by hz.  Its symbolic pitch will remain the same.
modify_hz :: PSignal.Scale -> (Pitch.Hz -> Pitch.Hz) -> PSignal.Pitch
    -> PSignal.Pitch
modify_hz scale modify pitch =
    PSignal.pitch scale (pitch_nn modify pitch)
        (PSignal.pitch_eval_note pitch) (PSignal.pitch_config pitch)
    where
    pitch_nn modify pitch = \config -> do
        nn <- PSignal.pitch_nn $ PSignal.coerce $ PSignal.config config pitch
        return $ Pitch.modify_hz modify nn

equal :: BaseTypes.RawPitch a -> BaseTypes.RawPitch a -> Bool
equal = BaseTypes.pitches_equal
