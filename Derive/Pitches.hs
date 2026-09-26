-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for 'PSignal.PSignal's and 'PSignal.Pitch's.
--
-- Functions here can't go into "Derive.PSignal" itself due to circular
-- imports---PSignal is a low level module imported by other low level
-- modules like "Derive.Score".
module Derive.Pitches where
import qualified Util.Segment as Segment
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.PSignal as PSignal

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import           Global


scale :: Derive.Scale -> PSignal.Scale
scale scale =
    PSignal.Scale (Derive.scaleId scale) (Derive.scaleTransposers scale)

-- | A pitch interpolated a certain distance between two other pitches.
interpolated :: PSignal.Pitch -> PSignal.Pitch -> Double -> PSignal.Pitch
interpolated p1 p2 =
    PSignal.interpolate (Segment.Sample 0 p1) (Segment.Sample 1 p2)
    . RealTime.seconds

-- | Transpose a pitch.
transpose :: Pitch.Transpose -> PSignal.RawPitch a -> PSignal.RawPitch a
transpose t = PSignal.addControl control val
    where (val, control) = Controls.transposeControl t

transposeNn :: Pitch.NoteNumber -> PSignal.RawPitch a -> PSignal.RawPitch a
transposeNn = transpose . Pitch.Nn . realToFrac

transposeD :: Pitch.Step -> PSignal.RawPitch a -> PSignal.RawPitch a
transposeD = transpose . Pitch.Diatonic . fromIntegral

transposeC :: Pitch.Step -> PSignal.RawPitch a -> PSignal.RawPitch a
transposeC = transpose . Pitch.Chromatic . fromIntegral

-- | Convert a Pitch to a NoteNumber, throwing an exception if the pitch
-- failed.
pitchNn :: PSignal.Transposed -> Derive.Deriver Pitch.NoteNumber
pitchNn = either (Derive.throw . ("evaluating pitch nn: " <>) . pretty)
    pure . PSignal.pitchNn

-- | Like 'pitch_nn', but return the Note.
pitchNote :: PSignal.Transposed -> Derive.Deriver Pitch.Note
pitchNote = either (Derive.throw . ("evaluating symbolic pitch: " <>) . pretty)
    pure . PSignal.pitchNote

-- | Modify a pitch by hz.  Its symbolic pitch will remain the same.
modifyHz :: PSignal.Scale -> (Pitch.Hz -> Pitch.Hz) -> PSignal.Pitch
    -> PSignal.Pitch
modifyHz scale modify pitch =
    PSignal.pitch scale (pitchNn modify pitch)
        (PSignal.pitchEvalNote pitch) (PSignal.pitchConfig pitch)
    where
    pitchNn modify pitch = \config -> do
        nn <- PSignal.pitchNn $ PSignal.coerce $
            PSignal.applyConfig config pitch
        pure $ Pitch.modifyHz modify nn

equal :: DeriveT.RawPitch a -> DeriveT.RawPitch a -> Bool
equal = DeriveT.pitchesEqual
