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
import qualified Util.Pretty as Pretty
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale

import qualified Perform.Pitch as Pitch


scale :: Scale.Scale -> PitchSignal.Scale
scale scale =
    PitchSignal.Scale (Scale.scale_id scale) (Scale.scale_transposers scale)

-- | A pitch interpolated a certain distance between two other pitches.
interpolated :: PitchSignal.Pitch -> PitchSignal.Pitch -> Double
    -> PitchSignal.Pitch
interpolated low high dist =
    PitchSignal.pitch (PitchSignal.pitch_scale low) nn note
    where
    nn controls = do
        low_nn <- PitchSignal.eval_pitch low controls
        high_nn <- PitchSignal.eval_pitch high controls
        return $ Num.scale low_nn high_nn (Pitch.NoteNumber dist)
    note = PitchSignal.eval_note (if dist < 1 then low else high)

-- | Transpose a pitch.
transpose :: Pitch.Transpose -> PitchSignal.Pitch -> PitchSignal.Pitch
transpose t = PitchSignal.add_control control val
    where (val, control) = Controls.transpose_control t

-- | Convert a Pitch to a NoteNumber, throwing an exception if the pitch
-- failed.
pitch_nn :: PitchSignal.Pitch -> Derive.Deriver Pitch.NoteNumber
pitch_nn = either (Derive.throw . ("evaluating pitch: " ++) . Pretty.pretty)
    return . PitchSignal.pitch_nn

-- | Like 'pitch_nn', but return the Note.
pitch_note :: PitchSignal.Pitch -> Derive.Deriver Pitch.Note
pitch_note = either (Derive.throw . ("evaluating pitch: " ++) . Pretty.pretty)
    return . PitchSignal.pitch_note

constant :: Pitch.NoteNumber -> PitchSignal.Pitch
constant nn = PitchSignal.pitch PitchSignal.no_scale
    (const (Right nn)) (const $ Right $ Pitch.Note $ Pretty.prettytxt nn)
