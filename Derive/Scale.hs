-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale (Scale(..), ScaleError(..), module Derive.Scale) where
import Derive.Derive (Scale(..), ScaleError(..))
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


type PitchNn = TrackLang.Environ -> PitchSignal.ControlValMap
    -> Either PitchSignal.PitchError Pitch.NoteNumber
type PitchNote = TrackLang.Environ -> PitchSignal.ControlValMap
    -> Either PitchSignal.PitchError Pitch.Note

-- | TODO this is incorrect, because you can't get from NN to note if the scale
-- retunes.
nn_to_note :: Scale -> Maybe Pitch.Key -> Pitch.NoteNumber -> Maybe Pitch.Note
nn_to_note scale key nn = scale_input_to_note scale key (to_input nn)
    where to_input (Pitch.NoteNumber n) = Pitch.InputKey n
