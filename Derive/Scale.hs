-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Scale is actually defined in "Derive.Deriver.Monad" to avoid circular
-- imports.  But you should refer to it from here.
module Derive.Scale (Scale(..), ScaleError(..), module Derive.Scale) where
import Derive.Derive (Scale(..), ScaleError(..))
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


type PitchNn = TrackLang.Environ -> Score.ControlValMap
    -> Either PitchSignal.PitchError Pitch.NoteNumber
type PitchNote = TrackLang.Environ -> Score.ControlValMap
    -> Either PitchSignal.PitchError Pitch.Note
