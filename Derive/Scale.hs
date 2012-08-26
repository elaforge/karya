module Derive.Scale (Scale(..), ScaleError(..), module Derive.Scale) where
import Derive.Derive (Scale(..), ScaleError(..))
import qualified Derive.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch


type NoteCall = Maybe Pitch.Key -> PitchSignal.Controls
    -> Either PitchSignal.PitchError Pitch.NoteNumber

nn_to_note :: Scale -> Maybe Pitch.Key -> Pitch.NoteNumber -> Maybe Pitch.Note
nn_to_note scale key nn = scale_input_to_note scale key (to_input nn)
    where to_input (Pitch.NoteNumber n) = Pitch.InputKey n
