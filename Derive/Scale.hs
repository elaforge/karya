module Derive.Scale (
    Scale(..)
    , Error(..)
    , GetNoteNumber
    , nn_to_note
) where
import Derive.Derive (Scale(..))
import qualified Perform.Pitch as Pitch


-- | Errors that can be returned by GetNoteNumber.
data Error = InvalidTransposition | UnparseableKey | KeyNeeded

-- | Chromatic -> Diatonic -> Key -> NoteNumber
type GetNoteNumber = Double -> Double -> Maybe Pitch.Key
    -> Either Error Pitch.NoteNumber

nn_to_note :: Scale -> Maybe Pitch.Key -> Pitch.NoteNumber -> Maybe Pitch.Note
nn_to_note scale key nn = scale_input_to_note scale key (to_input nn)
    where to_input (Pitch.NoteNumber n) = Pitch.InputKey n
