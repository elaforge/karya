module Derive.Scale (
    Scale(..)
    , GetNoteNumber
    , ScaleError(..)
    , nn_to_note
) where
import Derive.Derive (Scale(..), ScaleError(..))
import qualified Perform.Pitch as Pitch


-- | Chromatic -> Diatonic -> Key -> NoteNumber
type GetNoteNumber = Double -> Double -> Maybe Pitch.Key
    -> Either ScaleError Pitch.NoteNumber

nn_to_note :: Scale -> Maybe Pitch.Key -> Pitch.NoteNumber -> Maybe Pitch.Note
nn_to_note scale key nn = scale_input_to_note scale key (to_input nn)
    where to_input (Pitch.NoteNumber n) = Pitch.InputKey n
