module Derive.Scale (
    Scale(..)
    , Error(..)
    , GetNoteNumber
) where
import Derive.Derive (Scale(..))
import qualified Perform.Pitch as Pitch


-- | Errors that can be returned by GetNoteNumber.
data Error = InvalidTransposition | UnparseableKey | KeyNeeded

-- | Chromatic -> Diatonic -> Key -> NoteNumber
type GetNoteNumber = Double -> Double -> Maybe Pitch.Key
    -> Either Error Pitch.NoteNumber
