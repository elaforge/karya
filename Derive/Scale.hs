module Derive.Scale (
    pitch
    -- * Scale
    , Scale(..)
    , Error(..)
    , GetNoteNumber
) where
import qualified Data.Maybe as Maybe

import Derive.Derive (Scale(..))
import qualified Perform.Pitch as Pitch


-- | Verify that the string is a valid note in the scale, and return the
-- Pitch.
pitch :: Scale -> String -> Maybe Pitch.Pitch
pitch scale note_s
    | note_in_scale scale note = Just (Pitch.Pitch (scale_id scale) note)
    | otherwise = Nothing
    where
    note = Pitch.Note note_s
    note_in_scale scale = Maybe.isJust . scale_note_to_call scale

-- | Errors that can be returned by GetNoteNumber.
data Error = InvalidTransposition | UnparseableKey | KeyNeeded

-- | Chromatic -> Diatonic -> Key -> NoteNumber
type GetNoteNumber = Double -> Double -> Maybe Pitch.Key
    -> Either Error Pitch.NoteNumber
