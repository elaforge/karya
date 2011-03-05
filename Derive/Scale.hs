module Derive.Scale (
    pitch
    -- * Scale
    , Scale(..)
    , degree_to_double
    , non_transposing
) where
import qualified Data.Maybe as Maybe

import Derive.Derive (Scale(..))
import qualified Perform.Pitch as Pitch


-- | Verify that the string is a valid note in the scale, and return the Pitch.
pitch :: Scale -> String -> Maybe Pitch.Pitch
pitch scale note_s
    | note_in_scale scale note = Just (Pitch.Pitch (scale_id scale) note)
    | otherwise = Nothing
    where note = Pitch.Note note_s

-- | Tie together Pitches and their Scales.
-- type ScaleMap = Map.Map Pitch.ScaleId Scale

-- | Make the function for 'Perform.PitchSignal.to_nn', since it can't import
-- this module to do it itself.
degree_to_double :: Scale -> Pitch.Degree -> Maybe Double
degree_to_double scale d = fmap un_nn (scale_degree_to_nn scale d)
    where un_nn (Pitch.NoteNumber n) = n

note_in_scale :: Scale -> Pitch.Note -> Bool
note_in_scale scale = Maybe.isJust . scale_note_to_call scale

-- | Transpose function for a non-transposing scale.
--
-- I can't use Derive.Transpose because of circular imports.
non_transposing :: Pitch.Octave -> Integer -> Pitch.Note -> Maybe Pitch.Note
non_transposing _ _ _ = Nothing
