{- | The western tempered 12 note scale.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".

    TODO: this doesn't have any support for enharmonics, but I do want to
    support them for scale sensitive instruments and tunings.

    This, along with pengisep and pengumbang, will probably require that
    scale_note_to_nn and scale_input_to_note be passed performance and input
    context respectively.  And I'll need a flip enharmonic command here.

    middle c = nn 60 = 5c-
-}
module Derive.Scale.Twelve where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control () -- Monad Either instance
import qualified Perform.Pitch as Pitch


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[0-9][a-g][#-]"
    , Pitch.scale_note_to_nn = flip Map.lookup note_to_nn
    , Pitch.scale_input_to_note = flip Map.lookup input_to_note
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*12)
    , Pitch.scale_set_pitch_bend = False
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

(note_to_nn, input_to_note, transpose) = Pitch.make_scale_map
    (zip3 generics notes note_numbers) input_to_generic transpose_generic

generics = [Pitch.GenericPitch (o, n) | o <- [0..9], n <- [0..11]]
notes = [Pitch.Note (o:n) | o <- ['0'..'9'], n <- note_symbols]
note_symbols =
    ["c-", "c#", "d-", "d#", "e-", "f-", "f#", "g-", "g#", "a-", "a#", "b-"]
note_numbers = map Pitch.NoteNumber [0..127]

-- | (0, 14) is the same as (1, 1), so C is available in two places.  Of course
-- this is only visible on the computer keyboard since a midi keyboard won't
-- have 14 keys.
input_to_generic :: [(Pitch.InputKey, Pitch.GenericPitch)]
input_to_generic = do
    oct <- [0..9]
    (degree, (oct_offset, key)) <- input_degrees
    return (Pitch.InputKey (oct, degree),
        Pitch.GenericPitch (oct + oct_offset, key))

input_degrees :: [(Int, (Pitch.Octave, Int))]
input_degrees = zip [0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13] (map ((,) 0) [0..])
    ++ [(14, (1, 0)), (-1, (-1, 11))]

transpose_generic :: Int -> Pitch.GenericPitch -> Pitch.GenericPitch
transpose_generic n (Pitch.GenericPitch (oct, degree)) =
    Pitch.GenericPitch ((oct*12 + degree + n) `divMod` 12)
