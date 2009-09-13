-- | Saih Semar Pegulingan.
--
-- Tuning for my pelog instruments.
--
-- TODO: pengisep and pengumbang
module Derive.Scale.Semar where
import qualified Data.Map as Map

import qualified Perform.Pitch as Pitch
import Util.Control () -- Monad Either instance

scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[12356](\\.*|\\^*)"
    , Pitch.scale_note_to_nn = flip Map.lookup note_to_nn
    , Pitch.scale_input_to_note = flip Map.lookup input_to_note
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*5)
    , Pitch.scale_set_pitch_bend = True
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "semar"

(note_to_nn, input_to_note, transpose) = Pitch.make_scale_map
    (zip3 generics notes note_numbers) input_to_generic transpose_generic

note_numbers :: [Pitch.NoteNumber]
note_numbers = map Pitch.nn
    [ 50.8 -- ugal begin
    , 51.82 -- rambat begin
    , 55.7

    , 56.82 -- trompong begin
    , 60.73
    , 62.8
    , 63.35 -- reyong begin
    , 67.7
    , 68.2 -- pemade begin

    , 72.46
    , 73.9
    , 75.5
    , 79.4 -- trompong end
    , 80.5 -- kantilan begin

    , 84.46 -- rambat end, pemade end
    , 86
    , 87.67
    , 91.74 -- reyong end
    , 92.5

    , 96.46 -- kantilan end
    ]

generics = take (length note_numbers) $ drop 1
    [Pitch.GenericPitch (o, n) | o <- [-2..2], n <- [0..4]]
notes = take (length note_numbers) $ drop 1
    [Pitch.Note (d:o) | o <- ["..", ".", "", "^", "^^"], d <- "12356"]

input_to_generic :: [(Pitch.InputKey, Pitch.GenericPitch)]
input_to_generic = do
    oct <- [-2..2]
    (degree, (oct_offset, key)) <- input_degrees
    return (Pitch.InputKey (oct, degree),
        Pitch.GenericPitch (oct + oct_offset, key))

input_degrees :: [(Int, (Pitch.Octave, Int))]
input_degrees = zip [0, 2, 4, 6, 8] (map ((,) 0) [0..]) ++ [(10, (1, 0))]

transpose_generic :: Int -> Pitch.GenericPitch -> Pitch.GenericPitch
transpose_generic n (Pitch.GenericPitch (oct, degree)) =
    Pitch.GenericPitch ((oct*5 + degree + n) `divMod` 5)
