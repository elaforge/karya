-- | Saih gender wayang.
module Derive.Scale.Wayang where
import qualified Data.Map as Map

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale.Util as Util
import qualified Util.Seq as Seq


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[12356](\\.*|\\^*)"
    , Pitch.scale_note_to_nn = Util.note_to_nn degree_map
    , Pitch.scale_note_to_generic = Util.note_to_generic degree_map
    , Pitch.scale_input_to_note = Util.input_to_note input_map
    , Pitch.scale_input_to_nn = Util.input_to_nn input_map
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*5)
    , Pitch.scale_set_pitch_bend = True
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "wayang"

transpose :: Pitch.Transposer
transpose = Util.transpose degree_to_num num_to_degree

note_numbers_umbang :: [Pitch.NoteNumber]
note_numbers_umbang = map Pitch.nn
    [ 53 -- 6.., pemade begin

    , 55.15
    , 57.73
    , 60.4
    , 62.95
    , 64.7 -- 6., kantilan begin

    , 67.57 -- 1
    , 69.45
    , 72.1
    , 74.83 -- 5, pemade end
    , 76.85

    , 79.48
    , 81.63
    , 84.12
    , 86.88 -- 5^, kantilan end
    ]

note_numbers_isep :: [Pitch.NoteNumber]
note_numbers_isep = map Pitch.nn
    [ 52.3

    , 54.55
    , 57.35
    , 59.85
    , 62.5
    , 64.45 -- 6., kantilan begin

    , 67.26
    , 69.25
    , 71.81
    , 74.63 -- 5, pemade end
    , 76.73

    , 79.35
    , 81.51
    , 84
    , 86.78 -- 5^, kantilan end
    ]

note_numbers :: [Pitch.NoteNumber]
note_numbers = note_numbers_umbang

-- Line a list starting with nding up with 'note_numbers'.
align = take (length note_numbers) . drop 4

generics :: [Pitch.Generic]
generics = align
    [Pitch.Generic oct deg | oct <- [Pitch.middle_octave-2 ..], deg <- [0..4]]

degrees = align [(d:o) | o <- ["..", ".", "", "^", "^^"], d <- "12356"]
degree_to_num = Map.fromList (zip degrees [0..])
num_to_degree = Map.fromList (zip [0..] degrees)

degree_map :: Util.DegreeMap
degree_map = Map.fromList $ zip degrees
    [Util.Degree nn generic | (nn, generic)
        <- zip (Seq.zip_neighbors note_numbers) generics]

input_map :: Util.InputMap
input_map = Map.fromList $ zip inputs (zip note_numbers degrees)

input_keys = [Util.i_c, Util.i_d, Util.i_e, Util.i_f, Util.i_g]
inputs = align
    [Pitch.InputKey (middle + o*12 + d) | o <- [-2..2], d <- input_keys]
    where (Pitch.InputKey middle) = Pitch.middle_c
