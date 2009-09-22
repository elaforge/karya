-- | Saih gender wayang.
module Derive.Scale.Wayang where
import qualified Data.Map as Map

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale.Util as Util
import qualified Util.Seq as Seq


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[12356](\\.*|\\^*)"
    , Pitch.scale_note_to_nn = Util.note_to_nn degree_to_nn
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

note_numbers2 = map Pitch.nn
    [ 53
    , 55.15
    , 57.73
    , 60.4
    , 62.95
    ]

-- TODO: get the real pitches
note_numbers :: [Pitch.NoteNumber]
note_numbers = map Pitch.nn
    [ 50.0 -- pemade begin

    , 53
    , 55
    , 57
    , 60
    , 62 -- kantilan begin

    , 65 -- "middle nding", should be played by kbd middle c
    , 67
    , 69
    , 72 -- pemade end
    , 74

    , 77
    , 79
    , 81
    , 84 -- kantilan end
    ]

-- Line a list starting with nding up with 'note_numbers'.
align = take (length note_numbers) . drop 4

degrees = align [(d:o) | o <- ["..", ".", "", "^", "^^"], d <- "12356"]
degree_to_num = Map.fromList (zip degrees [0..])
num_to_degree = Map.fromList (zip [0..] degrees)

degree_to_nn :: Util.DegreeMap
degree_to_nn = Map.fromList $ zip degrees (Seq.zip_neighbors note_numbers)

input_map :: Util.InputMap
input_map = Map.fromList $ zip inputs (zip note_numbers degrees)

input_keys = [Util.i_c, Util.i_d, Util.i_e, Util.i_f, Util.i_g]
inputs = align
    [Pitch.InputKey (middle + o*12 + d) | o <- [-2..2], d <- input_keys]
    where (Pitch.InputKey middle) = Pitch.middle_c
