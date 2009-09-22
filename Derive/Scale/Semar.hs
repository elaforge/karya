-- | Saih Semar Pegulingan.
--
-- Tuning for my pelog instruments.
--
-- TODO: pengisep and pengumbang
module Derive.Scale.Semar where
import qualified Data.Map as Map
import qualified Util.Seq as Seq

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale.Util as Util

scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[12356](\\.*|\\^*)"
    , Pitch.scale_note_to_nn = Util.note_to_nn degree_map
    , Pitch.scale_input_to_note = Util.input_to_note input_map
    , Pitch.scale_input_to_nn = Util.input_to_nn input_map
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*5)
    , Pitch.scale_set_pitch_bend = True
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "semar"

transpose :: Pitch.Transposer
transpose = Util.transpose degree_to_num num_to_degree

note_numbers :: [Pitch.NoteNumber]
note_numbers = map Pitch.nn
    [ 50.8 -- ugal begin
    , 51.82 -- rambat begin
    , 55.7
    , 56.82 -- trompong begin

    , 60.73
    , 62.8 -- pemade begin
    , 63.35 -- reyong begin
    , 67.7
    , 68.2

    , 72.46 -- "middle nding", should be played by kbd middle c
    , 73.9 -- kantilan begin
    , 75.5
    , 79.4 -- trompong end
    , 80.5

    , 84.46 -- rambat end, pemade end
    , 86
    , 87.67
    , 91.74 -- reyong end
    , 92.5

    , 96.46 -- kantilan end
    ]

-- Line a list starting with nding up with 'note_numbers'.
align = take (length note_numbers) . drop 1

-- | These are not in the scale, but you can get them if you bend pitch from
-- the top or bottom note.
low_nn = Pitch.NoteNumber 48.73
high_nn = Pitch.NoteNumber 98

degrees = align [(d:o) | o <- ["..", ".", "", "^", "^^"], d <- "12356"]
degree_to_num = Map.fromList (zip degrees [0..])
num_to_degree = Map.fromList (zip [0..] degrees)

degree_map :: Util.DegreeMap
degree_map = Map.fromList $ zip degrees (Seq.zip_neighbors note_numbers)

input_keys = [Util.i_c, Util.i_d, Util.i_e, Util.i_f, Util.i_g]
inputs = align
    [Pitch.InputKey (middle + o*12 + d) | o <- [-2..2], d <- input_keys]
    where (Pitch.InputKey middle) = Pitch.middle_c

input_map :: Util.InputMap
input_map = Map.fromList $ zip inputs (zip note_numbers degrees)
