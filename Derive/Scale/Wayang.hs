-- | Saih gender wayang.
module Derive.Scale.Wayang where

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale.Util as Util
import qualified Derive.Scale.Symbols as Symbols


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[12356](\\.*|\\^*)"
    , Pitch.scale_map = Util.make_scale_map scale_map
    -- loaded from Derive.Scale.Symbols
    , Pitch.scale_symbols = []
    , Pitch.scale_octave = 5

    , Pitch.scale_note_to_call = Util.note_to_call scale_map
    , Pitch.scale_input_to_note = Util.input_to_note scale_map
    , Pitch.scale_input_to_nn = Util.input_to_nn scale_map
    , Pitch.scale_degree_to_nn = Util.degree_to_nn scale_map
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "wayang"

scale_map :: Util.ScaleMap
scale_map =
    Util.scale_map (align steps) (align inputs) note_numbers degrees

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
center = 5 -- index of middle pitch

steps :: [Pitch.Note]
steps = map Symbols.dotted_number
    [(num, oct) | oct <- [-2..2], num <- [1, 2, 3, 5, 6]]

input_keys = [Util.i_c, Util.i_d, Util.i_e, Util.i_f, Util.i_g]
inputs = [Pitch.InputKey (middle + o*12 + d) | o <- [-2..2], d <- input_keys]
    where (Pitch.InputKey middle) = Pitch.middle_c

degrees :: [Util.IntDegree]
degrees = [Pitch.middle_int_degree - center ..]
