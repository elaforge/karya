-- | Saih Pelegongan.
--
-- Tuning for my gender rambat.
--
-- TODO: pengisep and pengumbang
module Derive.Scale.Legong where

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Util as Util
import qualified Derive.Scale.Symbols as Symbols


scale :: Scale.Scale
scale = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = "[12356](\\.*|\\^*)"
    , Scale.scale_map = Util.make_scale_map scale_map
    -- loaded from Derive.Scale.Symbols
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = Util.transpose scale_map 5
    , Scale.scale_enharmonics = Util.no_enharmonics
    , Scale.scale_note_to_call = Util.note_to_call scale_map
    , Scale.scale_input_to_note = Util.input_to_note scale_map
    , Scale.scale_input_to_nn = Util.mapped_input_to_nn scale_map
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "legong"

scale_map :: Util.ScaleMap
scale_map = Util.scale_map (align notes) (align inputs) note_numbers

note_numbers :: [Pitch.NoteNumber]
note_numbers = map Pitch.nn
    [ 50.8 -- 2.., ugal begin
    , 51.82 -- 3.., rambat begin
    , 55.7
    , 56.82 -- 6.., trompong begin

    , 60.73
    , 62.8 -- 2., pemade begin
    , 63.35 -- 3., reyong begin
    , 67.7
    , 68.2

    , 72.46 -- 1
    , 73.9 -- 2, kantilan begin
    , 75.5
    , 79.4 -- 5, trompong end
    , 80.5

    , 84.46 -- 1^, rambat end, pemade end
    , 86
    , 87.67
    , 91.74 -- 5^, reyong end
    , 92.5

    , 96.46 -- 1^^, kantilan end
    ]

-- Line a list starting with nding up with 'note_numbers'.
align = take (length note_numbers) . drop 1
center = 9 -- index of middle pitch

notes :: [Pitch.Note]
notes = map Symbols.dotted_number
    [(num, oct) | oct <- [-2..2], num <- [1, 2, 3, 5, 6]]

inputs :: [Pitch.InputKey]
inputs = [Pitch.middle_c + fromIntegral (o*12) + d | o <- [-2..2], d <- keys]
    where keys = [Util.i_c, Util.i_d, Util.i_e, Util.i_f, Util.i_g]
