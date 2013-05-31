-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Octa where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector

import Util.Control
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TwelveScales as TwelveScales
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ make_scale octa21_id (Theory.layout [2, 1, 2, 1, 2, 1, 2, 1]) "21"
    , make_scale octa12_id (Theory.layout [1, 2, 1, 2, 1, 2, 1, 2]) "12"
    ]

octa21_id :: Pitch.ScaleId
octa21_id = Pitch.ScaleId "octa21"

octa12_id :: Pitch.ScaleId
octa12_id = Pitch.ScaleId "octa12"

make_scale :: Pitch.ScaleId -> Theory.Layout -> Text -> Scale.Scale
make_scale scale_id layout key_suffix = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = "[-1-9][a-h](b|bb|#|x)?"
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = TwelveScales.transpose scale_map
    , Scale.scale_enharmonics = TwelveScales.enharmonics scale_map
    , Scale.scale_note_to_call = TwelveScales.note_to_call scale_map
    , Scale.scale_input_to_note = TwelveScales.input_to_note scale_map
    , Scale.scale_input_to_nn = Util.direct_input_to_nn
    , Scale.scale_call_doc = TwelveScales.call_doc Util.standard_transposers
        scale_map
        "Octatonic scales as true 8 note scales, using notes from a-h.\
        \ There are two variants: octa21 starts with a whole step, while\
        \ octa12 starts with a half-step."
    }
    where
    scale_map = TwelveScales.scale_map layout all_pitches keys deflt
        where Just deflt = Map.lookup (Pitch.Key $ "a-" <> key_suffix) keys
    keys = all_keys layout key_suffix

all_notes :: [Theory.Note]
all_notes = [Theory.Note pc accs | pc <- [0..7], accs <- [-1..1]]

all_pitches :: [Theory.Pitch]
all_pitches = [Theory.Pitch oct note | oct <- [-2..9], note <- all_notes]

make_keys :: Theory.Layout -> Text -> [Theory.Semi] -> [Theory.Key]
make_keys layout name intervals =
    [Theory.key tonic name intervals layout
        | tonic <- all_notes, abs (Theory.note_accidentals tonic) <= 1]

all_keys :: Theory.Layout -> Text -> Map.Map Pitch.Key Theory.Key
all_keys layout name = Map.fromList $ zip (map Theory.show_key keys) keys
    where
    keys = make_keys layout name $
        Vector.toList (Theory.layout_intervals layout)
