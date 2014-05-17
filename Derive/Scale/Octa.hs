-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Octa where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector

import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ make_scale "octa21" layout21 keys21 absolute_fmt
    , make_scale "octa12" layout12 keys12 absolute_fmt
    , make_scale "octa21-r" layout21 keys21 (relative_fmt keys21)
    , make_scale "octa12-r" layout12 keys12 (relative_fmt keys12)
    ]
    where
    layout21 = Theory.layout [2, 1, 2, 1, 2, 1, 2, 1]
    layout12 = Theory.layout [1, 2, 1, 2, 1, 2, 1, 2]
    keys21 = all_keys layout21
    keys12 = all_keys layout12

absolute_fmt :: TheoryFormat.Format
absolute_fmt =
    TheoryFormat.make_absolute_format (TheoryFormat.make_pattern degrees)
        degrees TheoryFormat.ascii_accidentals
    where
    degrees = TheoryFormat.make_degrees ["a", "b", "c", "d", "e", "f", "g", "h"]

relative_fmt :: ChromaticScales.Keys -> TheoryFormat.Format
relative_fmt keys = make $ TheoryFormat.RelativeFormat
    { TheoryFormat.rel_acc_fmt = TheoryFormat.ascii_accidentals
    , TheoryFormat.rel_parse_key = parse_key
    , TheoryFormat.rel_default_key = default_tkey
    , TheoryFormat.rel_show_degree = TheoryFormat.show_degree_chromatic
    , TheoryFormat.rel_to_absolute = TheoryFormat.chromatic_to_absolute
    , TheoryFormat.rel_key_tonic = Pitch.degree_pc . Theory.key_tonic
    }
    where
    parse_key = Scales.get_key default_tkey keys
    Just default_tkey = Map.lookup default_key keys
    make = TheoryFormat.make_relative_format (TheoryFormat.make_pattern degrees)
        degrees
    degrees = TheoryFormat.make_degrees
        ["一", "二", "三", "四", "五", "六", "七", "八"]

make_scale :: Pitch.ScaleId -> Theory.Layout -> ChromaticScales.Keys
    -> TheoryFormat.Format -> Scale.Scale
make_scale scale_id layout keys fmt = Scales.set_direct_input_to_nn $
    ChromaticScales.make_scale scale_map scale_id doc
    where
    scale_map = ChromaticScales.scale_map layout fmt keys default_tkey
    Just default_tkey = Map.lookup default_key keys
    doc = "Octatonic scales as true 8 note scales, using notes from a-h.\
        \ There are two variants: octa21 starts with a whole step, while\
        \ octa12 starts with a half-step."

default_key :: Pitch.Key
default_key = Pitch.Key "a"

all_degrees :: [Pitch.Degree]
all_degrees = [Pitch.Degree pc accs | pc <- [0..7], accs <- [-1..1]]

make_keys :: Theory.Layout -> [Pitch.Semi] -> [Theory.Key]
make_keys layout intervals =
    [Theory.key tonic "" intervals layout
        | tonic <- all_degrees, abs (Pitch.degree_accidentals tonic) <= 1]

all_keys :: Theory.Layout -> ChromaticScales.Keys
all_keys layout =
    Map.fromList $ zip (map (TheoryFormat.show_key absolute_fmt) keys) keys
    where
    keys = make_keys layout $ Vector.toList (Theory.layout_intervals layout)
