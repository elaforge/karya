-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The western equal tempered 12 note scale, aka 12TET.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".  General to specific is more
    aesthetically appealing.

    4c is middle C, and the range is limited to the midi range.  Since
    'Pitch.NoteNumber's also use midi numbering, conversions are trivial.

    > nn 127 = 9g
    > nn 120 = 9c
    > middle c = nn 60 = 4c
    > nn 24 = 1c
    > nn 12 = 0c
    > nn 0 = -1c
-}
module Derive.Scale.Twelve where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Unboxed

import Util.Control
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales = [scale, relative_scale]

scale :: Scale.Scale
scale = Scales.set_direct_input_to_nn $
    ChromaticScales.make_scale absolute_scale_map scale_id
        "The world-famous equal tempered twelve note scale."

relative_scale :: Scale.Scale
relative_scale = ChromaticScales.make_scale relative_scale_map "twelve-r"
    "This is 12TET, but spelled relative to the current key and mode."

scale_id :: Pitch.ScaleId
scale_id = "twelve"

absolute_scale_map :: ChromaticScales.ScaleMap
absolute_scale_map =
    ChromaticScales.scale_map layout fmt all_keys default_theory_key

fmt :: TheoryFormat.Format
fmt = TheoryFormat.absolute_c

relative_scale_map :: ChromaticScales.ScaleMap
relative_scale_map = ChromaticScales.scale_map
    layout (TheoryFormat.sargam relative_fmt) all_keys default_theory_key

relative_fmt :: TheoryFormat.RelativeFormat Theory.Key
relative_fmt = TheoryFormat.RelativeFormat
    { TheoryFormat.rel_acc_fmt = TheoryFormat.ascii_accidentals
    , TheoryFormat.rel_parse_key = Scales.get_key default_theory_key all_keys
    , TheoryFormat.rel_default_key = default_theory_key
    , TheoryFormat.rel_show_degree = TheoryFormat.show_degree_chromatic
    , TheoryFormat.rel_to_absolute = TheoryFormat.chromatic_to_absolute
    , TheoryFormat.rel_key_tonic = Pitch.degree_pc . Theory.key_tonic
    }


-- * keys

default_key :: Pitch.Key
default_key = Pitch.Key "c-maj"

default_theory_key :: Theory.Key
Just default_theory_key = Map.lookup default_key all_keys

show_pitch :: Pitch.Pitch -> Maybe Pitch.Note
show_pitch = either (const Nothing) Just
    . ChromaticScales.show_pitch absolute_scale_map Nothing

show_nn :: Pitch.NoteNumber -> Maybe Pitch.Note
show_nn = show_pitch . Theory.semis_to_pitch_sharps layout
    . Theory.nn_to_semis . floor

read_absolute_pitch :: Pitch.Note -> Maybe Pitch.Pitch
read_absolute_pitch = either (const Nothing) Just
    . ChromaticScales.read_pitch absolute_scale_map Nothing


-- * implementation

all_keys :: Map.Map Pitch.Key Theory.Key
all_keys = ChromaticScales.make_keys fmt $
    church_keys ++ octatonic_keys ++ whole_keys ++ exotic_keys

church_keys :: [Theory.Key]
church_keys = concat (zipWith make_keys modes intervals)
    where
    modes = ["maj", "dorian", "phrygian", "lydian", "mixolydian", "min",
        "locrian"]
    intervals = [take 7 (drop n major) | n <- [0..6]]
    major = cycle $ Unboxed.toList (Theory.layout_intervals layout)

octatonic_keys :: [Theory.Key]
octatonic_keys = make_keys "octa21" (take 8 (cycle [2, 1]))
    ++ make_keys "octa12" (take 8 (cycle [1, 2]))

whole_keys :: [Theory.Key]
whole_keys = make_keys "whole" (replicate 6 2)

-- | Keys that are diatonic, but have nonstandard key signatures.
exotic_keys :: [Theory.Key]
exotic_keys = make_keys "hijaz" [1, 3, 1, 2, 1, 2, 2]

-- | The layout of keys on everyone's favorite boxed harp.
layout :: Theory.Layout
layout = Theory.piano_layout

all_degrees :: [Pitch.Degree]
all_degrees = [Pitch.Degree pc accs | pc <- [0..6], accs <- [-2..2]]

make_keys :: Text -> [Pitch.Semi] -> [Theory.Key]
make_keys name intervals =
    [Theory.key tonic name intervals layout
        | tonic <- all_degrees, abs (Pitch.degree_accidentals tonic) <= 1]
