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
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.TwelveScales as TwelveScales
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


-- Twelve is a popular default, so export these directly.

scale :: Scale.Scale
scale = make_scale scale_map scale_id
    "The world-famous equal tempered twelve note scale."

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

scale_map :: TwelveScales.ScaleMap
scale_map = TwelveScales.scale_map layout fmt all_keys default_theory_key

fmt :: TheoryFormat.Format
fmt = TheoryFormat.absolute_c

relative_scale_map :: TwelveScales.ScaleMap
relative_scale_map =
    TwelveScales.scale_map layout fmt all_keys default_theory_key
    where
    fmt = TheoryFormat.sargam parse_key default_theory_key
        TheoryFormat.show_note_chromatic TheoryFormat.adjust_chromatic
    parse_key maybe_key =
        TwelveScales.read_key relative_scale_map maybe_key

show_note_chromatic :: TheoryFormat.ShowNote Theory.Key
show_note_chromatic degrees acc_fmt key (Theory.Note pc acc) =
    (oct, text <> acc_text)
    where
    acc_text = TheoryFormat.show_accidentals acc_fmt
        (acc - Theory.note_accidentals tonic)
    (oct, text) = TheoryFormat.show_degree degrees (Theory.note_pc tonic) pc
    tonic = Theory.key_tonic key

adjust_chromatic :: TheoryFormat.Adjust Theory.Key
adjust_chromatic degrees key (Theory.Pitch octave (Theory.Note pc acc)) =
    Theory.Pitch (octave + oct) (Theory.Note pc2 acc2)
    where
    (oct, pc2) = (pc + Theory.note_pc tonic) `divMod` Vector.length degrees
    acc2 = acc + case Theory.key_signature key of
        -- If it's chromatic then I can't adjust for the mode, but I still
        -- want to map degree 1 to C# if I'm in C#.
        Nothing -> Theory.note_accidentals tonic
        Just sig -> case sig Unboxed.!? pc of
            Nothing -> 0 -- That shouldn't have happened.
            Just acc -> acc
    tonic = Theory.key_tonic key

-- * scales

scales :: [Scale.Scale]
scales =
    [ scale { Scale.scale_input_to_nn = Util.direct_input_to_nn }
    , make_scale relative_scale_map
        (Pitch.ScaleId "twelve-r")
        "This is 12TET, but spelled relative to the current key and mode.\
        \ It behaves oddly around accidentals. This is because the input is\
        \ taken to be relative, so the key is at C on the input. But the\
        \ input layout is still in C major, so the black keys are in the wrong\
        \ place. TODO to fix this I'd have to either abandon the relative\
        \ input, or reconfigure the input layout. The latter would be really\
        \ confusing, and incompatible with a piano keyboard."
    ]

make_scale :: TwelveScales.ScaleMap -> Pitch.ScaleId -> Text -> Scale.Scale
make_scale scale_map scale_id doc = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern =
        TheoryFormat.fmt_pattern (TwelveScales.smap_fmt scale_map)
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = TwelveScales.transpose scale_map
    , Scale.scale_enharmonics = TwelveScales.enharmonics scale_map
    , Scale.scale_note_to_call = TwelveScales.note_to_call scale_map
    , Scale.scale_input_to_note = TwelveScales.input_to_note scale_map
    , Scale.scale_input_to_nn = Util.computed_input_to_nn
        (TwelveScales.input_to_note scale_map)
        (TwelveScales.note_to_call scale_map)
    , Scale.scale_call_doc = TwelveScales.call_doc Util.standard_transposers
        scale_map doc
    }

default_key :: Pitch.Key
default_key = Pitch.Key "c-maj"

default_theory_key :: Theory.Key
Just default_theory_key = Map.lookup default_key all_keys

show_pitch :: Theory.Pitch -> Maybe Pitch.Note
show_pitch = either (const Nothing) Just
    . TwelveScales.show_pitch scale_map Nothing

show_nn :: Pitch.NoteNumber -> Maybe Pitch.Note
show_nn = show_pitch . Theory.semis_to_pitch_sharps layout
    . Theory.nn_to_semis . floor

read_pitch :: Pitch.Note -> Maybe Theory.Pitch
read_pitch = either (const Nothing) Just . TwelveScales.read_pitch scale_map

-- * constants

middle_c :: Pitch.Degree
middle_c = c4

c3, d3, e3, f3, g3, a3, b3 :: Pitch.Degree
(c3, d3, e3, f3, g3, a3, b3) = (48, 50, 52, 53, 55, 57, 59)

c4, d4, e4, f4, g4, a4, b4 :: Pitch.Degree
(c4, d4, e4, f4, g4, a4, b4) = (60, 62, 64, 65, 67, 69, 71)

c5, d5, e5, f5, g5, a5, b5 :: Pitch.Degree
(c5, d5, e5, f5, g5, a5, b5) = (72, 74, 76, 77, 79, 81, 83)

c6, d6, e6, f6, g6, a6, b6 :: Pitch.Degree
(c6, d6, e6, f6, g6, a6, b6) = (84, 86, 88, 89, 91, 93, 95)


-- * implementation

all_keys :: Map.Map Pitch.Key Theory.Key
all_keys = Map.fromList $ zip (map (TheoryFormat.show_key fmt) keys) keys
    where keys = church_keys ++ octatonic_keys ++ whole_keys ++ exotic_keys

church_keys :: [Theory.Key]
church_keys = concat (zipWith make_keys modes intervals)
    where
    modes = ["maj", "dorian", "phrygian", "lydian", "mixo", "min", "locrian"]
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
layout = Theory.layout TheoryFormat.absolute_c_intervals

all_notes :: [Theory.Note]
all_notes = [Theory.Note pc accs | pc <- [0..6], accs <- [-2..2]]

make_keys :: Text -> [Theory.Semi] -> [Theory.Key]
make_keys name intervals =
    [Theory.key tonic name intervals layout
        | tonic <- all_notes, abs (Theory.note_accidentals tonic) <= 1]
