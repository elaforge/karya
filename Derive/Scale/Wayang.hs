-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Saih gender wayang.

    I use ding deng dong dung dang.  I don't know if this is ever actually used
    for gender, but the notation is compact and I don't think there are any
    other conventions.

    @
    3o  3e  3u  3a  4i  4o  4e  4u  4a  5i  5o  5e  5u  5a  6i
    pemade -------------------------------
                        kantilan -----------------------------
    @
-}
module Derive.Scale.Wayang where
import qualified Data.Vector as Vector

import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales = map (Scale.Simple . Scales.add_doc "Saih gender wayang.")
    [ BaliScales.make_scale scale_id complete_scale
    , Scales.add_doc
        "Pemade scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.make_scale "wayang-pemade" pemade
    , Scales.add_doc
        "Kantilan scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.make_scale "wayang-kantilan" kantilan
    ]

complete_scale :: BaliScales.ScaleMap
complete_scale =
    BaliScales.scale_map layout BaliScales.ioeua_absolute base_oct all_keys
        default_key note_numbers Nothing

pemade :: BaliScales.ScaleMap
pemade = BaliScales.instrument_scale_map layout all_keys default_key
    note_numbers base_oct 4 (3, 1) (5, 0)

kantilan :: BaliScales.ScaleMap
kantilan = BaliScales.instrument_scale_map layout all_keys default_key
    note_numbers base_oct 5 (4, 1) (6, 0)

-- | Start octave for the extended scale.
base_oct :: Pitch.Octave
base_oct = 1

scale_id :: Pitch.ScaleId
scale_id = "wayang"

layout :: Theory.Layout
layout = Theory.layout [1, 1, 1, 1, 1]

all_keys :: ChromaticScales.Keys
all_keys = mempty

default_key :: Theory.Key
default_key = Theory.key (Pitch.Degree 0 0) "default" [1, 1, 1, 1, 1] layout

note_numbers :: BaliScales.NoteNumbers
note_numbers = BaliScales.NoteNumbers
    (Vector.fromList (extend umbang)) (Vector.fromList (extend isep))

umbang :: [Pitch.NoteNumber]
umbang =
    [ 53 -- pemade begin
    , 55.15
    , 57.73
    , 60.4

    , 62.95 -- pemade middle
    , 64.7 -- kantilan begin
    , 67.57
    , 69.45
    , 72.1

    , 74.83 -- pemade end, kantilan middle
    , 76.85
    , 79.48
    , 81.63
    , 84.12
    , 86.88 -- kantilan end
    ]

isep :: [Pitch.NoteNumber]
isep =
    [ 52.3 -- pemade begin
    , 54.55
    , 57.35
    , 59.85

    , 62.5 -- pemade middle
    , 64.45 -- kantilan begin
    , 67.26
    , 69.25
    , 71.81

    , 74.63 -- pemade end, kantilan middle
    , 76.73
    , 79.35
    , 81.51
    , 84
    , 86.78 -- kantilan end
    ]

-- | Extend down two octaves so that I start at 1i, and up two octaves to 8i.
--
-- pemade starts at 3o - 4i - 5i, kanti is 4o - 5i - 6i
extend :: [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend nns =
    ding - 36 : map (subtract 24) low ++ map (subtract 12) low
        ++ nns ++ map (+12) high ++ map (+24) high
    where
    ding = nns !! 4
    low = take 5 nns -- oeuai
    high = drop (length nns - 5) nns -- oeuai


-- * instrument integration

instrument_scale :: Bool -> BaliScales.Tuning -> Patch.Scale
instrument_scale extended tuning =
    Patch.make_scale ("wayang " <> ShowVal.show_val tuning) $
        zip (midi_keys extended) (if extended then extend nns else nns)
    where
    nns = case tuning of
        BaliScales.Umbang -> umbang
        BaliScales.Isep -> isep

-- | If extended is True, emit from i1 on up.  Otherwise, give pemade to
-- kantilan range.
midi_keys :: Bool -> [Midi.Key]
midi_keys extended = trim $ concatMap keys [base_oct + 1 ..]
    -- base_oct + 1 because MIDI starts at octave -1
    where
    trim
        | extended = take (7*5 + 1)
        | otherwise = take (3*5) . drop (1 + 3*5)
    keys oct = map (Midi.to_key (oct * 12) +) -- i o e u a
        [Key2.e_2, Key2.f_2, Key2.a_2, Key2.b_2, Key2.c_1]
