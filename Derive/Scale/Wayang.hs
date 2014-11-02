-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Saih gender wayang.
module Derive.Scale.Wayang where
import qualified Data.Vector as Vector

import Util.Control
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ Scales.add_doc "Saih gender wayang." $
        BaliScales.make_scale scale_id complete_scale
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
complete_scale = scale_map BaliScales.ioeua_absolute
    (0, Vector.length (BaliScales.nn_umbang note_numbers) - 1)

pemade :: BaliScales.ScaleMap
pemade = scale_map (BaliScales.ioeua_absolute_arrow 4) (2*5 + 1, 4*5)

kantilan :: BaliScales.ScaleMap
kantilan = scale_map (BaliScales.ioeua_absolute_arrow 5) (3*5 + 1, 5*5)

-- | Use ding deng dong dung dang.  I don't know if this is ever actually used
-- for gender, but the notation is compact.
--
-- > 3o  3e  3u  3a  4i  4o  4e  4u  4a  5i  5o  5e  5u  5a  6i
-- > pemade -------------------------------
-- >                     kantilan -----------------------------
scale_map :: TheoryFormat.Format -> (Pitch.Semi, Pitch.Semi)
    -> BaliScales.ScaleMap
scale_map fmt range =
    BaliScales.scale_map layout fmt all_keys default_key note_numbers range

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
