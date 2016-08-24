-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A version of a just intonation diatonic scale that is tuned based on
-- a pitch signal.
module Derive.Scale.Just where
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Vector as Vector

import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales = map Scale.Simple
    [ Scales.add_doc "7-limit just scale." $ JustScales.make_scale "just"
        (scale_map TheoryFormat.absolute_c) doc doc_fields
    , Scales.add_doc "7-limit just scale." $ JustScales.make_scale "just-r"
        (scale_map (TheoryFormat.sargam relative_fmt)) doc doc_fields
    ]

doc :: Derive.Doc
doc =
    "7-note Western style scales tuned in just intonation.\
    \\nKeys look like `c-maj`, where `c` is the tonic and `maj` selects\
    \ the ratios to use. For absolute notation, the tonic determines where\
    \ the scale starts, while for relative notation, the tonic determines\
    \ only which MIDI key maps to the first scale degree. So for the ASCII kbd\
    \ where the input is also relative, the tonic is irrelevant."

doc_fields :: [(ShowVal.Doc, ShowVal.Doc)]
doc_fields =
    [ (ShowVal.literal name,
        ShowVal.Doc $ JustScales.show_ratios (JustScales.key_ratios key))
    | (name, key) <- ChromaticScales.group_tonic_mode (Map.toList keys)
    ]

scale_map :: TheoryFormat.Format -> JustScales.ScaleMap
scale_map = JustScales.scale_map keys default_key

relative_fmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relative_fmt = JustScales.make_relative_fmt keys default_key

default_key :: JustScales.Key
Just default_key = Map.lookup (Pitch.Key "c-maj") keys

keys :: Map.Map Pitch.Key JustScales.Key
keys = JustScales.make_keys TheoryFormat.absolute_c_degrees key_ratios

key_ratios :: [(Text, JustScales.Ratios)]
key_ratios =
    [(name, Vector.fromList $ select (0:is) chromatic) |
        (name, is) <- intervals]
    where
    intervals = map (second (take 7)) $ zip names $ List.tails $
        cycle Theory.piano_intervals
    chromatic = [1, 16%15, 9%8, 6%5, 5%4, 4%3, 7%5, 3%2, 8%5, 5%3, 9%5, 15%8]
    -- The names are the same as in Twelve.
    names = ["maj", "dorian", "phrygian", "lydian", "mixolydian", "min",
        "locrian"]

select :: [Int] -> [a] -> [a]
select [] _ = []
select (n:ns) xs = case drop n xs of
    x : xs -> x : select ns (x:xs)
    [] -> []
