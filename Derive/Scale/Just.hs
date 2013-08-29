-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A version of a just intonation diatonic scale that is tuned based on
-- a pitch signal.
module Derive.Scale.Just where
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Vector as Vector

import Util.Control
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ JustScales.make_scale "just"
        (scale_map TheoryFormat.absolute_c) doc doc_fields
    , JustScales.make_scale "just-r"
        (scale_map (TheoryFormat.sargam relative_fmt)) doc doc_fields
    ]

doc :: Text
doc =
    "7-note Western style scales tuned in just intonation.\
    \\nKeys look like `c-maj`, where `c` is the tonic and `maj` selects\
    \ the ratios to use. For absolute notation, the tonic determines where\
    \ the scale starts, while for relative notation, the tonic determines\
    \ only which MIDI key maps to the first scale degree. So for the ASCII kbd\
    \ where the input is also relative, the tonic is irrelevant."

doc_fields :: [(Text, Text)]
doc_fields =
    [ (name, JustScales.show_ratios (JustScales.key_ratios key))
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
key_ratios = map (second Vector.fromList)
    [ ("maj", [1, 9%8, 5%4, 4%3, 3%2, 5%3, 15%8])
    , ("min", [1, 9%8, 6%5, 4%3, 3%2, 8%5, 9%5])
    , ("legong", [1, 10%9, 6%5, 4%3, 3%2, 25%16, 9%5])
    , ("hemavathi", [1, 10%9, 6%5, (3%2) / (16%15), 3%2, 5%3, 9%5])
    ]
