module Derive.Scale.Hex where
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Vector as Vector

import Util.Control
import qualified Derive.Scale as Scale
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ JustScales.make_scale (Pitch.ScaleId "hex")
        (scale_map (TheoryFormat.letters pc_per_octave))
        "This is a family of 6 note just scales.\n"
    , JustScales.make_scale (Pitch.ScaleId "hex-r")
        (scale_map (TheoryFormat.cipher pc_per_octave relative_fmt))
        "This is a family of 6 note just scales.\n"
    ]

pc_per_octave :: Int
pc_per_octave = 6

scale_map :: TheoryFormat.Format -> JustScales.ScaleMap
scale_map = JustScales.scale_map keys default_key

relative_fmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relative_fmt = JustScales.make_relative_fmt keys default_key

default_key :: JustScales.Key
Just default_key = Map.lookup (Pitch.Key "a-h2357") keys

keys :: JustScales.Keys
keys = JustScales.make_keys (take pc_per_octave TheoryFormat.letter_degrees)
    key_ratios

key_ratios :: [(Text, JustScales.Ratios)]
key_ratios = map (second Vector.fromList)
    -- hexany based on 2-3-5-7
    [ ("h2357", [1, 8%7, 6%5, 48%35, 8%5, 12%7])
    ]
