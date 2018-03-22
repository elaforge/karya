-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Hex where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Definition]
scales = map Scale.Simple
    [ JustScales.make_scale "hex"
        (scale_map (TheoryFormat.letters pc_per_octave))
        doc doc_fields
    , JustScales.make_scale "hex-r"
        (scale_map (TheoryFormat.cipher pc_per_octave relative_fmt))
        doc doc_fields
    ]

doc :: Doc.Doc
doc = "This is a family of 6 note just scales, based on Erv Wilson's hexanies.\
    \ The keys look like `a-159b-1`.  The `a` means that in absolute naming,\
    \ unity is on `a` (in relative naming, unity is always `1`).  `159b` means\
    \ the hexany is built on 1, 5, 9, 11.  The trailing `1` means unity is\
    \ assigned to the first ratio produced. For example, if you modulate to the\
    \ 5th scale degree, you would change the key to `a-159b-6` and set\
    \ `%just-base` accordingly.\n"

doc_fields :: [(Doc.Doc, Doc.Doc)]
doc_fields =
    [ (Doc.literal name,
        Doc.Doc $ JustScales.show_ratios (JustScales.key_ratios key))
    | (name, key) <- ChromaticScales.group_tonic_mode (Map.toList keys)
    ]

pc_per_octave :: Int
pc_per_octave = 6

scale_map :: TheoryFormat.Format -> JustScales.ScaleMap
scale_map = JustScales.scale_map keys default_key Nothing

relative_fmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relative_fmt = JustScales.make_relative_fmt keys default_key

default_key :: JustScales.Key
Just default_key = Map.lookup (Pitch.Key "a-1357-1") keys

keys :: JustScales.Keys
keys = JustScales.make_keys (take pc_per_octave TheoryFormat.letter_degrees)
    [(key, [("", ratios)]) | (key, ratios) <- key_ratios]

key_ratios :: [(Text, JustScales.Ratios)]
key_ratios = concatMap make_hexany
    [ [a, b, c, d] | a <- primes, b <- primes, b > a
    , c <- primes, c > b, d <- primes, d > c
    ]
    where primes = [1, 3, 5, 7, 9, 11, 13]

hexany_ratios :: [[Int]] -> [[Ratio]]
hexany_ratios = map (snd . head . make_ratios)

make_hexany :: [Int] -> [(Text, JustScales.Ratios)]
make_hexany xs =
    [ (show_roots xs <> "-" <> showt (n+1), Vector.fromList ratios)
    | (n, ratios) <- make_ratios xs
    ]

show_roots :: [Int] -> Text
show_roots = txt . mapMaybe Num.showHigit

make_ratios :: [Int] -> [(Int, [Ratio])]
make_ratios =
    map (second (List.sort . map reduce_octave)) . choose_unity
    . map (uncurry (*)) . permute . map (%1)
    where
    choose_unity xs = [(n, map (/x) xs) | (n, x) <- zip [0..] xs]

permute :: [a] -> [(a, a)]
permute (x:xs) = [(x, y) | y <- xs] ++ permute xs
permute [] = []

type Ratio = Ratio.Ratio Int

reduce_octave :: Ratio -> Ratio
reduce_octave ratio
    | ratio >= 2 = reduce_octave (ratio / 2)
    | ratio < 1 = reduce_octave (ratio * 2)
    | otherwise = ratio
