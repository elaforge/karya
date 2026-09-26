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
    [ JustScales.makeScale "hex"
        (scaleMap (TheoryFormat.letters pcPerOctave))
        doc docFields
    , JustScales.makeScale "hex-r"
        (scaleMap (TheoryFormat.cipher pcPerOctave relativeFmt))
        doc docFields
    ]

doc :: Doc.Doc
doc = "This is a family of 6 note just scales, based on Erv Wilson's hexanies.\
    \ The keys look like `a-159b-1`.  The `a` means that in absolute naming,\
    \ unity is on `a` (in relative naming, unity is always `1`).  `159b` means\
    \ the hexany is built on 1, 5, 9, 11.  The trailing `1` means unity is\
    \ assigned to the first ratio produced. For example, if you modulate to the\
    \ 5th scale degree, you would change the key to `a-159b-6` and set\
    \ `%just-base` accordingly.\n"

docFields :: [(Doc.Doc, Doc.Doc)]
docFields =
    [ (Doc.literal name,
        Doc.Doc $ JustScales.showRatios (JustScales.keyRatios key))
    | (name, key) <- ChromaticScales.groupTonicMode (Map.toList keys)
    ]

pcPerOctave :: Int
pcPerOctave = 6

scaleMap :: TheoryFormat.Format -> JustScales.ScaleMap
scaleMap = JustScales.scaleMap keys defaultKey Nothing

relativeFmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relativeFmt = JustScales.makeRelativeFmt keys defaultKey

defaultKey :: JustScales.Key
Just defaultKey = Map.lookup (Pitch.Key "a-1357-1") keys

keys :: JustScales.Keys
keys = JustScales.makeKeys (take pcPerOctave TheoryFormat.letterDegrees)
    [(key, [("", ratios)]) | (key, ratios) <- keyRatios]

keyRatios :: [(Text, JustScales.Ratios)]
keyRatios = concatMap makeHexany
    [ [a, b, c, d] | a <- primes, b <- primes, b > a
    , c <- primes, c > b, d <- primes, d > c
    ]
    where primes = [1, 3, 5, 7, 9, 11, 13]

hexanyRatios :: [[Int]] -> [[Ratio]]
hexanyRatios = map (snd . head . makeRatios)

makeHexany :: [Int] -> [(Text, JustScales.Ratios)]
makeHexany xs =
    [ (showRoots xs <> "-" <> showt (n+1), Vector.fromList ratios)
    | (n, ratios) <- makeRatios xs
    ]

showRoots :: [Int] -> Text
showRoots = txt . mapMaybe Num.showHigit

makeRatios :: [Int] -> [(Int, [Ratio])]
makeRatios =
    map (second (List.sort . map reduceOctave)) . chooseUnity
    . map (uncurry (*)) . permute . map (%1)
    where
    chooseUnity xs = [(n, map (/x) xs) | (n, x) <- zip [0..] xs]

permute :: [a] -> [(a, a)]
permute (x:xs) = [(x, y) | y <- xs] ++ permute xs
permute [] = []

type Ratio = Ratio.Ratio Int

reduceOctave :: Ratio -> Ratio
reduceOctave ratio
    | ratio >= 2 = reduceOctave (ratio / 2)
    | ratio < 1 = reduceOctave (ratio * 2)
    | otherwise = ratio
