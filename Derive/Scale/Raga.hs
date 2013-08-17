-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Carnatic ragas.
module Derive.Scale.Raga where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Vector as Vector

import Util.Control
import qualified Derive.Scale as Scale
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ JustScales.make_scale (Pitch.ScaleId "raga")
        (scale_map (TheoryFormat.sargam relative_fmt)) doc doc_fields
    ]

doc :: Text
doc =
    "Carnatic ragas, from the melakarta system.\
    \\nThe \"key\" is actually the raga, e.g.\
    \ `key = sankarabharanam`. I can't represent arohana and avarohana since I\
    \ can't always know in the notation whether the previous note was higher or\
    \ lower, so these are represented as \"enharmonics\". Also, these scales\
    \ deal only with pitches, and don't\
    \ differentiate between gamakam, or order, and I don't bother omitting\
    \ notes if there are less than seven, so many janya ragas can be aliased to\
    \ their janaka raga.\
    \\nSince there's no standard latin orthography, they\
    \ might be spelled slightly differently from how you expect."

doc_fields :: [(Text, Text)]
doc_fields = JustScales.group_relative_keys melakarta_keys

scale_map :: TheoryFormat.Format -> JustScales.ScaleMap
scale_map = JustScales.scale_map keys default_key

relative_fmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relative_fmt = JustScales.make_relative_fmt keys default_key

default_key :: JustScales.Key
Just default_key = Map.lookup (Pitch.Key "sankarabharanam") keys

keys :: Map.Map Pitch.Key JustScales.Key
keys = Map.fromList melakarta_keys


-- * implementation

melakarta_keys :: [(Pitch.Key, JustScales.Key)]
melakarta_keys =
    [ (Pitch.Key alias, JustScales.Key 0 ratios)
    | (name, ratios) <- melakarta_ratios
    , alias <- name : aliases_of name
    ]

-- | TODO this produces results that actually sound like what people play.
-- So why is it different from the "official" ratios?
melakarta_ratios :: [(Text, JustScales.Ratios)]
melakarta_ratios = zip melakarta_names $ map Vector.fromList $ do
    ma <- [ma1, ma2]
    ri <- [ri2, ri3, ri4]
    ga <- [ga1, ga2, ga3]
    dha <- [dha2, dha3, dha4]
    ni <- [ni1, ni2, ni3]
    guard $ dha == dha3 |- ni /= ni1
    guard $ dha == dha4 |- ni == ni3
    guard $ ri == ri3 |- ga /= ga1
    guard $ ri == ri4 |- ga == ga3
    return [sa, ri, ga, ma, pa, dha, ni]
    where
    infixr 3 |-
    x |- y = if x then y else True

-- melakarta_ratios :: [(Text, JustScales.Ratios)]
-- melakarta_ratios = zip melakarta_names $ map Vector.fromList $ do
--     ma <- [ma1, ma2]
--     ri <- [ri1, ri2, ri3]
--     ga <- [ga1, ga2, ga3]
--     dha <- [dha1, dha2, dha3]
--     ni <- [ni1, ni2, ni3]
--     guard $ dha == dha2 |- ni /= ni1
--     guard $ dha == dha3 |- ni == ni3
--     guard $ ri == ri2 |- ga /= ga1
--     guard $ ri == ri3 |- ga == ga3
--     return [sa, ri, ga, ma, pa, dha, ni]
--     where
--     infixr 3 |-
--     x |- y = if x then y else True

type Ratio = Ratio.Ratio Int

sa, ri1, ri2, ri3, ri4, ga1, ga2, ga3, ga4, ma1, ma2, ma3, ma4 :: Ratio
sa = 1
ri1 = 32 % 31
ri2 = 16 % 15
ri3 = 10 % 9
ri4 = 9 % 8
ga1 = 32 % 27
ga2 = 6 % 5
ga3 = 5 % 4
ga4 = 81 % 64
ma1 = 4 % 3
ma2 = 27 % 20
ma3 = 45 % 32
ma4 = 64 % 45

pa, dha1, dha2, dha3, dha4, ni1, ni2, ni3, ni4 :: Ratio
pa = 3 % 2
dha1 = 128 % 81
dha2 = 8 % 5
dha3 = 5 % 3
dha4 = 27 % 16
ni1 = 16 % 9
ni2 = 9 % 5
ni3 = 15 % 8
ni4 = 31 % 16

aliases_of :: Text -> [Text]
aliases_of = flip (Map.findWithDefault []) aliases

aliases :: Map.Map Text [Text]
aliases = Map.fromList
    [ ("dheerasankarabharanam", ["sankarabharanam"])
    , ("mechakalyani", ["kalyani"])
    , ("hanumatodi", ["todi"])

    -- janya
    , ("harikambhoji", ["mohanam", "kambhoji"])
    , ("natabhaivari", ["bhairavi"])
    ]

melakarta_names :: [Text]
melakarta_names =
    [ "kanakanki", "ratnangi", "ganamurti", "vanaspati"
    , "manavati", "tanarupi", "senavati", "hanumatodi"
    , "dhenuka", "natakapriya", "kokilapriya", "rupavati"
    , "gayakapriya", "vakulabharanam", "mayamalavagoulai", "chakravaham"
    , "suryakantam", "hatakambhari", "jhankaradhwani", "natabhairavi"
    , "keeravani", "kharaharapriya", "gourimanohari", "varunapriya"
    , "mararanjani", "charukesi", "sarasangi", "harikambhoji"
    , "dheerasankarabharanam", "naganandini", "yagapriya", "ragavardhini"
    , "gangeyabhusani", "vagadheeswari", "sulini", "chalanattai"
    , "salagam", "jalarnavam", "jhalavarali", "navaneetam"
    , "pavani", "raghupriya", "gavambodhi", "bhavapriya"
    , "subhapantuvarali", "shadvigamargini", "suvarnangi", "divyamani"
    , "dhavalambari", "namanarayani", "kamavardhini", "ramapriya"
    , "gamanasrama", "viswambhari", "syamalangi", "shanmukhapriya"
    , "simhendramadhyamam", "hemavati", "dharamavai", "nitimati"
    , "kantamani", "rishabhapriya", "latangi", "vachaspati"
    , "mechakalyani", "chitrambhari", "sucharitra", "jyotiswarupini"
    , "dhatuvardhini", "nasikabhusani", "kosalam", "rasikapriya"
    ]

-- [ ("maj", [1, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8])
-- , ("min", [1, 9/8, 6/5, 4/3, 3/2, 8/5, 9/5])
--
-- R2 G3 M1 P D2 N3 -> 1, 1b, 3, 4, 5, 6b, 7
{-
    Sa             1               240      1
    Ri 1         32/31             252.8
    Ri 2         16/15             256      1b
    Ri 3         10/9              266.6
    Ri 4         9/8               270      2
    Ga 1         32/27             284.4
    Ga 2          6/5              288      3b
    Ga 3          5/4              300      3
    Ga 4          81/64            303.7
    Ma 1          4/3              320      4
    Ma 2          27/20            324
    Ma 3          45/32            337.5
    Ma 4          64/45            341.3
    Pa             3/2             360      5
    Dha 1         128/81           379
    Dha 2          8/5             384      6b
    Dha 3          5/3             400      6
    Dha 4         27/16            405
    Ni 1           16/9            426.6
    Ni 2            9/5            432      7b
    Ni 3           15/8            450      7
    Ni 4           31/16           465
-}
