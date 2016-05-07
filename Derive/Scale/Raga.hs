-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Carnatic ragas.
module Derive.Scale.Raga where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Vector as Vector

import qualified Derive.Scale as Scale
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales = map Scale.Simple
    [ JustScales.make_scale "raga"
        (scale_map (TheoryFormat.sargam relative_fmt)) doc doc_fields
    ]

doc :: Text
doc = "Carnatic ragas, from the melakarta system.\
    \\nThe \"key\" is actually the raga, e.g.\
    \ `key=shankarabharanam`. I can't represent arohana and avarohana since I\
    \ can't always know in the notation whether the previous note was higher or\
    \ lower, so these are represented as \"enharmonics\". Also, these scales\
    \ deal only with pitches, and don't\
    \ differentiate between gamakam, or order, and I don't bother omitting\
    \ notes if there are less than seven, so many janya ragas can be aliased to\
    \ their janaka raga.\
    \\nSince latin orthography is inconsistent, they might be spelled slightly\
    \ differently from how you expect."

doc_fields :: [(Text, Text)]
doc_fields = JustScales.group_relative_keys melakarta_keys

scale_map :: TheoryFormat.Format -> JustScales.ScaleMap
scale_map = JustScales.scale_map keys default_key

relative_fmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relative_fmt = JustScales.make_relative_fmt keys default_key

default_key :: JustScales.Key
Just default_key = Map.lookup (Pitch.Key "shankarabharanam") keys

keys :: Map.Map Pitch.Key JustScales.Key
keys = Map.fromList melakarta_keys


-- * implementation

melakarta_keys :: [(Pitch.Key, JustScales.Key)]
melakarta_keys =
    [ (Pitch.Key alias, JustScales.Key 0 ratios)
    | (name, ratios) <- melakarta_ratios
    , alias <- name : aliases_of name
    ]

melakarta_ratios :: [(Text, JustScales.Ratios)]
melakarta_ratios = zip melakarta_names $ map Vector.fromList $ do
    ma <- [ma1, ma2]
    ri <- [ri1, ri2, ri3]
    ga <- [ga1, ga2, ga3]
    dha <- [dha1, dha2, dha3]
    ni <- [ni1, ni2, ni3]
    guard $ dha == dha2 |- ni /= ni1
    guard $ dha == dha3 |- ni == ni3
    guard $ ri == ri2 |- ga /= ga1
    guard $ ri == ri3 |- ga == ga3
    return [sa, ri, ga, ma, pa, dha, ni]
    where
    infixr 3 |-
    x |- y = not x || y

type Ratio = Ratio.Ratio Int

{- I don't think there is any official definition, so I use a 7-limit scale:

    > 1     2m    2M    3m    3M    4P    4A    5P    6m    6M    7m    7M
    > s     r1    r2    r3                      p     d1    d2    d3
    >             g1    g2    g3    m1    m2                n1    n2    n3
-}

sa, ri1, ri2, ri3, ga1, ga2, ga3, ma1, ma2 :: Ratio
sa = 1
ri1 = 16 % 15
ri2 = 8 % 7
ri3 = 6 % 5
ga1 = ri2
ga2 = ri3
ga3 = 5 % 4
ma1 = 4 % 3
ma2 = 7 % 5

pa, dha1, dha2, dha3, ni1, ni2, ni3 :: Ratio
pa = 3 % 2
dha1 = 8 % 5
dha2 = 5 % 3
dha3 = 7 % 4
ni1 = dha2
ni2 = dha3
ni3 = 15 % 8

aliases_of :: Text -> [Text]
aliases_of = flip (Map.findWithDefault []) $
    Map.unionWith (++) aliases $
        Map.fromList [(janaka, map fst janyas) | (janaka, janyas) <- janya]

-- | Common aliases for the melakarta names.
aliases :: Map.Map Text [Text]
aliases = Map.fromList $ map assert_valid_name
    [ ("dheerashankarabharanam", ["shankarabharanam"])
    , ("mechakalyani", ["kalyani"])
    , ("hanumatodi", ["todi"])
    , ("harikambhoji", ["kambhoji"])
    , ("natabhairavi", ["bhairavi"])
    ]

-- | Arohana \/ avarohana structure.  Sa is implicit, so it's omitted.
data Arohana =
    -- | The arohana is given, the avarohana is the same but reversed.
    Same [Swaram]
    -- | Arohana ascending from low sa, avarohana descending from high sa.
    | Different [Swaram] [Swaram]
    deriving (Show)

data Swaram = S | R | G | M | P | D | N
    deriving (Show)

-- | So far this is unused, but I should be able to put it some place where
-- calls can get at it.
janya :: [(Text, [(Text, Arohana)])]
janya = map assert_valid_name
    [ ("kharaharapriya",
        [ ("abheri", Different [G, M, P, N] [N, D, P, M, G, R])
        , ("abhogi", Same [R, G, M, D])
        ])
    , ("harikambhoji",
        [ ("mohanam", Same [R, G, P, D])
        ])
    ]

assert_valid_name :: (Text, a) -> (Text, a)
assert_valid_name val@(name, _)
    | name `elem` melakarta_names = val
    | otherwise = errorStack $ "not in melakarta_names: " ++ show name

melakarta_names :: [Text]
melakarta_names =
    [ "kanakanki", "ratnangi", "ganamurti", "vanaspati"
    , "manavati", "tanarupi", "senavati", "hanumatodi"
    , "dhenuka", "natakapriya", "kokilapriya", "rupavati"
    , "gayakapriya", "vakulabharanam", "mayamalavagoula", "chakravaham"
    , "suryakantam", "hatakambhari", "jhankaradhwani", "natabhairavi"
    , "kiravani", "kharaharapriya", "gourimanohari", "varunapriya"
    , "mararanjani", "charukesi", "sarasangi", "harikambhoji"
    , "dheerashankarabharanam", "naganandini", "yagapriya", "ragavardhini"
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
