-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Carnatic ragas.
module Derive.Scale.Raga where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Derive.Scale as Scale
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Definition]
scales = map Scale.Simple
    [ JustScales.make_scale "raga"
        (scale_map (TheoryFormat.sargam relative_fmt)) doc doc_fields
    ]

doc :: Doc.Doc
doc = "Carnatic ragas, from the melakarta system.\
    \\nThe \"key\" is actually the raga, e.g.\
    \ `key=shankarabharanam`. Arohana and avarohana are not represented in the\
    \ scale, so janya ragams look the same as their parent. The information\
    \ is present, though, so it can be interpreted by various gamakam ornaments\
    \ (not yet implemented though).  TODO There is also currently no way to\
    \ represent pitches which differ in arohana and avarohana.\
    \\nSince latin orthography is inconsistent, they might be spelled slightly\
    \ differently from how you expect."

doc_fields :: [(Doc.Doc, Doc.Doc)]
doc_fields = JustScales.group_relative_keys melakarta_keys

scale_map :: TheoryFormat.Format -> JustScales.ScaleMap
scale_map = JustScales.scale_map keys default_key Nothing

relative_fmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relative_fmt = JustScales.make_relative_fmt keys default_key

default_key :: JustScales.Key
Just default_key = Map.lookup (Pitch.Key "shankarabharanam") keys

keys :: Map Pitch.Key JustScales.Key
keys = Map.fromList melakarta_keys


-- * implementation

melakarta_keys :: [(Pitch.Key, JustScales.Key)]
melakarta_keys =
    [ (Pitch.Key alias, JustScales.Key 0 (Map.singleton "" ratios))
    | (name, ratios) <- melakarta_ratios
    , alias <- name : aliases_of name
    ]

melakarta_ratios :: [(Text, JustScales.Ratios)]
melakarta_ratios = zip melakarta_names $ map Vector.fromList $ do
    ma <- [ma1, ma2]
    ri <- [ri1, ri2, ri3]
    ga <- [ga1, ga2, ga3]
    da <- [da1, da2, da3]
    ni <- [ni1, ni2, ni3]
    guard $ da == da2 |- ni /= ni1
    guard $ da == da3 |- ni == ni3
    guard $ ri == ri2 |- ga /= ga1
    guard $ ri == ri3 |- ga == ga3
    return [sa, ri, ga, ma, pa, da, ni]
    where
    infixr 3 |-
    x |- y = not x || y

-- | This is the same as 'melakarta_ratios', but with intervals.  I should
-- probably derive melakarta_ratios from this.
melakarta_intervals :: [(Text, [Pitch.Semi])]
melakarta_intervals = zip melakarta_names $ do
    ma <- [ma1, ma2]
    ri <- [ri1, ri2, ri3]
    ga <- [ga1, ga2, ga3]
    da <- [da1, da2, da3]
    ni <- [ni1, ni2, ni3]
    guard $ da == da2 |- ni /= ni1
    guard $ da == da3 |- ni == ni3
    guard $ ri == ri2 |- ga /= ga1
    guard $ ri == ri3 |- ga == ga3
    let indices = [sa, ri, ga, ma, pa, da, ni, 12]
    return $ zipWith (-) (tail indices) indices
    where
    [sa, ri1, ri2, ri3, ga3, ma1, ma2, pa, da1, da2, da3, ni3]
        = take 12 [0..]
    ga1 = ri2
    ga2 = ri3
    ni1 = da2
    ni2 = da3
    infixr 3 |-
    x |- y = not x || y

-- | Find a raga's name from its swarams.
find :: [Ratio] -> Maybe Text
find swarams = fst <$> List.find ((==ratios) . snd) melakarta_ratios
    where
    ratios = Vector.fromList swarams

type Ratio = Ratio.Ratio Int

{- I don't think there is any official definition, but listeners agree 8/7 is
    too high for ri, so I use a 5-limit scale.  Except prati madhyama is 7/5,
    because I like it.  There is some theoretical material deriving 22 srutis
    via 5-limit, but I'm not sure what bearing it has on real practice.

    > 1     2m    2M    3m    3M    4P    4A    5P    6m    6M    7m    7M
    > s     r1    r2    r3                      p     d1    d2    d3
    >             g1    g2    g3    m1    m2                n1    n2    n3
-}

sa, ri1, ri2, ri3, ga3, ma1, ma2, pa, da1, da2, da3, ni3 :: Ratio
(sa, ri1, ri2, ri3, ga3, ma1, ma2, pa, da1, da2, da3, ni3) =
    (1, 16/15, 9/8, 6/5, 5/4, 4/3, 7/5, 3/2, 8/5, 5/3, 9/5, 15/8)

ga1, ga2, ni1, ni2 :: Ratio
ga1 = ri2
ga2 = ri3
ni1 = da2
ni2 = da3

aliases_of :: Text -> [Text]
aliases_of = flip (Map.findWithDefault []) $
    Map.unionWith (++) aliases $
        Map.fromList [(janaka, map fst janyas) | (janaka, janyas) <- janya]

-- | Common aliases for the melakarta names.
aliases :: Map Text [Text]
aliases = Map.fromList $ map assert_valid_name
    [ ("dheerashankarabharanam", ["shankarabharanam"])
    , ("mechakalyani", ["kalyani"])
    , ("hanumatodi", ["todi"])
    , ("harikambhoji", ["kambhoji"])
    , ("natabhairavi", ["bhairavi"])
    ]

-- | Arohana \/ avarohana structure.  Sa is implicit, so it's omitted.
data ArohanaAvarohana =
    -- | The arohana is given, the avarohana is the same but reversed.
    Same [Swaram]
    -- | Arohana ascending from low sa, avarohana descending from high sa.
    -- The first and last Sa are omitted, since they are implicit.
    | Different [Swaram] [Swaram]
    deriving (Show)

data Swaram = S | R | G | M | P | D | N
    deriving (Enum, Show)

-- | So far this is unused, but I should be able to put it some place where
-- calls can get at it.
janya :: [(Text, [(Text, ArohanaAvarohana)])]
janya = map assert_valid_name
    [ ("kharaharapriya",
        [ ("abheri", Different [G, M, P, N] down)
        -- hindustani, similar to abheri
        , ("dhanasri", Different [G, M, P, N] down)
        , ("abhogi", Same [R, G, M, D])
        , ("sri", Different [R, M, P, N] [N, P, M, R, G, R])
            -- or avarohana = [N, P, D, N, P, M, R, G, R]
        ])
    , ("harikambhoji",
        [ ("mohanam", Same [R, G, P, D])
        , ("natakuranji",
            Different [M, G, M, N, D, N, P, D, N] [N, D, M, G])
                -- Or [N, D, M, G, M, P, G, R]
        ])
    -- , ("mayamalavagoula",
    -- TODO web page says it's from mayamalavagoula, but it has m2
    , ("kamavardhini",
        [ ("goulipantu", Different [R, M, P, N] [N, D, P, M, D, M, G, R])
        ])
    ]
    where
    -- up = [R .. N]
    down = [N, D .. R]

assert_valid_name :: (Text, a) -> (Text, a)
assert_valid_name val@(name, _)
    | name `elem` melakarta_names = val
    | otherwise = errorStack $ "not in melakarta_names: " <> showt name

melakarta_names :: [Text]
melakarta_names =
    -- shuddha madhyama
    [ "kanakanki", "ratnangi", "ganamurti" -- r1 g1
    , "vanaspati", "manavati", "tanarupi"

    , "senavati", "hanumatodi", "dhenuka" -- r1 g2
    , "natakapriya", "kokilapriya", "rupavati"

    , "gayakapriya", "vakulabharanam", "mayamalavagoula" -- r1 g3
    , "chakravaham", "suryakantam", "hatakambhari"

    , "jhankaradhwani", "natabhairavi", "kiravani" -- r2 g2
    , "kharaharapriya", "gourimanohari", "varunapriya"

    , "mararanjani", "charukesi", "sarasangi" -- r2 g3
    , "harikambhoji", "dheerashankarabharanam", "naganandini"

    , "yagapriya", "ragavardhini", "gangeyabhusani" -- r3 g3
    , "vagadheeswari", "sulini", "chalanattai"

    -- prati madhyama
    , "salagam", "jalarnavam", "jhalavarali" -- r1 g1
    , "navaneetam", "pavani", "raghupriya"

    , "gavambodhi", "bhavapriya", "subhapantuvarali" -- r1 g2
    , "shadvigamargini", "suvarnangi", "divyamani"

    , "dhavalambari", "namanarayani", "kamavardhini" -- r1 g3
    , "ramapriya", "gamanasrama", "viswambhari"

    , "syamalangi", "shanmukhapriya", "simhendramadhyamam" -- r2 g2
    , "hemavati", "dharamavai", "nitimati"

    , "kantamani", "rishabhapriya", "latangi" -- r2 g3
    , "vachaspati" , "mechakalyani", "chitrambhari"

    , "sucharitra", "jyotiswarupini", "dhatuvardhini" -- r3 g3
    , "nasikabhusani", "kosalam", "rasikapriya"
    ]
