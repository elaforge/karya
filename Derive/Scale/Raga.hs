-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | Carnatic ragas.
module Derive.Scale.Raga where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Scale as Scale
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch

import           Global


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
    | (name, ragam) <- ragams
    , let ratios = ragam_ratios ragam
    , alias <- name : aliases_of name
    ]
    where
    aliases_of name = Map.findWithDefault [] name aliases

melakarta_ragams :: [(Name, Ragam)]
melakarta_ragams = zip melakarta_names $ do
    ma <- [Ma1, Ma2]
    ri <- [Ri1, Ri2, Ri3]
    ga <- [Ga1, Ga2, Ga3]
    da <- [Da1, Da2, Da3]
    ni <- [Ni1, Ni2, Ni3]
    guard $ da == Da2 |- ni /= Ni1
    guard $ da == Da3 |- ni == Ni3
    guard $ ri == Ri2 |- ga /= Ga1
    guard $ ri == Ri3 |- ga == Ga3
    return $ Same [ri, ga, ma, Pa, da, ni]
    where
    infixr 3 |-
    x |- y = not x || y

-- | This is the same as 'melakarta_ragams', but with intervals.  I should
-- probably derive melakarta_ragams from this.
melakarta_intervals :: [(Name, [Pitch.Semi])]
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

type Ratio = Ratio.Ratio Int

data SwaramP = Sa | Ri1 | Ri2 | Ri3 | Ga1 | Ga2 | Ga3 | Ma1 | Ma2 | Pa
    | Da1 | Da2 | Da3 | Ni1 | Ni2 | Ni3
    deriving (Eq, Ord, Enum, Show)

swaram_ratio :: SwaramP -> Ratio
swaram_ratio = \case
    Sa -> sa
    Ri1 -> ri1
    Ri2 -> ri2
    Ri3 -> ri3
    Ga1 -> ga1
    Ga2 -> ga2
    Ga3 -> ga3
    Ma1 -> ma1
    Ma2 -> ma2
    Pa -> pa
    Da1 -> da1
    Da2 -> da2
    Da3 -> da3
    Ni1 -> ni1
    Ni2 -> ni2
    Ni3 -> ni3
    where

    {- I don't think there is any official definition, but listeners agree 8/7
        is too high for ri, so I use a 5-limit scale.  Except prati madhyama is
        7/5, because I like it.  There is some theoretical material deriving 22
        srutis via 5-limit, but I'm not sure what bearing it has on real
        practice.

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

all_swarams :: [SwaramP]
all_swarams = [Sa, Ri1, Ri2, Ga2, Ga3, Ma1, Ma2, Pa, Da1, Da2, Ni2, Ni3]

all_ratios :: [Ratio]
all_ratios = map swaram_ratio all_swarams

unswaram :: SwaramP -> Swaram
unswaram = \case
    Sa -> S
    Ri1 -> R
    Ri2 -> R
    Ri3 -> R
    Ga1 -> G
    Ga2 -> G
    Ga3 -> G
    Ma1 -> M
    Ma2 -> M
    Pa -> P
    Da1 -> D
    Da2 -> D
    Da3 -> D
    Ni1 -> N
    Ni2 -> N
    Ni3 -> N

swaramps :: Swaram -> [SwaramP]
swaramps = \case
    S -> [Sa]
    R -> [Ri1, Ri2, Ri3]
    G -> [Ga1, Ga2, Ga3]
    M -> [Ma1, Ma2]
    P -> [Pa]
    D -> [Da1, Da2, Da3]
    N -> [Ni1, Ni2, Ni3]

data Swaram = S | R | G | M | P | D | N
    deriving (Eq, Ord, Enum, Show)
type Name = Text

type Ragam = RagamT SwaramP

-- | Arohana \/ avarohana structure.  Sa is implicit, so it's omitted.
data RagamT a =
    -- | The arohana is given, the avarohana is the same but reversed.
    Same [a]
    -- | Arohana ascending from low sa, avarohana descending from high sa.
    -- The first and last Sa are omitted, since they are implicit.
    | Different [a] [a]
    deriving (Functor, Show)

ragam_swarams :: Ragam -> ([SwaramP], Maybe [SwaramP])
ragam_swarams = \case
    Same swarams -> (Sa : swarams, Nothing)
    Different arohana avarohana -> (Sa : arohana, Just $ avarohana ++ [Sa])

ragam_ratios :: Ragam -> JustScales.Ratios
ragam_ratios = Vector.fromList . map swaram_ratio . fst . ragam_swarams
    -- TODO this is just the arohanam, I don't have a way to support multiple
    -- swarams yet.
    -- TODO What's more, I can't even support missing swarams, so this will
    -- be wrong for non-sampurna ragams.

-- | TODO these are integrated into 'ragams', but broken.
janya :: Map Name [(Name, Ragam)]
janya = Map.fromList $ map get
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
        , ("surutti", Different [R, M, P, N, D, N] [N, D, P, M, G, P, M, R])
        ])
    -- , ("mayamalavagoula",
    -- TODO web page says gaulipantu is from mayamalavagoula, but it has m2?
    , ("kamavardhini",
        [ ("goulipantu", Different [R, M, P, N] [N, D, P, M, D, M, G, R])
        ])
    ]
    -- TODO ragams with multiple pitches per swaram
    where
    get (parent, janyas) =
        ( parent
        , [(name, get_swarams parent name ragam) | (name, ragam) <- janyas]
        )
    down = [N, D .. R]

get_swarams :: Name -> Name -> RagamT Swaram -> RagamT SwaramP
get_swarams parent name ragam = case lookup parent melakarta_ragams of
    Nothing -> error $ "no parent ragam " <> show parent <> " of " <> show name
    Just parent -> get (fst (ragam_swarams parent)) <$> ragam
    where
    -- Should not error, melakarta ragams are sampurna.
    get present swaram =
        fromMaybe (error $ "melakarta parent doesn't have " <> show swaram) $
        List.find (`elem` present) (swaramps swaram)

ragams :: [(Name, Ragam)]
ragams = concatMap get melakarta_ragams
    where
    get (name, swarams) = (name, swarams) : Map.findWithDefault [] name janya

melakarta_names :: [Name]
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

alternates :: [([Char], [Char])]
alternates = concatMap (\(a, b) -> [(a, b), (b, a)])
    [ ("ou", "ow")
    , ("dh", "d")
    , ("bh", "b")
    , ("kh", "k")
    , ("v", "w")
    , ("s", "sh")
    ]

alternate :: Eq a => [([a], [a])] -> [a] -> [[a]]
alternate alts = (\x -> if x == [[]] then [] else x) . go
    where
    go [] = [[]]
    go (x : xs) = map (x:) (go xs) ++ concatMap apply ps
        where
        ps = filter ((`List.isPrefixOf` (x:xs)) . fst) alts
        apply (pre, post) = map (post++) $ go (drop (length pre) (x:xs))


-- | Common aliases for ragam names.
aliases :: Map Name [Name]
aliases = Map.fromList $ map assert_valid_name
    [ ("dheerashankarabharanam", ["shankarabharanam"])
    , ("mechakalyani", ["kalyani"])
    , ("hanumatodi", ["todi"])
    , ("harikambhoji", ["kambhoji"])
    , ("natabhairavi", ["bhairavi"])
    ]

assert_valid_name :: (Name, a) -> (Name, a)
assert_valid_name val@(name, _)
    | name `elem` melakarta_names = val
    | otherwise = errorStack $ "not in melakarta_names: " <> showt name

-- * query

print_all :: IO ()
print_all = Text.IO.putStr $ show_ragams ragams

print_ragams :: Pattern -> IO ()
print_ragams = Text.IO.putStr . show_ragams . find_ragams

find_by_swarams :: [SwaramP] -> IO ()
find_by_swarams swarams_ =
    Text.IO.putStr $ show_ragams $ filter (matches . snd) ragams
    where
    matches ragam = swarams `Set.isSubsetOf` rswarams
        where
        (aro, avaro) = ragam_swarams ragam
        rswarams = Set.fromList $ Sa : aro ++ fromMaybe [] avaro
    swarams = Set.fromList swarams_

-- ** query implementation

type Pattern = String

find_ragams :: Pattern -> [(Name, Ragam)]
find_ragams pattern =
    case List.partition fst (mapMaybe match (map (first untxt) ragams)) of
        (exact@(_:_), _) -> map snd exact
        ([], inexact) -> map snd inexact
    where
    match (name, ragam)
        | any (==name) infixes = Just (True, (txt name, ragam))
        | any (`List.isInfixOf` name) infixes = Just (False, (txt name, ragam))
        | otherwise = Nothing
    infixes = alternate alternates pattern

show_ragams :: [(Name, Ragam)] -> Text
show_ragams ragams = Text.unlines $
    map justify header ++ concatMap show_ragam ragams

show_ragam :: (Name, Ragam) -> [Text]
show_ragam (name, ragam) =
    map justify (Seq.map_head (++[name]) (show_swarams ragam))

header :: [[Text]]
header =
    [ map Pretty.improperRatio (all_ratios ++ [2])
    , ["S", "R1", "R2", "R3", "",   "M1", "M2", "P", "D1", "D2", "D3", "", "S"]
    , ["",  "",   "G1", "G2", "G3", "",   "",   "",  "",   "N1", "N2", "N3"]
    ]

data Dir = Down | Up deriving (Eq)

show_swarams :: Ragam -> [[Text]]
show_swarams ragam = lines Up aro ++ maybe [] (lines Down) avaro
    where
    (aro, avaro) = ragam_swarams ragam
    lines dir = map pad
        . ((if dir == Up then Seq.map_last else Seq.map_head) (<>["S"]))
        . map line
        . Seq.split_between (if dir == Up then (>) else (<))
    line = map cell . Seq.pair_sorted_on id swaram_ratio all_ratios . List.sort
    cell = \case
        Seq.Both _ s -> showt (unswaram s)
        _ -> ""
    pad = take (length all_swarams + 1) . (++ repeat "")

justify :: [Text] -> Text
justify = Text.unwords . map (Text.justifyLeft 5 ' ')
