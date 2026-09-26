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
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified Derive.Scale as Scale
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = map Scale.Simple
    [ JustScales.makeScale "raga"
        (scaleMap (TheoryFormat.sargam relativeFmt)) doc docFields
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

docFields :: [(Doc.Doc, Doc.Doc)]
docFields = JustScales.groupRelativeKeys melakartaKeys

scaleMap :: TheoryFormat.Format -> JustScales.ScaleMap
scaleMap = JustScales.scaleMap keys defaultKey Nothing

relativeFmt :: TheoryFormat.RelativeFormat TheoryFormat.Tonic
relativeFmt = JustScales.makeRelativeFmt keys defaultKey

defaultKey :: JustScales.Key
Just defaultKey = Map.lookup (Pitch.Key "shankarabharanam") keys

keys :: Map Pitch.Key JustScales.Key
keys = Map.fromList melakartaKeys


-- * implementation

melakartaKeys :: [(Pitch.Key, JustScales.Key)]
melakartaKeys =
    [ (Pitch.Key alias, JustScales.Key 0 (Map.singleton "" ratios))
    | (name, ragam) <- ragams
    , let ratios = ragamRatios ragam
    , alias <- name : aliasesOf name
    ]
    where
    aliasesOf name = Map.findWithDefault [] name aliases

melakartaRagams :: [(Name, Ragam)]
melakartaRagams = zip melakartaNames $ do
    ma <- [Ma1, Ma2]
    ri <- [Ri1, Ri2, Ri3]
    ga <- [Ga1, Ga2, Ga3]
    da <- [Da1, Da2, Da3]
    ni <- [Ni1, Ni2, Ni3]
    guard $ da == Da2 |- ni /= Ni1
    guard $ da == Da3 |- ni == Ni3
    guard $ ri == Ri2 |- ga /= Ga1
    guard $ ri == Ri3 |- ga == Ga3
    pure $ Same [ri, ga, ma, Pa, da, ni]
    where
    infixr 3 |-
    x |- y = not x || y

-- | This is the same as 'melakarta_ragams', but with intervals.  I should
-- probably derive melakarta_ragams from this.
melakartaIntervals :: [(Name, [Pitch.Semi])]
melakartaIntervals = zip melakartaNames $ do
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
    pure $ zipWith (-) (tail indices) indices
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

swaramRatio :: SwaramP -> Ratio
swaramRatio = \case
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

allSwarams :: [SwaramP]
allSwarams = [Sa, Ri1, Ri2, Ga2, Ga3, Ma1, Ma2, Pa, Da1, Da2, Ni2, Ni3]

allRatios :: [Ratio]
allRatios = map swaramRatio allSwarams

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
data RagamT a
    -- | The arohana is given, the avarohana is the same but reversed.
    = Same [a]
    -- | Arohana ascending from low sa, avarohana descending from high sa.
    -- The first and last Sa are omitted, since they are implicit.
    | Different [a] [a]
    deriving (Functor, Show)

ragamSwarams :: Ragam -> ([SwaramP], Maybe [SwaramP])
ragamSwarams = \case
    Same swarams -> (Sa : swarams, Nothing)
    Different arohana avarohana -> (Sa : arohana, Just $ avarohana ++ [Sa])

ragamRatios :: Ragam -> JustScales.Ratios
ragamRatios = Vector.fromList . map swaramRatio . fst . ragamSwarams
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
        , [(name, getSwarams parent name ragam) | (name, ragam) <- janyas]
        )
    down = [N, D .. R]

getSwarams :: Name -> Name -> RagamT Swaram -> RagamT SwaramP
getSwarams parent name ragam = case lookup parent melakartaRagams of
    Nothing -> error $ "no parent ragam " <> show parent <> " of " <> show name
    Just parent -> get (fst (ragamSwarams parent)) <$> ragam
    where
    -- Should not error, melakarta ragams are sampurna.
    get present swaram =
        fromMaybe (error $ "melakarta parent doesn't have " <> show swaram) $
        List.find (`elem` present) (swaramps swaram)

ragams :: [(Name, Ragam)]
ragams = concatMap get melakartaRagams
    where
    get (name, swarams) = (name, swarams) : Map.findWithDefault [] name janya

melakartaNames :: [Name]
melakartaNames =
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
    , "hemavati", "dharmavati", "nitimati"

    , "kantamani", "rishabhapriya", "latangi" -- r2 g3
    , "vachaspati" , "mechakalyani", "chitrambhari"

    , "sucharitra", "jyotiswarupini", "dhatuvardhini" -- r3 g3
    , "nasikabhusani", "kosalam", "rasikapriya"
    ]

-- | Try alternate spellings when searching by name.
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
aliases = Map.fromList $ map assertValidName
    [ ("dheerashankarabharanam", ["shankarabharanam"])
    , ("mechakalyani", ["kalyani"])
    , ("hanumatodi", ["todi"])
    , ("harikambhoji", ["kambhoji"])
    , ("natabhairavi", ["bhairavi"])
    ]

assertValidName :: (Name, a) -> (Name, a)
assertValidName val@(name, _)
    | name `elem` melakartaNames = val
    | otherwise = errorStack $ "not in melakarta_names: " <> showt name

-- * query

printAll :: IO ()
printAll = mapM_ Text.IO.putStrLn $ showRagams ragams

printRagams :: Pattern -> IO ()
printRagams = mapM_ Text.IO.putStrLn . showRagams . findRagams

findBySwarams :: [SwaramP] -> IO ()
findBySwarams swarams_ =
    mapM_ Text.IO.putStrLn $ showRagams $ filter (matches . snd) ragams
    where
    matches ragam = swarams `Set.isSubsetOf` rswarams
        where
        (aro, avaro) = ragamSwarams ragam
        rswarams = Set.fromList $ Sa : aro ++ fromMaybe [] avaro
    swarams = Set.fromList swarams_

-- ** query implementation

type Pattern = String

findRagams :: Pattern -> [(Name, Ragam)]
findRagams pattern =
    case List.partition fst (mapMaybe match (map (first untxt) ragams)) of
        (exact@(_:_), _) -> map snd exact
        ([], inexact) -> map snd inexact
    where
    match (name, ragam)
        | any (==name) infixes = Just (True, (txt name, ragam))
        | any (`List.isInfixOf` name) infixes = Just (False, (txt name, ragam))
        | otherwise = Nothing
    infixes = alternate alternates pattern

showRagams :: [(Name, Ragam)] -> [Text]
showRagams ragams = Texts.columns 1 $ header ++ concatMap showRagam ragams

showRagam :: (Name, Ragam) -> [[Text]]
showRagam (name, ragam) = Lists.mapHead (++[name]) (showSwarams ragam)

header :: [[Text]]
header =
    -- [ map Pretty.improperRatio (all_ratios ++ [2])
    [ ["S", "R1", "R2", "R3", "",   "M1", "M2", "P", "D1", "D2", "D3", "", "S"]
    , ["",  "",   "G1", "G2", "G3", "",   "",   "",  "",   "N1", "N2", "N3"]
    ]

data Dir = Down | Up deriving (Eq)

showSwarams :: Ragam -> [[Text]]
showSwarams ragam = lines Up aro ++ maybe [] (lines Down) avaro
    where
    (aro, avaro) = ragamSwarams ragam
    lines dir = add dir . map line
        . Lists.splitBetween (if dir == Up then (>) else (<))
    add Up = Lists.mapInit (++[""]) . Lists.mapLast (++["S"])
    add Down = Lists.mapTail (++[""]) . Lists.mapHead (++["S", "<-"])
    line = map cell . Lists.pairSortedOn id swaramRatio allRatios . List.sort
    cell = \case
        Lists.Both _ s -> showt (unswaram s)
        _ -> ""
