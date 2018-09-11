-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities shared among formatting backends.
module Solkattu.Format.Format (
    Abstraction, isAbstract
    , abstract, named, unnamed
    , defaultAbstraction, allAbstract
    , Highlight(..)
    -- * group
    , Flat
    , convertGroups, mapGroups
    -- * normalize speed
    , NormalizedFlat
    , makeGroupsAbstract, makeGroupsAbstractScore, normalizeSpeed
    -- * tala
    , breakAvartanams, formatFinalAvartanam
    , onSam, onAnga, onAkshara, angaSet
    -- * ruler
    , Ruler, pairWithRuler
    , inferRuler
    -- * util
    , mapSnd
) where
import qualified Data.List as List
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global


-- | Control what is rendered as strokes, and what is rendered as abstract
-- groups with durations.
newtype Abstraction = Abstraction (Set (Solkattu.GroupType, Named))
    deriving (Eq, Show, Semigroup, Monoid)

data Named = Unnamed | Named deriving (Eq, Ord, Show)

-- data Abstract = GroupType !Solkattu.GroupType | NamedGroups
--     deriving (Eq, Ord, Show)

isAbstract :: Abstraction -> Solkattu.Meta -> Bool
isAbstract (Abstraction abstract) group = Set.member (gtype, isNamed) abstract
    where
    gtype = Solkattu._type group
    isNamed = case Solkattu._name group of
        Nothing -> Unnamed
        Just _ -> Named

abstract, named, unnamed :: Solkattu.GroupType -> Abstraction
abstract gtype = Abstraction $ Set.fromList [(gtype, Named), (gtype, Unnamed)]
named gtype = Abstraction $ Set.singleton (gtype, Named)
unnamed gtype = Abstraction $ Set.singleton (gtype, Unnamed)

defaultAbstraction :: Abstraction
defaultAbstraction = mconcat
    [ abstract Solkattu.GPattern
    , abstract Solkattu.GSarva
    , named Solkattu.GTheme
    ]

allAbstract :: Abstraction
allAbstract = Abstraction $ Set.fromList
    [ (gtype, named)
    | gtype <- [minBound .. maxBound], named <- [Unnamed, Named]
    ]

data Highlight = StartHighlight | Highlight | EndHighlight
    deriving (Eq, Show)

-- * group

type Flat stroke = S.Flat Solkattu.Meta (Realize.Note stroke)

-- | Reduce 'Realize.Group's to local 'Group's.
convertGroups :: [Either Korvai.Error ([Korvai.Flat stroke], Korvai.Error)]
    -> [Either Korvai.Error ([Flat stroke], Korvai.Error)]
convertGroups = map (fmap (first mapGroups))

mapGroups :: [S.Flat (Realize.Group stroke) a] -> [S.Flat Solkattu.Meta a]
mapGroups = S.mapGroupFlat groupToMeta

groupToMeta :: Realize.Group stroke -> Solkattu.Meta
groupToMeta (Realize.GReduction _) = Solkattu.Meta
    { _matras = Nothing -- TODO pick a real duration?
    , _name = Nothing
    , _type = Solkattu.GTheme
    }
groupToMeta (Realize.GMeta meta) = meta

-- * normalize speed

-- | 'Flat' after 'normalizeSpeed'.
type NormalizedFlat stroke =
    S.Flat Solkattu.Meta (S.State, S.Stroke (Realize.Note stroke))

makeGroupsAbstract :: Abstraction
    -> [NormalizedFlat stroke] -> [NormalizedFlat stroke]
makeGroupsAbstract abstraction = concatMap combine
    where
    combine (S.FGroup tempo group children)
        | isAbstract abstraction group =
            Seq.map_head_tail (abstract S.Attack) (abstract S.Sustain)
                tempoNotes
        | otherwise = [S.FGroup tempo group (concatMap combine children)]
        where
        gtype = Solkattu._type group
        tempoNotes  = S.tempoNotes children
        abstract c = replace $ c $ Realize.Abstract $
            -- Some groups are named by their duration.  Since the next stop is
            -- the HasMatras instance, I put them name on here, when I still
            -- have access to the children duration.
            if Solkattu._name group == Nothing
                    && gtype `elem` [Solkattu.GTheme, Solkattu.GExplicitPattern]
                then group
                    { Solkattu._name = Just $
                        Pretty.fraction True fmatras <> Realize.typeName gtype
                    }
                else group
        fmatras = S.durationFMatra tempo $
            sum $ map (S.matraDuration . fst) tempoNotes
        replace n (tempo, (state, _)) = S.FNote tempo (state, n)
    combine n = [n]

-- | Like 'makeGroupsAbstract' except for non-normalized 'Realize.realize'
-- output.  This is used by LSol, not Format, but is defined here since it's
-- doing the same thing.
makeGroupsAbstractScore :: Abstraction
    -> [S.Flat (Realize.Group a) (Realize.Note stroke)]
    -> [S.Flat (Realize.Group a) (Realize.Note stroke)]
makeGroupsAbstractScore abstraction = concatMap combine
    where
    combine (S.FGroup tempo group children)
        | isAbstract abstraction meta = (:[]) $
            S.FNote tempo $ Realize.Abstract meta
        | otherwise = [S.FGroup tempo group (concatMap combine children)]
        where meta = groupToMeta group
    combine n = [n]

normalizeSpeed :: Tala.Tala -> [Flat stroke] -> [NormalizedFlat stroke]
normalizeSpeed tala =
    fmap (fmap (fmap normalizeRest)) . S.normalizeSpeed tala
    . S.filterFlat (not . isAlignment)
    where
    isAlignment (Realize.Alignment {}) = True
    isAlignment _ = False

-- | Rests are special in that S.normalizeSpeed can produce them.  Normalize
-- them to force them to all be treated the same way.
normalizeRest :: S.Stroke (Realize.Note a) -> S.Stroke (Realize.Note a)
normalizeRest (S.Attack (Realize.Space Solkattu.Rest)) = S.Rest
normalizeRest (S.Sustain (Realize.Space Solkattu.Rest)) = S.Rest
normalizeRest a = a

-- * tala

-- | Split on sam.
breakAvartanams :: [(S.State, a)] -> [[(S.State, a)]]
breakAvartanams = dropWhile null . Seq.split_before (onSam . fst)

-- | If the final non-rest is at sam, drop trailing rests, and don't wrap it
-- onto the next line.
formatFinalAvartanam :: (note -> Bool) -> [[[(a, note)]]]
    -- ^ [avartanams], broken by lines
    -> [[[(a, note)]]]
formatFinalAvartanam isRest avartanams = case reverse avartanams of
    [final : rests] : penultimate : prevs
        | not (isRest (snd final)) && all (isRest . snd) rests ->
            reverse $ (Seq.map_last (++[final]) penultimate) : prevs
        | otherwise -> avartanams
    _ -> avartanams

onSam :: S.State -> Bool
onSam state = S.stateMatra state == 0 && S.stateAkshara state == 0

onAnga :: Set Tala.Akshara -> S.State -> Bool
onAnga angas state =
    S.stateMatra state == 0 && Set.member (S.stateAkshara state) angas

onAkshara :: S.State -> Bool
onAkshara state = S.stateMatra state == 0

angaSet :: Tala.Tala -> Set Tala.Akshara
angaSet = Set.fromList . scanl (+) 0 . Tala.tala_angas

-- * ruler

type Ruler = [(Text, Int)]

-- | (prevRuler, linesSinceLastRuler)
type PrevRuler = (Maybe Ruler, Int)
type Line sym = [(S.State, sym)]

pairWithRuler :: Int -> PrevRuler -> Tala.Tala -> Int -> [[Line sym]]
    -> (PrevRuler, [[(Maybe Ruler, Line sym)]])
pairWithRuler rulerEach prevRuler tala strokeWidth =
    List.mapAccumL (List.mapAccumL strip) prevRuler . map (map addRuler)
    where
    addRuler line = (inferRuler tala strokeWidth (map fst line), line)
    -- Strip rulers when they are unchanged.  "Changed" is by structure, not
    -- mark text, so a wrapped ruler with the same structure will also be
    -- suppressed.
    strip (prev, lineNumber) (ruler, line) =
        ( (Just ruler, 1 + if wanted then 0 else lineNumber)
        , (if wanted then Just ruler else Nothing, line)
        )
        where
        wanted = lineNumber `mod` rulerEach == 0
            || Just (map snd ruler) /= (map snd <$> prev)

-- | Rather than generating the ruler purely from the Tala, I use the States
-- to figure out the mark spacing.  Otherwise I wouldn't know where nadai
-- changes occur.  But it does mean I can't generate ruler if I run out of
-- strokes, which is a bit annoying for incomplete korvais or ones with eddupu.
inferRuler :: Tala.Tala -> Int -> [S.State] -> Ruler
inferRuler tala strokeWidth =
    (++ [("|", 0)])
    . merge
    . map (second length)
    . concat . snd . List.mapAccumL insertNadai 0
    . concatMap insertDots
    . zip (Tala.tala_labels tala)
    . dropWhile null
    . Seq.split_before onAkshara
    where
    -- Merge 0 dur marks with the next mark.  HTML output puts one mark per
    -- matra, so it can't have 0 dur marks.
    merge ((n1, 0) : (n2, spaces) : xs) = merge ((n1<>n2, spaces) : xs)
    merge ((n, spaces) : xs) = (n, spaces) : merge xs
    merge xs = xs
    insertNadai :: S.Nadai -> (Text, [S.State])
        -> (S.Nadai, [(Text, [S.State])])
    insertNadai prevNadai (label, states) =
        ( maybe prevNadai fst (Seq.last groups)
        , case groups of
            (nadai, states) : rest | nadai == prevNadai ->
                (label, states) : map (first nadaiChange) rest
            _ -> (label, []) : map (first nadaiChange) groups
        )
        where
        groups = Seq.keyed_group_adjacent nadaiOf states
        nadaiOf = S._nadai . S.stateTempo
    -- Marker for a nadai change.  It has a colon to separate it from the ruler
    -- mark, in case it coincides with one.
    nadaiChange n = ":" <> showt n
    insertDots (label, states)
        | (spaces * strokeWidth > 8) && spaces `mod` 2 == 0 =
            [(label, pre) , (".", post)]
        | otherwise = [(label, states)]
        where
        (pre, post) = splitAt (spaces `div` 2) states
        spaces = length states

-- * util

-- | This assumes the function doesn't change the length of the list!
mapSnd :: ([a] -> [b]) -> [(x, a)] -> [(x, b)]
mapSnd f xas = zip xs (f as)
    where (xs, as) = unzip xas
