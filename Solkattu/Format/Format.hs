-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities shared among formatting backends.
module Solkattu.Format.Format (
    Abstraction, isAbstract, abstract
    , defaultAbstraction, allAbstract
    , Highlight(..)
    -- * group
    , Flat, Group(..)
    , convertGroups, mapGroups
    -- * normalize speed
    , NormalizedFlat
    , makeGroupsAbstract, makeGroupsAbstractRealize, normalizeSpeed
    -- * tala
    , breakAvartanams, formatFinalAvartanam
    , onSam, onAnga, onAkshara, angaSet
    -- * ruler
    , Ruler, inferRuler
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
newtype Abstraction = Abstraction (Set Solkattu.GroupType)
    deriving (Eq, Show, Semigroup, Monoid)

isAbstract :: Abstraction -> Solkattu.GroupType -> Bool
isAbstract (Abstraction abstract) gtype = Set.member gtype abstract

abstract :: [Solkattu.GroupType] -> Abstraction
abstract = Abstraction . Set.fromList

defaultAbstraction :: Abstraction
defaultAbstraction = abstract
    [ Solkattu.GPattern
    , Solkattu.GSarvaT
    ]

allAbstract :: Abstraction
allAbstract = Abstraction $ Set.fromList [minBound .. maxBound]

data Highlight = StartHighlight | Highlight | EndHighlight
    deriving (Eq, Show)

-- * group

type Flat stroke = S.Flat Group (Realize.Note stroke)

-- | Format-level Group.  This has just the group data which is needed to
-- format.
data Group = Group {
    _name :: Maybe Text
    , _type :: Solkattu.GroupType
    } deriving (Eq, Show)

instance Pretty Group where
    pretty (Group name typ) = fromMaybe "" name <> "(" <> showt typ <> ")"

-- | Reduce 'Realize.Group's to local 'Group's.
convertGroups :: [Either Korvai.Error ([Korvai.Flat stroke], Korvai.Error)]
    -> [Either Korvai.Error ([Flat stroke], Korvai.Error)]
convertGroups = map (fmap (first mapGroups))

mapGroups :: [S.Flat (Realize.Group stroke) a] -> [S.Flat Group a]
mapGroups = S.mapGroupFlat $ \g -> Group
    { _name = Realize._name g
    , _type = Realize._type g
    }

-- * normalize speed

-- | 'Flat' after 'normalizeSpeed'.
type NormalizedFlat stroke =
    S.Flat Group (S.State, S.Stroke (Realize.Note stroke))

makeGroupsAbstract :: Abstraction
    -> [NormalizedFlat stroke] -> [NormalizedFlat stroke]
makeGroupsAbstract abstraction = concatMap combine
    where
    combine (S.FGroup tempo group children)
        | isAbstract abstraction (_type group) =
            -- Sarva has no start symbol.
            if _type group == Solkattu.GSarvaT
                then map (replace abstractSarva) flattened
                else Seq.map_head_tail (abstract S.Attack) (abstract S.Sustain)
                    flattened
        | otherwise = [S.FGroup tempo group (concatMap combine children)]
        where
        flattened  = S.tempoNotes children
        abstract c =
            replace (c (Realize.Abstract (Realize.AbstractedGroup name)))
        replace n (tempo, (state, _)) = S.FNote tempo (state, n)
        fmatra = S.normalizeFMatra tempo (fromIntegral (length flattened))
        name = fromMaybe (Pretty.fraction True fmatra) (_name group)
    combine n = [n]
    abstractSarva = S.Sustain (Realize.Abstract Realize.AbstractedSarva)

-- | Like 'makeGroupsAbstract' except for non-normalized 'Realize.realize'
-- output.  This is used by LSol, not Format, but is defined here since it's
-- doing the same thing.
makeGroupsAbstractRealize :: Abstraction
    -> [S.Flat (Realize.Group a) (Realize.Note stroke)]
    -> [S.Flat (Realize.Group a) (Realize.Note stroke)]
makeGroupsAbstractRealize abstraction = concatMap combine
    where
    combine (S.FGroup tempo group children)
        | isAbstract abstraction (Realize._type group) =
            [S.FNote tempo (Realize.Abstract (Realize.AbstractedGroup name))]
        | otherwise = [S.FGroup tempo group (concatMap combine children)]
        where
        -- TODO shouldn't it depend on GroupType?
        name = fromMaybe (Pretty.fraction True fmatras) (Realize._name group)
        fmatras = S.durationFMatra tempo $ S.flatDuration children
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
