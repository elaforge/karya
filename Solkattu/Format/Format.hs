-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities shared among formatting backends.
module Solkattu.Format.Format (
    Abstraction, Abstract(..), abstract, isAbstract
    , defaultAbstraction
    , Highlight(..)
    -- * group
    , Flat, Group(..)
    , convertGroups, mapGroups
    -- * normalize speed
    , NormalizedFlat
    , makeGroupsAbstract, normalizeSpeed
    -- * tala
    , breakAvartanams
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
newtype Abstraction = Abstraction (Set Abstract)
    deriving (Eq, Show, Semigroup, Monoid)
data Abstract = Patterns | Groups !(Maybe Text)
    deriving (Eq, Ord, Show)

abstract :: Abstract -> Abstraction
abstract = Abstraction . Set.singleton

isAbstract :: Abstraction -> Abstract -> Bool
isAbstract (Abstraction abstract) (Groups name) =
    Groups Nothing `Set.member` abstract || Groups name `Set.member` abstract
isAbstract (Abstraction abstract) Patterns = Patterns `Set.member` abstract

defaultAbstraction :: Abstraction
defaultAbstraction = abstract Patterns <> abstract (Groups (Just "4n"))

data Highlight = StartHighlight | Highlight | EndHighlight
    deriving (Eq, Show)

-- * group

type Flat stroke = S.Flat Group (Realize.Note stroke)

-- | Format-level Group.  This has just the group data which is needed to
-- format.
data Group = Group {
    _name :: Maybe Text
    , _highlight :: Bool
    } deriving (Eq, Show)

-- | Reduce 'Realize.Group's to local 'Group's.
convertGroups :: [Either Korvai.Error ([Korvai.Flat stroke], Korvai.Error)]
    -> [Either Korvai.Error ([Flat stroke], Korvai.Error)]
convertGroups = map (fmap (first mapGroups))

mapGroups :: [S.Flat (Realize.Group stroke) a] -> [S.Flat Group a]
mapGroups = S.mapGroupFlat $ \g -> Group
    { _name = Realize._name g
    , _highlight = Realize._highlight g
    }

-- * normalize speed

-- | 'Flat' after 'normalizeSpeed'.
type NormalizedFlat stroke =
    S.Flat Group (S.State, S.Stroke (Realize.Note stroke))

makeGroupsAbstract :: Abstraction -> [NormalizedFlat stroke]
    -> [NormalizedFlat stroke]
makeGroupsAbstract abstraction = concatMap combine
    where
    combine (S.FGroup tempo group children)
        | isAbstract abstraction (Groups (_name group)) =
            Seq.map_head_tail (make S.Attack) (make S.Sustain) flattened
        | otherwise = [S.FGroup tempo group (concatMap combine children)]
        where
        flattened  = S.tempoNotes children
        make c (tempo, (state, _)) =
            S.FNote tempo (state, c (Realize.Abstract name))
        fmatra = S.normalizeFMatra tempo (fromIntegral (length flattened))
        name = fromMaybe (Pretty.fraction True fmatra) (_name group)
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
