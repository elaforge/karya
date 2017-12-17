-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- deriving (Real) for Duration emits this warning.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-identities #-}
-- | Low level support for rhythmic sequences in a Tala.  The actual Note
-- type is polymorphic, so this is purely rhythmic.
module Derive.Solkattu.Sequence (
    Note(..), TempoChange(..)
    , Duration(..), Matra, Speed, Nadai, Stride, speed_factor
    , change_speed
    , HasMatras(..)
    -- * transform
    , map_group, flatten_groups
    , simplify
    , map1
    -- * tempo
    , Tempo(..), default_tempo
    , change_tempo
    -- * flatten
    , Meta(..), GroupMark(..)
    , notes, flatten, flatten_with
    , tempo_to_state, tempo_to_duration
    , Stroke(..), normalize_speed
    -- * State
    , State(..), state_position, show_position
    -- * functions
    , note_duration, matra_duration
#ifdef TESTING
    , module Derive.Solkattu.Sequence
#endif
) where
import qualified Data.List as List
import qualified Data.Ratio as Ratio

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.Solkattu.Tala as Tala
import Global


data Note g a = Note !a
    | TempoChange !TempoChange ![Note g a]
    | Group !g ![Note g a]
    deriving (Eq, Ord, Show, Functor)

instance (Pretty a, Pretty g) => Pretty (Note g a) where
    format n = case n of
        Note a -> Pretty.format a
        TempoChange change notes ->
            Pretty.text (pretty change) <> "("
                <> Pretty.wrapWords (map Pretty.format notes)
                <> ")"
        Group g notes ->
            Pretty.text (pretty g) <> "("
                <> Pretty.wrapWords (map Pretty.format notes)
                <> ")"

-- | A single Duration unit is equivalent to 1 Akshara.
newtype Duration = Duration Ratio.Rational
    deriving (Show, Ord, Eq, Num, Real, Fractional, RealFrac, Pretty)

-- | Relative speed change.  Each positive number doubles the number of
-- 'Matra's per akshara.  Negative numbers halve them.
data TempoChange = ChangeSpeed Speed | Nadai Nadai | Stride Stride
    deriving (Eq, Ord, Show)

instance Pretty TempoChange where
    pretty (ChangeSpeed s) =
        "s" <> (if s > 0 then "+" else "-") <> showt (abs s)
    pretty (Nadai s) = "n" <> showt s
    pretty (Stride s) = "t" <> showt s

-- | A matra is an akshara divided by the nadai.  It corresponds to a single
-- sollu in first speed, which means the actual duration is dependent on Nadai
-- and Speed.
type Matra = Int

-- | 0 means nadai matras per akshara.  Positive numbers double that and
-- negative ones halve it.
type Speed = Int
type Nadai = Int
-- | This could be Duration, but it would make normalize_speed tricky.
type Stride = Int

speed_factor :: Speed -> Duration
speed_factor s
    | s > 0 = 2^s
    | otherwise = 1 / (2 ^ abs s)

change_speed :: Speed -> [Note g a] -> Note g a
change_speed = TempoChange . ChangeSpeed

class HasMatras a where
    matras_of :: a -> Matra
    -- | True if this note has a duration in time.  Otherwise, it's a single
    -- stroke, which logically has zero duration.  So far, this only affects
    -- how the note is drawn.
    has_duration :: a -> Bool

-- * transform

map_group :: (g -> h) -> Note g a -> Note h a
map_group f n = case n of
    Note a -> Note a
    TempoChange change ns -> TempoChange change (map (map_group f) ns)
    Group g ns -> Group (f g) (map (map_group f) ns)

flatten_groups :: [Note g a] -> [Note h a]
flatten_groups = concatMap $ \n -> case n of
    Group _ ns -> flatten_groups ns
    Note a -> [Note a]
    TempoChange change ns -> [TempoChange change (flatten_groups ns)]

-- | Drop empty TempoChanges, combine nested ones.  Drop empty groups.
simplify :: [Note g a] -> [Note g a]
simplify = merge . concatMap cancel
    where
    cancel (Note a) = [Note a]
    cancel (Group _ []) = []
    cancel (Group g ns) = [Group g ns]
    cancel (TempoChange _ []) = []
    cancel (TempoChange (ChangeSpeed s) xs) | s == 0 = xs
    cancel (TempoChange (ChangeSpeed s) xs) = concatMap (cancel_speed s) xs
    cancel (TempoChange (Nadai n) xs) = concatMap (cancel_nadai n) xs
    cancel (TempoChange (Stride s) xs) =
        [TempoChange (Stride s) (concatMap cancel xs)]

    cancel_speed s1 (TempoChange (ChangeSpeed s2) xs) =
        cancel (TempoChange (ChangeSpeed (s1+s2)) xs)
    cancel_speed s1 x = [TempoChange (ChangeSpeed s1) [x]]
    cancel_nadai _ (TempoChange (Nadai n) xs) =
        cancel (TempoChange (Nadai n) xs)
    cancel_nadai n x = [TempoChange (Nadai n) [x]]

    -- Merge adjacent TempoChanges.
    merge (TempoChange c sub : ns) =
        TempoChange c (concat (sub : same)) : merge rest
        where (same, rest) = Seq.span_while (same_change c) ns
    merge (Note a : ns) = Note a : merge ns
    merge (Group g a : ns) = Group g a : merge ns
    merge [] = []
    same_change change (TempoChange c ns) | change == c = Just ns
    same_change _ _ = Nothing

-- | Transform only the first Note.
map1 :: (a -> a) -> Note g a -> Note g a
map1 f n = case n of
    Note a -> Note (f a)
    TempoChange change ns -> TempoChange change (Seq.map_head (map1 f) ns)
    Group g ns -> Group g (Seq.map_head (map1 f) ns)

-- * flatten

data Meta g = Meta {
    -- | If non-empty, this marks the start of a group.  It's a list because
    -- nested groups can start on the same element.
    _marks :: ![GroupMark g]
    , _tempo :: !Tempo
    } deriving (Eq, Show, Functor)

data GroupMark g = GroupMark {
    -- | Number of elements in this group.  It includes the one this Meta is
    -- attached to, so it should be >=1.
    _count :: !Int
    , _group :: !g
    } deriving (Eq, Show, Functor)

instance Pretty g => Pretty (Meta g) where
    pretty (Meta g tempo) = pretty (g, tempo)
instance Pretty g => Pretty (GroupMark g) where
    pretty (GroupMark count g) = pretty count <> " " <> pretty g

notes :: [Note g a] -> [a]
notes = map snd . flatten

flatten :: [Note g a] -> [(Meta g, a)]
flatten = flatten_with default_tempo

flatten_with :: Tempo -> [Note g a] -> [(Meta g, a)]
flatten_with = go
    where
    go tempo = concatMap $ \n -> case n of
        Note note -> [(Meta [] tempo, note)]
        TempoChange change notes -> go (change_tempo change tempo) notes
        Group g notes -> case go tempo notes of
            (meta, a) : as ->
                (meta { _marks = GroupMark (length as + 1) g : _marks meta }, a)
                : as
            [] -> []

-- | Calculate Tala position for each note.
tempo_to_state :: HasMatras a => Tala.Tala -> [(Tempo, a)]
    -> (State, [(State, a)])
tempo_to_state tala = List.mapAccumL to_state initial_state . tempo_to_duration
    where
    to_state state (dur, note) =
        (advance_state_by tala dur state, (state, note))

tempo_to_duration :: HasMatras a => [(Tempo, a)] -> [(Duration, a)]
tempo_to_duration = map $ \(tempo, note) -> (,note) $
    fromIntegral (matras_of note) * matra_duration tempo
        * fromIntegral (stride tempo)

data Stroke a = Attack a | Sustain a | Rest
    deriving (Show, Eq)

instance Pretty a => Pretty (Stroke a) where
    pretty s = case s of
        Attack a -> pretty a
        Sustain _ -> "-"
        Rest -> "_"

-- | Normalize to the fastest speed, then mark position in the Tala.  This
-- normalizes speed, not nadai, because Realize.format lays out notation by
-- nadai, not in absolute time.
normalize_speed :: HasMatras a => Tala.Tala -> [(Meta g, a)]
    -> [([GroupMark g], (State, Stroke a))]
normalize_speed tala notes =
    zip (collect_indices groups) $ snd $
        List.mapAccumL process initial_state by_nadai
    where
    process state (nadai, stroke) = (next_state, (state, stroke))
        where
        next_state = advance_state_by tala (step_dur / fromIntegral nadai) state
    (by_nadai, step_dur) = flatten_speed (map (first _tempo) notes)
    groups = expand_groups (map (_marks . fst) notes) (map snd by_nadai)

-- | Put the indexed elements at the proper place in a list.  The result has
-- infinite []s on the end.
collect_indices :: [(Int, a)] -> [[a]]
collect_indices = go 0
    where
    go i groups = map snd here : go (i+1) rest
        where (here, rest) = span ((<=i) . fst) groups

-- | Re-associate groups with the output of 'flatten_speed' by expanding their
-- '_count's.  Each group entry corresponds to an Attack Stroke.
expand_groups :: [[GroupMark g]] -> [Stroke a] -> [(Int, GroupMark g)]
    -- ^ (stroke index, group)
expand_groups groups =
    concat . zipWith expand groups . List.tails . drop 1
        . Seq.split_with (is_attack . snd) . zip [0..]
    where
    expand marks strokes@(((i, _) : _) : _) =
        [ (i, GroupMark (new_count count strokes) g)
        | GroupMark count g <- marks
        ]
    -- Neither of these should happen, due to Seq.split_with's postcondition.
    expand _ [] = []
    expand _ ([] : _) = []
    new_count count = sum . map length . take count
    is_attack (Attack {}) = True
    is_attack _ = False

-- | Normalize to the fastest speed.  Fill slower strokes in with rests.
-- Speed 0 always gets at least one Stroke, even if it's not the slowest.
flatten_speed :: HasMatras a => [(Tempo, a)] -> ([(Nadai, Stroke a)], Duration)
flatten_speed notes = (concatMap flatten notes, step_dur)
    where
    flatten (tempo, note) = map (nadai tempo,) $
        Attack note : replicate (spaces - 1)
            (if has_duration note then Sustain note else Rest)
        where
        spaces = stride tempo * matras_of note
            * 2 ^ (max_speed - speed tempo)
    -- The smallest duration is a note at max speed.
    step_dur = 1 / speed_factor max_speed
    max_speed = maximum $ 0 : map (speed . fst) notes

data Tempo = Tempo { speed :: !Speed, nadai :: !Nadai, stride :: !Stride }
    deriving (Eq, Show)

instance Pretty Tempo where
    pretty (Tempo speed nadai stride) =
        "s" <> pretty speed <> "n" <> pretty nadai
        <> (if stride == 1 then "" else "t" <> pretty stride)

default_tempo :: Tempo
default_tempo = Tempo { speed = 0, nadai = default_nadai, stride = 1 }

default_nadai :: Nadai
default_nadai = 4

change_tempo :: TempoChange -> Tempo -> Tempo
change_tempo (ChangeSpeed s) tempo = tempo { speed = s + speed tempo }
change_tempo (Nadai n) tempo = tempo { nadai = n }
change_tempo (Stride s) tempo = tempo { stride = s }

-- ** State

-- | Keep track of timing and tala position.
data State = State {
    state_avartanam :: !Int
    , state_akshara :: !Tala.Akshara
    -- | Time through this akshara, so this is always < 1.
    -- TODO actually this is not matras, but fraction of the way through the
    -- akshara.  Is there a better term?
    , state_matra :: !Duration
    } deriving (Show)

instance Pretty State where
    format (State avartanam akshara matra) =
        Pretty.record "State"
            [ ("avartanam", Pretty.format avartanam)
            , ("akshara", Pretty.format akshara)
            , ("matra", Pretty.format matra)
            ]

initial_state :: State
initial_state = State
    { state_avartanam = 0
    , state_akshara = 0
    , state_matra = 0
    }

state_position :: State -> (Int, Tala.Akshara, Duration)
state_position state =
    (state_avartanam state, state_akshara state, state_matra state)

show_position :: State -> Text
show_position state =
    "avartanam " <> showt (state_avartanam state + 1)
    <> ", akshara " <> showt (state_akshara state)
    <> " + " <> pretty (state_matra state)

-- * functions

-- | Flatten the note and return its Duration.
note_duration :: HasMatras a => Tempo -> Note g a -> Duration
note_duration tempo n = case n of
    TempoChange change notes ->
        sum $ map (note_duration (change_tempo change tempo)) notes
    Note n -> matra_duration tempo * fromIntegral (matras_of n)
    Group _ notes -> sum $ map (note_duration tempo) notes

-- | Duration of one matra in the given tempo.
matra_duration :: Tempo -> Duration
matra_duration tempo =
    1 / speed_factor (speed tempo) / fromIntegral (nadai tempo)

advance_state_by :: Tala.Tala -> Duration -> State -> State
advance_state_by tala duration state = state
    { state_avartanam = state_avartanam state + akshara_carry
    , state_akshara = akshara
    , state_matra = dur
    }
    where
    (dur_carry, dur) = properFraction $ state_matra state + duration
    (akshara_carry, akshara) = (state_akshara state + dur_carry)
        `divMod` sum (Tala.tala_aksharas tala)
