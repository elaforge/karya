-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- deriving (Real) for Duration emits this warning.
{-# OPTIONS_GHC -fno-warn-identities #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Low level support for rhythmic sequences in a Tala.  The actual Note
-- type is polymorphic, so this is purely rhythmic.
module Derive.Solkattu.Sequence (
    Note(..), TempoChange(..)
    , Duration, Matra, Speed, Nadai, speed_factor
    , change_speed
    , HasMatras(..)
    -- * transform
    , map_group, flatten_groups
    , simplify
    -- * tempo
    , Tempo(..), default_tempo
    , change_tempo
    -- * flatten
    , Meta(..)
    , notes, flatten, flatten_with
    , tempo_to_state, tempo_to_duration
    , Stroke(..), normalize_speed
    -- * State
    , State(..), state_tempo, state_position, show_position
    -- * functions
    , note_duration, matra_duration
    -- * util
    , first_left, right_until_left
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
-- 'Matra's per akshara.  Negative numbers half them.
data TempoChange = ChangeSpeed Speed | Nadai Nadai
    deriving (Eq, Ord, Show)

instance Pretty TempoChange where
    pretty (ChangeSpeed s) =
        "s" <> (if s > 0 then "+" else "-") <> showt (abs s)
    pretty (Nadai s) = "n" <> showt s

-- | A matra is an akshara divided by the nadai.  It corresponds to a single
-- sollu in first speed, which means the actual duration is dependent on Nadai
-- and Speed.
type Matra = Int

-- | 0 means nadai matras per akshara.  Positive numbers double that and
-- negative ones halve it.
type Speed = Int
type Nadai = Int

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

-- * flatten

notes :: [Note g a] -> [a]
notes = map snd . flatten

flatten :: [Note g a] -> [(Meta g, a)]
flatten = flatten_with default_tempo

data Meta g = Meta {
    -- | If Just, this marks the start of a group, with count of elements
    -- and polymorphic payload.  The count is >=1.
    _group :: !(Maybe (Int, g))
    , _tempo :: !Tempo
    } deriving (Eq, Show, Functor)

instance Pretty g => Pretty (Meta g) where
    pretty (Meta g tempo) = pretty (g, tempo)

flatten_with :: Tempo -> [Note g a] -> [(Meta g, a)]
flatten_with = go
    where
    go tempo = concatMap $ \n -> case n of
        Note note -> [(Meta Nothing tempo, note)]
        TempoChange change notes -> go (change_tempo change tempo) notes
        Group g notes -> case go tempo notes of
            (meta, a) : as ->
                (meta { _group = Just (length as + 1, g) }, a) : as
            [] -> []

-- | Calculate Tala position for each note.
tempo_to_state :: HasMatras a => Tala.Tala -> [(Tempo, a)]
    -> (State, [(State, a)])
tempo_to_state tala = List.mapAccumL process initial_state
    where
    process state (tempo, note) = (next_state, (set_tempo tempo state, note))
        where
        next_state = advance_state_by tala
            (matra_duration tempo * fromIntegral (matras_of note)) state

data Stroke a = Attack a | Sustain a | Rest
    deriving (Show, Eq)

instance Pretty a => Pretty (Stroke a) where
    pretty s = case s of
        Attack a -> pretty a
        Sustain _ -> "-"
        Rest -> "_"

-- | Normalize to the fastest speed, then mark position in the Tala.
normalize_speed :: HasMatras a => Tala.Tala -> [(Tempo, a)]
    -> [(State, Stroke a)]
normalize_speed tala notes =
    snd $ List.mapAccumL process initial_state by_nadai
    where
    process state (nadai, stroke) =
        (next_state, (state { state_nadai = nadai }, stroke))
        where
        next_state = advance_state_by tala (min_dur / fromIntegral nadai) state
    (by_nadai, min_dur) = flatten_speed notes

-- | Normalize to the fastest speed.  Fill slower strokes in with rests.
-- Speed 0 always gets at least one Stroke, even if everything it's not the
-- slowest.
flatten_speed :: HasMatras a => [(Tempo, a)] -> ([(Nadai, Stroke a)], Duration)
flatten_speed notes = (concatMap flatten notes, min_dur)
    where
    flatten (tempo, note) = map (nadai tempo,) $
        Attack note : replicate (spaces - 1)
            (if has_duration note then Sustain note else Rest)
        where
        spaces = matras_of note * 2 ^ (max_speed - speed tempo)
    -- The smallest duration is a note at max speed.
    min_dur = 1 / speed_factor max_speed
    max_speed = maximum $ 0 : map (speed . fst) notes

tempo_to_duration :: HasMatras a => [(Tempo, a)] -> [(Duration, a)]
tempo_to_duration = map $ \(tempo, note) ->
    (fromIntegral (matras_of note) * matra_duration tempo, note)

data Tempo = Tempo { speed :: !Speed, nadai :: !Nadai }
    deriving (Eq, Show)

instance Pretty Tempo where
    pretty (Tempo speed nadai) = "s" <> pretty speed <> "n" <> pretty nadai

default_tempo :: Tempo
default_tempo = Tempo { speed = 0, nadai = default_nadai }

default_nadai :: Nadai
default_nadai = 4

change_tempo :: TempoChange -> Tempo -> Tempo
change_tempo (ChangeSpeed s) tempo = tempo { speed = s + speed tempo }
change_tempo (Nadai n) tempo = tempo { nadai = n }

-- ** State

-- | Keep track of timing and tala position.
data State = State {
    state_avartanam :: !Int
    , state_akshara :: !Tala.Akshara
    -- | Time through this akshara, so this is always < 1.
    -- TODO actually this is not matras, but fraction of the way through the
    -- akshara.  Is there a better term?
    , state_matra :: !Duration
    -- | How many nadai in this akshara.  This is different from 'state_nadai'
    -- because if nadai changes in the middle of an akshara, that akshara will
    -- have an irregular number of matra in it.  For instance, if you change
    -- from nadai 4 to 3 at matra 2, then you have a 2+3 = 5 matra akshara.
    , state_akshara_nadai :: !Nadai
    , state_speed :: !Speed
    , state_nadai :: !Nadai
    } deriving (Show)

instance Pretty State where
    format (State avartanam akshara matra akshara_nadai speed nadai) =
        Pretty.record "State"
            [ ("avartanam", Pretty.format avartanam)
            , ("akshara", Pretty.format akshara)
            , ("matra", Pretty.format matra)
            , ("akshara_nadai", Pretty.format akshara_nadai)
            , ("speed", Pretty.format speed)
            , ("nadai", Pretty.format nadai)
            ]

initial_state :: State
initial_state = State
    { state_avartanam = 0
    , state_akshara = 0
    , state_matra = 0
    , state_akshara_nadai = default_nadai
    , state_speed = 0
    , state_nadai = default_nadai
    }

state_tempo :: State -> Tempo
state_tempo state = Tempo
    { speed = state_speed state
    , nadai = state_nadai state
    }

state_position :: State -> (Int, Tala.Akshara, Duration)
state_position state =
    (state_avartanam state, state_akshara state, state_matra state)

set_tempo :: Tempo -> State -> State
set_tempo tempo state = state
    { state_speed = speed tempo
    , state_nadai = nadai tempo
    }

show_position :: State -> Text
show_position state =
    "avartanam " <> showt (state_avartanam state + 1)
    <> ", akshara " <> showt (state_akshara state)
    <> ", matra "
    <> pretty (state_matra state * fromIntegral (state_nadai state))

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
advance_state_by tala matras state = state
    { state_avartanam = state_avartanam state + akshara_carry
    , state_akshara = akshara
    , state_matra = matra
    , state_akshara_nadai = if matra_carry > 0
        then state_nadai state else state_akshara_nadai state
    }
    where
    (matra_carry, matra) = properFraction $ state_matra state + matras
    (akshara_carry, akshara) = (state_akshara state + matra_carry)
        `divMod` sum (Tala.tala_aksharas tala)

-- * util

first_left :: [Either err a] -> Either ([a], err) [a]
first_left xs = maybe (Right vals) (Left . (vals,)) err
    where (vals, err) = right_until_left xs

-- | Collect Rights until I hit a Left.
right_until_left :: [Either a b] -> ([b], Maybe a)
right_until_left = go
    where
    go [] = ([], Nothing)
    go (Left a : _) = ([], Just a)
    go (Right b : xs) = first (b:) (go xs)
