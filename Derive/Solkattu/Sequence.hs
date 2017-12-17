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
    , Duration, FMatra, Matra, Speed, Nadai, Stride, speed_factor
    , change_speed
    , HasMatras(..)
    -- * transform
    , map_group, flatten_groups
    , simplify
    , map1
    -- * tempo
    , Tempo(..), default_tempo
    , change_tempo
    , decompose, decomposeM
    -- * flatten
    , Flat(..)
    , notes, flatten, flatten_with, flattened_notes
    , tempo_to_state, with_durations, tempo_notes
    , Stroke(..), normalize_speed
    -- * State
    , State(..), state_position, show_position
    -- * functions
    , note_duration, note_fmatra, fmatra_duration, normalize_fmatra
    , matra_duration
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

-- | This is a fractional 'Matra'.
newtype FMatra = FMatra Ratio.Rational
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
    -- TODO this should be has_sustain
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

data Flat g a = FGroup !Tempo !Int !g | FNote !Tempo !a
    deriving (Eq, Show, Functor)

instance (Pretty g, Pretty a) => Pretty (Flat g a) where
    pretty (FGroup tempo count g) = pretty (tempo, count, g)
    pretty (FNote tempo note) = pretty (tempo, note)

notes :: [Note g a] -> [a]
notes = flattened_notes . flatten

flatten :: [Note g a] -> [Flat g a]
flatten = flatten_with default_tempo

flatten_with :: Tempo -> [Note g a] -> [Flat g a]
flatten_with tempo = concat . snd . List.mapAccumL go tempo
    where
    go tempo n = case n of
        Note note -> (tempo, [FNote tempo note])
        TempoChange change notes ->
            (tempo, flatten_with (change_tempo change tempo) notes)
        Group g notes -> (tempo, FGroup tempo (length children) g : children)
            where children = flatten_with tempo notes

flattened_notes :: [Flat g a] -> [a]
flattened_notes = map snd . tempo_notes

tempo_notes :: [Flat g a] -> [(Tempo, a)]
tempo_notes = mapMaybe $ \n -> case n of
    FGroup {} -> Nothing
    FNote tempo note -> Just (tempo, note)

tempo_to_state :: HasMatras a => Tala.Tala -> [(Tempo, a)]
    -> (State, [(State, a)])
tempo_to_state tala = List.mapAccumL to_state initial_state
    where
    to_state state (tempo, note) =
        (advance_state_by tala dur state, (state, note))
        where dur = duration_of tempo note

-- | Calculate Duration for each note.
with_durations :: HasMatras a => [Flat g a] -> [Flat g (Duration, a)]
with_durations = map $ \n -> case n of
    FGroup tempo count g -> FGroup tempo count g
    FNote tempo note -> FNote tempo (duration_of tempo note, note)

data Stroke a = Attack a | Sustain a | Rest
    deriving (Show, Eq)

instance Pretty a => Pretty (Stroke a) where
    pretty s = case s of
        Attack a -> pretty a
        Sustain _ -> "-"
        Rest -> "_"

-- | Normalize to the fastest speed.  Fill slower strokes in with rests.
-- Speed 0 always gets at least one Stroke, even if it's not the slowest.
--
-- This normalizes speed, not nadai, because Realize.format lays out notation
-- by nadai, not in absolute time.
normalize_speed :: HasMatras a => Tala.Tala -> [Flat g a]
    -> [Flat g (State, (Stroke a))]
normalize_speed tala flattened =
    snd $ List.mapAccumL with_state initial_state $ expand flattened
    where
    with_state state (FNote tempo stroke) =
        ( advance_state_by tala (matra_duration tempo) state
        , FNote tempo (state, stroke)
        )
    with_state state (FGroup tempo count g) = (state, FGroup tempo count g)

    expand [] = []
    expand (FGroup tempo count g : notes) =
        FGroup new_tempo (length expanded) g : expanded ++ expand post
        where
        new_tempo = tempo { _speed = max_speed }
        expanded = expand pre
        (pre, post) = splitAt count notes
    expand (FNote tempo note : notes) = expanded ++ expand notes
        where
        expanded = map (FNote new_tempo) $
            Attack note : replicate (spaces - 1)
                (if has_duration note then Sustain note else Rest)
        new_tempo = tempo { _speed = max_speed }
        spaces = _stride tempo * matras_of note * 2 ^ (max_speed - _speed tempo)
    max_speed = maximum $ 0 : map _speed (mapMaybe tempo_of flattened)
    tempo_of (FNote tempo _) = Just tempo
    tempo_of _ = Nothing

-- ** Tempo

data Tempo = Tempo { _speed :: !Speed, _nadai :: !Nadai, _stride :: !Stride }
    deriving (Eq, Show)

instance Pretty Tempo where
    pretty (Tempo speed nadai stride) =
        "s" <> pretty speed <> "n" <> pretty nadai
        <> (if stride == 1 then "" else "t" <> pretty stride)

default_tempo :: Tempo
default_tempo = Tempo { _speed = 0, _nadai = default_nadai, _stride = 1 }

default_nadai :: Nadai
default_nadai = 4

change_tempo :: TempoChange -> Tempo -> Tempo
change_tempo (ChangeSpeed s) tempo = tempo { _speed = s + _speed tempo }
change_tempo (Nadai n) tempo = tempo { _nadai = n }
change_tempo (Stride s) tempo = tempo { _stride = s }

-- | Given a duration, return the speeds of 1 duration notes needed to add up
-- to that duration.  Error if the speed went past 4, which means the duration
-- probably isn't binary.
decompose :: Duration -> Either Text [Speed]
decompose dur = go (- floor (logBase 2 (realToFrac dur))) dur
    where
    go speed left
        | left == 0 = Right []
        | speed > 4 = Left $ "not a binary multiple: " <> pretty dur
        | matra <= left = (speed:) <$> go (speed+1) (left - matra)
        | otherwise = go (speed+1) left
        where matra = 1 / speed_factor speed

decomposeM :: FMatra -> Either Text [Speed]
decomposeM (FMatra m) = decompose (Duration m)

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
    Note n -> duration_of tempo n
    Group _ notes -> sum $ map (note_duration tempo) notes

duration_of :: HasMatras a => Tempo -> a -> Duration
duration_of tempo n = matra_duration tempo * fromIntegral (matras_of n)
    * fromIntegral (_stride tempo)

note_fmatra :: HasMatras a => Tempo -> Note g a -> FMatra
note_fmatra tempo n =
    realToFrac $ note_duration tempo n * fromIntegral (_nadai tempo)

fmatra_duration :: Tempo -> FMatra -> Duration
fmatra_duration tempo (FMatra matra) = Duration matra * matra_duration tempo

normalize_fmatra :: Tempo -> FMatra -> FMatra
normalize_fmatra tempo = (/ realToFrac (speed_factor (_speed tempo)))

-- | Duration of one matra in the given tempo.  This doesn't include '_stride',
-- because stride adds matras to the note duration, it doesn't change the
-- duration of a matra itself.
matra_duration :: Tempo -> Duration
matra_duration tempo =
    1 / speed_factor (_speed tempo) / fromIntegral (_nadai tempo)

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
