-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- deriving (Real) for Duration emits this warning.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-identities #-}
-- | Low level support for rhythmic sequences in a Tala.  The actual Note
-- type is polymorphic, so this is purely rhythmic.
module Solkattu.Sequence (
    Note(..), TempoChange(..)
    , Duration, FMatra, Matra, Speed, Nadai, Stride, speedFactor
    , changeSpeed
    , HasMatras(..)
    -- * transform
    , mapGroup, flattenGroups
    , simplify
    , map1
    -- * tempo
    , Tempo(..), defaultTempo
    , changeTempo
    , decompose, decomposeM
    -- * flatten
    , Flat(..)
    , filterFlat
    , notes, flatten, flattenWith, flattenedNotes
    , tempoToState, withDurations, tempoNotes
    , Stroke(..), normalizeSpeed
    -- * State
    , State(..), statePosition, showPosition
    -- * functions
    , durationOf, noteDuration, noteFmatra, fmatraDuration, normalizeFmatra
    , matraDuration
#ifdef TESTING
    , module Solkattu.Sequence
#endif
) where
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Ratio as Ratio

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Solkattu.Tala as Tala
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

-- | A single Duration unit is equivalent to 1 Akshara.  Unlike 'FMatra' and
-- 'Matra', this is an absolute duration, so it doesn't depend on '_nadai' or
-- '_speed'.
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
-- | This could be Duration, but it would make normalizeSpeed tricky.
type Stride = Int

speedFactor :: Speed -> Duration
speedFactor s
    | s > 0 = 2^s
    | otherwise = 1 / (2 ^ abs s)

changeSpeed :: Speed -> [Note g a] -> Note g a
changeSpeed = TempoChange . ChangeSpeed

class HasMatras a where
    matrasOf :: a -> Matra
    -- | True if this note has a duration in time.  Otherwise, it's a single
    -- stroke, which logically has zero duration.  This only affects how the
    -- note is drawn and whether it becomes a tracklang event with duration.
    hasSustain :: a -> Bool

-- * transform

mapGroup :: (g -> h) -> Note g a -> Note h a
mapGroup f n = case n of
    Note a -> Note a
    TempoChange change ns -> TempoChange change (map (mapGroup f) ns)
    Group g ns -> Group (f g) (map (mapGroup f) ns)

flattenGroups :: [Note g a] -> [Note h a]
flattenGroups = concatMap $ \n -> case n of
    Group _ ns -> flattenGroups ns
    Note a -> [Note a]
    TempoChange change ns -> [TempoChange change (flattenGroups ns)]

-- | Drop empty TempoChanges, combine nested ones.  Drop empty groups.
simplify :: [Note g a] -> [Note g a]
simplify = merge . concatMap cancel
    where
    cancel (Note a) = [Note a]
    cancel (Group _ []) = []
    cancel (Group g ns) = [Group g ns]
    cancel (TempoChange _ []) = []
    cancel (TempoChange (ChangeSpeed s) xs) | s == 0 = xs
    cancel (TempoChange (ChangeSpeed s) xs) = concatMap (cancelSpeed s) xs
    cancel (TempoChange (Nadai n) xs) = concatMap (cancelNadai n) xs
    cancel (TempoChange (Stride s) xs) =
        [TempoChange (Stride s) (concatMap cancel xs)]

    cancelSpeed s1 (TempoChange (ChangeSpeed s2) xs) =
        cancel (TempoChange (ChangeSpeed (s1+s2)) xs)
    cancelSpeed s1 x = [TempoChange (ChangeSpeed s1) [x]]
    cancelNadai _ (TempoChange (Nadai n) xs) =
        cancel (TempoChange (Nadai n) xs)
    cancelNadai n x = [TempoChange (Nadai n) [x]]

    -- Merge adjacent TempoChanges.
    merge (TempoChange c sub : ns) =
        TempoChange c (concat (sub : same)) : merge rest
        where (same, rest) = Seq.span_while (sameChange c) ns
    merge (Note a : ns) = Note a : merge ns
    merge (Group g a : ns) = Group g a : merge ns
    merge [] = []
    sameChange change (TempoChange c ns) | change == c = Just ns
    sameChange _ _ = Nothing

-- | Transform only the first Note.
map1 :: (a -> a) -> Note g a -> Note g a
map1 f n = case n of
    Note a -> Note (f a)
    TempoChange change ns -> TempoChange change (Seq.map_head (map1 f) ns)
    Group g ns -> Group g (Seq.map_head (map1 f) ns)

-- * flatten

{- | This is an intermediate structure where TempoChange has been flattened
    out.  A flat list is easier to deal with, especially since I need to match
    and replace sections of notes, which may overlap tempo groups arbitrarily.

    However, 'FGroup' is actually nested, not flat.  Oops.  Originally it was
    flat too, with a count to indicate the scope, with Tempo in a Meta type.
    I still needed to express the tempo and group order, so I added a separate
    FGroup, and then it got complicated to keep the count up to date when the
    children changed size, and process things recursively and it seemed like
    recursive data would make that simpler again.  I only replace sections of
    notes within group boundaries, so it doesn't need to be flat like
    TempoChange does.

    It turns out it's still annoying to modify trees though, evidence in
    'Solkattu.Solkattu.cancelKarvai'.
-}
data Flat g a = FGroup !Tempo !g ![Flat g a] | FNote !Tempo !a
    deriving (Eq, Show, Functor)

instance (Pretty g, Pretty a) => Pretty (Flat g a) where
    pretty (FGroup tempo g notes) = pretty (tempo, g, notes)
    pretty (FNote tempo note) = pretty (tempo, note)

filterFlat :: (a -> Bool) -> [Flat g a] -> [Flat g a]
filterFlat f = go
    where
    go (n : ns) = case n of
        FGroup tempo g children -> FGroup tempo g (go children) : go ns
        FNote tempo n
            | f n -> FNote tempo n : go ns
            | otherwise -> go ns
    go [] = []

notes :: [Note g a] -> [a]
notes = flattenedNotes . flatten

flatten :: [Note g a] -> [Flat g a]
flatten = flattenWith defaultTempo

flattenWith :: Tempo -> [Note g a] -> [Flat g a]
flattenWith tempo = concat . snd . List.mapAccumL go tempo
    where
    go tempo n = case n of
        Note note -> (tempo, [FNote tempo note])
        TempoChange change notes ->
            (tempo, flattenWith (changeTempo change tempo) notes)
        Group g notes -> (tempo, [FGroup tempo g (flattenWith tempo notes)])

flattenedNotes :: [Flat g a] -> [a]
flattenedNotes = concatMap $ \n -> case n of
    FGroup _ _ children -> flattenedNotes children
    FNote _ note -> [note]

tempoNotes :: [Flat g a] -> [(Tempo, a)]
tempoNotes = concatMap $ \n -> case n of
    FGroup _ _ children  -> tempoNotes children
    FNote tempo note -> [(tempo, note)]

tempoToState :: HasMatras a => Tala.Tala -> [(Tempo, a)]
    -> (State, [(State, a)])
tempoToState tala = List.mapAccumL toState initialState
    where
    toState state (tempo, note) =
        (advanceStateBy tala dur state, (state, note))
        where dur = noteDuration tempo note

-- | Calculate Duration for each note.
withDurations :: HasMatras a => [Flat g a] -> [Flat g (Duration, a)]
withDurations = map $ \n -> case n of
    FGroup tempo g children -> FGroup tempo g (withDurations children)
    FNote tempo note -> FNote tempo (noteDuration tempo note, note)

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
normalizeSpeed :: HasMatras a => Tala.Tala -> [Flat g a]
    -> [Flat g (State, (Stroke a))]
normalizeSpeed tala flattened = fst $
    State.runState (mapM addState (concatMap expand flattened)) initialState
    where
    addState (FNote tempo stroke) = do
        state <- State.get
        State.modify' $ advanceStateBy tala (matraDuration tempo)
        return $ FNote tempo (state, stroke)
    addState (FGroup tempo g children) =
        FGroup tempo g <$> mapM addState children
    expand (FGroup tempo g children) =
        [FGroup (tempo { _speed = maxSpeed }) g (concatMap expand children)]
    expand (FNote tempo note) =
        map (FNote (tempo { _speed = maxSpeed })) $
            Attack note : replicate (spaces - 1)
                (if hasSustain note then Sustain note else Rest)
        where
        spaces = _stride tempo * matrasOf note * 2 ^ (maxSpeed - _speed tempo)
    maxSpeed = maximum $ 0 : map _speed (tempoOf flattened)
    tempoOf = concatMap $ \n -> case n of
        FNote tempo _ -> [tempo]
        FGroup tempo _ children -> tempo : tempoOf children

-- ** Tempo

data Tempo = Tempo { _speed :: !Speed, _nadai :: !Nadai, _stride :: !Stride }
    deriving (Eq, Show)

instance Pretty Tempo where
    pretty (Tempo speed nadai stride) =
        "s" <> pretty speed <> "n" <> pretty nadai
        <> (if stride == 1 then "" else "t" <> pretty stride)

defaultTempo :: Tempo
defaultTempo = Tempo { _speed = 0, _nadai = defaultNadai, _stride = 1 }

defaultNadai :: Nadai
defaultNadai = 4

changeTempo :: TempoChange -> Tempo -> Tempo
changeTempo (ChangeSpeed s) tempo = tempo { _speed = s + _speed tempo }
changeTempo (Nadai n) tempo = tempo { _nadai = n }
changeTempo (Stride s) tempo = tempo { _stride = s }

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
        where matra = 1 / speedFactor speed

decomposeM :: FMatra -> Either Text [Speed]
decomposeM (FMatra m) = decompose (Duration m)

-- ** State

-- | Keep track of timing and tala position.
data State = State {
    stateAvartanam :: !Int
    , stateAkshara :: !Tala.Akshara
    -- | Time through this akshara, so this is always < 1.
    -- TODO actually this is not matras, but fraction of the way through the
    -- akshara.  Is there a better term?
    , stateMatra :: !Duration
    } deriving (Show)

instance Pretty State where
    format (State avartanam akshara matra) =
        Pretty.record "State"
            [ ("avartanam", Pretty.format avartanam)
            , ("akshara", Pretty.format akshara)
            , ("matra", Pretty.format matra)
            ]

initialState :: State
initialState = State
    { stateAvartanam = 0
    , stateAkshara = 0
    , stateMatra = 0
    }

statePosition :: State -> (Int, Tala.Akshara, Duration)
statePosition state =
    (stateAvartanam state, stateAkshara state, stateMatra state)

showPosition :: State -> Text
showPosition state =
    "avartanam " <> showt (stateAvartanam state + 1)
    <> ", akshara " <> showt (stateAkshara state)
    <> " + " <> pretty (stateMatra state)

-- * functions

-- | Flatten the note and return its Duration.
durationOf :: HasMatras a => Tempo -> Note g a -> Duration
durationOf tempo n = case n of
    TempoChange change notes ->
        sum $ map (durationOf (changeTempo change tempo)) notes
    Note n -> noteDuration tempo n
    Group _ notes -> sum $ map (durationOf tempo) notes

noteDuration :: HasMatras a => Tempo -> a -> Duration
noteDuration tempo n = matraDuration tempo * fromIntegral (matrasOf n)
    * fromIntegral (_stride tempo)

noteFmatra :: HasMatras a => Tempo -> Note g a -> FMatra
noteFmatra tempo n =
    realToFrac $ durationOf tempo n * fromIntegral (_nadai tempo)

fmatraDuration :: Tempo -> FMatra -> Duration
fmatraDuration tempo (FMatra matra) = Duration matra * matraDuration tempo

normalizeFmatra :: Tempo -> FMatra -> FMatra
normalizeFmatra tempo = (/ realToFrac (speedFactor (_speed tempo)))

-- | Duration of one matra in the given tempo.  This doesn't include '_stride',
-- because stride adds matras to the note duration, it doesn't change the
-- duration of a matra itself.
matraDuration :: Tempo -> Duration
matraDuration tempo =
    1 / speedFactor (_speed tempo) / fromIntegral (_nadai tempo)

advanceStateBy :: Tala.Tala -> Duration -> State -> State
advanceStateBy tala duration state = state
    { stateAvartanam = stateAvartanam state + aksharaCarry
    , stateAkshara = akshara
    , stateMatra = dur
    }
    where
    (durCarry, dur) = properFraction $ stateMatra state + duration
    (aksharaCarry, akshara) = (stateAkshara state + durCarry)
        `divMod` Tala.tala_aksharas tala
