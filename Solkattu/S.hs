-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- deriving (Real) for Duration emits this warning.
{-# OPTIONS_GHC -fno-warn-identities #-}
-- | Low level support for rhythmic sequences in a Tala.  The actual Note
-- type is polymorphic, so this is purely rhythmic.
module Solkattu.S (
    Note(..), TempoChange(..)
    , Duration, FMatra, Matra, Speed, Nadai, Stride, speedFactor
    , changeSpeed
    , HasMatras(..)
    -- * transform
    , mapGroup, flattenGroups
    , simplify
    , map1
    , filterNotes
    -- * tempo
    , Tempo(..), defaultTempo
    , changeTempo
    , decompose, decomposeM
    -- * flatten
    , Flat(..)
    , filterFlat, mapGroupFlat
    , notes, flatten, flattenWith, flattenedNotes
    , tempoToState, withDurations
    , tempoNotes, flatDuration, maxSpeed
    , Stroke(..), normalizeSpeed, flattenSpeed
    -- * State
    , State(..), statePosition, stateMatraPosition, showPosition
    -- * functions
    , durationOf, noteDuration, noteFMatra, fmatraDuration, durationFMatra
    , matraFMatra
    , matraDuration
#ifdef TESTING
    , module Solkattu.S
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
    -- See NOTE [nested-groups] for how I arrived at this design.
    | Group !g ![Note g a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

-- | A matra is an akshara divided by the nadai.  It corresponds to a single
-- sollu in first speed, which means the actual duration is dependent on Nadai
-- and Speed.
--
-- Matra being integral is important, since together with TempoChange, it
-- can encode the invariant that durations are always a power of two rational,
-- once you multiply out the nadai.
type Matra = Int

-- | This is a fractional 'Matra'.  The reason to need a fraction matra is if
-- there are speed changes below, so often this is used to mean toplevel matra,
-- which is to say duration / nadai, in which case it's relative to nadai, not
-- speed.  But not always!  Sometimes it's used to name a number of Matras
-- where it's assumed they have speed changes to accommodate the fraction.
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
    -- TODO I used to do this, but there is now such a thing as empty sarva
    -- groups, which have a duration.  I could get this back put putting in
    -- a dummy [NoSollu], but I'm not sure if that's better or worse.
    -- cancel (Group _ []) = []
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

filterNotes :: (a -> Bool) -> [Note g a] -> [Note g a]
filterNotes f = mapMaybe $ \case
    note@(Note a) -> if f a then Just note else Nothing
    TempoChange change ns -> Just $ TempoChange change (filterNotes f ns)
    Group g ns -> Just $ Group g (filterNotes f ns)

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

    Another way to look at this, is that each FNote is one Matra.
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

mapGroupFlat :: (g -> h) -> [Flat g a] -> [Flat h a]
mapGroupFlat f = map convert
    where
    convert (FGroup tempo g children) =
        FGroup tempo (f g) (map convert children)
    convert (FNote tempo a) = FNote tempo a

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

-- | This should be equivalent to but more efficient than
-- @sum . map (uncurry noteDuration) . tempoNotes@
flatDuration :: HasMatras a => [Flat g a] -> Duration
flatDuration = List.foldl' dur 0
    where
    dur accum (FGroup _ _ children) = List.foldl' dur accum children
    dur accum (FNote tempo note) = accum + noteDuration tempo note

maxSpeed :: [Flat g a] -> Speed
maxSpeed = maximum . (_speed defaultTempo :) . map _speed . tempoOf
    where
    tempoOf = concatMap $ \n -> case n of
        FNote tempo _ -> [tempo]
        FGroup tempo _ children -> tempo : tempoOf children
    -- If I use tempoNotes, I miss the Tempos at FGroups, which turn out to be
    -- important.

tempoToState :: HasMatras a => Tala.Tala -> Duration -- ^ start time
    -> [(Tempo, a)]
    -> (State, [(State, a)])
tempoToState tala start = List.mapAccumL toState (stateFrom tala start)
    where
    toState state (tempo, note) =
        ( advanceStateBy tala dur state
        , (state { stateTempo = tempo }, note)
        )
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
        return $ FNote tempo (state { stateTempo = tempo }, stroke)
    addState (FGroup tempo g children) =
        FGroup tempo g <$> mapM addState children
    expand (FGroup tempo g children) =
        [FGroup (tempo { _speed = toSpeed }) g (concatMap expand children)]
    expand (FNote tempo note) =
        map (FNote (tempo { _speed = toSpeed })) $
            Attack note : replicate (spaces - 1)
                (if hasSustain note then Sustain note else Rest)
        where
        spaces = _stride tempo * matrasOf note * 2 ^ (toSpeed - _speed tempo)

    toSpeed = maximum $ 0 : map _speed (tempoOf flattened)
    tempoOf = concatMap $ \n -> case n of
        FNote tempo _ -> [tempo]
        FGroup tempo _ children -> tempo : tempoOf children

-- | This is similar to 'normalizeSpeed', but working on 'Note's instead of
-- 'Flat's.  Expand speed to the given toSpeed, or error if there's a speed
-- above it, or if I run into a nadai change.  This will eliminate all
-- 'TempoChange's.
flattenSpeed :: HasMatras a => Speed -> [Note g a]
    -> Either Text [Note g (Stroke a)]
flattenSpeed toSpeed = normalize defaultTempo
    where
    normalize tempo = concatMapM (go tempo)
    go tempo = \case
        Note a -> Right $ map Note $ Attack a : replicate (spaces-1) sustain
            where
            sustain = if hasSustain a then Sustain a else Rest
            spaces = _stride tempo * matrasOf a * 2 ^ (toSpeed - _speed tempo)
        TempoChange change subs -> case change of
            Nadai _ -> Left $ "unsupported nadai change: " <> pretty change
            ChangeSpeed s | speed + s > toSpeed ->
                Left $ "speed " <> showt (speed+s) <> " > toSpeed "
                    <> showt toSpeed
            _ -> normalize (changeTempo change tempo) subs
        Group g subs -> (:[]) . Group g <$> normalize tempo subs
        where
        speed = _speed tempo

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
    -- | The tempo at the time of the State.  This is not needed internally,
    -- but it's easier to record this explicitly than try to figure it out
    -- based on the difference between this state and the next.
    --
    -- TODO this is a bit error prone, because while the rest of the fields are
    -- about the current state, this is about the next time step.  That means
    -- 'advanceStateBy' is too late to set it, and it has to be set by whoever
    -- calls advanceStateBy.  Ugh.
    , stateTempo :: !Tempo
    } deriving (Show)

instance Pretty State where
    format (State avartanam akshara matra tempo) =
        Pretty.record "State"
            [ ("avartanam", Pretty.format avartanam)
            , ("akshara", Pretty.format akshara)
            , ("matra", Pretty.format matra)
            , ("tempo", Pretty.format tempo)
            ]

stateFrom :: Tala.Tala -> Duration -> State
stateFrom tala dur = advanceStateBy tala dur initialState

initialState :: State
initialState = State
    { stateAvartanam = 0
    , stateAkshara = 0
    , stateMatra = 0
    , stateTempo = defaultTempo
    }

statePosition :: State -> (Int, Tala.Akshara, Duration)
statePosition state =
    (stateAvartanam state, stateAkshara state, stateMatra state)

stateMatraPosition :: State -> Duration
stateMatraPosition state = fromIntegral (stateAkshara state) + stateMatra state

-- | Show avartanam, akshara, and matra as avartanam:akshara+n/d.
showPosition :: State -> Text
showPosition state = showt (stateAvartanam state + 1)
    <> ":" <> pretty (stateMatraPosition state)

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

noteFMatra :: HasMatras a => Tempo -> Note g a -> FMatra
noteFMatra tempo n =
    realToFrac $ durationOf tempo n * fromIntegral (_nadai tempo)

fmatraDuration :: Tempo -> FMatra -> Duration
fmatraDuration tempo (FMatra matra) = Duration matra * matraDuration tempo

durationFMatra :: Tempo -> Duration -> FMatra
durationFMatra tempo dur = realToFrac $ dur * fromIntegral (_nadai tempo)

-- | Convert a tempo-relative Matra to a toplevel FMatra, which should only
-- be nadai-relative.
matraFMatra :: Tempo -> Matra -> FMatra
matraFMatra tempo matra =
    fromIntegral matra * (1 / realToFrac (speedFactor (_speed tempo)))

-- normalizeFMatra :: Tempo -> FMatra -> FMatra
-- normalizeFMatra tempo = (/ realToFrac (speedFactor (_speed tempo)))

-- | Duration of one matra in the given tempo.  This doesn't include '_stride',
-- because stride adds matras to the note duration, it doesn't change the
-- duration of a matra itself.
matraDuration :: Tempo -> Duration
matraDuration tempo =
    1 / speedFactor (_speed tempo) / fromIntegral (_nadai tempo)

advanceStateBy :: Tala.Tala -> Duration -> State -> State
advanceStateBy tala duration state = State
    { stateAvartanam = stateAvartanam state + aksharaCarry
    , stateAkshara = akshara
    , stateMatra = dur
    -- This will probably have to be updated by the caller.
    , stateTempo = stateTempo state
    }
    where
    (durCarry, dur) = properFraction $ stateMatra state + duration
    (aksharaCarry, akshara)
        | avartanam == 0 = (0, stateAkshara state + durCarry)
        | otherwise = (stateAkshara state + durCarry) `divMod` avartanam
        where avartanam = Tala.tala_aksharas tala
