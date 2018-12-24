-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- deriving (Real) for Time emits this warning.
{-# OPTIONS_GHC -fno-warn-identities #-}
-- | Post-process 'T.Token's.  Check barlines, resolve ties, etc.
module Derive.TScore.Check where
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import           Global


-- | Integral time.  This is the smallest time unit expressed.
newtype Time = Time Ratio.Rational
    deriving (Ord, Eq, Num, Enum, Real, Fractional, RealFrac, Pretty)

instance Show Time where
    show (Time t) = prettys t

data Error = Error !Time !Text
    deriving (Eq, Show)

-- * pipeline

pipeline :: Scale -> Meter -> [T.Token T.Pitch T.Duration]
    -> [Either Error (Time, T.Note Pitch Time)]
pipeline scale meter = resolve_pitch scale . resolve_time . barlines meter
    . multiplicative
    -- TODO resolve pitch before time, so the pitches are right, so ties work.
    -- But then I still have TBarline and the like.

-- * meter

data Meter = Meter {
    -- | Rank pattern.
    --
    -- Adi: [2, 0, 0, 0, 1, 0, 0, 1, 0, 0]
    -- > || ssss ; rrrr ; gggg ; mmmm | pppp ; dddd | nnnn ; sssss ||
    -- 4/4: [1, 0, 0, 0]
    -- > | ssss ; rrrr ; gggg ; mmmm |
    meter_pattern :: [T.Rank]
    , meter_step :: !Time
    -- | If true, beats fall at the end of measures.
    , meter_negative :: !Bool
    } deriving (Eq, Show)

meter_duration :: Meter -> Time
meter_duration m = meter_step m * fromIntegral (length (meter_pattern m))

-- If I do akshara as 1, then kanda is 1/5th notes.  I'd want to reduce the
-- pulse to 1/5, or write .1--.5?
meter_adi :: Meter
meter_adi = Meter
    { meter_pattern = [2, 0, 0, 0, 1, 0, 1, 0]
    , meter_step = 1
    , meter_negative = False
    }

meter_44 :: Meter
meter_44 = Meter
    { meter_pattern = [1, 0, 0, 0]
    , meter_step = 1/4
    , meter_negative = False
    }

-- * resolve_time

-- | Remove TBarline and TRest, add start times, and resolve ties.
resolve_time :: (Eq pitch, Parse.Element pitch)
    => [Either Error (T.Token pitch (Time, Bool))]
    -> [Either Error (Time, T.Note pitch Time)]
resolve_time tokens = go . zip starts $ tokens
    where
    starts = scanl (\n -> (n+) . either (const 0) duration_of) 0 tokens
    go ((start, Right t) : ts) = case t of
        T.TNote note
            | is_tied t -> case tied_notes start note (sndRights pre) of
                Left err -> Left err : go post
                Right end -> Right (start, set_dur (end-start) note) : go post
            | otherwise ->
                Right (start, set_dur (fst (T.note_duration note)) note)
                : go ts
        T.TBarline {} -> go ts
        T.TRest {}
            | is_tied t -> case tied_rests (sndRights pre) of
                Just err -> Left err : go post
                Nothing -> go post
            | otherwise -> go post
        where
        (pre, post) = Then.span any_tied (splitAt 1) ts
        any_tied (_, Left {}) = True
        any_tied (_, Right n) = is_barline n || is_tied n
    go ((_, Left e) : ts) = Left e : go ts
    go [] = []
    set_dur dur note = note { T.note_duration = dur }
    is_barline (T.TBarline {}) = True
    is_barline _ = False
    sndRights abs = [(a, b) | (a, Right b) <- abs]

tied_notes :: (Eq pitch, Parse.Element pitch)
    => Time -> T.Note pitch (Time, Bool)
    -> [(Time, T.Token pitch (Time, Bool))]
    -> Either Error Time
tied_notes start note tied = case others of
    [] -> case Seq.last matches of
        -- Nothing -> Right $ start + dur_of note
        Nothing -> Left $ Error start "final note has a tie"
        Just (s, n)
            | snd $ T.note_duration n ->
                Left $ Error start "final note has a tie"
            | otherwise -> Right $ s + dur_of n
    (t, bad) : _ -> Left $ Error t $ case bad of
        T.TNote n -> "note tied to different pitch: "
            <> Parse.unparse (T.note_pitch note) <> " ~ "
            <> Parse.unparse (T.note_pitch n)
        _ -> "note tied to " <> T.token_name bad
    where
    (matches, others) = first concat $ Seq.partition_on match tied
    dur_of = fst . T.note_duration
    match (s, T.TNote n) | T.note_pitch note == T.note_pitch n = Just [(s, n)]
    match (_, T.TBarline {}) = Just []
    match _ = Nothing

tied_rests :: [(Time, T.Token pitch (Time, Bool))] -> Maybe Error
tied_rests = fmap format . List.find (not . matches . snd)
    where
    format (start, token) =
        Error start $ "rest tied to " <> T.token_name token
    matches (T.TRest {}) = True
    matches (T.TBarline {}) = True
    matches _ = False

is_tied (T.TNote note) = snd $ T.note_duration note
is_tied (T.TRest (T.Rest (_, tied))) = tied
is_tied _ = False

-- * barlines

barlines :: Meter -> [T.Token pitch (Time, tie)]
    -> [Either Error (T.Token pitch (Time, tie))]
barlines meter = concat . snd . List.mapAccumL token 0 . zip [0..]
    where
    token now (i, token) = (now + dur, Right token : warning)
        where
        dur = duration_of token
        warning = case token of
            T.TBarline bar -> maybe [] ((:[]) . Left) (check now i bar)
            _ -> []
    check now i (T.Barline rank) = case Map.lookup beat expected_rank of
        Just r
            | r == rank -> Nothing
            | otherwise -> Just $ warn i now $
                "saw " <> Parse.unparse (T.Barline rank)
                <> ", expected " <> Parse.unparse (T.Barline r)
        Nothing -> Just $ warn i now $
            "saw " <> Parse.unparse (T.Barline rank) <> ", expected none"
        where
        beat = now `Num.fmod` cycle_dur
    cycle_dur = meter_duration meter
    expected_rank = Map.fromList $ zip (Seq.range_ 0 (meter_step meter))
        (meter_pattern meter)
    warn :: Int -> Time -> Text -> Error
    warn i now msg = Error now ("token " <> showt i <> ": " <> msg)

show_time :: Time -> Time -> Text
show_time cycle_dur t = pretty (cycle :: Int) <> ":" <> pretty beat
    where (cycle, beat) = t `Num.fDivMod` cycle_dur

duration_of :: T.Token pitch (Time, tie) -> Time
duration_of = \case
    T.TBarline _ -> 0
    T.TNote note -> fst (T.note_duration note)
    T.TRest (T.Rest (dur, _)) -> dur

-- * resolve duration

-- | Each number is the inverse of the number of beats, so 2 is 1/2, 8 is 1/8
-- etc.
multiplicative :: [T.Token pitch T.Duration] -> [T.Token pitch (Time, Bool)]
multiplicative = run . mapM (T.map_duration convert)
    where
    run = fst . flip State.runState 1
    convert (T.Duration intDur dots tie) = do
        intDur <- carry_duration intDur
        let dur = Time (1 / fromIntegral intDur)
        let dotDur = sum $ take dots $ drop 1 $ iterate (/2) dur
        return (dur + dotDur, tie)

-- | Each number is just the number of Time beats.
additive :: [T.Token pitch T.Duration] -> [T.Token pitch (Time, Bool)]
additive = run . mapM (T.map_duration convert)
    where
    run = fst . flip State.runState 1
    convert (T.Duration intDur dots tie) = do
        intDur <- carry_duration intDur
        let dur = Time (fromIntegral intDur)
        let dotDur = sum $ take dots $ drop 1 $ iterate (/2) dur
        return (dur + dotDur, tie)

carry_duration :: State.MonadState Int m => Maybe Int -> m Int
carry_duration mbDur = do
    int <- maybe State.get return mbDur
    State.put int
    return int

-- * resolve pitch

type PitchDifference = (Octave, Text) -> (Octave, Text) -> Maybe Int

data Scale = Scale {
    scale_parse :: Text -> Maybe PitchClass
    , scale_per_octave :: !PitchClass
    , scale_initial_octave :: !Octave
    }

type PitchClass = Int
type Octave = Int

data Pitch = Pitch !Octave !PitchClass
    deriving (Eq, Show)

resolve_pitch :: Scale
    -> [Either Error (Time, T.Note T.Pitch dur)]
    -> [Either Error (Time, T.Note Pitch dur)]
resolve_pitch (Scale parse per_octave initial_octave) =
    infer_octaves per_octave initial_octave . parse_pitch parse

parse_pitch :: (Text -> Maybe pitch)
    -> [Either Error (Time, T.Note T.Pitch dur)]
    -> [Either Error (Time, T.Note (T.Octave, pitch) dur)]
parse_pitch parse = snd . mapRightE token Nothing
    where
    token maybe_prev (start, note)
        | Text.null call = case maybe_prev of
            Nothing ->
                ( maybe_prev
                , Left $ Error start "no pitch and no previous pitch"
                )
            Just p -> with_pitch p
        | otherwise = case parse call of
            Nothing ->
                ( maybe_prev
                , Left $ Error start $ "can't parse pitch: " <> call
                )
            Just p -> with_pitch p
        where
        T.Pitch oct call = T.note_pitch note
        with_pitch p =
            (Just p, Right (start, note { T.note_pitch = (oct, p) }))

infer_octaves :: PitchClass -> Octave
    -> [Either e (time, T.Note (T.Octave, PitchClass) dur)]
    -> [Either e (time, T.Note Pitch dur)]
infer_octaves per_octave initial_oct =
    snd . mapRight infer (initial_oct, Nothing)
    where
    infer (prev_oct, prev_pc) (start, note) = with_octave $ case oct of
        T.Relative n -> n + case prev_pc of
            Just prev ->
                min_on3 (distance prev) (prev_oct-1) prev_oct (prev_oct+1)
            Nothing -> prev_oct
        T.Absolute o -> o
        where
        (oct, pc) = T.note_pitch note
        with_octave o =
            ( (o, Just pc)
            , (start, note { T.note_pitch = Pitch o pc })
            )
        distance prev oct = abs $
            pitch_diff per_octave (Pitch prev_oct prev) (Pitch oct pc)
    min_on3 key a b c = Seq.min_on key a (Seq.min_on key b c)

-- * pitch

sargam :: Scale
sargam = Scale
    { scale_parse = (`List.elemIndex` degrees)
    , scale_per_octave = 8
    , scale_initial_octave = 4
    } where degrees = map Text.singleton "srgmpdn"

ioeua :: Scale
ioeua = Scale
    { scale_parse = (`List.elemIndex` degrees)
    , scale_per_octave = 5
    , scale_initial_octave = 4
    } where degrees = map Text.singleton "ioeua"

pitch_diff :: PitchClass -> Pitch -> Pitch -> PitchClass
pitch_diff per_octave (Pitch oct1 pc1) (Pitch oct2 pc2) = oct_diff + pc1 - pc2
    where oct_diff = per_octave * (oct1 - oct2)


-- * util

-- TODO this is much like LEvent, but with any error, not just logs.

-- | Like 'List.mapAccumL', but pass through lefts.
mapRight :: (state -> a -> (state, b)) -> state -> [Either e a]
    -> (state, [Either e b])
mapRight f = mapRightE (\state -> second Right . f state)

-- | Like 'mapRight', but the function can also contribute Lefts.
mapRightE :: (state -> a -> (state, Either e b)) -> state -> [Either e a]
    -> (state, [Either e b])
mapRightE f = go
    where
    go !state [] = (state, [])
    go !state (ea : as) = case ea of
        Left e -> second (Left e :) (go state as)
        Right a -> second (eb:) (go state2 as)
            where (state2, eb) = f state a
