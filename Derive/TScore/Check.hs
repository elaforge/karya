-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Post-process 'T.Token's.  Check barlines, resolve ties, etc.
module Derive.TScore.Check (
    Error(..), show_error, Config(..), default_config
    , parse_directive, parse_directives, process
    , Pitch(..), PitchClass, Octave
    , Meter(..)
#ifdef TESTING
    , module Derive.TScore.Check
#endif
) where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters
import qualified Cmd.Ruler.Tala as Tala

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import           Global
import           Types


data Error = Error !T.Time !Text
    deriving (Eq, Show)

instance Pretty Error where
    pretty (Error t msg) = pretty t <> ": " <> msg

show_error :: Meter -> Error -> Text
show_error meter (Error t msg) =
    show_time (meter_duration meter) t <> ": " <> msg

data Config = Config {
    -- | If true, notes with no call get the pitch as their call.  This is
    -- a hack so that e.g. "na" is interpreted as a call with no pitch.
    -- Otherwise I'd have to let directives affect parsing.
    config_default_call :: !Bool
    , config_meter :: !Meter
    , config_scale :: !Scale
    , config_duration :: !DurationMode
    } deriving (Eq, Show)

default_config :: Config
default_config = Config
    { config_default_call = False
    , config_meter = meter_44
    , config_scale = scale_sargam
    , config_duration = Multiplicative
    }

parse_directives :: Config -> [T.Directive] -> Either Text Config
parse_directives = foldM (flip parse_directive)

parse_directive :: T.Directive -> Config -> Either Text Config
parse_directive (T.Directive name maybe_val) config = case (name, maybe_val) of
    ("meter", Just val) ->
        set_config (\c a -> c { config_meter = a }) meter_map val
    ("scale", Just val) ->
        set_config (\c a -> c { config_scale = a }) scale_map val
    ("dur", Just val) ->
        set_config (\c a -> c { config_duration = a }) duration_map val
    ("default-call", maybe_val) ->
        set_config (\c a -> c { config_default_call = a }) bool_map
            (fromMaybe "t" maybe_val)
    _ -> Left $ "unknown directive name: " <> name
    where
    bool_map = Map.fromList [("f", False), ("t", True)]
    set_config setter m k = fmap (setter config) (lookup k m)
    lookup k m = maybe (Left $ "unknown directive val: " <> k) Right $
        Map.lookup k m

-- * process

process :: Config -> [T.Token T.Pitch T.Duration]
    -> [Either Error (T.Time, T.Note (Maybe Text) T.Time)]
process (Config default_call meter scale duration) =
    pitch_to_symbolic scale . resolve_pitch scale . resolve_time
    . barlines meter . duration_mode duration
    . (if default_call then pitch_to_call else id)
    -- TODO resolve pitch before time, so the pitches are right, so ties work.
    -- But then I still have TBarline and the like.

-- * time

-- ** meter

data Meter = Meter {
    -- | Rank pattern.
    --
    -- Adi: [2, 0, 0, 0, 1, 0, 0, 1, 0, 0]
    -- > || ssss ; rrrr ; gggg ; mmmm | pppp ; dddd | nnnn ; sssss ||
    -- 4/4: [1, 0, 0, 0]
    -- > | ssss ; rrrr ; gggg ; mmmm |
    meter_pattern :: [T.Rank]
    , meter_step :: !T.Time
    -- | If true, beats fall at the end of measures.
    , meter_negative :: !Bool
    , meter_labeled :: ![Meter.LabeledMark]
    } deriving (Eq, Show)

meter_duration :: Meter -> T.Time
meter_duration m = meter_step m * fromIntegral (length (meter_pattern m))

meter_map :: Map Text Meter
meter_map = Map.fromList
    [ ("adi", meter_adi)
    , ("44", meter_44)
    ]

-- If I do akshara as 1, then kanda is 1/5th notes.  I'd want to reduce the
-- pulse to 1/5, or write .1--.5?
meter_adi :: Meter
meter_adi = Meter
    { meter_pattern = [2, 0, 0, 0, 1, 0, 1, 0]
    , meter_step = 1
    , meter_negative = False
    , meter_labeled = Tala.simple_meter Tala.adi_tala nadai 1 1
    }
    where nadai = 4 -- TODO don't hardcode this

meter_44 :: Meter
meter_44 = Meter
    { meter_pattern = [1, 0, 0, 0]
    , meter_step = 1/4
    , meter_negative = False
    , meter_labeled = make_labeled (1/16) Meters.m44
    }

make_labeled :: TrackTime -> Meter.AbstractMeter -> [Meter.LabeledMark]
make_labeled dur =
    Meter.label_meter Meter.default_config . Meter.make_meter dur . (:[])

-- ** resolve_time

-- | Remove TBarline and TRest, add start times, and resolve ties.
resolve_time :: (Eq pitch, Parse.Element pitch)
    => [Either Error (T.Token pitch (T.Time, Bool))]
    -> [Either Error (T.Time, T.Note pitch T.Time)]
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
            | otherwise -> go ts
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
    => T.Time -> T.Note pitch (T.Time, Bool)
    -> [(T.Time, T.Token pitch (T.Time, Bool))]
    -> Either Error T.Time
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

tied_rests :: [(T.Time, T.Token pitch (T.Time, Bool))] -> Maybe Error
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

-- ** barlines

barlines :: Meter -> [T.Token pitch (T.Time, tie)]
    -> [Either Error (T.Token pitch (T.Time, tie))]
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
    warn :: Int -> T.Time -> Text -> Error
    warn i now msg = Error now $
        "barline check: token " <> showt i <> ": " <> msg

show_time :: T.Time -> T.Time -> Text
show_time cycle_dur t = pretty (cycle :: Int) <> ":" <> pretty beat
    where (cycle, beat) = t `Num.fDivMod` cycle_dur

duration_of :: T.Token pitch (T.Time, tie) -> T.Time
duration_of = \case
    T.TBarline _ -> 0
    T.TNote note -> fst (T.note_duration note)
    T.TRest (T.Rest (dur, _)) -> dur

-- ** resolve duration

duration_map :: Map Text DurationMode
duration_map = Map.fromList
    [ ("mul", Multiplicative)
    , ("add", Additive)
    ]

data DurationMode = Multiplicative | Additive
    deriving (Eq, Show)

duration_mode :: DurationMode -> [T.Token pitch T.Duration]
    -> [T.Token pitch (T.Time, Bool)]
duration_mode = \case
    Multiplicative -> multiplicative
    Additive -> additive

-- | Each number is the inverse of the number of beats, so 2 is 1/2, 8 is 1/8
-- etc.
multiplicative :: [T.Token pitch T.Duration] -> [T.Token pitch (T.Time, Bool)]
multiplicative = run . mapM (T.map_duration convert)
    where
    run = fst . flip State.runState 1
    convert (T.Duration intDur dots tie) = do
        intDur <- carry_duration intDur
        let dur = T.Time (1 / fromIntegral intDur)
        let dotDur = sum $ take dots $ drop 1 $ iterate (/2) dur
        return (dur + dotDur, tie)

-- | Each number is just the number of Time beats.
additive :: [T.Token pitch T.Duration] -> [T.Token pitch (T.Time, Bool)]
additive = run . mapM (T.map_duration convert)
    where
    run = fst . flip State.runState 1
    convert (T.Duration intDur dots tie) = do
        intDur <- carry_duration intDur
        let dur = T.Time (fromIntegral intDur)
        let dotDur = sum $ take dots $ drop 1 $ iterate (/2) dur
        return (dur + dotDur, tie)

carry_duration :: State.MonadState Int m => Maybe Int -> m Int
carry_duration mbDur = do
    int <- maybe State.get return mbDur
    State.put int
    return int

-- * pitch

data Scale = Scale {
    scale_degrees :: Vector.Vector Text
    , scale_per_octave :: !PitchClass
    , scale_initial_octave :: !Octave
    } deriving (Eq, Show)

scale_parse :: Scale -> Text -> Maybe PitchClass
scale_parse scale = (`Vector.elemIndex` scale_degrees scale)

scale_unparse :: Scale -> Pitch -> Maybe Text
scale_unparse scale (Pitch oct pc) = (showt oct <>) <$>
    scale_degrees scale Vector.!? pc

data Pitch = Pitch !Octave !PitchClass
    deriving (Eq, Show)

type PitchClass = Int
type Octave = Int

resolve_pitch :: Scale
    -> [Either Error (T.Time, T.Note T.Pitch dur)]
    -> [Either Error (T.Time, T.Note (Maybe Pitch) dur)]
resolve_pitch scale@(Scale _ per_octave initial_octave) =
    infer_octaves per_octave initial_octave . parse_pitch (scale_parse scale)

parse_pitch :: (Text -> Maybe pitch)
    -> [Either Error (T.Time, T.Note T.Pitch dur)]
    -> [Either Error (T.Time, T.Note (Maybe (T.Octave, pitch)) dur)]
parse_pitch parse = snd . mapRightE token Nothing
    where
    token maybe_prev (start, note)
        | Text.null call = case maybe_prev of
            Nothing ->
                ( maybe_prev
                , with_pitch Nothing
                )
            Just p -> (Just p, with_pitch (Just p))
        | otherwise = case parse call of
            Nothing ->
                ( maybe_prev
                , Left $ Error start $ "can't parse pitch: " <> call
                )
            Just p -> (Just p, with_pitch (Just p))
        where
        T.Pitch oct call = T.note_pitch note
        with_pitch p = Right (start, note { T.note_pitch = (oct,) <$> p })

infer_octaves :: PitchClass -> Octave
    -> [Either e (time, T.Note (Maybe (T.Octave, PitchClass)) dur)]
    -> [Either e (time, T.Note (Maybe Pitch) dur)]
infer_octaves per_octave initial_oct =
    snd . mapRight infer (initial_oct, Nothing)
    where
    infer (prev_oct, prev_pc) (start, note) = case T.note_pitch note of
        Nothing ->
            ((prev_oct, prev_pc), (start, note { T.note_pitch = Nothing }))
        Just (oct, pc) -> with_octave pc $ case oct of
            T.Relative n -> n + case prev_pc of
                Just prev ->
                    min_on3 (distance prev pc)
                        (prev_oct-1) prev_oct (prev_oct+1)
                Nothing -> prev_oct
            T.Absolute o -> o
        where
        with_octave pc o =
            ( (o, Just pc)
            , (start, note { T.note_pitch = Just (Pitch o pc) })
            )
        distance prev pc oct = abs $
            pitch_diff per_octave (Pitch prev_oct prev) (Pitch oct pc)
    min_on3 key a b c = Seq.min_on key a (Seq.min_on key b c)

-- | Convert 'Pitch'es back to symbolic form.
pitch_to_symbolic :: Scale -> [Either Error (T.Time, T.Note (Maybe Pitch) dur)]
    -> [Either Error (T.Time, T.Note (Maybe Text) dur)]
pitch_to_symbolic scale = map to_sym
    where
    to_sym (Left e) = Left e
    to_sym (Right (t, note)) = do
        sym <- case T.note_pitch note of
            Nothing -> return Nothing
            Just pitch -> Just <$> tryJust
                (Error t ("bad pc: " <> showt (T.note_pitch note)))
                (scale_unparse scale pitch)
        return (t, note { T.note_pitch = sym })

-- ** scale

scale_map :: Map Text Scale
scale_map = Map.fromList
    [ ("sargam", scale_sargam)
    , ("ioeau", scale_ioeua)
    ]

scale_sargam :: Scale
scale_sargam = Scale
    { scale_degrees = degrees
    , scale_per_octave = 8
    , scale_initial_octave = 4
    } where degrees = Vector.fromList $ map Text.singleton "srgmpdn"

scale_ioeua :: Scale
scale_ioeua = Scale
    { scale_degrees = degrees
    , scale_per_octave = 5
    , scale_initial_octave = 4
    } where degrees = Vector.fromList $ map Text.singleton "ioeua"

pitch_diff :: PitchClass -> Pitch -> Pitch -> PitchClass
pitch_diff per_octave (Pitch oct1 pc1) (Pitch oct2 pc2) = oct_diff + pc1 - pc2
    where oct_diff = per_octave * (oct1 - oct2)


-- * parsing

pitch_to_call :: [T.Token T.Pitch T.Duration] -> [T.Token T.Pitch T.Duration]
pitch_to_call = Identity.runIdentity . mapM (T.map_note (return . to_call))
    where
    to_call note
        | T.note_call note == T.Call "" = note
            { T.note_call = T.Call $ Parse.unparse (T.note_pitch note)
            , T.note_pitch = Parse.empty_pitch
            }
        | otherwise = note


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
