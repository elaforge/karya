-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Post-process 'T.Token's.  Check barlines, resolve ties, etc.
module Derive.TScore.Check (
    Config(..), default_config
    , parse_directive, parse_directives
    , preprocess, process
    , call_block_id
    , Meter(..)
#ifdef TESTING
    , module Derive.TScore.Check
#endif
) where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Void as Void

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Meters as Meters
import qualified Cmd.Ruler.Tala as Tala

import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Scale.Theory as Theory
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import qualified Perform.Pitch as Pitch
import qualified Ui.Id as Id

import           Global
import           Types


data Config = Config {
    -- | If true, notes with no call get the pitch as their call.  This is
    -- a hack so that e.g. "na" is interpreted as a call with no pitch.
    -- Otherwise I'd have to let directives affect parsing.
    config_default_call :: !Bool
    , config_meter :: !Meter
    , config_scale :: !Scale
    , config_duration :: !DurationMode
    } deriving (Show)

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

type Stream a = [Either T.Error a]
type Token call pitch dur = T.Token call pitch dur dur

type GetCallDuration = Text -> (Either Text T.Time, [Log.Msg])

-- | This goes before the recursion check, because it handles %default-call.
-- The recursion check depends on that because it looks for block calls.
preprocess :: Config -> [T.Token T.CallT T.Pitch T.NDuration T.Duration]
    -> [T.Token T.CallT T.Pitch T.NDuration T.Duration]
preprocess config
    | config_default_call config = pitch_to_call
    | otherwise = id

process :: GetCallDuration -> Config
    -> [T.Token T.CallT T.Pitch T.NDuration T.Duration]
    -> Stream (T.Time, T.Note T.CallT (Maybe Text) T.Time)
process get_dur (Config _default_call meter scale duration) =
    resolve_pitch scale
    . resolve_time
    . check_barlines meter
    . duration_mode duration
    . resolve_call_duration get_dur
    . carry_call_duration
    -- TODO resolve pitch before time, so the pitches are right, so ties work.
    -- But then I still have TBarline and the like.

-- * time

-- | Carry CallDuration if the next note has no duration, but does have a call.
carry_call_duration :: [T.Token T.CallT T.Pitch T.NDuration T.Duration]
    -> [T.Token T.CallT T.Pitch T.NDuration T.Duration]
carry_call_duration = flip State.evalState False . mapM (T.map_note carry)
    where
    carry note = do
        dur <- case T.note_duration note of
            T.CallDuration -> do
                State.put True
                return T.CallDuration
            T.NDuration dur
                | can_carry note dur -> ifM State.get
                    (State.put True >> return T.CallDuration)
                    (return $ T.NDuration dur)
                | otherwise -> State.put False >> return (T.NDuration dur)
        return $ note { T.note_duration = dur }
    can_carry note dur = T.note_call note /= ""
        && all Maybe.isNothing [T.dur_int1 dur, T.dur_int2 dur]

resolve_call_duration :: GetCallDuration
    -> [T.Token T.CallT T.Pitch T.NDuration rdur]
    -> Stream (T.Token T.CallT T.Pitch (Either T.Time T.Duration) rdur)
resolve_call_duration get_dur = concatMap $ \case
    T.TBarline pos a -> [Right $ T.TBarline pos a]
    T.TRest pos a -> [Right $ T.TRest pos a]
    T.TNote pos note ->
        map (second set) (resolve pos (T.note_call note) (T.note_duration note))
        where set dur = T.TNote pos $ note { T.note_duration = dur }
    where
    resolve _ _ (T.NDuration dur) = [Right $ Right dur]
    resolve pos call T.CallDuration
        | Text.null call =
            [Left $ T.Error pos "can't get call duration of empty call"]
        | otherwise = case get_dur call of
            (Left err, logs) -> Left (T.Error pos err) : map to_error logs
            (Right time, logs) -> Right (Left time) : map to_error logs
        where to_error = Left . T.Error pos . Log.format_msg

call_block_id :: Id.BlockId -> Text -> Maybe Id.BlockId
call_block_id parent =
    Eval.call_to_block_id Parse.default_namespace (Just parent) . Expr.Symbol

-- ** meter

data Meter = Meter {
    -- | Rank pattern.
    --
    -- Adi: [2, 0, 0, 0, 1, 0, 0, 1, 0, 0]
    -- > || ssss ; rrrr ; gggg ; mmmm | pppp ; dddd | nnnn ; sssss ||
    -- 4/4: [1, 0, 0, 0]
    -- > | ssss ; rrrr ; gggg ; mmmm |
    meter_pattern :: [T.Rank]
    -- | This is the duration one one element of 'meter_pattern'.
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
    => Stream (Token call pitch (T.Time, Bool))
    -> Stream (T.Time, T.Note call pitch T.Time)
resolve_time tokens = go . zip starts $ tokens
    where
    starts = scanl (\n -> (n+) . either (const 0) duration_of) 0 tokens
    go ((start, Right t) : ts) = case t of
        T.TNote _ note
            | is_tied t -> case tied_notes note (sndRights pre) of
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
    => T.Note call pitch dur -> [(T.Time, Token call pitch (T.Time, Bool))]
    -> Either T.Error T.Time
tied_notes note tied = case others of
    [] -> case Seq.last matches of
        Nothing -> Left $ T.Error (T.note_pos note) "final note has a tie"
        Just (s, n)
            | snd $ T.note_duration n ->
                Left $ T.Error (T.note_pos n) "final note has a tie"
            | otherwise -> Right $ s + dur_of n
    (_, bad) : _ -> Left $ T.Error (T.token_pos bad) $ case bad of
        T.TNote _ n -> "note tied to different pitch: "
            <> Parse.unparse (T.note_pitch note) <> " ~ "
            <> Parse.unparse (T.note_pitch n)
        _ -> "note tied to " <> T.token_name bad
    where
    (matches, others) = first concat $ Seq.partition_on match tied
    dur_of = fst . T.note_duration
    match (s, T.TNote _ n) | T.note_pitch note == T.note_pitch n = Just [(s, n)]
    match (_, T.TBarline {}) = Just []
    match _ = Nothing

tied_rests :: [(time, Token call pitch (T.Time, Bool))] -> Maybe T.Error
tied_rests = fmap format . List.find (not . matches . snd)
    where
    format (_, token) =
        T.Error (T.token_pos token) $ "rest tied to " <> T.token_name token
    matches (T.TRest {}) = True
    matches (T.TBarline {}) = True
    matches _ = False

is_tied :: T.Token call pitch (a1, Bool) (a2, Bool) -> Bool
is_tied (T.TNote _ note) = snd $ T.note_duration note
is_tied (T.TRest _ (T.Rest (_, tied))) = tied
is_tied _ = False

-- ** check_barlines

check_barlines :: Meter -> Stream (Token call pitch (T.Time, tie))
    -> Stream (Token call pitch (T.Time, tie))
check_barlines meter =
    fst . flip State.runState 0 . concat_rmap_e check_token . zip_right [0..]
    where
    check_token (i, token) = do
        now <- State.get
        State.put (now + dur)
        return $ Right token : warning now
        where
        dur = duration_of token
        warning now = case token of
            T.TBarline pos bar -> maybe [] ((:[]) . Left) (check pos now i bar)
            _ -> []
    check pos now i (T.Barline rank) = case Map.lookup beat expected_rank of
        Just r
            | r == rank -> Nothing
            | otherwise -> Just $ T.Error pos $ warn i $
                "saw " <> Parse.unparse (T.Barline rank)
                <> ", expected " <> Parse.unparse (T.Barline r)
        Nothing -> Just $ T.Error pos $ warn i $
            "saw " <> Parse.unparse (T.Barline rank) <> ", expected none"
        where
        beat = now `Num.fmod` cycle_dur
    cycle_dur = meter_duration meter
    expected_rank = Map.fromList $ zip (Seq.range_ 0 (meter_step meter))
        (meter_pattern meter)
    warn i msg = "barline check: token " <> showt i <> ": " <> msg

show_time :: T.Time -> T.Time -> Text
show_time cycle_dur t = pretty (cycle :: Int) <> ":" <> pretty beat
    where (cycle, beat) = t `Num.fDivMod` cycle_dur

duration_of :: Token call pitch (T.Time, tie) -> T.Time
duration_of = \case
    T.TBarline {} -> 0
    T.TNote _ note -> fst (T.note_duration note)
    T.TRest _ (T.Rest (dur, _)) -> dur

-- ** resolve duration

duration_map :: Map Text DurationMode
duration_map = Map.fromList
    [ ("mul", Multiplicative)
    , ("add", Additive)
    ]

data DurationMode = Multiplicative | Additive
    deriving (Eq, Show)

duration_mode :: DurationMode
    -> Stream (T.Token call pitch (Either T.Time T.Duration) T.Duration)
    -> Stream (Token call pitch (T.Time, Bool))
duration_mode = \case
    Multiplicative -> multiplicative
    Additive -> additive

-- | Each number is the inverse of the number of beats, so 2 is 1/2, 8 is 1/8
-- etc.  If there are two numbers, you can set both numerator and denominator.
-- This carrier both values or none.
multiplicative
    :: Stream (T.Token call pitch (Either T.Time T.Duration) T.Duration)
    -> Stream (Token call pitch (T.Time, Bool))
multiplicative =
    flip State.evalState (1, Nothing) . rmap_e (map_duration carry time_of)
    where
    time_of int1 mb_int2 dots = dur + dot_dur
        where
        dur = T.Time $ case mb_int2 of
            Nothing -> 1 / fromIntegral int1
            Just int2 -> fromIntegral int1 / fromIntegral int2
        dot_dur = sum $ take dots $ drop 1 $ iterate (/2) dur
    carry Nothing Nothing = State.get
    carry Nothing (Just int2) = return (int2, Nothing)
    carry (Just int1) mb_int2 = return (int1, mb_int2)

-- | Each number is just the number of Time beats.  If there are two numbers,
-- int1 corresponds to a higher level of division, which is meter specific.
-- E.g. akshara:matra.
additive :: Stream (T.Token call pitch (Either T.Time T.Duration) T.Duration)
    -> Stream (Token call pitch (T.Time, Bool))
additive = flip State.evalState (1, 4) . rmap_e (map_duration carry time_of)
    where
    time_of int1 int2 dots = dur + dot_dur
        where
        dur = fromIntegral int1 / fromIntegral int2
        dot_dur = sum $ take dots $ drop 1 $ iterate (/2) dur
    carry int1 int2 = do
        (p_int1, p_int2) <- State.get
        return (fromMaybe p_int1 int1, fromMaybe p_int2 int2)

map_duration :: State.MonadState (int1, int2) m
    => (Maybe Int -> Maybe Int -> m (int1, int2))
    -> (int1 -> int2 -> Int -> time)
    -> T.Token call pitch (Either time T.Duration) T.Duration
    -> m (Either T.Error (T.Token call pitch (time, Bool) (time, Bool)))
map_duration carry time_of = \case
    T.TBarline pos a -> return $ Right $ T.TBarline pos a
    T.TNote pos note -> do
        result <- case T.note_duration note of
            Left time -> return $ Right (time, False)
            Right (T.Duration mb_int1 mb_int2 dots tie) -> do
                (int1, int2) <- carry mb_int1 mb_int2
                State.put (int1, int2)
                return $ Right (time_of int1 int2 dots, tie)
        return $ case result of
            Left err -> Left $ T.Error pos err
            Right (time, tie) -> Right $ T.TNote pos $
                note { T.note_duration = (time, tie) }
    T.TRest pos (T.Rest (T.Duration mb_int1 mb_int2 dots tie)) -> do
        (int1, int2) <- carry mb_int1 mb_int2
        State.put (int1, int2)
        return $ Right $ T.TRest pos $ T.Rest (time_of int1 int2 dots, tie)

-- * pitch

data Scale = Scale {
    scale_name :: !Text
    , scale_parse :: Text -> Maybe Pitch.Degree
    , scale_unparse :: Pitch.Degree -> Maybe Text
    , scale_layout :: !Theory.Layout
    , scale_initial_octave :: !Pitch.Octave
    }

instance Show Scale where
    show scale = "((" <> untxt (scale_name scale) <> "))"

resolve_pitch :: Scale
    -> Stream (T.Time, T.Note call T.Pitch dur)
    -> Stream (T.Time, T.Note call (Maybe Text) dur)
resolve_pitch scale =
    pitch_to_symbolic scale
    . infer_octaves per_octave (scale_initial_octave scale)
    . parse_pitches (scale_parse scale)
    where
    per_octave = Theory.layout_pc_per_octave (scale_layout scale)

parse_pitches :: (Text -> Maybe pitch)
    -> Stream (T.Time, T.Note call T.Pitch dur)
    -> Stream (T.Time, T.Note call (Maybe (T.Octave, pitch)) dur)
parse_pitches parse = fst . flip State.runState Nothing . rmap_e token
    where
    token (start, note)
        | Text.null call = with_pitch =<< State.get
        | otherwise = case parse call of
            Nothing -> return $ Left $ T.Error (T.note_pos note) $
                "can't parse pitch: " <> call
            Just p -> State.put (Just p) >> with_pitch (Just p)
        where
        T.Pitch oct call = T.note_pitch note
        with_pitch p =
            return $ Right (start, note { T.note_pitch = (oct,) <$> p })

infer_octaves :: Pitch.PitchClass -> Pitch.Octave
    -> [Either e (time, T.Note call (Maybe (T.Octave, Pitch.Degree)) dur)]
    -> [Either e (time, T.Note call (Maybe Pitch.Pitch) dur)]
infer_octaves per_octave initial_oct =
    fst . flip State.runState (initial_oct, Nothing) . rmap infer
    where
    infer (start, note) = case T.note_pitch note of
        Nothing -> return (start, note { T.note_pitch = Nothing })
        Just (oct, degree) -> do
            (prev_oct, prev_degree) <- State.get
            oct <- return $ case oct of
                T.Relative n -> n + case prev_degree of
                    Just prev -> min_on3 (distance prev_oct prev degree)
                        (prev_oct-1) prev_oct (prev_oct+1)
                    Nothing -> prev_oct
                T.Absolute oct -> oct
            State.put (oct, Just degree)
            return
                (start, note { T.note_pitch = Just (Pitch.Pitch oct degree) })
        where
        distance prev_oct prev degree oct = abs $
            Pitch.diff_pc per_octave (Pitch.Pitch prev_oct prev)
                (Pitch.Pitch oct degree)

-- | Convert 'Pitch'es back to symbolic form.
pitch_to_symbolic :: Scale
    -> Stream (T.Time, T.Note call (Maybe Pitch.Pitch) dur)
    -> Stream (T.Time, T.Note call (Maybe Text) dur)
pitch_to_symbolic scale = map to_sym
    where
    to_sym (Left e) = Left e
    to_sym (Right (t, note)) = do
        sym <- case T.note_pitch note of
            Nothing -> return Nothing
            Just pitch -> Just <$> tryJust
                (T.Error (T.note_pos note)
                    ("bad pitch: " <> pretty (T.note_pitch note)))
                (unparse pitch)
        return (t, note { T.note_pitch = sym })
    unparse (Pitch.Pitch oct degree) =
        (showt oct <>) <$> scale_unparse scale degree

-- ** scale

scale_map :: Map Text Scale
scale_map = Map.fromList $ Seq.key_on scale_name
    [ scale_sargam
    , scale_bali
    , scale_twelve
    ]

diatonic_scale :: Text -> [Text] -> Scale
diatonic_scale name degrees_ = Scale
    { scale_name = name
    , scale_parse = \s -> Pitch.Degree <$> Vector.elemIndex s degrees <*> pure 0
    , scale_unparse = unparse
    , scale_layout = Theory.diatonic_layout (Vector.length degrees)
    , scale_initial_octave = 4
    }
    where
    unparse (Pitch.Degree pc accs)
        | accs == 0 = degrees Vector.!? pc
        | otherwise = Nothing
    degrees = Vector.fromList degrees_

scale_sargam :: Scale
scale_sargam = diatonic_scale "sargam" $ map Text.singleton "srgmpdn"

scale_bali :: Scale
scale_bali = diatonic_scale "bali" $ map Text.singleton "ioeua"

scale_twelve :: Scale
scale_twelve = Scale
    { scale_name = "twelve"
    , scale_parse = P.parseMaybe p_degree
    , scale_unparse = unparse
    , scale_layout = Theory.piano_layout
    , scale_initial_octave = 4
    }
    where
    p_degree :: Parser Pitch.Degree
    p_degree = do
        pc <- P.choice [P.string c *> pure i | (i, c) <- zip [0..] degrees]
        accs <- P.choice $ map (\(n, c) -> P.string c *> pure n) accidentals
        return $ Pitch.Degree pc accs
    unparse (Pitch.Degree pc accs) = (<>)
        <$> Seq.at degrees pc <*> lookup accs accidentals
    accidentals =
        [ (0, ""), (0, "n")
        , (1, "#"), (2, "x")
        , (-1, "b"), (-2, "bb")
        ]
    degrees = map Text.singleton "cdefgab"


type Parser a = P.Parsec Void.Void Text a


-- * parsing

pitch_to_call :: [T.Token T.CallT T.Pitch T.NDuration T.Duration]
    -> [T.Token T.CallT T.Pitch T.NDuration T.Duration]
pitch_to_call =
    Identity.runIdentity . mapM (T.map_note (return . to_call))
    where
    to_call note
        | T.note_call note == "" = note
            { T.note_call = Parse.unparse (T.note_pitch note)
            , T.note_pitch = Parse.empty_pitch
            }
        | otherwise = note


-- * util

-- | Like mapM, but pass through Lefts.
rmap :: Monad m => (a -> m b) -> [Either e a] -> m [Either e b]
rmap f = rmap_e (fmap Right . f)

-- | Like 'rmap', except the function can also return Lefts.
rmap_e :: Monad m => (a -> m (Either e b)) -> [Either e a] -> m [Either e b]
rmap_e f = concat_rmap_e (fmap (:[]) . f)

-- | Like 'rmap_e', except the function can return a list.
concat_rmap_e :: Monad m => (a -> m [Either e b])
    -> [Either e a] -> m [Either e b]
concat_rmap_e f = go
    where
    go [] = return []
    go (ea : eas) = case ea of
        Left e -> (Left e :) <$> go eas
        Right a -> do
            eb <- f a
            (eb++) <$> go eas

-- | This is the Either equivalent of 'Derive.LEvent.zip'.
zip_right :: [b] -> [Either a c] -> [Either a (b, c)]
zip_right (b:bs) (Right c : acs) = Right (b, c) : zip_right bs acs
zip_right bs (Left a : acs) = Left a : zip_right bs acs
zip_right [] _ = []
zip_right _ [] = []

min_on3 :: Ord k => (a -> k) -> a -> a -> a -> a
min_on3 key a b c = Seq.min_on key a (Seq.min_on key b c)
