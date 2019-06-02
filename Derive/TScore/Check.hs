-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Post-process 'T.Token's.  Check barlines, resolve ties, etc.
module Derive.TScore.Check (
    Config(..), default_config
    , Scope(..)
    , From(..)
    , AssertCoincident(..)
    , parse_directive, parse_directives
    , apply_block_from
    , check
    , call_block_id
    , Meter(..)
#ifdef TESTING
    , module Derive.TScore.Check
#endif
) where
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector
import qualified Data.Void as Void

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import qualified Util.EList as EList
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
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
    config_meter :: !Meter
    , config_scale :: !Scale
    , config_duration :: !DurationMode
    , config_from :: !(Maybe From)
    } deriving (Show)

-- | The target of a 'T.CopyFrom'.
data From = From {
    from_block :: !(Maybe Id.BlockId)
    , from_tracknum :: !TrackNum
    , from_pos :: !T.Pos
    } deriving (Show)

default_config :: Config
default_config = Config
    { config_meter = meter_44
    , config_scale = scale_sargam
    , config_duration = Multiplicative
    , config_from = Nothing
    }

parse_directives :: Scope -> Config -> [T.Directive] -> Either T.Error Config
parse_directives scope = foldM (flip (parse_directive scope))

-- | Directives can appear in various places, which affects their scope.
data Scope = Global | Block | Track
    deriving (Eq, Show)

parse_directive :: Scope -> T.Directive -> Config -> Either T.Error Config
parse_directive scope (T.Directive pos name maybe_val) config =
    first (T.Error pos . ((name<>": ")<>)) $ case name of
        "meter" -> set_config (\c a -> c { config_meter = a }) parse_meter
            =<< with_arg
        "scale" -> set_config (\c a -> c { config_scale = a })
            (`Map.lookup` scale_map) =<< with_arg
        "dur" -> set_config (\c a -> c { config_duration = a })
            (`Map.lookup` duration_map) =<< with_arg
        -- f for from.  This is abbreviated because it shows up per-track.
        "f" -> do
            from <- parse_from scope pos =<< with_arg
            return $ config { config_from = Just from }
        _ | name == Parse.default_call -> without_arg >> Right config
        _ -> Left $ "unknown directive: " <> name
    where
    with_arg = tryJust "expected arg" maybe_val
    without_arg = maybe (Right ()) (Left . ("unexpected arg: "<>)) maybe_val
    set_config setter parse val = fmap (setter config) (lookup parse val)
    lookup parse val = tryJust ("unknown " <> name <> ": " <> val) (parse val)

parse_from :: Scope -> T.Pos -> Text -> Either Text From
parse_from scope pos arg = case scope of
    Global -> Left "can't use at global scope"
    Block -> do
        block_id <- tryJust ("not a valid block_id: " <> arg) $
            Id.make $ Id.read_short Parse.default_namespace arg
        return $ From
            { from_block = Just block_id
            , from_tracknum = 0
            , from_pos = pos
            }
    Track -> do
        tracknum <- tryJust ("not a tracknum: " <> arg) $ ParseText.int arg
        when (tracknum <= 0) $ Left "tracknums start at 1"
        return $ From
            { from_block = Nothing
            , from_tracknum = tracknum
            , from_pos = pos
            }

-- | Convert a block-level From into a track-level From for each tracknum.
-- Due to 'parse_from', the block From should have a 'from_block'.  It's ok
-- to have both block and track From, in this case the track From gives the
-- tracknum from a different block.
apply_block_from :: Config -> TrackNum -> Config -> Config
apply_block_from block_config tracknum track_config =
    set $ case (config_from block_config, config_from track_config) of
        (Nothing, mb_track) -> mb_track
        (Just block, Nothing) -> Just $ block { from_tracknum = tracknum }
        -- If the block had a From, the track will have inherited it.
        (Just block, Just track) -> Just $ track
            { from_block = from_block block
            , from_tracknum = if from_tracknum track /= 0
                then from_tracknum track else tracknum
            }
    where set from = track_config { config_from = from }

-- * check

type Stream a = [EList.Elt Meta a]
type Token call pitch dur = T.Token call pitch dur dur
type Meta = Either T.Error AssertCoincident

type NPitch = T.NPitch T.Pitch

type GetCallDuration = Text -> (Either Text T.Time, [Log.Msg])

mkerror :: T.Pos -> Text -> EList.Elt Meta a
mkerror pos msg = EList.Meta $ Left $ T.Error pos msg

data AssertCoincident = AssertCoincident !T.Time !T.Pos
    deriving (Eq, Show)

check :: GetCallDuration -> Config
    -> [T.Token T.CallText NPitch T.NDuration T.Duration]
    -> ( [Either Meta
            (T.Time, T.Note T.CallText (T.NPitch (Maybe T.PitchText)) T.Time)]
       , T.Time
       )
check get_dur (Config meter scale duration _) =
    Tuple.swap
    . fmap (map EList.toEither)
    . fmap (resolve_pitch scale)
    . resolve_time
    . check_barlines meter
    . duration_mode duration
    . resolve_call_duration get_dur
    . carry_call_duration
    . resolve_repeats
    . map EList.Elt

-- * time

-- | Carry CallDuration if the next note has no duration, but does have a call.
carry_call_duration
    :: Stream (T.Token T.CallText pitch T.NDuration T.Duration)
    -> Stream (T.Token T.CallText pitch T.NDuration T.Duration)
carry_call_duration = flip State.evalState False . EList.mapM (T.map_note carry)
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
    -> Stream (T.Token T.CallText pitch T.NDuration rdur)
    -> Stream (T.Token T.CallText pitch (Either T.Time T.Duration) rdur)
resolve_call_duration get_dur = EList.concatMapE $ \case
    T.TBarline pos a -> [EList.Elt $ T.TBarline pos a]
    T.TRest pos a -> [EList.Elt $ T.TRest pos a]
    T.TNote pos note ->
        map (fmap set) (resolve pos (T.note_call note) (T.note_duration note))
        where set dur = T.TNote pos $ note { T.note_duration = dur }
    where
    resolve _ _ (T.NDuration dur) = [EList.Elt $ Right dur]
    resolve pos call T.CallDuration
        | Text.null call =
            [mkerror pos "can't get call duration of empty call"]
        | otherwise = case get_dur call of
            (Left err, logs) -> mkerror pos err : map to_error logs
            (Right time, logs) -> EList.Elt (Left time) : map to_error logs
        where to_error = mkerror pos . Log.format_msg

call_block_id :: Id.BlockId -> Text -> Maybe Id.BlockId
call_block_id parent =
    Eval.call_to_block_id Parse.default_namespace (Just parent) . Expr.Symbol

resolve_repeats :: Stream (T.Token T.CallText NPitch T.NDuration T.Duration)
    -> Stream (T.Token T.CallText NPitch T.NDuration T.Duration)
resolve_repeats =
    snd . EList.mapAccumLE resolve_dot Nothing
        . map (fmap resolve_tie) . zip_next_note
    where
    resolve_tie (T.TNote pos note, Just next)
        | strip next == Parse.tie_note = T.TNote pos $ set_tie True note
    resolve_tie (T.TNote pos note, _)
        -- Normalize Parse.tie_note to a Parse.dot_note, since the tie part has
        -- been handled in the previous equation.  It's important to do this
        -- here, so I only normalize "original" 'Parse.tie_note's, not tied
        -- notes created by the previous equation, which is what happens with
        -- two 'Parse.tie_note's in a row.
        | strip note == Parse.tie_note = T.TNote pos Parse.dot_note
    resolve_tie (token, _) = token
    -- x ~ .    (tie, no, no)
    -- x . ~    (no, tie, no)
    -- x ~ ~    (tie, tie, no)
    -- x~ . .   (tie, no, no)
    -- So don't inherit the tie from the prev note, get it from the current
    -- note.
    resolve_dot mb_prev token@(T.TNote pos note)
        | strip note `elem` repeat_notes = case mb_prev of
            Just prev ->
                ( mb_prev
                , EList.Elt $ T.TNote pos (set_tie tied (set_dots 0 prev))
                )
                where tied = strip note `elem` [tie_dot_note, Parse.tie_note]
            Nothing ->
                ( mb_prev
                , mkerror pos "repeat with no previous note"
                )

        | otherwise = (Just note, EList.Elt token)
    resolve_dot mb_prev token = (mb_prev, EList.Elt token)

    strip note = note { T.note_pos = T.Pos 0 }
    repeat_notes = [Parse.dot_note, Parse.tie_note, tie_dot_note]
    tie_dot_note = set_tie True Parse.dot_note
    set_tie tie = modify_duration $ \dur -> dur { T.dur_tie = tie }
    set_dots n = modify_duration $ \dur -> dur { T.dur_dots = n }

zip_next_note :: Stream (T.Token call pitch ndur rdur)
    -> Stream (T.Token call pitch ndur rdur, Maybe (T.Note call pitch ndur))
zip_next_note = EList.map (second (msum . map note_of)) . EList.zipNexts
    where
    note_of (T.TNote _ n) = Just n
    note_of _ = Nothing

modify_duration :: (T.Duration -> T.Duration) -> T.Note call pitch T.NDuration
    -> T.Note call pitch T.NDuration
modify_duration modify note =
    note { T.note_duration = set (T.note_duration note) }
    where
    set (T.NDuration dur) = T.NDuration (modify dur)
    set T.CallDuration = T.CallDuration

-- ** meter

data Meter = Meter {
    -- | Rank pattern.
    --
    -- Adi: [2, 0, 0, 0, 1, 0, 0, 1, 0, 0]
    -- > || ssss rrrr gggg mmmm | pppp dddd | nnnn sssss ||
    -- 4/4: [1, 0, 0, 0]
    -- > | ssss rrrr gggg mmmm |
    meter_pattern :: [T.Rank]
    -- | This is the duration one one element of 'meter_pattern'.
    , meter_step :: !T.Time
    -- | If true, beats fall at the end of measures.
    , meter_negative :: !Bool
    , meter_labeled :: ![Meter.LabeledMark]
    } deriving (Eq, Show)

meter_duration :: Meter -> T.Time
meter_duration m = meter_step m * fromIntegral (length (meter_pattern m))

parse_meter :: Text -> Maybe Meter
parse_meter name = case untxt name of
    [n1, n2] | Just n1 <- Num.readDigit n1, Just n2 <- Num.readDigit n2 ->
        simple_meter n1 n2
    'a':'d':'i' : n
        | Just n <- ParseText.nat (txt n) -> meter_adi n
        | null n -> meter_adi 4
    _ -> Nothing

-- If I do akshara as 1, then kanda is 1/5th notes.  I'd want to reduce the
-- pulse to 1/5, or write .1--.5?
meter_adi :: Int -> Maybe Meter
meter_adi nadai
    | 2 <= nadai && nadai <= 9 = Just $ Meter
        { meter_pattern = [2, 0, 0, 0, 1, 0, 1, 0]
        , meter_step = 1
        , meter_negative = False
        , meter_labeled = Tala.simple_meter Tala.adi_tala nadai 1 1
        }
    | otherwise = Nothing

meter_44 :: Meter
Just meter_44 = simple_meter 4 4

simple_meter :: Int -> Int -> Maybe Meter
simple_meter n1 n2 = do
    labeled <- Meters.simple n1 n2
    return $ Meter
        { meter_pattern = 1 : replicate (n1-1) 0
        , meter_step = 1 / fromIntegral n2
        , meter_negative = False
        -- TODO I picked 1/16 just because it works out.  But it's probably
        -- wrong for everything other than 4/4.
        , meter_labeled = make_labeled (1/16) labeled
        }

make_labeled :: TrackTime -> Meter.AbstractMeter -> [Meter.LabeledMark]
make_labeled dur =
    Meter.label_meter Meter.default_config . Meter.make_meter dur . (:[])

-- ** resolve_time

-- | Remove TBarline and TRest, add start times, and resolve ties.
resolve_time :: (Eq pitch, Pretty pitch)
    => Stream (Token call pitch (T.Time, Bool))
    -> (T.Time, Stream (T.Time, T.Note call pitch T.Time))
resolve_time tokens = go (EList.zipPaddedSnd starts tokens)
    where
    starts = scanl (\n -> (n+)) 0 (map duration_of (EList.elts tokens))
    go (EList.Elt (start, Nothing) : ts)
        | null ts = (start, [])
        | otherwise = go ts
    go (EList.Elt (start, Just t) : ts) = case t of
        T.TNote _ note
            | is_tied t ->
                case tied_notes note (Seq.map_maybe_snd id (EList.elts pre)) of
                    Left err -> (EList.Meta (Left err) :) <$> go post
                    Right end ->
                        (EList.Elt (start, set_dur (end-start) note) :) <$>
                            go post
            | otherwise ->
                (EList.Elt (start, set_dur (fst (T.note_duration note)) note) :)
                    <$> go ts
        T.TBarline _ (T.Barline {}) -> go ts
        T.TBarline pos T.AssertCoincident ->
            (EList.Meta (Right (AssertCoincident start pos)) :) <$> go ts
        T.TRest {}
            | is_tied t -> case tied_rests (mapMaybe snd (EList.elts pre)) of
                Just err -> (EList.Meta (Left err) :) <$> go post
                Nothing -> go post
            | otherwise -> go ts
        where
        (pre, post) = Then.span any_tied (splitAt 1) ts
        any_tied (EList.Meta _) = True
        any_tied (EList.Elt (_, Just n)) = is_barline n || is_tied n
        any_tied (EList.Elt (_, Nothing)) = False
    go (EList.Meta e : ts) = (EList.Meta e :) <$> go ts
    go [] = (0, [])
    set_dur dur note = note { T.note_duration = dur }
    is_barline (T.TBarline {}) = True
    is_barline _ = False

tied_notes :: (Eq pitch, Pretty pitch)
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
            <> pretty (T.note_pitch note) <> " ~ "
            <> pretty (T.note_pitch n)
        _ -> "note tied to " <> T.token_name bad
    where
    (matches, others) = first concat $ Seq.partition_on match tied
    dur_of = fst . T.note_duration
    match (s, T.TNote _ n) | T.note_pitch note == T.note_pitch n = Just [(s, n)]
    match (_, T.TBarline {}) = Just []
    match _ = Nothing

tied_rests :: [Token call pitch (T.Time, Bool)] -> Maybe T.Error
tied_rests = fmap format . List.find (not . matches)
    where
    format token =
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
    fst . flip State.runState 0 . EList.concatMapEM check_token
        . EList.zip [0..]
    where
    check_token (i, token) = do
        now <- State.get
        State.put $! now + dur
        return $ EList.Elt token : warning now
        where
        dur = duration_of token
        warning now = case token of
            T.TBarline pos bar -> maybe [] (:[]) (check pos now i bar)
            _ -> []
    check pos now i (T.Barline rank) = case check_beat beat_rank beat rank of
        Nothing -> Nothing
        Just err -> Just $ mkerror pos $ warn i $
            "beat " <> pretty beat <> ": " <> err
        where beat = now `Num.fmod` cycle_dur
    -- resolve_time will turn this into 'AssertCoincident'.
    check _ _ _ T.AssertCoincident = Nothing
    cycle_dur = meter_duration meter
    beat_rank = Map.fromList $ filter ((>0) . snd) $
        zip (Seq.range_ 0 (meter_step meter))
            -- Two bars, to ensure 'check_beat' can always find the next beat.
            (meter_pattern meter ++ meter_pattern meter)
    warn i msg = "barline check: token " <> showt i <> ": " <> msg

-- | Nothing if the expected rank falls on the given beat, or an error msg.
check_beat :: Map T.Time T.Rank -> T.Time -> T.Rank -> Maybe Text
check_beat beat_rank beat rank = (prefix<>) <$> case at of
    Nothing -> Just next_beat
    Just r
        | r == rank -> Nothing
        | otherwise -> Just $ "expected " <> pretty (T.Barline r)
            <> ", " <> next_beat
    where
    prefix = "saw " <> pretty (T.Barline rank) <> ", "
    next_beat = case List.find ((==rank) . snd) (Map.toList post) of
        Nothing -> "no following beat with that rank"
        Just (t, _) -> "next beat of that rank is " <> pretty t
            <> " (short " <> pretty (t-beat) <> ")"
    (_, at, post) = Map.splitLookup beat beat_rank

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
    flip State.evalState (1, Nothing) . EList.mapEM (map_duration carry time_of)
    where
    time_of int1 mb_int2 dots = dur + dot_dur
        where
        dur = T.Time $ case mb_int2 of
            Nothing -> 1 / fromIntegral int1
            Just int2 -> fromIntegral int1 / fromIntegral int2
        dot_dur = Num.sum $ take dots $ drop 1 $ iterate (/2) dur
    carry Nothing Nothing = State.get
    carry Nothing (Just int2) = return (int2, Nothing)
    carry (Just int1) mb_int2 = return (int1, mb_int2)

-- | Each number is just the number of Time beats.  If there are two numbers,
-- int1 corresponds to a higher level of division, which is meter specific.
-- E.g. akshara:matra.
additive :: Stream (T.Token call pitch (Either T.Time T.Duration) T.Duration)
    -> Stream (Token call pitch (T.Time, Bool))
additive =
    flip State.evalState (1, 4) . EList.mapEM (map_duration carry time_of)
    where
    time_of int1 int2 dots = dur + dot_dur
        where
        dur = fromIntegral int1 / fromIntegral int2
        dot_dur = Num.sum $ take dots $ drop 1 $ iterate (/2) dur
    carry int1 int2 = do
        (p_int1, p_int2) <- State.get
        return (fromMaybe p_int1 int1, fromMaybe p_int2 int2)

map_duration :: State.MonadState (int1, int2) m
    => (Maybe Int -> Maybe Int -> m (int1, int2))
    -> (int1 -> int2 -> Int -> time)
    -> T.Token call pitch (Either time T.Duration) T.Duration
    -> m (EList.Elt Meta (T.Token call pitch (time, Bool) (time, Bool)))
map_duration carry time_of = \case
    T.TBarline pos a -> return $ EList.Elt $ T.TBarline pos a
    T.TNote pos note -> do
        result <- case T.note_duration note of
            Left time -> return $ Right (time, False)
            Right (T.Duration mb_int1 mb_int2 dots tie) -> do
                (int1, int2) <- carry mb_int1 mb_int2
                State.put (int1, int2)
                return $ Right (time_of int1 int2 dots, tie)
        return $ case result of
            Left err -> mkerror pos err
            Right (time, tie) -> EList.Elt $ T.TNote pos $
                note { T.note_duration = (time, tie) }
    T.TRest pos (T.Rest (T.Duration mb_int1 mb_int2 dots tie)) -> do
        (int1, int2) <- carry mb_int1 mb_int2
        State.put (int1, int2)
        return $ EList.Elt $ T.TRest pos $ T.Rest (time_of int1 int2 dots, tie)

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
    -> Stream (T.Time, T.Note call NPitch dur)
    -> Stream (T.Time, T.Note call (T.NPitch (Maybe Text)) dur)
resolve_pitch scale =
    fst . flip State.runState (scale_initial_octave scale, Nothing)
        . EList.mapEM token
    where
    token (start, note) = fmap set <$> case T.note_pitch note of
        T.CopyFrom -> return $ EList.Elt T.CopyFrom
        T.NPitch pitch -> fmap T.NPitch <$> resolve (T.note_pos note) pitch
        where set pitch = (start, note { T.note_pitch = pitch })
    resolve pos pitch = case parse_pitch pos (scale_parse scale) pitch of
        EList.Meta m -> return $ EList.Meta m
        EList.Elt Nothing -> return $ EList.Elt Nothing
        EList.Elt (Just pitch) -> do
            prev <- State.get
            pitch@(Pitch.Pitch oct degree) <- return $
                infer_octave per_octave prev pitch
            State.put (oct, Just degree)
            return $ Just <$> pitch_to_symbolic pos scale pitch
    per_octave = Theory.layout_pc_per_octave (scale_layout scale)

parse_pitch :: T.Pos -> (T.PitchText -> Maybe pitch) -> T.Pitch
    -> EList.Elt Meta (Maybe (T.Octave, pitch))
parse_pitch pos parse (T.Pitch oct call)
    -- If there's a previous pitch, the pitch track will carry it.
    | Text.null call = EList.Elt Nothing
    | otherwise = case parse call of
        Nothing -> mkerror pos $ "can't parse pitch: " <> call
        Just p -> EList.Elt (Just (oct, p))

infer_octave :: Pitch.PitchClass -> (Pitch.Octave, Maybe Pitch.Degree)
    -> (T.Octave, Pitch.Degree) -> Pitch.Pitch
infer_octave per_octave (prev_oct, prev_degree) (oct, degree) =
    Pitch.Pitch inferred_oct degree
    where
    inferred_oct = case oct of
        T.Relative n -> n + case prev_degree of
            Just prev -> min_on3 (distance prev_oct prev degree)
                (prev_oct-1) prev_oct (prev_oct+1)
            Nothing -> prev_oct
        T.Absolute oct -> oct
    distance prev_oct prev degree oct = abs $
        Pitch.diff_pc per_octave (Pitch.Pitch prev_oct prev)
            (Pitch.Pitch oct degree)

-- | Convert 'Pitch'es back to symbolic form.
pitch_to_symbolic :: T.Pos -> Scale -> Pitch.Pitch -> EList.Elt Meta T.PitchText
pitch_to_symbolic pos scale pitch = case unparse pitch of
    Nothing -> mkerror pos $ "bad pitch: " <> pretty pitch
    Just sym -> EList.Elt sym
    where
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


-- * util

min_on3 :: Ord k => (a -> k) -> a -> a -> a -> a
min_on3 key a b c = Seq.min_on key a (Seq.min_on key b c)
