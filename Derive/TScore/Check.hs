-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}
-- | Post-process 'T.Token's.  Check barlines, resolve ties, etc.
module Derive.TScore.Check (
    Config(..), defaultConfig
    , Scope(..)
    , From(..)
    , AssertCoincident(..)
    , parseDirective, parseDirectives
    , applyBlockFrom
    , check
    , callBlockId
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
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Then as Then

import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.Tala as Tala
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Scale.Theory as Theory
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import qualified Perform.Pitch as Pitch
import qualified Ui.Id as Id
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Meter.Meters as Meters

import           Global
import           Types


data Config = Config
    { configDuration :: DurationMode
    , configFrom :: Maybe From
    , configKy :: Text
    , configMeter :: Meter
    , configNegative :: Bool -- ^ Notes arrive at beats instead of departing.
    , configOrder :: Bool
    , configScale :: Scale
    } deriving (Show)

-- | The target of a 'T.CopyFrom'.
data From = From
    { fromBlock :: Maybe Id.BlockId
    , fromTracknum :: TrackNum
    , fromPos :: T.Pos
    } deriving (Show)

instance Pretty From where
    pretty (From block tracknum _) =
        maybe "" ((<>":") . Parse.showBlock) block <> showt tracknum

defaultConfig :: Config
defaultConfig = Config
    { configDuration = Multiplicative
    , configFrom = Nothing
    , configKy = ""
    , configMeter = meter44
    , configNegative = False
    , configOrder = False
    , configScale = scaleSargam
    }

parseDirectives :: Scope -> Config -> [T.Directive] -> Either T.Error Config
parseDirectives scope = foldM (flip (parseDirective scope))

-- | Directives can appear in various places, which affects their scope.
data Scope = Global | Block | Track
    deriving (Eq, Show)

parseDirective :: Scope -> T.Directive -> Config -> Either T.Error Config
parseDirective scope (T.Directive pos name maybeVal) config =
    first (T.Error pos . ((name<>": ")<>)) $ case name of
        "dur" -> setConfig (\c a -> c { configDuration = a })
            (`Map.lookup` durationMap) =<< withArg
        -- f for from.  This is abbreviated because it shows up per-track.
        "f" -> do
            from <- parseFrom scope pos =<< withArg
            pure $ config { configFrom = Just from }
        "meter" -> setConfig (\c a -> c { configMeter = a }) parseMeter
            =<< withArg
        "negative" -> withoutArg >> pure (config { configNegative = True })
        "scale" -> setConfig (\c a -> c { configScale = a })
            (`Map.lookup` scaleMap) =<< withArg
        "ky" -> do
            requireToplevel
            unless (configKy config == "") $
                Left "should only be one %ky"
            setConfig (\c a -> c { configKy = a }) Just =<< withArg
        _ | name == Parse.defaultCall -> withoutArg >> pure config
        _ | name == Parse.orderBlock -> do
            withoutArg
            Right $ config { configOrder = True }
        _ -> Left $ "unknown directive: " <> name
    where
    requireToplevel = case scope of
        Global -> pure ()
        _ -> Left "must be at global scope"
    withArg = tryJust "expected arg" maybeVal
    withoutArg = maybe (Right ()) (Left . ("unexpected arg: "<>)) maybeVal
    setConfig setter parse val = fmap (setter config) (lookup parse val)
    lookup parse val = tryJust ("unknown " <> name <> ": " <> val) (parse val)

parseFrom :: Scope -> T.Pos -> Text -> Either Text From
parseFrom scope pos arg = case scope of
    Global -> Left "can't use at global scope"
    Block -> do
        blockId <- tryJust ("not a valid block_id: " <> arg) $
            Id.make $ Id.readShort Parse.defaultNamespace arg
        pure $ From
            { fromBlock = Just blockId
            , fromTracknum = 0
            , fromPos = pos
            }
    Track -> do
        tracknum <- tryJust ("not a tracknum: " <> arg) $ ParseText.int arg
        when (tracknum <= 0) $ Left "tracknums start at 1"
        pure $ From
            { fromBlock = Nothing
            , fromTracknum = tracknum
            , fromPos = pos
            }

-- | Convert a block-level From into a track-level From for each tracknum.
-- Due to 'parse_from', the block From should have a 'from_block'.  It's ok
-- to have both block and track From, in this case the track From gives the
-- tracknum from a different block.
applyBlockFrom :: Config -> TrackNum -> Config -> Config
applyBlockFrom blockConfig tracknum trackConfig =
    set $ case (configFrom blockConfig, configFrom trackConfig) of
        (Nothing, mbTrack) -> mbTrack
        (Just block, Nothing) -> Just $ block { fromTracknum = tracknum }
        -- If the block had a From, the track will have inherited it.
        (Just block, Just track) -> Just $ track
            { fromBlock = fromBlock block
            , fromTracknum = if fromTracknum track /= 0
                then fromTracknum track else tracknum
            }
    where set from = trackConfig { configFrom = from }

-- * check

type Stream a = [EList.Elt Meta a]
type Token call pitch dur = T.Token call pitch dur dur
type Meta = Either T.Error AssertCoincident

type NPitch = T.NPitch T.Pitch

type GetCallDuration = Text -> (Either Text T.Time, [Log.Msg])

mkerror :: T.Pos -> Text -> EList.Elt Meta a
mkerror pos msg = EList.Meta $ Left $ T.Error pos msg

data AssertCoincident = AssertCoincident T.Time T.Pos
    deriving (Eq, Show)

check :: GetCallDuration -> Config
    -> [T.Token T.CallText NPitch T.NDuration T.Duration]
    -> ( [Either Meta
            (T.Time, T.Note T.CallText (T.NPitch (Maybe T.PitchText)) T.Time)]
       , T.Time
       )
check getDur config =
    Tuple.swap
    . fmap (map EList.toEither)
    . fmap (resolvePitch (configScale config))
    . (if configNegative config then timeToNegative else id)
    . resolveTime
    . checkBarlines (configMeter config)
    . resolveDuration (configDuration config)
    . resolveCallDuration getDur
    . carryCallDuration (configOrder config)
    . resolveRepeats
    . map EList.Elt

-- * time

-- | Carry CallDuration if the next note has no duration, but does have a call.
carryCallDuration :: Bool
    -> Stream (T.Token T.CallText pitch T.NDuration T.Duration)
    -> Stream (T.Token T.CallText pitch T.NDuration T.Duration)
carryCallDuration defaultCallDuration =
    flip State.evalState defaultCallDuration . EList.mapM (T.mapNote carry)
    where
    carry note = do
        dur <- case T.noteDuration note of
            T.CallDuration -> do
                State.put True
                pure T.CallDuration
            T.NDuration dur
                | canCarry note dur -> ifM State.get
                    (pure T.CallDuration)
                    (pure $ T.NDuration dur)
                | otherwise -> State.put False >> pure (T.NDuration dur)
        pure $ note { T.noteDuration = dur }
    canCarry note dur = T.noteCall note /= ""
        && all Maybe.isNothing [T.durInt1 dur, T.durInt2 dur]

resolveCallDuration :: GetCallDuration
    -> Stream (T.Token T.CallText pitch T.NDuration rdur)
    -> Stream (T.Token T.CallText pitch (Either T.Time T.Duration) rdur)
resolveCallDuration getDur = EList.concatMapE $ \case
    T.TBarline pos a -> [EList.Elt $ T.TBarline pos a]
    T.TRest pos a -> [EList.Elt $ T.TRest pos a]
    T.TNote pos note ->
        map (fmap set) (resolve pos (T.noteCall note) (T.noteDuration note))
        where set dur = T.TNote pos $ note { T.noteDuration = dur }
    where
    resolve _ _ (T.NDuration dur) = [EList.Elt $ Right dur]
    resolve pos call T.CallDuration
        | Text.null call =
            [mkerror pos "can't get call duration of empty call"]
        | otherwise = case getDur call of
            (Left err, logs) -> mkerror pos err : map toError logs
            (Right time, logs) -> EList.Elt (Left time) : map toError logs
        where toError = mkerror pos . Log.formatMsg

callBlockId :: Id.BlockId -> Text -> Maybe Id.BlockId
callBlockId parent =
    Eval.callToBlockId Parse.defaultNamespace (Just parent) . Expr.Symbol

resolveRepeats
    :: Stream (T.Token T.CallText NPitch T.NDuration T.Duration)
    -> Stream (T.Token T.CallText NPitch T.NDuration T.Duration)
resolveRepeats =
    snd . EList.mapAccumLE resolveDot Nothing
        . map (fmap resolveTie) . zipNextNote
    where
    resolveTie (T.TNote pos note, Just next)
        | strip next == Parse.tieNote = T.TNote pos $ setTie True note
    resolveTie (T.TNote pos note, _)
        -- Normalize Parse.tie_note to a Parse.dot_note, since the tie part has
        -- been handled in the previous equation.  It's important to do this
        -- here, so I only normalize "original" 'Parse.tie_note's, not tied
        -- notes created by the previous equation, which is what happens with
        -- two 'Parse.tie_note's in a row.
        | strip note == Parse.tieNote = T.TNote pos Parse.dotNote
    resolveTie (token, _) = token
    -- x ~ .    (tie, no, no)
    -- x . ~    (no, tie, no)
    -- x ~ ~    (tie, tie, no)
    -- x~ . .   (tie, no, no)
    -- So don't inherit the tie from the prev note, get it from the current
    -- note.
    resolveDot mbPrev token@(T.TNote pos note)
        | strip note `elem` repeatNotes = case mbPrev of
            Just prev ->
                ( mbPrev
                , EList.Elt $ T.TNote pos (setTie tied (setDots 0 prev))
                )
                where tied = strip note `elem` [tieDotNote, Parse.tieNote]
            Nothing ->
                ( mbPrev
                , mkerror pos "repeat with no previous note"
                )

        | otherwise = (Just note, EList.Elt token)
    resolveDot mbPrev token = (mbPrev, EList.Elt token)

    strip note = note { T.notePos = T.Pos 0 }
    repeatNotes = [Parse.dotNote, Parse.tieNote, tieDotNote]
    tieDotNote = setTie True Parse.dotNote
    setTie tie = modifyDuration $ \dur -> dur { T.durTie = tie }
    setDots n = modifyDuration $ \dur -> dur { T.durDots = n }

zipNextNote :: Stream (T.Token call pitch ndur rdur)
    -> Stream (T.Token call pitch ndur rdur, Maybe (T.Note call pitch ndur))
zipNextNote = EList.map (second (msum . map noteOf)) . EList.zipNexts
    where
    noteOf (T.TNote _ n) = Just n
    noteOf _ = Nothing

modifyDuration :: (T.Duration -> T.Duration) -> T.Note call pitch T.NDuration
    -> T.Note call pitch T.NDuration
modifyDuration modify note =
    note { T.noteDuration = set (T.noteDuration note) }
    where
    set (T.NDuration dur) = T.NDuration (modify dur)
    set T.CallDuration = T.CallDuration

-- ** meter

data Meter = Meter
    -- | Rank pattern.
    --
    -- Adi: [2, 0, 0, 0, 1, 0, 0, 1, 0, 0]
    -- > || ssss rrrr gggg mmmm | pppp dddd | nnnn sssss ||
    -- 4/4: [1, 0, 0, 0]
    -- > | ssss rrrr gggg mmmm |
    { meterPattern :: [T.Rank]
    -- | This is the duration one one element of 'meter_pattern'.
    , meterStep :: T.Time
    -- | If true, beats fall at the end of measures.
    -- TODO has no effect yet, not even sure what it would be for
    , meterNegative :: Bool
    -- | Meter as used by the UI.
    -- TODO redundant with meter_pattern, merge them?
    , meterUi :: (Meter.Config, Meter.MSection)
    } deriving (Eq, Show)

meterDuration :: Meter -> T.Time
meterDuration m = meterStep m * fromIntegral (length (meterPattern m))

parseMeter :: Text -> Maybe Meter
parseMeter name = case untxt name of
    [n1, n2] | Just n1 <- Num.readDigit n1, Just n2 <- Num.readDigit n2 ->
        simpleMeter n1 n2
    'a':'d':'i' : n
        | Just n <- ParseText.nat (txt n) -> meterAdi n
        | null n -> meterAdi 4
    "gong" -> Just meterGong
    _ -> Nothing

-- If I do akshara as 1, then kanda is 1/5th notes.  I'd want to reduce the
-- pulse to 1/5, or write .1--.5?
meterAdi :: Int -> Maybe Meter
meterAdi nadai
    | 2 <= nadai && nadai <= 9 = Just $ Meter
        { meterPattern = [2, 0, 0, 0, 1, 0, 1, 0]
        , meterStep = 1
        , meterNegative = False
        , meterUi =
            ( Tala.config Tala.adi
            , Meter.MSection 4 1 (Tala.talaToMeter Tala.adi nadai)
            )
        }
    | otherwise = Nothing

meterGong :: Meter
meterGong = Meter
    { meterPattern = [1, 0, 0, 0, 0, 0, 0, 0]
    , meterStep = 1
    , meterNegative = True
    , meterUi =
        ( Gong.config
        , Meter.MSection 4 Gong.measureDur
            (Meter.regularSubdivision [2, 2, 2, 2, 2, 2])
        )
    }

meter44 :: Meter
Just meter44 = simpleMeter 4 4

simpleMeter :: Int -> Int -> Maybe Meter
simpleMeter n1 n2 = do
    absMeter <- Meters.simple n1 n2
    pure $ Meter
        { meterPattern = 1 : replicate (n1-1) 0
        , meterStep = 1 / fromIntegral n2
        , meterNegative = False
        , meterUi =
            ( Meter.defaultConfig
            , Meter.MSection
                { sectionMeasures = 4
                , sectionMeasureDuration = 1
                , sectionMeasure = absMeter
                }
            )
        }

-- ** resolveTime

timeToNegative ::
    (T.Time, Stream (T.Time, T.Note call pitch T.Time))
    -> (T.Time, Stream (T.Time, T.Note call pitch T.Time))
timeToNegative (end, stream) = (end, EList.map invert stream)
    where
    invert (start, note) = (start + dur, note { T.noteDuration = -dur })
        where dur = T.noteDuration note

-- | Remove TBarline and TRest, add start times, and resolve ties.
resolveTime :: (Eq pitch, Pretty pitch)
    => Stream (Token call pitch (T.Time, Bool))
    -> (T.Time, Stream (T.Time, T.Note call pitch T.Time))
resolveTime tokens = go (EList.zipPaddedSnd starts tokens)
    where
    starts = scanl (+) 0 (map durationOf (EList.elts tokens))
    go (EList.Elt (!start, Nothing) : ts)
        | null ts = (start, [])
        | otherwise = go ts
    go (EList.Elt (!start, Just t) : ts) = case t of
        T.TNote _ note
            | isTied t ->
                case tiedNotes note (Lists.mapMaybeSnd id (EList.elts pre)) of
                    Left err -> (EList.Meta (Left err) :) <$> go post
                    Right end ->
                        (EList.Elt (start, setDur (end-start) note) :) <$>
                            go post
            | otherwise ->
                (EList.Elt (start, setDur (fst (T.noteDuration note)) note) :)
                    <$> go ts
        T.TBarline _ (T.Barline {}) -> go ts
        T.TBarline pos T.AssertCoincident ->
            (EList.Meta (Right (AssertCoincident start pos)) :) <$> go ts
        T.TRest {}
            | isTied t -> case tiedRests (mapMaybe snd (EList.elts pre)) of
                Just err -> (EList.Meta (Left err) :) <$> go post
                Nothing -> go post
            | otherwise -> go ts
        where
        (pre, post) = Then.span anyTied (splitAt 1) ts
        anyTied (EList.Meta _) = True
        anyTied (EList.Elt (_, Just n)) = isBarline n || isTied n
        anyTied (EList.Elt (_, Nothing)) = False
    go (EList.Meta e : ts) = (EList.Meta e :) <$> go ts
    go [] = (0, [])
    setDur dur note = note { T.noteDuration = dur }
    isBarline (T.TBarline {}) = True
    isBarline _ = False

tiedNotes :: (Eq pitch, Pretty pitch)
    => T.Note call pitch dur -> [(T.Time, Token call pitch (T.Time, Bool))]
    -> Either T.Error T.Time
tiedNotes note tied = case others of
    [] -> case Lists.last matches of
        Nothing -> Left $ T.Error (T.notePos note) "final note has a tie"
        Just (s, n)
            | snd $ T.noteDuration n ->
                Left $ T.Error (T.notePos n) "final note has a tie"
            | otherwise -> Right $ s + durOf n
    (_, bad) : _ -> Left $ T.Error (T.tokenPos bad) $ case bad of
        T.TNote _ n -> "note tied to different pitch: "
            <> pretty (T.notePitch note) <> " ~ "
            <> pretty (T.notePitch n)
        _ -> "note tied to " <> T.tokenName bad
    where
    (matches, others) = first concat $ Lists.partitionOn match tied
    durOf = fst . T.noteDuration
    match (s, T.TNote _ n) | T.notePitch note == T.notePitch n = Just [(s, n)]
    match (_, T.TBarline {}) = Just []
    match _ = Nothing

tiedRests :: [Token call pitch (T.Time, Bool)] -> Maybe T.Error
tiedRests = fmap format . List.find (not . matches)
    where
    format token =
        T.Error (T.tokenPos token) $ "rest tied to " <> T.tokenName token
    matches (T.TRest {}) = True
    matches (T.TBarline {}) = True
    matches _ = False

isTied :: T.Token call pitch (a1, Bool) (a2, Bool) -> Bool
isTied (T.TNote _ note) = snd $ T.noteDuration note
isTied (T.TRest _ (T.Rest (_, tied))) = tied
isTied _ = False

-- ** check_barlines

checkBarlines :: Meter
    -> Stream (Token call pitch (T.Time, tie))
    -> Stream (Token call pitch (T.Time, tie))
checkBarlines meter =
    fst . flip State.runState 0 . EList.concatMapEM checkToken
        . EList.zip [0..]
    where
    checkToken (i, token) = do
        now <- State.get
        State.put $! now + dur
        pure $ EList.Elt token : warning now
        where
        dur = durationOf token
        warning now = case token of
            T.TBarline pos bar -> maybe [] (:[]) (check pos now i bar)
            _ -> []
    check pos now i (T.Barline rank) = case checkBeat beatRank beat rank of
        Nothing -> Nothing
        Just err -> Just $ mkerror pos $ warn i $
            "beat " <> pretty beat <> ": " <> err
        where beat = now `Num.fmod` cycleDur
    -- resolve_time will turn this into 'AssertCoincident'.
    check _ _ _ T.AssertCoincident = Nothing
    cycleDur = meterDuration meter
    beatRank = Map.fromList $ filter ((>0) . snd) $
        zip (Lists.range_ 0 (meterStep meter))
            -- Two bars, to ensure 'check_beat' can always find the next beat.
            (meterPattern meter ++ meterPattern meter)
    warn i msg = "barline check: token " <> showt i <> ": " <> msg

-- | Nothing if the expected rank falls on the given beat, or an error msg.
checkBeat :: Map T.Time T.Rank -> T.Time -> T.Rank -> Maybe Text
checkBeat beatRank beat rank = (prefix<>) <$> case at of
    Nothing -> Just nextBeat
    Just r
        | r == rank -> Nothing
        | otherwise -> Just $ "expected " <> pretty (T.Barline r)
            <> ", " <> nextBeat
    where
    prefix = "saw " <> pretty (T.Barline rank) <> ", "
    nextBeat = case List.find ((==rank) . snd) (Map.toList post) of
        Nothing -> "no following beat with that rank"
        Just (t, _) -> "next beat of that rank is " <> pretty t
            <> " (short " <> pretty (t-beat) <> ")"
    (_, at, post) = Map.splitLookup beat beatRank

durationOf :: Token call pitch (T.Time, tie) -> T.Time
durationOf = \case
    T.TBarline {} -> 0
    T.TNote _ note -> fst (T.noteDuration note)
    T.TRest _ (T.Rest (dur, _)) -> dur

-- ** resolve duration

durationMap :: Map Text DurationMode
durationMap = Map.fromList
    [ ("mul", Multiplicative)
    , ("add", Additive)
    ]

data DurationMode = Multiplicative | Additive
    deriving (Eq, Show)

resolveDuration :: DurationMode
    -> Stream (T.Token call pitch (Either T.Time T.Duration) T.Duration)
    -> Stream (T.Token call pitch (T.Time, Bool) (T.Time, Bool))
resolveDuration = \case
    Multiplicative -> multiplicative
    Additive -> additive

-- | Each number is the inverse of the number of beats, so 2 is 1/2, 8 is 1/8
-- etc.  If there are two numbers, you can set both numerator and denominator.
-- This carrier both values or none.
multiplicative
    :: Stream (T.Token call pitch (Either T.Time T.Duration) T.Duration)
    -- ^ Dur is either an absolute dur, or a logical one to resolve.
    -> Stream (Token call pitch (T.Time, Bool))
multiplicative =
    flip State.evalState (1, Nothing) . EList.mapEM (mapDuration carry timeOf)
    where
    timeOf int1 mbInt2 dots = dur + dotDur
        where
        dur = T.Time $ case mbInt2 of
            Nothing -> 1 / fromIntegral int1
            Just int2 -> fromIntegral int1 / fromIntegral int2
        dotDur = Num.sum $ take dots $ drop 1 $ iterate (/2) dur
    carry Nothing Nothing = State.get
    carry Nothing (Just int2) = pure (int2, Nothing)
    carry (Just int1) mbInt2 = pure (int1, mbInt2)

-- | Each number is just the number of Time beats.  If there are two numbers,
-- int1 corresponds to a higher level of division, which is meter specific.
-- E.g. akshara:matra.
additive :: Stream (T.Token call pitch (Either T.Time T.Duration) T.Duration)
    -> Stream (Token call pitch (T.Time, Bool))
additive =
    flip State.evalState (1, 4) . EList.mapEM (mapDuration carry timeOf)
    where
    timeOf int1 int2 dots = dur + dotDur
        where
        dur = fromIntegral int1 / fromIntegral int2
        dotDur = Num.sum $ take dots $ drop 1 $ iterate (/2) dur
    carry int1 int2 = do
        (pInt1, pInt2) <- State.get
        pure (fromMaybe pInt1 int1, fromMaybe pInt2 int2)

mapDuration :: State.MonadState (int1, int2) m
    => (Maybe Int -> Maybe Int -> m (int1, int2))
    -> (int1 -> int2 -> Int -> time)
    -> T.Token call pitch (Either time T.Duration) T.Duration
    -> m (EList.Elt Meta (T.Token call pitch (time, Bool) (time, Bool)))
mapDuration carry timeOf = \case
    T.TBarline pos a -> pure $ EList.Elt $ T.TBarline pos a
    T.TNote pos note -> do
        result <- case T.noteDuration note of
            Left time -> pure $ Right (time, False)
            Right (T.Duration mbInt1 mbInt2 dots tie) -> do
                (int1, int2) <- carry mbInt1 mbInt2
                State.put (int1, int2)
                pure $ Right (timeOf int1 int2 dots, tie)
        pure $ case result of
            Left err -> mkerror pos err
            Right (time, tie) -> EList.Elt $ T.TNote pos $
                note { T.noteDuration = (time, tie) }
    T.TRest pos (T.Rest (T.Duration mbInt1 mbInt2 dots tie)) -> do
        (int1, int2) <- carry mbInt1 mbInt2
        State.put (int1, int2)
        pure $ EList.Elt $ T.TRest pos $ T.Rest (timeOf int1 int2 dots, tie)

-- * pitch

data Scale = Scale
    { scaleName :: Text
    , scaleParse :: Text -> Maybe Pitch.Degree
    , scaleUnparse :: Pitch.Degree -> Maybe Text
    , scaleLayout :: Theory.Layout
    , scaleInitialOctave :: Pitch.Octave
    }

instance Show Scale where
    show scale = "((" <> untxt (scaleName scale) <> "))"

resolvePitch :: Scale
    -> Stream (time, T.Note call NPitch dur)
    -> Stream (time, T.Note call (T.NPitch (Maybe Text)) dur)
resolvePitch scale =
    fst . flip State.runState (scaleInitialOctave scale, Nothing)
        . EList.mapEM token
    where
    token (start, note) = fmap set <$> case T.notePitch note of
        T.CopyFrom -> pure $ EList.Elt T.CopyFrom
        T.NPitch pitch -> fmap T.NPitch <$> resolve (T.notePos note) pitch
        where set pitch = (start, note { T.notePitch = pitch })
    resolve pos pitch = case parsePitch pos (scaleParse scale) pitch of
        EList.Meta m -> pure $ EList.Meta m
        EList.Elt Nothing -> pure $ EList.Elt Nothing
        EList.Elt (Just pitch) -> do
            prev <- State.get
            pitch@(Pitch.Pitch oct degree) <- pure $
                inferOctave perOctave prev pitch
            State.put (oct, Just degree)
            pure $ Just <$> pitchToSymbolic pos scale pitch
    perOctave = Theory.layoutPcPerOctave (scaleLayout scale)

parsePitch :: T.Pos -> (T.PitchText -> Maybe pitch) -> T.Pitch
    -> EList.Elt Meta (Maybe (T.Octave, pitch))
parsePitch pos parse (T.Pitch oct call)
    -- If there's a previous pitch, the pitch track will carry it.
    | Text.null call = EList.Elt Nothing
    | otherwise = case parse call of
        Nothing -> mkerror pos $ "can't parse pitch: " <> call
        Just p -> EList.Elt (Just (oct, p))

inferOctave :: Pitch.PitchClass -> (Pitch.Octave, Maybe Pitch.Degree)
    -> (T.Octave, Pitch.Degree) -> Pitch.Pitch
inferOctave _ _ (T.Absolute oct, degree) =
    Pitch.Pitch oct degree
inferOctave _ (prevOct, Nothing) (T.Relative relOct, degree) =
    Pitch.Pitch (prevOct + relOct) degree
inferOctave perOct (prevOct, Just prevDegree) (T.Relative relOct, degree) =
    case compare relOct 0 of
        -- If distances are equal, favor downward motion.  But in a 7 note
        -- scale, they won't be equal.
        EQ -> Lists.minOn (abs . Pitch.diffPc perOct prev) below above
        GT -> Pitch.addOctave (relOct-1) above
        LT -> Pitch.addOctave (relOct+1) below
    where
    prev = Pitch.Pitch prevOct prevDegree
    -- If I'm moving up, then I don't need to increment the octave to be above.
    above = Pitch.Pitch (if degree > prevDegree then prevOct else prevOct+1)
        degree
    below = Pitch.Pitch (if degree < prevDegree then prevOct else prevOct-1)
        degree

-- | Convert 'Pitch'es back to symbolic form.
pitchToSymbolic :: T.Pos -> Scale -> Pitch.Pitch -> EList.Elt Meta T.PitchText
pitchToSymbolic pos scale pitch = case unparse pitch of
    Nothing -> mkerror pos $ "bad pitch: " <> pretty pitch
    Just sym -> EList.Elt sym
    where
    unparse (Pitch.Pitch oct degree) =
        (showt oct <>) <$> scaleUnparse scale degree

-- ** scale

scaleMap :: Map Text Scale
scaleMap = Map.fromList $ Lists.keyOn scaleName
    [ scaleSargam
    , scaleBali
    , scaleTwelve
    ]

diatonicScale :: Text -> [Text] -> Scale
diatonicScale name degrees_ = Scale
    { scaleName = name
    , scaleParse = \s -> Pitch.Degree <$> Vector.elemIndex s degrees <*> pure 0
    , scaleUnparse = unparse
    , scaleLayout = Theory.diatonicLayout (Vector.length degrees)
    , scaleInitialOctave = 4
    }
    where
    unparse (Pitch.Degree pc accs)
        | accs == 0 = degrees Vector.!? pc
        | otherwise = Nothing
    degrees = Vector.fromList degrees_

scaleSargam :: Scale
scaleSargam = diatonicScale "sargam" $ map Text.singleton "srgmpdn"

scaleBali :: Scale
scaleBali = diatonicScale "bali" $ map Text.singleton "ioeua"

scaleTwelve :: Scale
scaleTwelve = Scale
    { scaleName = "twelve"
    , scaleParse = P.parseMaybe pDegree
    , scaleUnparse = unparse
    , scaleLayout = Theory.pianoLayout
    , scaleInitialOctave = 4
    }
    where
    pDegree :: Parser Pitch.Degree
    pDegree = do
        pc <- P.choice [P.string c *> pure i | (i, c) <- zip [0..] degrees]
        accs <- P.choice $ map (\(n, c) -> P.string c *> pure n) accidentals
        pure $ Pitch.Degree pc accs
    unparse (Pitch.Degree pc accs) = (<>)
        <$> Lists.at degrees pc <*> lookup accs accidentals
    accidentals =
        [ (0, ""), (0, "n")
        , (1, "#"), (2, "x")
        , (-1, "b"), (-2, "bb")
        ]
    degrees = map Text.singleton "cdefgab"

type Parser a = P.Parsec Void.Void Text a
