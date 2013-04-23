-- | Convert Lilypond Events to lilypond code.
--
-- It's a terrible name, but what else am I supposed to call it?  Render?
-- Realize?  Perform?
module Perform.Lilypond.Process where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Attrs as Attrs
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types
       (Event(..), event_end, event_attributes, ToLily, to_lily, Time(..))
import qualified Perform.Pitch as Pitch


-- | Automatically add lilypond code for certain attributes.
simple_articulations :: [(Score.Attributes, Code)]
simple_articulations =
    [ (Attrs.harm, "-\\flageolet")
    , (Attrs.mute, "-+")
    -- Previously pizz<>right, comment on "Attrs".
    , (Attrs.pizz_right, "-+")
    , (Attrs.marcato, "-^")
    , (Attrs.staccato, "-.")
    , (Attrs.trill, "\\trill")
    , (Attrs.portato, "-_")
    , (Attrs.tenuto, "--")
    , (Attrs.accent, "->")
    , (Attrs.trem, ":32")
    ]

-- | Certain attributes are modal, in that they emit one thing when they
-- start, and another when they stop.
modal_articulations :: [(Score.Attributes, Code, Code)]
modal_articulations =
    [ (Attrs.pizz, "^\"pizz.\"", "^\"arco\"")
    , (Attrs.nv, "^\"nv\"", "^\"vib\"")
    ]

-- * process

run_process :: (Monad m) => m [b] -> ([a] -> m ([b], [a])) -> [a] -> m [b]
run_process complete f = go
    where
    go xs = do
        (ys, remaining) <- f xs
        if null remaining then liftM (ys++) complete else do
            remaining_ys <- go remaining
            return $ ys ++ remaining_ys

type VoiceLy = Either Voices Ly

process :: Types.Config -> Time -> [Meter.Meter] -> [Event]
    -> Either String [VoiceLy]
process config start meters events = do
    let state1 = make_state config start meters default_key
    key <- maybe (return default_key)
        (fmap fst . run_convert state1 . lookup_key) (Seq.head events)
    let state2 = state1 { state_key = key }
    (lys, _) <- run_convert state2 $
        error_context ("start: " <> Pretty.pretty start) $ convert events
    let meter = fromMaybe Meter.default_meter (Seq.head meters)
    return $ Right (Code $ "\\time " <> to_lily meter)
        : Right (KeyChange key) : lys

convert :: [Event] -> ConvertM [VoiceLy]
convert = run_process trailing_rests go
    where
    go :: [Event] -> ConvertM ([VoiceLy], [Event])
    go [] = return ([], [])
    go events = do
        (voices, events) <- either throw return $ collect_voices events
        voices <- to_voices <$> convert_voices voices
        (lys, remaining) <- convert_chunk events
        return (voices ++ map Right lys, remaining)
    trailing_rests = do
        meters <- State.gets state_meters
        if null meters then return [Right final_barline] else do
        end <- State.gets state_measure_end
        rests <- rests_until end
        remaining <- trailing_rests
        return $ map Right rests ++ remaining
    to_voices [] = []
    to_voices voices = [Left (Voices voices)]
    final_barline = Code "\\bar \"|.\""

-- | This is a simplified version of 'convert', designed for converting little
-- chunks of lilypond that occur in other expressions.  So it doesn't handle
-- clef changes, meter changes, or even barlines.  It will apply simple
-- articulations from 'simple_articulations', but not modal ones from
-- 'modal_articulations'.
simple_convert :: Types.Config -> Meter.Meter -> Time -> [Event] -> [Ly]
simple_convert config meter = go
    where
    go _ [] = []
    go start (event : events) =
        leading_rests ++ lys ++ go end rest_events
        where
        leading_rests = map LyRest $
            make_rests config meter start (event_start event)
        (lys, end, _, rest_events) = make_lys 0 mempty meter (event :| events)

convert_voice :: Time -> [Event] -> ConvertM [Ly]
convert_voice end = run_process (rests_until end) convert_chunk

-- ** convert chunk

convert_chunk :: [Event] -> ConvertM ([Ly], [Event])
convert_chunk events = error_context current $ case zero_dur_in_rest events of
    ([], []) -> return ([], [])
    (zero@(event:_), []) -> return (mix_in_code (event_start event) zero [], [])
    (zero, event : events) -> do
        start <- State.gets state_time
        rests <- mix_in_code start zero <$> rests_until (event_start event)
        meter <- get_meter
        (lys, remaining) <- convert_chord meter (event :| events)
        return (rests <> lys, remaining)
    where current = maybe "no more events" Pretty.pretty (Seq.head events)

-- | Code events are mixed into the Lys, depending on their prepend or append
-- attrs.
--
-- This is doing the same thing as 'make_lys', but since rests aren't
-- represented explicitly be events as notes are, I have to first generate the
-- notes, and then mix in the code afterwards.
mix_in_code :: Time -> [Event] -> [Ly] -> [Ly]
mix_in_code start dur0 lys =
    go dur0 $ zip (scanl (+) start (map ly_dur lys)) lys
    where
    go events [] = prepend ++ append
        where (prepend, append) = partition_code events
    go events ((start, LyRest note) : lys) =
        prepend ++ LyRest note : append ++ go rest lys
        where
        (prepend, append) = partition_code overlapping
        (overlapping, rest) = span ((< note_end note) . event_start) events
        note_end = (+start) . rest_time
    go events ((_, ly) : lys) = ly : go events lys
    ly_dur (LyNote note) = Types.note_dur_to_time (note_duration note)
    ly_dur (LyRest rest) = rest_time rest
    ly_dur _ = 0

partition_code :: [Event] -> ([Ly], [Ly])
partition_code events = (map Code prepend, map Code append)
    where
    prepend = filter (not . Text.null) $
        map (get Constants.v_ly_prepend) events
    append = filter (not . Text.null) $
        map (get Constants.v_ly_append_all) events
    get :: TrackLang.ValName -> Event -> Text
    get v = fromMaybe "" . TrackLang.maybe_val v . event_environ

zero_dur_in_rest :: [Event] -> ([Event], [Event])
zero_dur_in_rest events = span (\e -> zero_dur e && in_rest e) events
    where
    next_note = Seq.head (filter (not . zero_dur) events)
    in_rest e = maybe True ((> event_start e) . event_start) next_note

zero_dur :: Event -> Bool
zero_dur = (==0) . event_duration

-- ** convert_chord

convert_chord :: Meter.Meter -> NonEmpty Event -> ConvertM ([Ly], [Event])
convert_chord meter events = do
    key <- lookup_key (NonEmpty.head events)
    state <- State.get
    let key_change = [KeyChange key | key /= state_key state]
    let (chord_notes, end, last_attrs, remaining) = make_lys
            (state_measure_start state) (state_prev_attrs state) meter events
    barline <- advance_measure end
    State.modify $ \state -> state
        { state_key = key
        , state_prev_attrs = last_attrs
        }
    return (key_change <> chord_notes <> maybe [] (:[]) barline, remaining)

-- | Convert a chunk of events all starting at the same time.  Events
-- with 0 duration or null pitch are expected to have either
-- 'Constants.v_ly_prepend' or 'Constants.v_ly_append_all', and turn into
-- 'Code' Notes.
--
-- The rules are documented in 'Perform.Lilypond.Convert.convert_event'.
make_lys :: Time -> Score.Attributes -> Meter.Meter -> NonEmpty Event
    -> ([Ly], Time, Score.Attributes, [Event])
    -- ^ (note, note end time, last attrs, remaining events)
make_lys measure_start prev_attrs meter events =
    (notes, end, last_attrs, clipped ++ remaining)
    where
    -- As with rests, figuring out which notes to put the code events on is
    -- tricky.  The idea is the same, but rests are generated en masse for
    -- a block of time while notes are generated one at a time.

    -- Get events that all start at the same time, and make a Ly if there are
    -- any.  Otherwise they must all be zero dur code events that are
    -- "free standing", not overlapped by any previous note.
    (here, after) = NonEmpty.break
        ((> event_start (NonEmpty.head events)) . event_start) events
    (dur0, with_dur) = List.partition zero_dur here

    (maybe_note, end, last_attrs, clipped) = case NonEmpty.nonEmpty with_dur of
        Nothing -> (Nothing, event_start (NonEmpty.head events), prev_attrs, [])
        Just chord ->
            let next = event_start <$> List.find (not . zero_dur) after
                (n, end, clipped) =
                    make_note measure_start prev_attrs meter chord next
            in (Just n, end, event_attributes (NonEmpty.last chord), clipped)

    -- Now that I know the duration of the Ly (if any) I can get the zero-dur
    -- code events it overlaps.
    (overlapping, remaining) =
        span (\e -> zero_dur e && event_start e < end) after
    (prepend, append) = partition_code (dur0 ++ overlapping)
    -- Circumfix the possible real note with zero-dur code placeholders.
    notes = prepend ++ maybe [] (:[]) maybe_note ++ append

make_note :: Time -> Score.Attributes -> Meter.Meter
    -> NonEmpty Event -- ^ Events that occur at the same time.
    -- All these events must have >0 duration!
    -> Maybe Time -> (Ly, Time, [Event])
    -- ^ (note, note end time, clipped)
make_note measure_start prev_attrs meter events next = (ly, end, clipped)
    where
    ly = case NonEmpty.nonEmpty pitches of
        Nothing -> Code (prepend first <> append first)
        Just pitches -> LyNote (note pitches)
    first = NonEmpty.head events
    -- If there are no pitches, then this is code with duration.
    pitches = do
        event <- NonEmpty.toList events
        let pitch = get_pitch event
        guard (not (Text.null pitch))
        return (pitch, note_tie event)
    note pitches = Note
        { note_pitch = pitches
        , note_duration = allowed_dur
        , note_prepend = prepend first
        , note_append =
            append first <> attrs_to_code prev_attrs (event_attributes first)
        , note_stack = Seq.last (Stack.to_ui (event_stack first))
        }
    get_pitch event = event_pitch event
        <> if is_first event then append_pitch event else ""
    append_pitch = fromMaybe ""
        . TrackLang.maybe_val Constants.v_ly_append_pitch . event_environ

    prepend event =
        if is_first event then get Constants.v_ly_prepend event else ""
    append event = mconcat
        [ if is_first event then get Constants.v_ly_append_first event else ""
        , if is_last then get Constants.v_ly_append_last event else ""
        , get Constants.v_ly_append_all event
        ]
    get val = fromMaybe "" . TrackLang.maybe_val val . event_environ

    note_tie event
        | event_end event <= end = NoTie
        | Text.null direction = TieNeutral
        | direction == "^" = TieUp
        | otherwise = TieDown
        where
        direction :: Text
        direction = get Constants.v_ly_tie_direction event
    is_first = not . event_clipped
    is_last = not $ any (is_tied . snd) pitches
    is_tied NoTie = False
    is_tied _ = True

    allowed = min (max_end - start) $
        Meter.allowed_time_greedy True meter (start - measure_start)
    allowed_dur = Types.time_to_note_dur allowed
    allowed_time = Types.note_dur_to_time allowed_dur
    -- Maximum end, the actual end may be shorter since it has to conform to
    -- a Duration.
    max_end = fromMaybe (event_end first) $ Seq.minimum $
        Maybe.maybeToList next ++ map event_end (NonEmpty.toList events)
    clipped = mapMaybe (clip_event end) (NonEmpty.toList events)
    start = event_start first
    end = start + allowed_time

-- * convert voices

-- | Like 'convert', but converts within a voice, which means no nested voices
-- are expected.
convert_voices :: [(Voice, [Event])] -> ConvertM [(Voice, [Ly])]
convert_voices [] = return []
convert_voices voices = do
    state <- State.get
    let max_dur = fromMaybe (state_measure_start state) $ Seq.maximum $
            mapMaybe (Seq.maximum . map event_end . snd) voices
    (states, voice_lys) <- unzip <$> mapM (convert max_dur state) voices
    -- Since I pad with rests to the longest voice, I also want the State from
    -- that one.
    when_just (Seq.maximum_on state_time states) State.put
    return voice_lys
    where
    convert max_dur state (v, events) = do
        (measures, final_state) <- either throw return $
            run_convert state (convert_voice max_dur events)
        return (final_state, (v, measures))

-- | Span events until they don't have a 'Constants.v_voice' val.
collect_voices :: [Event] -> Either String ([(Voice, [Event])], [Event])
collect_voices events = do
    let (spanned, rest) = span_voices events
        (code, with_voice) = Seq.partition_either spanned
    with_voice <- mapM check_type with_voice
    return $ case map (second (map fst)) $ Seq.keyed_group_on snd with_voice of
        -- Code events get mixed into the first voice.  It probably doesn't
        -- matter what voice they go into.
        (voice, events) : voices ->
            ((voice, Seq.merge_on Types.event_start code events) : voices, rest)
        [] -> ([], code ++ rest)
    where
    check_type (event, Right num) = do
        voice <- maybe (Left $ "voice should be 1--4: " ++ show num) Right $
            parse_voice num
        return (event, voice)
    check_type (event, Left err) = Left $ Pretty.pretty event <> ": " <> err

-- | Strip off events with voices.  This is complicated by the possibility of
-- intervening zero_dur code events.
span_voices :: [Event] -> ([Either Event (Event, Either String Int)], [Event])
span_voices events = (spanned2, trailing_code ++ rest)
    where
    (spanned1, rest) = Seq.span_while voice_of events
    (spanned2, trailing_code) =
        Seq.span_end_while (either Just (const Nothing)) spanned1
    voice_of event
        | zero_dur event = Just $ Left event
        | otherwise = case get event of
            Nothing -> Nothing
            Just voice -> Just $ Right (event, voice)
        where get = TrackLang.checked_val2 Constants.v_voice . event_environ

-- * misc

-- | Pad with rests until given Time, which is not necessarily on a measure
-- boundary.
rests_until :: Time -> ConvertM [Ly]
rests_until end = do
    now <- State.gets state_time
    if now >= end then return [] else do
    measure_end <- State.gets state_measure_end
    rests <- create_rests (min end measure_end)
    remaining <- rests_until end
    return $ rests <> remaining
    where
    create_rests end = do
        state <- State.get
        meter <- get_meter
        barline <- advance_measure end
        let rests = make_rests (state_config state) meter
                (state_time state - state_measure_start state)
                (end - state_measure_start state)
        return $ map LyRest rests <> maybe [] (:[]) barline

-- | Advance now to the given time, up to and including the end of the measure,
-- but no further.  Return Ly with a Barline if this is a new measure.
--
-- If I wanted to emit Barlines automatically I'd have to collect the output
-- [Ly] in the State, which I'd then need to parameterize since it can be
-- [VoiceLy] too.
advance_measure :: Time -> ConvertM (Maybe Ly)
advance_measure time = advance =<< State.get
    where
    advance state
        | time < state_time state =
            throw $ "can't advance time backward: " ++ Pretty.pretty time
        | time < state_measure_end state = do
            State.put $ state { state_time = time }
            return Nothing
        | time == state_measure_end state = do
            case state_meters state of
                prev_meter : meters -> advance1 prev_meter meters
                _ -> throw $ "out of meters, can't advance time to "
                    <> Pretty.pretty time
        | otherwise = do
            throw $ "can't advance time past barline: " ++ Pretty.pretty time
                <> " > " <> Pretty.pretty (state_measure_end state)
    advance1 prev_meter meters = do
        State.modify $ \state -> state
            { state_meters = meters
            , state_measure_start = state_measure_end state
            , state_measure_end = state_measure_end state
                + Meter.measure_time (fromMaybe prev_meter (Seq.head meters))
            , state_time = time
            }
        return $ case Seq.head meters of
            Just meter
                | to_lily prev_meter == to_lily meter -> Just (Barline Nothing)
                | otherwise -> Just (Barline (Just meter))
            _ -> Nothing

get_meter :: ConvertM Meter.Meter
get_meter = do
    meters <- State.gets state_meters
    maybe (throw "out of meters") return $ Seq.head meters

-- * ConvertM

run_convert :: State -> ConvertM a -> Either String (a, State)
run_convert state = Identity.runIdentity . Error.runErrorT
    . flip State.runStateT state

type ConvertM a = State.StateT State (Error.ErrorT String Identity.Identity) a

error_context :: String -> ConvertM a -> ConvertM a
error_context msg = map_error (msg <> ": " <>)

map_error :: (String -> String) -> ConvertM a -> ConvertM a
map_error f action = Error.catchError action $ \err -> Error.throwError (f err)

data State = State {
    -- Constant:
    state_config :: Types.Config

    -- Changes on each measure:
    -- | One Meter for each expected measure in the output.
    -- The head of the list is the current meter.  It's valid for the meters to
    -- be [] as long as you don't have any more notes or rests to generate.
    , state_meters :: [Meter.Meter]
    , state_measure_start :: Time
    , state_measure_end :: Time

    -- Changes on each note:
    -- | Current position in time, aka the end of the previous note.
    , state_time :: Time
        -- | Used in conjunction with 'modal_articulations'.
    , state_prev_attrs :: Score.Attributes
    , state_key :: Key
    } deriving (Show)

make_state :: Types.Config -> Time -> [Meter.Meter] -> Key -> State
make_state config start meters key = State
    { state_config = config
    , state_meters = meters
    , state_measure_start = start
    , state_measure_end = start + maybe 0 Meter.measure_time (Seq.head meters)
    , state_time = start
    , state_prev_attrs = mempty
    , state_key = key
    }

-- ** util

throw :: String -> ConvertM a
throw msg = do
    now <- State.gets state_time
    Error.throwError $ Pretty.pretty now <> ": " <> msg

lookup_key :: Event -> ConvertM Key
lookup_key = lookup_val TrackLang.v_key parse_key default_key

default_key :: Key
default_key = Key "c" "major"

lookup_val :: TrackLang.ValName -> (Text -> Either String a) -> a -> Event
    -> ConvertM a
lookup_val key parse deflt event = prefix $ do
    maybe_val <- TrackLang.checked_val key (event_environ event)
    maybe (Right deflt) parse maybe_val
    where
    prefix = either (throw . ((Pretty.pretty key ++ ": ") ++)) return


-- * types

data Ly = Barline !(Maybe Meter.Meter) | LyNote !Note | LyRest !Rest
    | KeyChange !Key | Code !Code
    deriving (Show)

instance Pretty.Pretty Ly where pretty = untxt . to_lily

instance ToLily Ly where
    to_lily ly = case ly of
        Barline Nothing -> "|"
        Barline (Just meter) -> "| " <> "\\time " <> to_lily meter
        LyNote note -> to_lily note
        LyRest rest -> to_lily rest
        KeyChange key -> to_lily key
        Code code -> code

-- ** Note

data Note = Note {
    -- | @[]@ means this is a rest, and greater than one pitch indicates
    -- a chord.
    note_pitch :: !(NonEmpty (Text, Tie))
    , note_duration :: !Types.NoteDuration
    -- | Additional code to prepend to the note.
    , note_prepend :: !Code
    -- | Additional code to append to the note.
    , note_append :: !Code
    , note_stack :: !(Maybe Stack.UiFrame)
    } deriving (Show)

data Tie = NoTie | TieNeutral | TieUp | TieDown deriving (Show)

-- | Arbitrary bit of lilypond code.  This type isn't used for non-arbitrary
-- chunks, like 'note_pitch'.
type Code = Text

instance ToLily Note where
    to_lily (Note pitches dur prepend append _stack) =
        (prepend<>) . (<>append) $ case pitches of
            (pitch, tie) :| [] -> pitch <> ly_dur <> to_lily tie
            _ -> "<"
                <> Text.unwords [p <> to_lily tie | (p, tie)
                    <- NonEmpty.toList pitches]
                <> ">" <> ly_dur
        where ly_dur = to_lily dur

instance ToLily Tie where
    to_lily t = case t of
        NoTie -> ""
        TieNeutral -> "~"
        TieUp -> "^~"
        TieDown -> "_~"

-- ** Rest

data Rest = Rest {
    rest_duration :: !Types.NoteDuration
    -- | If true, the rest is emitted as @R@ and is expected to cover a whole
    -- measure.
    , rest_type :: !RestType
    , rest_prepend :: !Code
    , rest_append :: !Code
    } deriving (Show)

make_rest :: RestType -> Types.NoteDuration -> Rest
make_rest typ dur = Rest
    { rest_duration = dur
    , rest_type = typ
    , rest_prepend = ""
    , rest_append = ""
    }

data RestType = NormalRest | FullMeasure !Int | HiddenRest deriving (Show)

rest_time :: Rest -> Time
rest_time rest =
    Types.note_dur_to_time (rest_duration rest) * case rest_type rest of
        FullMeasure mult -> fromIntegral mult
        _ -> 1

instance ToLily Rest where
    to_lily (Rest dur typ prepend append) =
        (prepend<>) . (<>append) $ to_lily typ <> to_lily dur <> mult
        where
        mult = case typ of
            FullMeasure mult -> "*" <> showt mult
            _ -> ""

instance ToLily RestType where
    to_lily r = case r of
        NormalRest -> "r"
        FullMeasure {} -> "R"
        HiddenRest -> "s"

make_rests :: Types.Config -> Meter.Meter -> Time -> Time -> [Rest]
make_rests config meter start end
    | start >= end = []
    | full_measure = (:[]) $ make_rest
        (FullMeasure (Meter.time_num meter))
        (Types.NoteDuration (Meter.meter_denom meter) False)
    | otherwise = map (make_rest NormalRest) $ Meter.convert_duration meter
        (Types.config_dotted_rests config) True start (end - start)
    where
    full_measure = start `mod` measure == 0 && end - start >= measure
    measure = Meter.measure_time meter

-- ** Key

data Key = Key !Text !Mode deriving (Eq, Show)
type Mode = Text

instance ToLily Key where
    to_lily (Key tonic mode) = "\\key " <> tonic <> " \\" <> mode

parse_key :: Text -> Either String Key
parse_key key_name = do
    key <- maybe (Left $ "unknown key: " ++ untxt key_name) Right $
        Map.lookup (Pitch.Key key_name) Twelve.all_keys
    tonic <- Types.show_pitch_note (Theory.key_tonic key)
    mode <- maybe (Left $ "unknown mode: " ++ untxt (Theory.key_name key))
        Right $ Map.lookup (Theory.key_name key) modes
    return $ Key tonic mode
    where
    modes = Map.fromList
        [ ("min", "minor"), ("locrian", "locrian"), ("maj", "major")
        , ("dorian", "dorian"), ("phrygian", "phrygian"), ("lydian", "lydian")
        , ("mixo", "mixolydian")
        ]

-- ** voice

-- | Each Ly list should be the same duration and have the same number of
-- barlines.
newtype Voices = Voices [(Voice, [Ly])] deriving (Show)

data Voice = VoiceOne | VoiceTwo | VoiceThree | VoiceFour
    deriving (Eq, Ord, Show)

instance ToLily Voice where
    to_lily v = case v of
        VoiceOne -> "\\voiceOne"; VoiceTwo -> "\\voiceTwo"
        VoiceThree -> "\\voiceThree"; VoiceFour -> "\\voiceFour"

instance Pretty.Pretty Voice where pretty = show

parse_voice :: Int -> Maybe Voice
parse_voice v = case v of
    1 -> Just VoiceOne; 2 -> Just VoiceTwo
    3 -> Just VoiceThree; 4 -> Just VoiceFour
    _ -> Nothing

-- * Event

-- | Clip off the part of the event before the given time, or Nothing if it
-- was entirely clipped off.
clip_event :: Time -> Event -> Maybe Event
clip_event end e
    | left <= 0 = Nothing
    | otherwise = Just $
        e { event_start = end, event_duration = left, event_clipped = True }
    where left = event_end e - end

attrs_to_code :: Score.Attributes -> Score.Attributes -> Code
attrs_to_code prev_attrs attrs = mconcat $ mconcat $
    [ [code | (attr, code) <- simple_articulations, has attr]
    , [start | (attr, start, _) <- modal_articulations,
        has attr, not (prev_has attr)]
    , [end | (attr, _, end) <- modal_articulations,
        not (has attr), prev_has attr]
    ]
    where
    has = Score.attrs_contain attrs
    prev_has = Score.attrs_contain prev_attrs
