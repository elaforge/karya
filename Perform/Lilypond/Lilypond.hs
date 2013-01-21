{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Convert from Score events to a lilypond score.
module Perform.Lilypond.Lilypond (
    module Perform.Lilypond.Lilypond
    , module Perform.Lilypond.Types
) where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Meter as Meter
import Perform.Lilypond.Meter (TimeSignature)
import Perform.Lilypond.Types
import qualified Perform.Pitch as Pitch

import Types


-- * constants

-- | String: @\'right\'@ or @\'left\'@.
v_hand :: TrackLang.ValName
v_hand = TrackLang.Symbol "hand"

-- | String: whatever @\\clef@ accepts, defaults to @\'treble\'@.
v_clef :: TrackLang.ValName
v_clef = TrackLang.Symbol "clef"

-- | String: should be parseable by 'Meter.parse_signature',
-- e.g. @\'3/4\'@.
v_time_signature :: TrackLang.ValName
v_time_signature = TrackLang.Symbol "time-sig"

-- | String: in place of the note, include this lilypond code directly.
v_ly_code :: TrackLang.ValName
v_ly_code = TrackLang.Symbol "ly-code"

v_ly_code_prepend :: TrackLang.ValName
v_ly_code_prepend = TrackLang.Symbol "ly-code-prepend"

v_ly_code_append :: TrackLang.ValName
v_ly_code_append = TrackLang.Symbol "ly-code-append"

-- * types

default_config :: RealTime -> Config
default_config quarter = Config
    { config_quarter_duration = quarter
    , config_dotted_rests = False
    , config_dynamics =
        map (first (/0xff)) [(0x40, "p"), (0x80, "mf"), (0xff, "f")]
    }

-- ** Event

data Event = Event {
    event_start :: !Time
    , event_duration :: !Time
    , event_pitch :: !String
    , event_instrument :: !Score.Instrument
    , event_dynamic :: !Double
    , event_environ :: !TrackLang.Environ
    , event_stack :: !Stack.Stack
    } deriving (Show)

event_end :: Event -> Time
event_end event = event_start event + event_duration event

event_attributes :: Event -> Score.Attributes
event_attributes = Score.environ_attributes . event_environ

instance Pretty.Pretty Event where
    format (Event start dur pitch inst dyn attrs _stack) =
        Pretty.constructor "Event" [Pretty.format start, Pretty.format dur,
            Pretty.text pitch, Pretty.format inst, Pretty.format dyn,
            Pretty.format attrs]

-- ** Note

data Note = Note {
    -- _* functions are partial.
    -- | @[]@ means this is a rest, and greater than one pitch indicates
    -- a chord.
    _note_pitch :: ![String]
    , _note_duration :: !NoteDuration
    , _note_tie :: !Bool
    -- | Additional code to prepend to the note.
    , _note_prepend :: !String
    -- | Additional code to append to the note.
    , _note_append :: !String
    , _note_stack :: !(Maybe Stack.UiFrame)
    }
    | ClefChange String
    | KeyChange Key
    | TimeChange TimeSignature
    | Code !String
    deriving (Show)

rest :: NoteDuration -> Note
rest dur = Note
    { _note_pitch = []
    , _note_duration = dur
    , _note_tie = False
    , _note_prepend = ""
    , _note_append = ""
    , _note_stack = Nothing
    }

is_rest :: Note -> Bool
is_rest note@(Note {}) = null (_note_pitch note)
is_rest _ = False

is_note :: Note -> Bool
is_note (Note {}) = True
is_note _ = False

instance ToLily Note where
    to_lily (Note pitches dur tie prepend append _stack) =
        (prepend++) . (++append) $ case pitches of
            [] -> 'r' : ly_dur
            [pitch] -> pitch ++ ly_dur
            _ -> '<' : unwords pitches ++ ">" ++ ly_dur
        where ly_dur = to_lily dur ++ if tie then "~" else ""
    to_lily (ClefChange clef) = "\\clef " ++ clef
    to_lily (KeyChange (tonic, mode)) = "\\key " ++ tonic ++ " \\" ++ mode
    to_lily (TimeChange tsig) = "\\time " ++ to_lily tsig
    to_lily (Code code) = code

note_time :: Note -> Time
note_time note@(Note {}) = note_dur_to_time (_note_duration note)
note_time _ = 0

note_stack :: Note -> Maybe Stack.UiFrame
note_stack note@(Note {}) = _note_stack note
note_stack _ = Nothing

-- * time signature

-- | Get a time signature map for the events.  There is one TimeSignature for
-- each measure.
extract_time_signatures :: [Event] -> Either String [TimeSignature]
extract_time_signatures events = go 0 Meter.default_signature events
    where
    go _ _ [] = Right []
    go at prev_tsig events = do
        tsig <- maybe (return prev_tsig) lookup_time_signature (Seq.head events)
        let end = at + Meter.measure_time tsig
        rest <- go end tsig (dropWhile ((<=end) . event_end) events)
        return $ tsig : rest

    lookup_time_signature = do
        lookup_val v_time_signature Meter.parse_signature
            Meter.default_signature
    lookup_val :: TrackLang.ValName -> (String -> Either String a) -> a -> Event
        -> Either String a
    lookup_val key parse deflt event = prefix $ do
        maybe_val <- TrackLang.checked_val key (event_environ event)
        maybe (Right deflt) parse maybe_val
        where
        prefix = either (Error.throwError . ((Pretty.pretty key ++ ": ") ++))
            return

-- * convert

type ConvertM a = State.StateT State (Error.ErrorT String Identity.Identity) a

data State = State {
    -- constant
    state_config :: Config
    -- change on each measure
    , state_times :: [TimeSignature]
    , state_measure_start :: Time
    , state_measure_end :: Time

    -- change on each note
    -- | End of the previous note.
    , state_note_end :: Time
    , state_dynamic :: Maybe String
    , state_clef :: Maybe Clef
    , state_key :: Maybe Key
    } deriving (Show)

-- | Turn Events, which are in absolute Time, into Notes, which are divided up
-- into tied Durations depending on the time signature.  The Notes are divided
-- up by measure.
convert_measures :: Config -> [TimeSignature] -> [Event]
    -> Either String [[Note]]
convert_measures config time_sigs events =
    run_convert initial $ add_time_changes <$> go events
    where
    initial = State config time_sigs 0 0 0 Nothing Nothing Nothing
    go [] = return []
    go events = do
        (measure, events) <- convert_measure events
        measures <- go events
        return (measure : measures)

    -- Add TimeChanges when the time signature changes, and pad with empty
    -- measures until I run out of time signatures.
    add_time_changes = map add_time . Seq.zip_padded2 (Seq.zip_prev time_sigs)
    add_time ((prev_tsig, tsig), maybe_measure) = time_change
        ++ fromMaybe (make_rests config tsig 0 (Meter.measure_time tsig))
            maybe_measure
        where time_change = [TimeChange tsig | maybe True (/=tsig) prev_tsig]

-- | This is a simplified version of 'convert_measures', designed for
-- converting little chunks of lilypond that occur in other expressions.
-- So it doesn't handle clef changes, time signature changes, or even barlines.
simple_convert :: Config -> TimeSignature -> Time -> [Event] -> [Note]
simple_convert config_ time_sig = go
    where
    config = config_ { config_dynamics = [] }
    go _ [] = []
    go start (event : events) = leading_rests ++ [note] ++ go end rest_events
        where
        leading_rests = make_rests config time_sig start (event_start event)
        (note, end, rest_events) =
            convert_note config Nothing time_sig event events

-- TODO The time signatures are still not correct.  Since time sig is only on
-- notes, I can't represent a time sig change during silence.  I would need to
-- generate something other than notes, or create a silent note for each time
-- sig change.
convert_measure :: [Event] -> ConvertM ([Note], [Event])
convert_measure events = case events of
    [] -> return ([], []) -- Out of events at the beginning of a measure.
    first_event : _ -> do
        tsig <- State.gets state_times >>= \x -> case x of
            [] -> Error.throwError $
                "out of time signatures but not out of events: "
                ++ show first_event
            tsig : tsigs -> do
                State.modify $ \state -> state { state_times = tsigs }
                return tsig
        event_tsig <- lookup_time_signature first_event
        when (event_tsig /= tsig) $
            Error.throwError $
                "inconsistent time signatures, analysis says it should be "
                ++ show tsig ++ " but the event has " ++ show event_tsig
        State.modify $ \state -> state
            { state_measure_start = state_measure_end state
            , state_measure_end =
                state_measure_end state + Meter.measure_time tsig
            }
        measure1 tsig events
    where
    measure1 tsig [] = trailing_rests tsig []
    measure1 tsig (event : events) = do
        state <- State.get
        -- This assumes that events that happen at the same time all have the
        -- same clef and key.
        measure_end <- State.gets state_measure_end
        if event_start event >= measure_end
            then trailing_rests tsig (event : events)
            else note_column state tsig event events
    note_column state tsig event events = do
        clef <- lookup_clef event
        let clef_change = [ClefChange clef | Just clef /= state_clef state]
        key <- lookup_key event
        let key_change = [KeyChange key | Just key /= state_key state]
        let (note, end, rest_events) = convert_note (state_config state)
                (state_dynamic state) tsig event events
            leading_rests = make_rests (state_config state) tsig
                (state_note_end state) (event_start event)
            notes = leading_rests ++ clef_change ++ key_change ++ [note]
        State.modify $ \state -> state
            { state_clef = Just clef
            , state_key = Just key
            , state_dynamic = Just $
                get_dynamic (config_dynamics (state_config state)) event
            , state_note_end = end
            }
        (rest_notes, rest_events) <- measure1 tsig rest_events
        return (notes ++ rest_notes, rest_events)
    trailing_rests tsig events = do
        state <- State.get
        let end = state_measure_start state + Meter.measure_time tsig
        let rests = make_rests (state_config state) tsig
                (state_note_end state) end
        State.modify $ \state -> state { state_note_end = end }
        return (rests, events)

convert_note :: Config -> Maybe String -> TimeSignature -> Event -> [Event]
    -> (Note, Time, [Event]) -- ^ (note, note end time, remaining events)
convert_note config prev_dynamic tsig event events
    | Just code <- TrackLang.maybe_val v_ly_code env =
        (Code code, start + event_duration event, events)
    | otherwise = (note, end, clipped ++ rest)
    where
    env = event_environ event
    note = Note
        { _note_pitch = map event_pitch here
        , _note_duration = allowed_dur
        , _note_tie = any (> end) (map event_end here)
        , _note_prepend =
            fromMaybe "" (TrackLang.maybe_val v_ly_code_prepend env)
        , _note_append = fromMaybe "" (TrackLang.maybe_val v_ly_code_append env)
            ++ dynamic_to_code (config_dynamics config) prev_dynamic event
        , _note_stack = Seq.last (Stack.to_ui (event_stack event))
        }
    (here, rest) = break ((> start) . event_start) (event : events)
    allowed = min (max_end - start) (allowed_time_dotted tsig False start)
    allowed_dur = time_to_note_dur allowed
    allowed_time = note_dur_to_time allowed_dur
    -- Maximum end, the actual end may be shorter since it has to conform to
    -- a Duration.
    max_end = fromMaybe (event_end event) $
        Seq.minimum (next ++ map event_end here)
    clipped = mapMaybe (clip_event end) here
    start = event_start event
    end = start + allowed_time
    next = maybe [] ((:[]) . event_start) (Seq.head rest)

make_rests :: Config -> TimeSignature -> Time -> Time -> [Note]
make_rests config tsig start end
    | start < end = map rest $ convert_duration tsig
        (config_dotted_rests config) True start (end - start)
    | otherwise = []


-- ** util

run_convert :: State -> ConvertM a -> Either String a
run_convert state = fmap fst . Identity.runIdentity . Error.runErrorT
    . flip State.runStateT state

lookup_time_signature :: Event -> ConvertM TimeSignature
lookup_time_signature = lookup_val v_time_signature Meter.parse_signature
    Meter.default_signature

lookup_clef :: Event -> ConvertM Clef
lookup_clef = lookup_val v_clef Right default_clef

default_clef :: Clef
default_clef = "treble"

lookup_key :: Event -> ConvertM Key
lookup_key = lookup_val TrackLang.v_key parse_key default_key

default_key :: Key
default_key = ("c", "major")

lookup_val :: TrackLang.ValName -> (String -> Either String a) -> a -> Event
    -> ConvertM a
lookup_val key parse deflt event = prefix $ do
    maybe_val <- TrackLang.checked_val key (event_environ event)
    maybe (Right deflt) parse maybe_val
    where
    prefix = either (Error.throwError . ((Pretty.pretty key ++ ": ") ++))
        return

get_dynamic :: DynamicConfig -> Event -> String
get_dynamic dynamics event = get dynamics (event_dynamic event)
    where
    get dynamics dyn = case dynamics of
        [] -> ""
        ((val, dyn_str) : dynamics)
            | null dynamics || val >= dyn -> dyn_str
            | otherwise -> get dynamics dyn

dynamic_to_code :: DynamicConfig -> Maybe String -> Event -> String
dynamic_to_code dynamics prev_dyn event
    | not (null dyn) && prev_dyn /= Just dyn = '\\' : dyn
    | otherwise = ""
    where dyn = get_dynamic dynamics event

-- | Clip off the part of the event before the given time, or Nothing if it
-- was entirely clipped off.
clip_event :: Time -> Event -> Maybe Event
clip_event end e
    | left <= 0 = Nothing
    | otherwise = Just $ e { event_start = end, event_duration = left }
    where left = event_end e - end

-- | Given a starting point and a duration, emit the list of Durations
-- needed to express that duration.
convert_duration :: TimeSignature -> Bool -> Bool -> Time -> Time
    -> [NoteDuration]
convert_duration sig use_dot is_rest pos time_dur
    | time_dur <= 0 = []
    | allowed >= time_dur = to_durs time_dur
    | otherwise = dur
        : convert_duration sig use_dot is_rest
            (pos + allowed) (time_dur - allowed)
    where
    dur = time_to_note_dur allowed
    allowed = (if use_dot
        then allowed_time_dotted else allowed_time_plain) sig is_rest pos
    to_durs = if use_dot then time_to_note_durs
        else map (flip NoteDuration False) . time_to_durs

-- | Figure out how much time a note at the given position should be allowed
-- before it must tie.
--
-- TODO return Duration
allowed_time_plain :: TimeSignature -> Bool -> Time -> Time
allowed_time_plain sig is_rest = dur_to_time . fst . time_to_dur
    . allowed_time sig is_rest

-- | This is like 'allowed_time_plain', but will return dotted rhythms.
--
-- TODO return a NoteDuration
allowed_time_dotted :: TimeSignature -> Bool
    -> Time -> Time
allowed_time_dotted sig is_rest pos = note_dur_to_time $ time_to_note_dur $
    allowed_time sig is_rest pos

allowed_time :: TimeSignature -> Bool
    -- ^ True if this is a rest.  The algorithm for note durations is greedy,
    -- in that it will seek to find the longest note that doesn't span a
    -- beat whose rank is too low.  But that results in rests being spelled
    -- @c4 r2 r4@ instead of @c4 r4 r2@.  Unlike notes, all rests are the same.
    -- So rests will pick the duration that ends on the lowest rank.
    --
    -- TODO this uses only undotted rhythms, which is ok since I don't like
    -- dotted rests.
    -> Time -> Time
allowed_time sig is_rest start_ = subtract start $
    (if is_rest then best_duration else id) $ fromMaybe measure $
        Meter.find_rank start (rank - if is_duple then 2 else 1) sig
    where
    -- Try notes up to the end, select the one that lands on the lowest rank.
    best_duration end = fromMaybe (start + 1) $
        Seq.minimum_on (Meter.rank_at sig) candidates
        where
        candidates = takeWhile (<=end) $ map ((+start) . dur_to_time) durs
    durs = reverse [D1 .. D128]
    start = start_ `mod` measure
    rank = Meter.rank_at sig start
    is_duple = case Meter.time_nums sig of
        [num] -> (==0) $ snd $ properFraction $ logBase 2 (fromIntegral num)
        _ -> False
    measure = Meter.measure_time sig

-- * types

type Title = String
type Mode = String
type Clef = String
-- | (tonic, Mode)
type Key = (String, Mode)

parse_key :: String -> Either String Key
parse_key key_name = do
    key <- maybe (Left $ "unknown key: " ++ key_name) Right $
        Map.lookup (Pitch.Key key_name) Twelve.all_keys
    tonic <- show_pitch_note (Theory.key_tonic key)
    mode <- maybe (Left $ "unknown mode: " ++ Theory.key_name key) Right $
        Map.lookup (Theory.key_name key) modes
    return (tonic, mode)
    where
    modes = Map.fromList
        [ ("min", "minor"), ("locrian", "locrian"), ("maj", "major")
        , ("dorian", "dorian"), ("phrygian", "phrygian"), ("lydian", "lydian")
        , ("mixo", "mixolydian")
        ]

-- * split staves

-- | If the staff group has >1 staff, it is bracketed as a grand staff.
data StaffGroup = StaffGroup Score.Instrument [Staff]
    deriving (Show)

-- | List of measures, where each measure is a list of Notes.
data Staff = Staff [[Note]] deriving (Show)

-- | Group a stream of events into individual staves based on instrument, and
-- for keyboard instruments, left or right hand.  Then convert each staff of
-- Events to Notes, divided up into measures.
convert_staff_groups :: Config -> [Event] -> Either String [StaffGroup]
convert_staff_groups config events = do
    let staff_groups = split_events events
    time_sigs <- get_time_signatures staff_groups
    forM staff_groups $ \(inst, staves) ->
        staff_group config time_sigs inst staves

-- | Get the per-measure time signatures from the longest staff and verify it
-- against the time signatures from the other staves.
get_time_signatures :: [(Score.Instrument, [[Event]])]
    -> Either String [TimeSignature]
get_time_signatures staff_groups = do
    let with_inst inst = error_context ("staff for " ++ Pretty.pretty inst)
    with_tsigs <- forM staff_groups $ \(inst, staves) -> with_inst inst $ do
        time_sigs <- mapM extract_time_signatures staves
        return (inst, zip time_sigs staves)
    let flatten (_inst, measures) = map fst measures
        maybe_longest = Seq.maximum_on length (concatMap flatten with_tsigs)
    when_just maybe_longest $ \longest -> forM_ with_tsigs $ \(inst, staves) ->
        with_inst inst $ forM_ staves $ \(time_sigs, _measures) ->
            unless (time_sigs `List.isPrefixOf` longest) $
                Left $ "inconsistent time signatures: "
                    ++ Pretty.pretty time_sigs ++ " is not a prefix of "
                    ++ Pretty.pretty longest
    return $ fromMaybe [] maybe_longest

error_context :: String -> Either String a -> Either String a
error_context msg = either (Left . ((msg ++ ": ") ++)) Right

split_events :: [Event] -> [(Score.Instrument, [[Event]])]
split_events events =
    [(inst, Seq.group_on (lookup_hand . event_environ) events)
        | (inst, events) <- by_inst]
    where
    by_inst = Seq.keyed_group_on event_instrument events
    lookup_hand environ = case TrackLang.get_val v_hand environ of
        Right (val :: String)
            | val == "right" -> 0
            | val == "left" -> 1
            | otherwise -> 2
        _ -> 0

-- | Right hand goes at the top, left hand goes at the bottom.  Any other hands
-- goe below that.  Events that are don't have a hand are assumed to be in the
-- right hand.
staff_group :: Config -> [TimeSignature] -> Score.Instrument -> [[Event]]
    -> Either String StaffGroup
staff_group config time_sigs inst staves = do
    staff_measures <- mapM (convert_measures config time_sigs) staves
    return $ StaffGroup inst $ map (Staff . promote_annotations) staff_measures

-- | Normally clef or key changes go right before the note with the changed
-- status.  But if there are leading rests, the annotations should go at the
-- beginning of the score.  It's more complicated because I have to skip
-- leading measures of rests.
promote_annotations :: [[Note]] -> [[Note]]
promote_annotations measures = case empty ++ stripped of
        [] -> []
        measure : measures -> (annots ++ measure) : measures
    where
    -- Yack.  There must be a better way.
    (empty, rest_measures) = span (all not_note) measures
    (annots, stripped) = strip rest_measures
    strip [] = ([], [])
    strip (measure:measures) = (annots, (pre ++ rest) : measures)
        where
        (pre, post) = span not_note measure
        (annots, rest) = span is_annot post
    not_note n = is_time n || is_rest n
    is_annot (Note {}) = False
    is_annot (Code {}) = False
    is_annot _ = True
    is_time (TimeChange {}) = True
    is_time _ = False

-- * make_ly

-- | Same as 'Cmd.Cmd.StackMap', but I don't feel like importing Cmd here.
type StackMap = Map.Map Int Stack.UiFrame

make_ly :: Config -> Title -> [Event] -> Either String ([Text.Text], StackMap)
make_ly config title events =
    ly_file title <$> convert_staff_groups config events

inst_name :: Score.Instrument -> String
inst_name = dropWhile (=='/') . dropWhile (/='/') . Score.inst_name

show_pitch :: Theory.Pitch -> Either String String
show_pitch pitch = (++ oct_mark) <$> show_pitch_note note
    where
    (octave, note) = Theory.pitch_c_octave pitch
    oct_mark = let oct = octave - 5
        in if oct >= 0 then replicate oct '\'' else replicate (abs oct) ','

show_pitch_note :: Theory.Note -> Either String String
show_pitch_note (Theory.Note pc accs) = do
    acc <- case accs of
        -2 -> Right "ff"
        -1 -> Right "f"
        0 -> Right ""
        1 -> Right "s"
        2 -> Right "ss"
        _ -> Left $ "too many accidentals: " ++ show accs
    return $ Theory.pc_char pc : acc


-- * output

ly_file :: Title -> [StaffGroup] -> ([Text.Text], StackMap)
ly_file title staff_groups = run_output $ do
    outputs
        [ "\\version" <+> str "2.14.2"
        , "\\language" <+> str "english"
        , "\\header { title =" <+> str title <+> "tagline = \"\" }"
        , "\\score { <<"
        ]
    mapM_ ly_staff_group staff_groups
    outputs [">> }"]
    where
    str text = "\"" <> Text.pack text <> "\""
    x <+> y = x <> " " <> y

    ly_staff_group (StaffGroup inst staves) = case staves of
        [staff] -> do
            output "\n"
            ly_staff inst staff
        _ -> do
            outputs ["\n\\new PianoStaff <<"]
            mapM_ (ly_staff inst) staves
            output ">>\n"
    ly_staff inst (Staff measures) = do
        output "\\new Staff {"
        output $ "\\set Staff.instrumentName =" <+> str (inst_name inst)
            <> "\n\\set Staff.shortInstrumentName =" <+> str (inst_name inst)
            <> "\n{\n"
        mapM_ show_measures (zip [0, 4 ..] (group 4 measures))
        output "} }\n"
    -- Show 4 measures per line and comment with the measure number.
    show_measures (num, measures) = do
        output "  "
        mapM_ show_measure measures
        output $ "%" <+> Text.pack (show num) <> "\n"
    show_measure notes = do
        mapM_ show_note notes
        output "| "
    show_note note = do
        when_just (note_stack note) record_stack
        output $ Text.pack (to_lily note) <> " "
    group _ [] = []
    group n ms = let (pre, post) = splitAt n ms in pre : group n post

type Output a = State.State OutputState a

run_output :: Output a -> ([Text.Text], StackMap)
run_output m = (reverse (output_chunks state), output_map state)
    where state = State.execState m (OutputState [] Map.empty 1)

data OutputState = OutputState {
    -- | Chunks of text to write, in reverse order.  I could use
    -- Text.Lazy.Builder, but this is simpler and performance is probably ok.
    output_chunks :: ![Text.Text]
    , output_map :: !StackMap
    -- | Running sum of the length of the chunks.
    , output_char_num :: !Int
    } deriving (Show)

outputs :: [Text.Text] -> Output ()
outputs = output . Text.unlines

output :: Text.Text -> Output ()
output text = State.modify $ \(OutputState chunks omap num) ->
    OutputState (text:chunks) omap (num + Text.length text)

record_stack :: Stack.UiFrame -> Output ()
record_stack stack = State.modify $ \st -> st { output_map =
    Map.insert (output_char_num st) stack (output_map st) }
