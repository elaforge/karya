{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Convert from Score events to a lilypond score.
module Perform.Lilypond.Lilypond where
import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import Util.Control
import qualified Util.ParseBs as ParseBs
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Cmd.Cmd as Cmd
import qualified Derive.Attrs as Attrs
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- * constants

v_hand :: TrackLang.ValName
v_hand = TrackLang.Symbol "hand"

v_clef :: TrackLang.ValName
v_clef = TrackLang.Symbol "clef"

-- * types

-- | Configure how the lilypond score is generated.
data Config = Config {
    -- | Allow dotted rests?
    config_dotted_rests :: Bool
    -- | If non-null, generate dynamics from each event's dynamic control.
    -- This has cutoffs for each dynamic level, which should be \"p\", \"mf\",
    -- etc.
    , config_dynamics :: [(Double, String)]
    } deriving (Show)

default_config :: Config
default_config = Config
    { config_dotted_rests = False
    , config_dynamics =
        map (first (/0xff)) [(0x40, "p"), (0x80, "mf"), (0xff, "f")]
    }

-- | Convert a value to its lilypond representation.
-- TODO go to Pretty.Doc instead of String?
class ToLily a where
    to_lily :: a -> String

-- | Time in score units.  The maximum resolution is a 128th note, so one unit
-- is 128th of a whole note.
newtype Time = Time Int deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Pretty.Pretty Time where
    pretty (Time t) = show t ++ "t"

time_per_whole :: Time
time_per_whole = Time 128

-- | This time duration measured as the fraction of a whole note.
data Duration = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
    deriving (Enum, Eq, Ord, Show)

data NoteDuration = NoteDuration Duration Bool
    deriving (Eq, Show)

read_duration :: String -> Maybe Duration
read_duration s = case s of
    -- GHC incorrectly reports overlapping patterns.  This bug is fixed in 7.4.
    "1" -> Just D1; "2" -> Just D2; "4" -> Just D4; "8" -> Just D8
    "16" -> Just D16; "32" -> Just D32; "64" -> Just D64; "128" -> Just D128
    _ -> Nothing

instance ToLily Duration where
    to_lily = drop 1 . show

instance ToLily NoteDuration where
    to_lily (NoteDuration dur dot) = to_lily dur ++ if dot then "." else ""

data TimeSignature = TimeSignature
    { time_num :: !Int, time_denom :: !Duration }
    deriving (Show)

instance ToLily TimeSignature where
    to_lily (TimeSignature num denom) = show num ++ "/" ++ to_lily denom

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
    -- | [] means this is a rest, and greater than one pitch indicates a chord.
    _note_pitch :: ![String]
    , _note_duration :: !NoteDuration
    , _note_tie :: !Bool
    -- | Additional code to append to the note.
    , _note_code :: !String
    , _note_stack :: !(Maybe Stack.UiFrame)
    }
    | Clef String
    deriving (Show)

rest :: NoteDuration -> Note
rest dur = Note [] dur False "" Nothing

instance ToLily Note where
    to_lily (Note pitches dur tie code _stack) = case pitches of
            [] -> 'r' : ly_dur ++ code
            [pitch] -> pitch ++ ly_dur ++ code
            _ -> '<' : unwords pitches ++ ">" ++ ly_dur ++ code
        where ly_dur = to_lily dur ++ if tie then "~" else ""
    to_lily (Clef clef) = "\\clef " ++ clef

note_time :: Note -> Time
note_time note@(Note {}) = note_dur_to_time (_note_duration note)
note_time (Clef {}) = 0

note_stack :: Note -> Maybe Stack.UiFrame
note_stack note@(Note {}) = _note_stack note
note_stack _ = Nothing

-- * convert

-- | Turn Events, which are in absolute Time, into Notes, which are divided up
-- into tied Durations depending on the time signature.
convert_notes :: Config -> TimeSignature -> Time -> [Event] -> [Note]
convert_notes config sig end events = go Nothing 0 events
    where
    go :: Maybe Event -> Time -> [Event] -> [Note]
    go _ prev_end [] = trailing_rests prev_end
    go prev_event prev_end events@(event:_) =
        mkrests prev_end start ++ clef_change ++ note
            : go (Just event) (start + allowed_time) (clipped ++ rest)
        where
        note = Note
            { _note_pitch = map event_pitch here
            , _note_duration = allowed_dur
            , _note_tie = any (> start + allowed_time) (map event_end here)
            , _note_code = attributes_to_code (event_attributes event)
                ++ dynamic_to_code config prev_event event
            , _note_stack = Seq.last (Stack.to_ui (event_stack event))
            }
        clef_change
            | maybe True ((/= cur) . event_clef) prev_event = [Clef cur]
            | otherwise = []
            where cur = event_clef event
        (here, rest) = break ((>start) . event_start) events
        end = subtract start $ fromMaybe (event_end event) $
            Seq.minimum (next ++ map event_end here)
        next = maybe [] ((:[]) . event_start) (Seq.head rest)
        allowed = min end $ allowed_dotted_time sig start
        allowed_dur = time_to_note_dur allowed
        allowed_time = note_dur_to_time allowed_dur
        clipped = mapMaybe (clip_event (start + allowed_time)) here
        start = event_start event

    mkrests prev start
        | prev < start = map rest $ convert_duration sig
            (config_dotted_rests config) prev (start - prev)
        | otherwise = []

    trailing_rests prev = map rest $
        convert_duration sig (config_dotted_rests config) prev
            (max 0 (end - prev))

event_clef :: Event -> String
event_clef event = case TrackLang.lookup_val v_clef (event_environ event) of
    Right val -> val
    _ -> "treble"

-- | Guess a dynamic from the dyn control.
get_dynamic :: [(Double, String)] -> Double -> String
get_dynamic dynamics dyn = case dynamics of
    [] -> ""
    ((val, dyn_str) : dynamics)
        | null dynamics || val >= dyn -> dyn_str
        | otherwise -> get_dynamic dynamics dyn

dynamic_to_code :: Config -> Maybe Event -> Event -> String
dynamic_to_code config prev_event event =
    to_code (get <$> prev_event) (get event)
    where
    get = get_dynamic (config_dynamics config) . event_dynamic
    to_code :: Maybe String -> String -> String
    to_code prev_dyn dyn
        | not (null dyn) && maybe True (/=dyn) prev_dyn = '\\':dyn
        | otherwise = ""

attributes_to_code :: Score.Attributes -> String
attributes_to_code =
    concat . mapMaybe (flip Map.lookup attributes . Score.attr)
        . Score.attrs_list
    where
    attributes = Map.fromList
        [ (Attrs.trill, "\\trill")
        , (Attrs.trem, ":32")
        ]

-- | Clip off the part of the event before the given time, or Nothing if it
-- was entirely clipped off.
clip_event :: Time -> Event -> Maybe Event
clip_event end e
    | left <= 0 = Nothing
    | otherwise = Just $ e { event_start = end, event_duration = left }
    where left = event_end e - end

-- | Given a starting point and a duration, emit the list of Durations
-- needed to express that duration.
convert_duration :: TimeSignature -> Bool -> Time -> Time -> [NoteDuration]
convert_duration sig use_dot pos time_dur
    | time_dur <= 0 = []
    | allowed >= time_dur = to_durs time_dur
    | otherwise = dur
        : convert_duration sig use_dot (pos + allowed) (time_dur - allowed)
    where
    dur = time_to_note_dur allowed
    allowed = (if use_dot then allowed_dotted_time else allowed_time) sig pos
    to_durs = if use_dot then time_to_note_durs
        else map (flip NoteDuration False) . time_to_durs

-- | Figure out how much time a note at the given position should be allowed
-- before it must tie.
-- TODO Only supports duple time signatures.
allowed_dotted_time :: TimeSignature -> Time -> Time
allowed_dotted_time sig measure_pos
    | pos == 0 = measure
    | otherwise = min measure next - pos
    where
    pos = measure_pos `mod` measure
    measure = measure_time sig
    level = log2 pos + 2
    -- TODO inefficient way to find the next power of 2 greater than pos.
    -- There must be a direct way.
    next = Maybe.fromJust (List.find (>pos) [0, 2^level ..])

-- | Like 'allowed_dotted_time', but only emit powers of two.
allowed_time :: TimeSignature -> Time -> Time
allowed_time sig pos = 2 ^ (log2 (allowed_dotted_time sig pos))

-- * split_measures

split_measures :: TimeSignature -> [Note] -> [[Note]]
split_measures sig = go
    where
    go [] = []
    go ns = pre : go post
        where (pre, post) = split 0 ns
    split _ [] = ([], [])
    split prev (n:ns)
        | t > measure = ([], n:ns)
        | otherwise = let (pre, post) = split t ns in (n:pre, post)
        where t = prev + note_time n
    measure = measure_time sig

-- * duration / time conversion

note_dur_to_time :: NoteDuration -> Time
note_dur_to_time (NoteDuration dur dotted) =
    dur_to_time dur + if dotted && dur /= D128 then dur_to_time (succ dur)
        else 0

dur_to_time :: Duration -> Time
dur_to_time dur = Time $ whole `div` case dur of
    D1 -> 1; D2 -> 2; D4 -> 4; D8 -> 8
    D16 -> 16; D32 -> 32; D64 -> 64; D128 -> 128
    where Time whole = time_per_whole

time_to_note_dur :: Time -> NoteDuration
time_to_note_dur t = case time_to_durs t of
    [d1, d2] | d2 == succ d1 -> NoteDuration d1 True
    d : _ -> NoteDuration d False
    -- I have no 0 duration, so I'm forced to pick something.
    [] -> NoteDuration D1 False

-- | This rounds up to the next Duration, so any Time over a half note will
-- wind up as a whole note.
time_to_dur :: Time -> Duration
time_to_dur (Time time) =
    toEnum $ min (fromEnum D128) (log2 (whole `div` time))
    where Time whole = time_per_whole

time_to_note_durs :: Time -> [NoteDuration]
time_to_note_durs t
    | t > 0 = dur : time_to_note_durs (t - note_dur_to_time dur)
    | otherwise = []
    where dur = time_to_note_dur t

time_to_durs :: Time -> [Duration]
time_to_durs (Time time) =
    map fst $ filter ((/=0) . snd) $ reverse $ zip durs (binary time)
    where
    durs = [D128, D64 ..]
    binary rest
        | rest > 0 = m : binary d
        | otherwise = []
        where (d, m) = rest `divMod` 2

-- | Integral log2.  So 63 is 0, because it divides by 2 zero times.
log2 :: (Integral a) => a -> Int
log2 = go 0
    where
    go n val
        | div > 0 && mod == 0 = go (n+1) div
        | otherwise = n
        where (div, mod) = val `divMod` 2

measure_time :: TimeSignature -> Time
measure_time sig = Time (time_num sig) * dur_to_time (time_denom sig)

measure_duration :: TimeSignature -> Duration
measure_duration (TimeSignature num denom) =
    time_to_dur $ Time num * dur_to_time denom

-- * score

data Score = Score {
    score_title :: String
    , score_time :: TimeSignature
    -- | (tonic, Mode)
    , score_key :: (String, Mode)
    } deriving (Show)

data Mode = Major | Minor deriving (Show)

meta_ly :: String
meta_ly = "ly"

meta_title, meta_time_signature :: String
meta_title = "ly.title"
meta_time_signature = "ly.time-signature"
-- meta_duration1 = "ly.duration1"

-- | This was used by the automatic lilypond derivation, but is no longer
-- used.  TODO remove it someday
meta_to_score :: Maybe Pitch.Key -> Map.Map String String
    -> Maybe (Either String Score)
meta_to_score maybe_score_key meta = case Map.lookup meta_ly meta of
    Nothing -> Nothing
    Just _ -> Just $ do
        score_key <- maybe (Left "key required") return maybe_score_key
        key <- parse_key score_key
        time_sig <- parse_time_signature $ get "4/4" meta_time_signature
        return $ Score
            { score_title = title
            , score_time = time_sig
            , score_key = key
            }
    where
    title = get "" meta_title
    get deflt k = Map.findWithDefault deflt k meta

parse_key :: Pitch.Key -> Either String (String, Mode)
parse_key key = case Map.lookup key Twelve.all_keys of
    Nothing -> Left $ "key not found: " ++ show key
    Just k -> do
        tonic <- show_pitch_note (Theory.key_tonic k)
        mode <- case Theory.key_name k of
            "maj" -> Right Major
            "min" -> Right Minor
            -- Actually lilypond supports a few church modes too.
            mode -> Left $ "unknown mode: " ++ show mode
        return (tonic, mode)

parse_time_signature :: String -> Either String TimeSignature
parse_time_signature sig = do
    let (num, post) = break (=='/') sig
        unparseable = Left $ "signature must be ##/##: " ++ show sig
    denom <- case post of
        '/' : d -> return d
        _ -> unparseable
    TimeSignature <$> maybe unparseable return (ParseBs.int num)
        <*> maybe unparseable return (read_duration denom)

-- * split staves

-- | If the staff group has >1 staff, it is bracketed as a grand staff.
data StaffGroup = StaffGroup Score.Instrument [Staff]
    deriving (Show)

-- | List of measures, where each measure is a list of Notes.
data Staff = Staff [[Note]] deriving (Show)

-- | Group a stream of events into individual staves based on instrument, and
-- for keyboard instruments, left or right hand.
split_staves :: Config -> TimeSignature -> [Event] -> [StaffGroup]
split_staves config time_sig events =
    [ staff_group config time_sig end inst inst_events
    | (inst, inst_events) <- Seq.keyed_group_on event_instrument events
    ]
    where
    end = round_up (measure_time time_sig) $ fromMaybe 0 $
        Seq.maximum (map event_end events)

-- | Right hand goes at the top, left hand goes at the bottom.  Any other hands
-- goe below that.  Events that are don't have a hand are assumed to be in the
-- right hand.
staff_group :: Config -> TimeSignature -> Time
    -> Score.Instrument -> [Event] -> StaffGroup
staff_group config time_sig end inst events =
    StaffGroup inst $ map Staff $
        map measures $ Seq.group_on (lookup_hand . event_environ) events
    where
    lookup_hand environ = case TrackLang.lookup_val v_hand environ of
        Right (val :: String)
            | val == "right" -> 0
            | val == "left" -> 1
            | otherwise -> 2
        _ -> 0
    measures events = split_measures time_sig
        (convert_notes config time_sig end events)

round_up :: (Integral a) => a -> a -> a
round_up interval n
    | m == 0 = n
    | otherwise = (d+1) * interval
    where
    (d, m) = n `divMod` interval

-- * make_ly

make_ly :: Config -> Score -> [Event] -> ([Text.Text], Cmd.StackMap)
make_ly config score events = ly_file score
    (split_staves config (score_time score) events)

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

ly_file :: Score -> [StaffGroup] -> ([Text.Text], Cmd.StackMap)
ly_file (Score title time_sig (key, mode)) staff_groups = run_output $ do
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
        output $ "\\new Staff { "
            <+> "{ \\key" <+> Text.pack key <+> "\\"
                <> Text.pack (map Char.toLower (show mode))
            <+> "\\time" <+> Text.pack (to_lily time_sig) <> "\n"
        output $ "\\set Staff.instrumentName =" <+> str (inst_name inst)
            <> "\n\\set Staff.shortInstrumentName =" <+> str (inst_name inst)
            <> "\n{\n"
        mapM_ show_measures (zip [0, 4 ..] (group 4 measures))
        output "} } }\n"
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

run_output :: Output a -> ([Text.Text], Cmd.StackMap)
run_output m = (reverse (output_chunks state), output_map state)
    where state = State.execState m (OutputState [] Map.empty 1)

data OutputState = OutputState {
    -- | Chunks of text to write, in reverse order.  I could use
    -- Text.Lazy.Builder, but this is simpler and performance is probably ok.
    output_chunks :: ![Text.Text]
    , output_map :: !Cmd.StackMap
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
