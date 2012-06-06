{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Convert from Score events to a lilypond score.
module Perform.Lilypond.Lilypond where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ratio ((%))

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc)
import Text.PrettyPrint ((<+>), ($+$))

import Util.Control
import qualified Util.ParseBs as ParseBs
import qualified Util.Seq as Seq

import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


-- * notes

class ToLily a where
    to_lily :: a -> String

-- | Time in score units.  The maximum resolution is a 128th note, so one unit
-- is 128th of a whole note.
newtype Time = Time Int deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

time_per_whole :: Time
time_per_whole = Time 128

-- | This time duration measured as the fraction of a whole note, so e.g. 1 is
-- a whole note, 4 is a quarter note, etc.
newtype Duration = Duration Int deriving (Eq, Show)

whole, half, quarter, eighth :: Duration
whole = Duration 1
half = Duration 2
quarter = Duration 4
eighth = Duration 8

read_duration :: String -> Maybe Duration
read_duration d = Map.lookup d durations
    where
    durations = Map.fromList [("whole", whole), ("half", half),
        ("quarter", quarter), ("eighth", eighth)]

instance ToLily Duration where
    to_lily (Duration dur) = show dur

data TimeSignature = TimeSignature { time_num :: !Int, time_denom :: !Duration }
    deriving (Show)

instance ToLily TimeSignature where
    to_lily (TimeSignature num denom) = show num ++ "/" ++ to_lily denom

data Event = Event {
    event_start :: !Time
    , event_duration :: !Time
    , event_pitch :: !String
    } deriving (Show)

event_end :: Event -> Time
event_end event = event_start event + event_duration event

data Note = Note {
    note_pitch :: !String
    , note_duration :: !Duration
    , note_tie :: !Bool
    } deriving (Show)

instance ToLily Note where
    to_lily (Note pitch dur tie) =
        pitch ++ to_lily dur ++ if tie then "~" else ""

newtype Rest = Rest Duration deriving (Show)

instance ToLily Rest where
    to_lily (Rest dur) = 'r' : to_lily dur


-- ** convert

convert_notes :: TimeSignature -> [Event] -> [Either Rest Note]
convert_notes sig events =
    concat $ zipWith mk (0 : map event_end events) events
        ++ [maybe [] (map Left . trailing_rests) (Seq.last events)]
    where
    mk prev (Event start dur pitch) =
        map Left (mkrests prev start) ++ map Right (mknotes start dur pitch)
    mkrests prev start
        | prev < start = map Rest $ convert_duration sig prev (start - prev)
        | otherwise = []
    mknotes start dur pitch = zipWith mk (finals durs) durs
        where
        mk is_final dur = Note pitch dur (not is_final)
        durs = convert_duration sig start dur
        finals = map null . drop 1 . List.tails
    trailing_rests last_event
        | rest == 0 = []
        | otherwise =
            map Rest $ convert_duration sig (event_end last_event) rest
        where rest = measure_time sig - event_end last_event

notes_to_lily :: [Either Rest Note] -> [String]
notes_to_lily = map (either to_lily to_lily)


-- | Given a starting point and a duration, emit the list of Durations
-- needed to express that duration.
convert_duration :: TimeSignature -> Time -> Time -> [Duration]
convert_duration sig pos time
    | time <= 0 = []
    | allowed >= time = time_to_durs time
    | otherwise = dur : convert_duration sig (pos + allowed) (time - allowed)
    where
    dur = time_to_dur allowed
    allowed = allowed_time sig pos

dur_to_time :: Duration -> Time
dur_to_time (Duration dur) = Time $ floor $ (1 % dur) * fromIntegral whole
    where Time whole = time_per_whole

time_to_dur :: Time -> Duration
time_to_dur (Time time) = Duration $ whole `div` time
    where Time whole = time_per_whole

time_to_durs :: Time -> [Duration]
time_to_durs (Time time) =
    map fst $ filter ((/=0) . snd) $ reverse $ zip durs (binary time)
    where
    durs = map (Duration . (whole `div`)) (iterate (*2) 1)
    Time whole = time_per_whole
    binary rest
        | rest > 0 = m : binary d
        | otherwise = []
        where (d, m) = rest `divMod` 2

allowed_time :: TimeSignature -> Time -> Time
allowed_time sig pos
    | even = allowed
    | otherwise = min (dur_to_time (time_denom sig)) allowed
    where
    measure = measure_time sig
    rest = measure - pos `mod` measure
    allowed = 2 ^ log2 rest
    even = case time_denom sig of
        Duration denom -> time_num sig `mod` denom == 0

log2 :: (Integral a) => a -> Int
log2 = go 0
    where
    go n val
        | div > 0 && mod == 0 = go (n+1) div
        | otherwise = n
        where (div, mod) = val `divMod` 2

measure_time :: TimeSignature -> Time
measure_time sig = Time (time_num sig) * dur_to_time (time_denom sig)


-- * score

data Score = Score {
    score_title :: String
    , score_time :: TimeSignature
    , score_clef :: String
    , score_key :: (String, Mode)
    -- | 1 second of RealTime is converted to this Duration.
    , score_duration1 :: Duration
    } deriving (Show)

data Mode = Major | Minor deriving (Show)

meta_ly :: String
meta_ly = "ly"

meta_title, meta_clef, meta_time_signature :: String
meta_title = "ly.title"
meta_clef = "ly.clef"
meta_time_signature = "ly.time-signature"
meta_duration1 = "ly.duration1"

meta_to_score :: Maybe Pitch.Key -> Map.Map String String
    -> Maybe (Either String Score)
meta_to_score maybe_score_key meta = case Map.lookup meta_ly meta of
    Nothing -> Nothing
    Just _ -> Just $ do
        score_key <- maybe (Left "key required") return maybe_score_key
        key <- parse_key score_key
        time_sig <- parse_time_signature $ get "4/4" meta_time_signature
        let dur1s = get "quarter" meta_duration1
        dur1 <- maybe (Left $ "duration1 unparseable: " ++ show dur1s) return
            (read_duration dur1s)
        return $ Score
            { score_title = title
            , score_time = time_sig
            , score_clef = clef
            , score_key = key
            , score_duration1 = dur1
            }
    where
    title = get "" meta_title
    clef = get "treble" meta_clef
    get deflt k = Map.findWithDefault deflt k meta
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
        <*> maybe unparseable (return . Duration) (ParseBs.int denom)


-- ** show

make_score :: Score -> [Event] -> Doc
make_score score events = score_file score (ly_notes events)
    where
    ly_notes = notes_to_lily . convert_notes (score_time score)

score_file :: Score -> [String] -> Doc
score_file (Score title time_sig clef (key, mode) _dur1) notes =
    command "version" <+> string "2.14.2"
    $+$ command "language" <+> string "english"
    -- Could I put the stack in there so I can click on the notes and get them
    -- highlighted in the original score?
    $+$ command "pointAndClickOff"
    $+$ brackets (command "header")
        [assign name <+> string val | (name, val) <- [("title", title)]]
    $+$ brackets (assign "notes") [PP.fsep (map PP.text notes)]
    $+$ command "score" <+> "{" <+> "<<" <+> staff <+> ">>" <+> "}"
    where
    staff = command "new" <+> "Staff"
        <+> "{"
            <+> command "clef" <+> string clef <+> "{"
            <+> command "key" <+> PP.text key
                <+> command (map Char.toLower (show mode))
            <+> command "time" <+> PP.text (to_lily time_sig)
            <+> command "notes"
            <+> "}"
        <+> "}"

command :: String -> Doc
command text = PP.char '\\' <> PP.text text

bcommand :: String -> [Doc] -> Doc
bcommand text contents = command text <+> PP.char '{'
    $+$ PP.nest 2 (PP.fsep contents)
    $+$ PP.char '}'

brackets :: Doc -> [Doc] -> Doc
brackets prefix contents = prefix
    <+> PP.char '{' $+$ PP.nest 2 (PP.fsep contents) $+$ PP.char '}'

assign :: String -> Doc
assign name = PP.text name <+> PP.char '='

string :: String -> Doc
string = PP.doubleQuotes . PP.text

show_pitch :: Theory.Pitch -> Either String String
show_pitch pitch = (++ oct_mark) <$> show_pitch_note note
    where
    (octave, note) = Theory.split_pitch pitch
    oct_mark = let oct = octave - 3
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
