{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Convert from Score events to a lilypond score.
module Perform.Lilypond.Lilypond where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc)
import Text.PrettyPrint ((<+>), ($+$))

import Util.Control
import qualified Util.ParseBs as ParseBs
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


-- * types

-- | Convert a value to its lilypond representation.
-- TODO go to Pretty.Doc instead of String?
class ToLily a where
    to_lily :: a -> String

-- | Time in score units.  The maximum resolution is a 128th note, so one unit
-- is 128th of a whole note.
newtype Time = Time Int deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

time_per_whole :: Time
time_per_whole = Time 128

-- | This time duration measured as the fraction of a whole note.
data Duration = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
    deriving (Enum, Eq, Ord, Show)

data NoteDuration = NoteDuration Duration Bool
    deriving (Eq, Show)

read_duration :: String -> Maybe Duration
read_duration s = case s of
    -- GHC says these are overlapped, but only if I use "" syntax, not if
    -- I use [] syntax.  I think it's a bug.  TODO try on newer version of
    -- ghc.
    "1" -> Just D1; "2" -> Just D2; "4" -> Just D4; "8" -> Just D8
    "16" -> Just D16; "32" -> Just D32; "64" -> Just D64; "128" -> Just D128
    _ -> Nothing

instance ToLily Duration where
    to_lily = drop 1 . show

instance ToLily NoteDuration where
    to_lily (NoteDuration dur dot) = to_lily dur ++ if dot then "." else ""

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

-- ** Note

data Note = Note {
    note_pitch :: !(Maybe String)
    , note_duration :: !NoteDuration
    , note_tie :: !Bool
    } deriving (Show)

note :: String -> NoteDuration -> Bool -> Note
note pitch dur tie = Note (Just pitch) dur tie

rest :: NoteDuration -> Note
rest dur = Note Nothing dur False

instance ToLily Note where
    to_lily (Note pitch dur tie) =
        Maybe.fromMaybe "r" pitch ++ to_lily dur ++ if tie then "~" else ""

note_time :: Note -> Time
note_time = note_dur_to_time . note_duration


-- * convert

-- Another way:
-- Always emit the longest possible note.  But I still have to use ties
-- because if I have 7 16ths I have to choose between
-- 'a4.~ a16' and 'a16~ a4.'.  The longer note should fall on a larger
-- division.

-- | Turn Events, which are in absolute Time, into Notes, which are divided up
-- into tied Durations depending on the time signature.
convert_notes :: TimeSignature -> [Event] -> [Note]
convert_notes sig events =
    concat $ zipWith mk (0 : map event_end events) events
        ++ [maybe [] trailing_rests (Seq.last events)]
    where
    mk prev (Event start dur pitch) =
        mkrests prev start ++ mknotes start dur pitch
    mkrests prev start
        | prev < start = map rest $ convert_duration sig prev (start - prev)
        | otherwise = []
    mknotes start dur pitch = zipWith mk (finals durs) durs
        where
        mk is_final dur = note pitch dur (not is_final)
        durs = convert_duration sig start dur
        finals = map null . drop 1 . List.tails
    trailing_rests last_event
        | remaining == 0 = []
        | otherwise =
            map rest (convert_duration sig (event_end last_event) remaining)
        where remaining = measure_time sig - event_end last_event

-- | Given a starting point and a duration, emit the list of Durations
-- needed to express that duration.
convert_duration :: TimeSignature -> Time -> Time -> [NoteDuration]
convert_duration sig pos time
    | time <= 0 = []
    | allowed >= time = time_to_note_durs time
    | otherwise = dur : convert_duration sig (pos + allowed) (time - allowed)
    where
    dur = time_to_note_dur allowed
    allowed = allowed_time sig pos

allowed_time :: TimeSignature -> Time -> Time
allowed_time sig pos
    | power_of_2 (time_num sig) = allowed
    | otherwise = min (dur_to_time (time_denom sig)) allowed
    where
    measure = measure_time sig
    rest = measure - pos `mod` measure
    allowed = 2 ^ log2 rest
    power_of_2 = (==0) . snd . properFraction . logBase 2 . fromIntegral

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
time_to_note_dur = flip NoteDuration False . time_to_dur
    -- TODO use dots

time_to_dur :: Time -> Duration
time_to_dur (Time time) =
    toEnum $ min (fromEnum D128) (log2 (whole `div` time))
    where Time whole = time_per_whole

time_to_note_durs :: Time -> [NoteDuration]
time_to_note_durs = map (flip NoteDuration False) . time_to_durs

time_to_durs :: Time -> [Duration]
time_to_durs (Time time) =
    map fst $ filter ((/=0) . snd) $ reverse $ zip durs (binary time)
    where
    durs = [D128, D64 ..]
    binary rest
        | rest > 0 = m : binary d
        | otherwise = []
        where (d, m) = rest `divMod` 2

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

-- * simplify

-- | Post processing on Notes to make the score simpler.  This winds up undoing
-- some of the splitting into Durations done by 'convert_notes', but it's
-- simpler this way than to try to build all sorts of heuristics into
-- convert_notes.
-- simplify :: TimeSignature -> [Note] -> [Note]
-- simplify time_sig notes =

-- | Tied notes starting at the beginning of a measure and having the duration
-- of a full measure can be turned into a single whole note.
measure_to_whole :: Time -> Time -> [Note] -> [Note]
measure_to_whole _ _ [] = []
measure_to_whole measure_time pos notes@(n:ns)
    | n : _ <- tied, pos `mod` measure_time == 0 && tied_time == measure_time =
        n { note_duration = whole, note_tie = note_tie (last tied) }
            : measure_to_whole measure_time (pos + tied_time) rest
    | otherwise = n : measure_to_whole measure_time (pos + note_time n) ns
    where
    (tied, rest) = take_tied_until True measure_time notes
    tied_time = sum $ map note_time tied
    whole = NoteDuration D1 False

-- | Dur n + Dur (n*2) can be turned into a dot if it doesn't span a major
-- division.  Major division is the middle of 4/4, ...
-- dotted_rhythms :: [Note] -> [Note]
-- dotted_rhythms measure_time pos = undefined


-- *** util

take_tied_until :: Bool -> Time -> [Note] -> ([Note], [Note])
take_tied_until include_rest until notes = (map fst pre, map fst post ++ rest)
    where
    (tied, rest) = take_tied include_rest notes
    (pre, post) = break ((>=until) . snd) $
        zip tied (scanl (+) 0 (map note_time tied))

take_tied :: Bool -> [Note] -> ([Note], [Note])
take_tied include_rest notes@(n:_)
    | include_rest && is_rest n = break (not . is_rest) notes
    | otherwise = Then.break1 (not . note_tie) notes
take_tied _ [] = ([], [])

break_state :: state -> (state -> a -> (state, Bool)) -> [a]
    -> (state, ([a], [a]))
break_state state _ [] = (state, ([], []))
break_state state f (x:xs)
    | broken = (state, ([], x:xs))
    | otherwise = let (last_state, (pre, post)) = break_state next_state f xs
        in (last_state, (x:pre, post))
    where (next_state, broken) = f state x

is_rest :: Note -> Bool
is_rest = Maybe.isNothing . note_pitch


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
        let dur1s = get "4" meta_duration1
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
        <*> maybe unparseable return (read_duration denom)


-- ** show

make_score :: Score -> [Event] -> Doc
make_score score events = score_file score (ly_notes events)
    where
    ly_notes = map to_lily . convert_notes (score_time score)

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
