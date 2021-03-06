-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic types used by both "Perform.Lilypond.Lilypond" and module that use
-- it.  Defined here to avoid circular imports.
module Perform.Lilypond.Types where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import           Global
import           Types


-- | Convert a value to its lilypond representation.
class ToLily a where
    to_lily :: a -> Text

instance ToLily Text where
    -- Lilypond's string literal is undocumented, but this seems to work.
    to_lily txt = "\"" <> Text.replace "\"" "\\\"" txt <> "\""

-- | Configure how the lilypond score is generated.
data Config = Config {
    -- | Amount of RealTime per quarter note.  This is the same value used by
    -- 'Perform.Lilypond.Convert'.
    config_quarter_duration :: !RealTime
    -- | Round everything to this duration.
    , config_quantize :: !Duration
    -- | Allow dotted rests?
    , config_dotted_rests :: !Bool
    -- | Map each instrument to its long name and short name.  The order is
    -- the order they should appear in the score.
    , config_staves :: ![(ScoreT.Instrument, StaffConfig)]
    } deriving (Eq, Read, Show)

quarter_duration = Lens.lens config_quarter_duration
    (\f r -> r { config_quarter_duration = f (config_quarter_duration r) })
quantize = Lens.lens config_quantize
    (\f r -> r { config_quantize = f (config_quantize r) })
dotted_rests = Lens.lens config_dotted_rests
    (\f r -> r { config_dotted_rests = f (config_dotted_rests r) })
staves = Lens.lens config_staves
    (\f r -> r { config_staves = f (config_staves r) })

data StaffConfig = StaffConfig {
    -- | Set Staff.instrumentName or PianoStaff.instrumentName.
    -- If an instrument doesn't have a StaffConfig, the long name defaults to
    -- the instrument name.
    staff_long :: !Instrument
    -- | Set Staff.shortInstrumentName or PianoStaff.shortInstrumentName.
    , staff_short :: !Instrument
    -- | Additional code to include verbatim, after the \\new Staff line.
    , staff_code :: ![Text]
    -- | If false, this staff is omitted from the score.
    , staff_display :: !Bool
    -- | If true, add an additional staff named \"down\".  The new staff has
    -- a bass clef and all of the notes replaced with hidden rests, but the
    -- key and meter changes remain.  It is configured so it will be removed
    -- from the score for systems during which it has no notes.
    --
    -- The idea is that you then use xstaff to put notes on this staff.  This
    -- is for instruments like 揚琴 that have a wide range, but aren't divided
    -- into two hands, like the piano.
    , staff_add_bass_staff :: !Bool
    } deriving (Eq, Read, Show)

long = Lens.lens staff_long (\f r -> r { staff_long = f (staff_long r) })
short = Lens.lens staff_short (\f r -> r { staff_short = f (staff_short r) })
code = Lens.lens staff_code (\f r -> r { staff_code = f (staff_code r) })
display = Lens.lens staff_display
    (\f r -> r { staff_display = f (staff_display r) })
add_bass_staff = Lens.lens staff_add_bass_staff
    (\f r -> r { staff_add_bass_staff = f (staff_add_bass_staff r) })

type Instrument = Text

instance Pretty Config where
    format (Config quarter quantize dotted staves) = Pretty.record "Config"
        [ ("quarter_duration", Pretty.format quarter)
        , ("quantize", Pretty.format quantize)
        , ("dotted_rests", Pretty.format dotted)
        , ("staves", Pretty.format staves)
        ]

instance Pretty StaffConfig where
    format (StaffConfig long short code display add_bass) =
        Pretty.record "StaffConfig"
            [ ("long", Pretty.format long)
            , ("short", Pretty.format short)
            , ("code", Pretty.format code)
            , ("display", Pretty.format display)
            , ("add_bass_staff", Pretty.format add_bass)
            ]

default_config :: Config
default_config = Config
    { config_quarter_duration = 1
    , config_quantize = D32
    , config_dotted_rests = False
    , config_staves = []
    }

empty_staff_config :: StaffConfig
empty_staff_config = StaffConfig
    { staff_long = ""
    , staff_short = ""
    , staff_code = []
    , staff_display = True
    , staff_add_bass_staff = False
    }

default_staff_config :: ScoreT.Instrument -> StaffConfig
default_staff_config inst =
    empty_staff_config { staff_long = ScoreT.instrument_name inst }

-- | This is emitted for every staff, regardless of its 'staff_code'.
global_staff_code :: [Text]
global_staff_code =
    [ "\\numericTimeSignature" -- Use 4/4 and 2/4 instead of C
    , "\\set Staff.printKeyCancellation = ##f"
    -- Show bar numbers at (end, middle, beginning), i.e. every bar.  I prefer
    -- this to spending rehearsal time counting measures.
    -- This only needs to go on the top staff, but it doesn't hurt to put it on
    -- all of them.
    , "\\override Score.BarNumber.break-visibility = ##(#f #t #t)"
    ]

-- * Duration

-- | This time duration measured as the fraction of a whole note.
data Duration = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
    deriving (Enum, Bounded, Eq, Ord, Read, Show)

instance Pretty Duration where pretty = showt
instance ToLily Duration where to_lily = txt . drop 1 . show

dur_char :: Duration -> Char
dur_char d = case d of
    D1 -> 'w'; D2 -> 'h'; D4 -> 'q'; D8 -> 'e'
    D16 -> 's'; D32 -> 't'; D64 -> 'y'; D128 -> 'z'

int_dur :: Int -> Maybe Duration
int_dur i = case i of
    1 -> Just D1; 2 -> Just D2; 4 -> Just D4; 8 -> Just D8
    16 -> Just D16; 32 -> Just D32; 64 -> Just D64; 128 -> Just D128
    _ -> Nothing

dur_to_time :: Duration -> Time
dur_to_time dur = Time $ case dur of
    D1 -> 128; D2 -> 64; D4 -> 32; D8 -> 16
    D16 -> 8; D32 -> 4; D64 -> 2; D128 -> 1
    -- or: (2^) . (fromEnum (maxBound :: Duration) -) . fromEnum

-- | Get the longest dur that will fit within the Time, so this rounds down.
time_to_dur :: Time -> Duration
time_to_dur (Time t)
    | t < 2 = D128
    | t < 4 = D64
    | t < 8 = D32
    | t < 16 = D16
    | t < 32 = D8
    | t < 64 = D4
    | t < 128 = D2
    | otherwise = D1

time_to_durs :: Time -> [Duration]
time_to_durs time = go D1 time
    where
    go dur time
        | time >= dur_to_time dur = dur : go dur (time - dur_to_time dur)
        | dur == D128 = []
        | otherwise = go (succ dur) time

-- * NoteDuration

-- | A Duration plus a possible dot.
data NoteDuration = NoteDuration !Duration !Bool
    deriving (Eq, Show)

instance ToLily NoteDuration where
    to_lily (NoteDuration dur dot) = to_lily dur <> if dot then "." else ""

instance Pretty NoteDuration where pretty = ("D"<>) . to_lily

note_dur_char :: NoteDuration -> Text
note_dur_char (NoteDuration dur dotted) =
    Text.singleton (dur_char dur) <> if dotted then "." else ""

note_dur_to_time :: NoteDuration -> Time
note_dur_to_time (NoteDuration dur dotted) =
    dur_to_time dur + if dotted && dur /= D128 then dur_to_time (succ dur)
        else 0

-- | Get the longest NoteDuration that will fit in the Time.  0 becomes D128
-- since there's no 0 duration.  This puts a bottom bound on the duration of
-- a note, which is good since 0 duration notes aren't notateable, but can
-- happen after quantization.
time_to_note_dur :: Time -> NoteDuration
time_to_note_dur t = case time_to_durs t of
    [d1, d2] | d2 == succ d1 -> NoteDuration d1 True
    d : _ -> NoteDuration d False
    -- I have no 0 duration, so pick the smallest available duration.
    [] -> NoteDuration D128 False

dur_to_note_dur :: Duration -> NoteDuration
dur_to_note_dur d = NoteDuration d False

-- | Only Just if the Time fits into a single NoteDuration.
is_note_dur :: Time -> Maybe NoteDuration
is_note_dur t = case time_to_note_durs t of
    [d] -> Just d
    _ -> Nothing

time_to_note_durs :: Time -> [NoteDuration]
time_to_note_durs t
    | t > 0 = dur : time_to_note_durs (t - note_dur_to_time dur)
    | otherwise = []
    where dur = time_to_note_dur t

-- * Time

-- | Time in score units.  The maximum resolution is a 128th note, so one unit
-- is 128th of a whole note.
newtype Time = Time Int deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Pretty Time where pretty = pretty . to_whole

to_whole :: Time -> Rational
to_whole t = fromIntegral t / fromIntegral time_per_whole

time_per_whole :: Time
time_per_whole = dur_to_time D1

real_to_time :: RealTime -> RealTime -> Time
real_to_time quarter = Time . floor . adjust . RealTime.to_seconds
    where
    adjust n = n * (1 / RealTime.to_seconds quarter * qtime)
    qtime = fromIntegral (dur_to_time D4)

multiply :: Rational -> Time -> Maybe Time
multiply factor t
    | frac == 0 = Just (Time i)
    | otherwise = Nothing
    where (i, frac) = properFraction (fromIntegral t * factor)

multiply_int :: Int -> Time -> Time
multiply_int factor (Time t) = Time (t * factor)

-- * Event

data Event = Event {
    event_start :: !Time
    , event_duration :: !Time
    , event_pitch :: !(Maybe Pitch)
    , event_instrument :: !ScoreT.Instrument
    , event_environ :: !DeriveT.Environ
    , event_stack :: !Stack.Stack
    -- | True if this event is the tied continuation of a previous note.  In
    -- other words, if it was generated by the tie-splitting code.  This is
    -- a hack so 'Constants.v_append_first' and 'Constants.v_append_last' can
    -- differentiate.
    , event_clipped :: !Bool
    } deriving (Show)

event_end :: Event -> Time
event_end event = event_start event + event_duration event

event_overlaps :: Event -> Event -> Bool
event_overlaps e1 e2 =
    not $ event_end e1 <= event_start e2 || event_start e1 >= event_end e2

event_attributes :: Event -> Attrs.Attributes
event_attributes = DeriveT.environ_attributes . event_environ

instance Pretty Event where
    format (Event start dur pitch inst env_ _stack _clipped) =
        Pretty.constructor "Event" $
            [ Pretty.format start, Pretty.format dur
            , maybe "rest" (Pretty.text . to_lily) pitch, Pretty.format inst
            ] ++ if Map.null e then [] else [Pretty.format env]
        where env@(DeriveT.Environ e) = strip_environ env_

-- | Strip out non-ly environ keys so error messages are less cluttered.
strip_environ :: DeriveT.Environ -> DeriveT.Environ
strip_environ (DeriveT.Environ env) =
    DeriveT.Environ $ Map.filterWithKey interesting env
    where
    interesting key val = "ly-" `Text.isPrefixOf` key
        || (key == EnvKey.attributes && has_attrs val)
        || key `elem` [EnvKey.voice, EnvKey.hand]
    has_attrs (DeriveT.VAttributes attrs) = attrs /= mempty
    has_attrs _ = True

-- * pitch

data Pitch = Pitch !Int !PitchClass !Accidental
    deriving (Eq, Show, Ord)
data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Show, Ord, Enum)
data Accidental = FlatFlat | Flat | Natural | Sharp | SharpSharp
    deriving (Eq, Show, Ord)

instance Pretty Pitch where pretty = to_lily
instance ToLily Pitch where
    to_lily (Pitch octave pc acc) = to_lily pc <> to_lily acc <> oct_mark
        where
        oct_mark
            | oct >= 0 = Text.replicate oct "'"
            | otherwise = Text.replicate (abs oct) ","
            where oct = octave - 3

instance ToLily PitchClass where
    to_lily pc = case pc of
        C -> "c"; D -> "d"; E -> "e"; F -> "f"
        G -> "g"; A -> "a"; B -> "b"

instance ToLily Accidental where
    to_lily acc = case acc of
        FlatFlat -> "ff"
        Flat -> "f"
        Natural -> ""
        Sharp -> "s"
        SharpSharp -> "ss"

parse_pitch :: Pitch.Pitch -> Either Text Pitch
parse_pitch (Pitch.Pitch octave degree) =
    uncurry (Pitch octave) <$> parse_degree degree

parse_degree :: Pitch.Degree -> Either Text (PitchClass, Accidental)
parse_degree (Pitch.Degree pc acc) = (,) <$> p_pc <*> p_acc
    where
    p_pc = case pc of
        0 -> Right C; 1 -> Right D; 2 -> Right E; 3 -> Right F
        4 -> Right G; 5 -> Right A; 6 -> Right B
        _ -> Left $ "pitch class out of range 0-6: " <> showt pc
    p_acc = case acc of
        -2 -> Right FlatFlat
        -1 -> Right Flat
        0 -> Right Natural
        1 -> Right Sharp
        2 -> Right SharpSharp
        _ -> Left $ "too many accidentals: " <> showt acc

to_pitch :: Pitch -> Pitch.Pitch
to_pitch (Pitch octave pc acc) =
    Pitch.Pitch octave (Pitch.Degree (fromEnum pc) sharps)
    where
    sharps = case acc of
        FlatFlat -> -2
        Flat -> -1
        Natural -> 0
        Sharp -> 1
        SharpSharp -> 2
