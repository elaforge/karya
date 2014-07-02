-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Basic types used by both "Perform.Lilypond.Lilypond" and module that use
-- it.  Defined here to avoid circular imports.
module Perform.Lilypond.Types where
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- | Convert a value to its lilypond representation.
class ToLily a where
    to_lily :: a -> Text

instance ToLily Text where
    -- Lilypond's string literal is undocumented, but from lily/lexer.ll it
    -- looks haskell-like enough.  It'll probably choke on non-ascii.
    to_lily = showt

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
    , config_staves :: ![(Score.Instrument, StaffConfig)]
    } deriving (Eq, Read, Show)

data StaffConfig = StaffConfig {
    -- | Set Staff.instrumentName or PianoStaff.instrumentName.
    staff_long :: !Instrument
    -- | Set Staff.shortInstrumentName or PianoStaff.shortInstrumentName.
    , staff_short :: !Instrument
    -- | Additional code to include verbatim, after the \\new Staff line.
    , staff_code :: ![Text]
    -- | If false, this staff is omitted from the score.
    , staff_display :: !Bool
    } deriving (Eq, Read, Show)

type Instrument = Text

instance Pretty.Pretty Config where
    format (Config quarter quantize dotted staves) = Pretty.record "Config"
        [ ("quarter_duration", Pretty.format quarter)
        , ("quantize", Pretty.format quantize)
        , ("dotted_rests", Pretty.format dotted)
        , ("staves", Pretty.format staves)
        ]

instance Pretty.Pretty StaffConfig where
    format (StaffConfig long short code display) = Pretty.record "StaffConfig"
        [ ("long", Pretty.format long)
        , ("short", Pretty.format short)
        , ("code", Pretty.format code)
        , ("display", Pretty.format display)
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
    }

default_staff_config :: Score.Instrument -> StaffConfig
default_staff_config inst = empty_staff_config { staff_long = inst_name inst }

-- | This is emitted for every staff, regardless of its 'staff_code'.
global_staff_code :: [Text]
global_staff_code =
    [ "\\numericTimeSignature" -- Use 4/4 and 2/4 instead of C
    , "\\set Staff.printKeyCancellation = ##f"
    ]

inst_name :: Score.Instrument -> Instrument
inst_name = Text.dropWhile (=='/') . Text.dropWhile (/='/') . Score.inst_name

-- * Duration

-- | This time duration measured as the fraction of a whole note.
data Duration = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
    deriving (Enum, Bounded, Eq, Ord, Read, Show)

instance Pretty.Pretty Duration where pretty = show
instance ToLily Duration where to_lily = txt . drop 1 . show

read_duration :: String -> Maybe Duration
read_duration s = case s of
    -- GHC incorrectly reports overlapping patterns.  This bug is fixed in 7.4.
    "1" -> Just D1; "2" -> Just D2; "4" -> Just D4; "8" -> Just D8
    "16" -> Just D16; "32" -> Just D32; "64" -> Just D64; "128" -> Just D128
    _ -> Nothing

show_duration :: Duration -> String
show_duration = drop 1 . show

dur_to_time :: Duration -> Time
dur_to_time dur = Time $ case dur of
    D1 -> 128; D2 -> 64; D4 -> 32; D8 -> 16
    D16 -> 8; D32 -> 4; D64 -> 2; D128 -> 1
    -- or: (2^) . (fromEnum (maxBound :: Duration) -) . fromEnum

-- | This rounds up to the next Duration, so any Time over a half note will
-- wind up as a whole note.

-- | Convert Time to a Duration, along with any Time left over.
time_to_dur :: Time -> (Duration, Time)
time_to_dur time = (dur, time - dur_to_time dur)
    where
    dur = toEnum $ max 0 $
        fromEnum (maxBound :: Duration) - log2 (fromIntegral time)
    log2 = floor . logBase 2 . fromIntegral

time_to_durs :: Time -> [Duration]
time_to_durs time = go D1 time
    where
    go dur time
        | time >= dur_to_time dur = dur : go dur (time - dur_to_time dur)
        | dur == D128 = []
        | otherwise = go (succ dur) time

-- * NoteDuration

-- | A Duration plus a possible dot.
data NoteDuration = NoteDuration Duration Bool
    deriving (Eq, Show)

instance ToLily NoteDuration where
    to_lily (NoteDuration dur dot) = to_lily dur <> if dot then "." else ""

note_dur_to_time :: NoteDuration -> Time
note_dur_to_time (NoteDuration dur dotted) =
    dur_to_time dur + if dotted && dur /= D128 then dur_to_time (succ dur)
        else 0

-- | Time 0 becomes D128 since there's no 0 duration.  This puts a bottom bound
-- on the duration of a note, which is good since 0 duration notes aren't
-- notateable, but can happen after quantization.
time_to_note_dur :: Time -> NoteDuration
time_to_note_dur t = case time_to_durs t of
    [d1, d2] | d2 == succ d1 -> NoteDuration d1 True
    d : _ -> NoteDuration d False
    -- I have no 0 duration, so pick the smallest available duration.
    [] -> NoteDuration D128 False

-- | Only Just if the Time fits into a NoteDuration.
is_note_dur :: Time -> Maybe NoteDuration
is_note_dur t = case time_to_durs t of
    [d1, d2] | d2 == succ d1 -> Just $ NoteDuration d1 True
    [d] -> Just $ NoteDuration d False
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

instance Pretty.Pretty Time where
    pretty t = Pretty.show_float 10
        (fromIntegral t / fromIntegral time_per_whole) ++ "t"

time_per_whole :: Time
time_per_whole = dur_to_time D1

real_to_time :: RealTime -> RealTime -> Time
real_to_time quarter = Time . floor . adjust . RealTime.to_seconds
    where
    adjust n = n * (1 / RealTime.to_seconds quarter * qtime)
    qtime = fromIntegral (dur_to_time D4)

-- * Event

data Event = Event {
    event_start :: !Time
    , event_duration :: !Time
    , event_pitch :: !Text
    , event_instrument :: !Score.Instrument
    , event_environ :: !TrackLang.Environ
    , event_stack :: !Stack.Stack
    -- | True if this event is the tied continuation of a previous note.  In
    -- other words, if it was generated by the tie-splitting code.  This is
    -- a hack so v_ly_append_first and v_ly_append_last can differentiate.
    , event_clipped :: !Bool
    } deriving (Show)

event_end :: Event -> Time
event_end event = event_start event + event_duration event

event_attributes :: Event -> Score.Attributes
event_attributes = Score.environ_attributes . event_environ

instance Pretty.Pretty Event where
    format (Event start dur pitch inst attrs _stack _clipped) =
        Pretty.constructor "Event" [Pretty.format start, Pretty.format dur,
            Pretty.text (untxt pitch), Pretty.format inst, Pretty.format attrs]

-- * pitch

show_pitch :: Pitch.Pitch -> Either Text Text
show_pitch (Pitch.Pitch octave note) = (<> oct_mark) <$> show_pitch_note note
    where
    oct_mark
        | oct >= 0 = Text.replicate oct "'"
        | otherwise = Text.replicate (abs oct) ","
        where oct = octave - 3

show_pitch_note :: Pitch.Degree -> Either Text Text
show_pitch_note (Pitch.Degree pc accs) = do
    acc <- case accs of
        -2 -> Right "ff"
        -1 -> Right "f"
        0 -> Right ""
        1 -> Right "s"
        2 -> Right "ss"
        _ -> Left $ "too many accidentals: " <> showt accs
    t <- case pc of
            0 -> Right 'c'; 1 -> Right 'd'; 2 -> Right 'e'; 3 -> Right 'f'
            4 -> Right 'g'; 5 -> Right 'a'; 6 -> Right 'b'
            _ -> Left $ "pitch class out of range 0-6: " <> showt pc
    return $ Text.cons t acc
