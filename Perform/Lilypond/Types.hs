-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
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
    toLily :: a -> Text

instance ToLily Text where
    -- Lilypond's string literal is undocumented, but this seems to work.
    toLily txt = "\"" <> Text.replace "\"" "\\\"" txt <> "\""

-- | Configure how the lilypond score is generated.
data Config = Config
    -- | Amount of RealTime per quarter note.  This is the same value used by
    -- 'Perform.Lilypond.Convert'.
    { configQuarterDuration :: RealTime
    -- | Round everything to this duration.
    , configQuantize :: Duration
    -- | Allow dotted rests?
    , configDottedRests :: Bool
    -- | Map each instrument to its long name and short name.  The order is
    -- the order they should appear in the score.
    , configStaves :: [(ScoreT.Instrument, StaffConfig)]
    } deriving (Eq, Read, Show)

quarterDuration = Lens.lens configQuarterDuration
    (\r a -> r { configQuarterDuration = a })
quantize = Lens.lens configQuantize (\r a -> r { configQuantize = a })
dottedRests = Lens.lens configDottedRests
    (\r a -> r { configDottedRests = a })
staves = Lens.lens configStaves (\r a -> r { configStaves = a })

data StaffConfig = StaffConfig
    -- | Set Staff.instrumentName or PianoStaff.instrumentName.
    -- If an instrument doesn't have a StaffConfig, the long name defaults to
    -- the instrument name.
    { staffLong :: Instrument
    -- | Set Staff.shortInstrumentName or PianoStaff.shortInstrumentName.
    , staffShort :: Instrument
    -- | Additional code to include verbatim, after the \\new Staff line.
    , staffCode :: [Text]
    -- | If false, this staff is omitted from the score.
    , staffDisplay :: Bool
    -- | If true, add an additional staff named \"down\".  The new staff has
    -- a bass clef and all of the notes replaced with hidden rests, but the
    -- key and meter changes remain.  It is configured so it will be removed
    -- from the score for systems during which it has no notes.
    --
    -- The idea is that you then use xstaff to put notes on this staff.  This
    -- is for instruments like 揚琴 that have a wide range, but aren't divided
    -- into two hands, like the piano.
    , staffAddBassStaff :: Bool
    } deriving (Eq, Read, Show)

long = Lens.lens staffLong (\r a -> r { staffLong = a })
short = Lens.lens staffShort (\r a -> r { staffShort = a })
code = Lens.lens staffCode (\r a -> r { staffCode = a })
display = Lens.lens staffDisplay (\r a -> r { staffDisplay = a })
addBassStaff = Lens.lens staffAddBassStaff
    (\r a -> r { staffAddBassStaff = a })

type Instrument = Text

instance Pretty Config where
    format (Config quarter quantize dotted staves) = Pretty.record "Config"
        [ ("quarter_duration", Pretty.format quarter)
        , ("quantize", Pretty.format quantize)
        , ("dotted_rests", Pretty.format dotted)
        , ("staves", Pretty.format staves)
        ]

instance Pretty StaffConfig where
    format (StaffConfig long short code display addBass) =
        Pretty.record "StaffConfig"
            [ ("long", Pretty.format long)
            , ("short", Pretty.format short)
            , ("code", Pretty.format code)
            , ("display", Pretty.format display)
            , ("add_bass_staff", Pretty.format addBass)
            ]

defaultConfig :: Config
defaultConfig = Config
    { configQuarterDuration = 1
    , configQuantize = D32
    , configDottedRests = False
    , configStaves = []
    }

emptyStaffConfig :: StaffConfig
emptyStaffConfig = StaffConfig
    { staffLong = ""
    , staffShort = ""
    , staffCode = []
    , staffDisplay = True
    , staffAddBassStaff = False
    }

defaultStaffConfig :: ScoreT.Instrument -> StaffConfig
defaultStaffConfig inst =
    emptyStaffConfig { staffLong = ScoreT.instrumentName inst }

-- | This is emitted for every staff, regardless of its 'staff_code'.
globalStaffCode :: [Text]
globalStaffCode =
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
instance ToLily Duration where toLily = txt . drop 1 . show

durChar :: Duration -> Char
durChar d = case d of
    D1 -> 'w'; D2 -> 'h'; D4 -> 'q'; D8 -> 'e'
    D16 -> 's'; D32 -> 't'; D64 -> 'y'; D128 -> 'z'

intDur :: Int -> Maybe Duration
intDur i = case i of
    1 -> Just D1; 2 -> Just D2; 4 -> Just D4; 8 -> Just D8
    16 -> Just D16; 32 -> Just D32; 64 -> Just D64; 128 -> Just D128
    _ -> Nothing

durToTime :: Duration -> Time
durToTime dur = Time $ case dur of
    D1 -> 128; D2 -> 64; D4 -> 32; D8 -> 16
    D16 -> 8; D32 -> 4; D64 -> 2; D128 -> 1
    -- or: (2^) . (fromEnum (maxBound :: Duration) -) . fromEnum

-- | Get the longest dur that will fit within the Time, so this rounds down.
timeToDur :: Time -> Duration
timeToDur (Time t)
    | t < 2 = D128
    | t < 4 = D64
    | t < 8 = D32
    | t < 16 = D16
    | t < 32 = D8
    | t < 64 = D4
    | t < 128 = D2
    | otherwise = D1

timeToDurs :: Time -> [Duration]
timeToDurs time = go D1 time
    where
    go dur time
        | time >= durToTime dur = dur : go dur (time - durToTime dur)
        | dur == D128 = []
        | otherwise = go (succ dur) time

-- * NoteDuration

-- | A Duration plus a possible dot.
data NoteDuration = NoteDuration Duration Bool
    deriving (Eq, Show)

instance ToLily NoteDuration where
    toLily (NoteDuration dur dot) = toLily dur <> if dot then "." else ""

instance Pretty NoteDuration where pretty = ("D"<>) . toLily

noteDurChar :: NoteDuration -> Text
noteDurChar (NoteDuration dur dotted) =
    Text.singleton (durChar dur) <> if dotted then "." else ""

noteDurToTime :: NoteDuration -> Time
noteDurToTime (NoteDuration dur dotted) =
    durToTime dur + if dotted && dur /= D128 then durToTime (succ dur)
        else 0

-- | Get the longest NoteDuration that will fit in the Time.  0 becomes D128
-- since there's no 0 duration.  This puts a bottom bound on the duration of
-- a note, which is good since 0 duration notes aren't notateable, but can
-- happen after quantization.
timeToNoteDur :: Time -> NoteDuration
timeToNoteDur t = case timeToDurs t of
    [d1, d2] | d2 == succ d1 -> NoteDuration d1 True
    d : _ -> NoteDuration d False
    -- I have no 0 duration, so pick the smallest available duration.
    [] -> NoteDuration D128 False

durToNoteDur :: Duration -> NoteDuration
durToNoteDur d = NoteDuration d False

-- | Only Just if the Time fits into a single NoteDuration.
isNoteDur :: Time -> Maybe NoteDuration
isNoteDur t = case timeToNoteDurs t of
    [d] -> Just d
    _ -> Nothing

timeToNoteDurs :: Time -> [NoteDuration]
timeToNoteDurs t
    | t > 0 = dur : timeToNoteDurs (t - noteDurToTime dur)
    | otherwise = []
    where dur = timeToNoteDur t

-- * Time

-- | Time in score units.  The maximum resolution is a 128th note, so one unit
-- is 128th of a whole note.
newtype Time = Time Int deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Pretty Time where pretty = pretty . toWhole

toWhole :: Time -> Rational
toWhole t = fromIntegral t / fromIntegral timePerWhole

timePerWhole :: Time
timePerWhole = durToTime D1

realToTime :: RealTime -> RealTime -> Time
realToTime quarter = Time . floor . adjust . RealTime.toSeconds
    where
    adjust n = n * (1 / RealTime.toSeconds quarter * qtime)
    qtime = fromIntegral (durToTime D4)

multiply :: Rational -> Time -> Maybe Time
multiply factor t
    | frac == 0 = Just (Time i)
    | otherwise = Nothing
    where (i, frac) = properFraction (fromIntegral t * factor)

multiplyInt :: Int -> Time -> Time
multiplyInt factor (Time t) = Time (t * factor)

-- * Event

data Event = Event
    { eventStart :: Time
    , eventDuration :: Time
    , eventPitch :: Maybe Pitch
    , eventInstrument :: ScoreT.Instrument
    , eventEnviron :: DeriveT.Environ
    , eventStack :: Stack.Stack
    -- | True if this event is the tied continuation of a previous note.  In
    -- other words, if it was generated by the tie-splitting code.  This is
    -- a hack so 'Constants.v_append_first' and 'Constants.v_append_last' can
    -- differentiate.
    , eventClipped :: Bool
    } deriving (Show)

eventEnd :: Event -> Time
eventEnd event = eventStart event + eventDuration event

eventOverlaps :: Event -> Event -> Bool
eventOverlaps e1 e2 =
    not $ eventEnd e1 <= eventStart e2 || eventStart e1 >= eventEnd e2

eventAttributes :: Event -> Attrs.Attributes
eventAttributes = DeriveT.environAttributes . eventEnviron

instance Pretty Event where
    format (Event start dur pitch inst env_ _stack _clipped) =
        Pretty.constructor "Event" $
            [ Pretty.format start, Pretty.format dur
            , maybe "rest" (Pretty.text . toLily) pitch, Pretty.format inst
            ] ++ if Map.null e then [] else [Pretty.format env]
        where env@(DeriveT.Environ e) = stripEnviron env_

-- | Strip out non-ly environ keys so error messages are less cluttered.
stripEnviron :: DeriveT.Environ -> DeriveT.Environ
stripEnviron (DeriveT.Environ env) =
    DeriveT.Environ $ Map.filterWithKey interesting env
    where
    interesting key val = "ly-" `Text.isPrefixOf` key
        || (key == EnvKey.attributes && hasAttrs val)
        || key `elem` [EnvKey.voice, EnvKey.hand]
    hasAttrs (DeriveT.VAttributes attrs) = attrs /= mempty
    hasAttrs _ = True

-- * pitch

data Pitch = Pitch Int PitchClass Accidental
    deriving (Eq, Show, Ord)
data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Show, Ord, Enum)
data Accidental = FlatFlat | Flat | Natural | Sharp | SharpSharp
    deriving (Eq, Show, Ord)

instance Pretty Pitch where pretty = toLily
instance ToLily Pitch where
    toLily (Pitch octave pc acc) = toLily pc <> toLily acc <> octMark
        where
        octMark
            | oct >= 0 = Text.replicate oct "'"
            | otherwise = Text.replicate (abs oct) ","
            where oct = octave - 3

instance ToLily PitchClass where
    toLily pc = case pc of
        C -> "c"; D -> "d"; E -> "e"; F -> "f"
        G -> "g"; A -> "a"; B -> "b"

instance ToLily Accidental where
    toLily acc = case acc of
        FlatFlat -> "ff"
        Flat -> "f"
        Natural -> ""
        Sharp -> "s"
        SharpSharp -> "ss"

parsePitch :: Pitch.Pitch -> Either Text Pitch
parsePitch (Pitch.Pitch octave degree) =
    uncurry (Pitch octave) <$> parseDegree degree

parseDegree :: Pitch.Degree -> Either Text (PitchClass, Accidental)
parseDegree (Pitch.Degree pc acc) = (,) <$> pPc <*> pAcc
    where
    pPc = case pc of
        0 -> Right C; 1 -> Right D; 2 -> Right E; 3 -> Right F
        4 -> Right G; 5 -> Right A; 6 -> Right B
        _ -> Left $ "pitch class out of range 0-6: " <> showt pc
    pAcc = case acc of
        -2 -> Right FlatFlat
        -1 -> Right Flat
        0 -> Right Natural
        1 -> Right Sharp
        2 -> Right SharpSharp
        _ -> Left $ "too many accidentals: " <> showt acc

toPitch :: Pitch -> Pitch.Pitch
toPitch (Pitch octave pc acc) =
    Pitch.Pitch octave (Pitch.Degree (fromEnum pc) sharps)
    where
    sharps = case acc of
        FlatFlat -> -2
        Flat -> -1
        Natural -> 0
        Sharp -> 1
        SharpSharp -> 2
