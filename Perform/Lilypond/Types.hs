{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Basic types used by both "Perform.Lilypond.Lilypond" and module that use
-- it.  Defined here to avoid circular imports.
module Perform.Lilypond.Types where
import qualified Util.Pretty as Pretty
import qualified Derive.Score as Score
import qualified Perform.RealTime as RealTime
import Types


-- | Convert a value to its lilypond representation.
class ToLily a where
    to_lily :: a -> String

instance ToLily String where
    -- lilypond will probably choke on non-ascii chars, but that's ok
    to_lily = show

-- | Configure how the lilypond score is generated.
data Config = Config {
    -- | Amount of RealTime per quarter note.  This is the same value used by
    -- 'Perform.Lilypond.Convert'.
    config_quarter_duration :: !RealTime
    -- | Round everything to this duration.
    , config_quantize :: !Duration
    -- | Allow dotted rests?
    , config_dotted_rests :: !Bool
    , config_dynamics :: !DynamicConfig
    -- | Map each instrument to its long name and short name.  The order is
    -- the order they should appear in the score.
    , config_staves :: ![(Score.Instrument, String, String)]
    } deriving (Show)

-- | If non-null, generate dynamics from each event's dynamic control.
-- This has cutoffs for each dynamic level, which should be \"p\", \"mf\",
-- etc.
type DynamicConfig = [(Double, String)]

-- * Duration

-- | This time duration measured as the fraction of a whole note.
data Duration = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
    deriving (Enum, Bounded, Eq, Ord, Show)

instance ToLily Duration where to_lily = drop 1 . show

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
time_to_durs (Time time) =
    map fst $ filter ((/=0) . snd) $ reverse $ zip durs (binary time)
    where
    durs = [D128, D64 ..]
    binary rest
        | rest > 0 = m : binary d
        | otherwise = []
        where (d, m) = rest `divMod` 2

-- * NoteDuration

-- | A Duration plus a possible dot.
data NoteDuration = NoteDuration Duration Bool
    deriving (Eq, Show)

instance ToLily NoteDuration where
    to_lily (NoteDuration dur dot) = to_lily dur ++ if dot then "." else ""

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
