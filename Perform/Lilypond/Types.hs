-- | Basic types used by both "Perform.Lilypond.Lilypond" and module that use
-- it.  Defined here to avoid circular imports.
module Perform.Lilypond.Types where
import Types


-- | Configure how the lilypond score is generated.
data Config = Config {
    -- | Amount of RealTime per quarter note.  This is the same value used by
    -- 'Perform.Lilypond.Convert'.
    config_quarter_duration :: !RealTime
    -- | Allow dotted rests?
    , config_dotted_rests :: !Bool
    , config_dynamics :: !DynamicConfig
    } deriving (Show)

-- | If non-null, generate dynamics from each event's dynamic control.
-- This has cutoffs for each dynamic level, which should be \"p\", \"mf\",
-- etc.
type DynamicConfig = [(Double, String)]

data TimeConfig = TimeConfig {
    -- | Amount of RealTime for one quarter note.
    time_quarter :: RealTime
    -- | Round everything to this duration.
    , time_quantize :: Duration
    } deriving (Show)

-- | This time duration measured as the fraction of a whole note.
data Duration = D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
    deriving (Enum, Eq, Ord, Show)

-- | A Duration plus a possible dot.
data NoteDuration = NoteDuration Duration Bool
    deriving (Eq, Show)

read_duration :: String -> Maybe Duration
read_duration s = case s of
    -- GHC incorrectly reports overlapping patterns.  This bug is fixed in 7.4.
    "1" -> Just D1; "2" -> Just D2; "4" -> Just D4; "8" -> Just D8
    "16" -> Just D16; "32" -> Just D32; "64" -> Just D64; "128" -> Just D128
    _ -> Nothing

show_duration :: Duration -> String
show_duration = drop 1 . show
