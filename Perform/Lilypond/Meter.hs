-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Support for rhythmic spelling in different meters.
module Perform.Lilypond.Meter (
    Meter, meter_nums, meter_denom
    , time_num
    , default_meter
    , measure_time
    , unparse_meter, parse_meter
    -- * allowed time
    , convert_duration
    , allowed_time_best
#ifdef TESTING
    , module Perform.Lilypond.Meter
#endif
) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed ((!))

import qualified Text.Parsec as Parsec

import qualified Util.Parse as Parse
import qualified Util.Seq as Seq
import qualified Cmd.Ruler.Meter as Meter
import Cmd.Ruler.Meter (AbstractMeter(..))
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Time, Duration(..), NoteDuration(..))
import Global


data Meter = Meter {
    -- | NonEmpty list of numerators.  E.g. [2, 3] indicates 2+3.
    meter_nums :: ![Int]
    , meter_denom :: !Duration
    , meter_ranks :: !Ranks
    } deriving (Eq, Show)

-- | Meter rank, indexed by 128th note.
type Ranks = Vector.Vector Rank
type Rank = Int

time_num :: Meter -> Int
time_num = sum . meter_nums

rank_at :: Meter -> Time -> Int
rank_at meter t = v ! (time_index t `mod` Vector.length v)
    where v = meter_ranks meter

time_index :: Time -> Int
time_index = fromIntegral

index_time :: Int -> Time
index_time = fromIntegral

-- | Find the time of the rank <= the given one.  Rank 0 can never be spanned,
-- so always stop at a rank 0.
find_rank :: Time -> Rank -> Meter -> Maybe Time
find_rank start rank = fmap ((+ (start + 1)) . index_time)
    . Vector.findIndex (<= max 0 rank)
    . Vector.drop (time_index start + 1) . meter_ranks

instance Pretty Meter where pretty = Types.to_lily
instance Types.ToLily Meter where
    to_lily (Meter nums denom _) =
        showt (sum nums) <> "/" <> Types.to_lily denom

is_duple :: Meter -> Bool
is_duple meter = case meter_nums meter of
    [num] -> (==0) $ snd $ properFraction $ logBase 2 (fromIntegral num)
    _ -> False

-- | Duration of a measure, in Time.
measure_time :: Meter -> Time
measure_time meter =
    fromIntegral (time_num meter) * Types.dur_to_time (meter_denom meter)

unparse_meter :: Meter -> Text
unparse_meter meter = Text.intercalate "+" (map showt (meter_nums meter))
    <> "/" <> Types.to_lily (meter_denom meter)

default_meter :: Meter
Right default_meter = parse_meter "4/4"

parse_meter :: Text -> Either Text Meter
parse_meter text = do
    (nums, denom) <- first ("parsing meter: "<>) $ Parse.parse p_meter text
    denom <- tryJust ("denominator not a valid duration: " <> showt denom) $
        Types.int_dur denom
    make_meter nums denom (abstract_meter nums denom)

abstract_meter :: [Int] -> Duration -> [AbstractMeter]
abstract_meter nums denom = case Map.lookup (nums, denom) default_meters of
    Just m -> [m]
    Nothing -> case default_divisions nums denom of
        [num] -> [D (replicate num T)]
        nums -> [D [D (replicate n T) | n <- nums]]
    where
    -- Certain simple duple meters get a simpler division.  This has the effect
    -- of allowing notes to cross beat divisions, e.g. 4 2 4 in 4/4.
    default_meters = Map.fromList
        [ (([1], D4), T)
        , (([2], D4), T)
        , (([4], D4), T)
        ]

default_divisions :: [Int] -> Duration -> [Int]
default_divisions [num] denom = Map.findWithDefault [num] (num, denom) defaults
    where
    defaults = Map.fromList
        [ ((5, D8), [3, 2])
        , ((6, D8), [3, 3])
        , ((7, D8), [3, 4])
        , ((9, D8), [3, 3, 3])
        , ((12, D8), [3, 3, 3, 3])
        ]
default_divisions nums _ = nums

p_meter :: Parse.Parser () ([Int], Int)
p_meter = (,) <$> Parsec.sepBy1 Parse.p_positive (Parsec.char '+')
    <*> (Parsec.char '/' *> Parse.p_positive)


make_meter :: [Int] -> Duration -> [AbstractMeter] -> Either Text Meter
make_meter nums denom meters = Meter nums denom <$> vector
    where
    vector
        | frac /= 0 = Left $ "can't fit " <> showt ranks <> " into "
            <> showt expected <> " by doubling"
        | otherwise = Right $ to_vector $ subdivides (replicate exp 2) meters
    (exp, frac) = properFraction $
        logBase 2 (fromIntegral expected / fromIntegral ranks)
    -- expected = sum nums * fromIntegral (Types.dur_to_time denom)
    -- TODO sure?
    expected = sum nums * time_index (Types.dur_to_time denom)
    ranks = sum $ map abstract_length meters
    to_vector = Vector.fromList . map fst . Meter.make_meter 1

subdivides :: [Int] -> [AbstractMeter] -> [AbstractMeter]
subdivides divs meter = foldr subdivide meter (reverse divs)

subdivide :: Int -> [AbstractMeter] -> [AbstractMeter]
subdivide n = map (Meter.subdivide n)

abstract_length :: AbstractMeter -> Int
abstract_length (D ds) = sum $ map abstract_length ds
abstract_length T = 1

-- * allowed time

-- | Given a starting point and a duration, emit the list of Durations
-- needed to express that duration.
convert_duration :: Meter -> Bool -> Time -> Time -> [NoteDuration]
convert_duration meter use_dot_ = go
    where
    -- Dotted rests are always allowed for non-duple meters.
    use_dot = use_dot_ || not (is_duple meter)
    go pos time_dur
        | time_dur <= 0 = []
        | allowed >= time_dur = to_durs time_dur
        | otherwise = dur : go (pos + allowed) (time_dur - allowed)
        where
        dur = Types.time_to_note_dur allowed
        allowed = allowed_time_best use_dot meter pos
        to_durs = if use_dot then Types.time_to_note_durs
            else map (flip NoteDuration False) . Types.time_to_durs

{- | Figure out how much time a note at the given position should be allowed
    before it must tie.  The heuristic is to find the duration that ends on the
    lowest rank, but never go over a rank which is too low.  The "too low" rule
    is more lenient for duple meters, since they're easier to read.

    I used to have a greedy algorithm that found the longest duration that
    didn't break the "too low" rule, and used that for note durations while
    I used this for rest durations, but I'm not sure why I did that now.  Notes
    should be easy to read like rests.
-}
allowed_time_best :: Bool -> Meter -> Time -> Time
allowed_time_best use_dot meter start_ =
    subtract start $ best_duration $ allowed_time meter start
    where
    -- Try notes up to the end, select the one that lands on the lowest rank.
    best_duration end = fromMaybe (start + 1) $
        Seq.minimum_on (rank_at meter) candidates
        where
        candidates = takeWhile (<=end) $
            map ((+start) . Types.note_dur_to_time) $
                if use_dot then dot_durs else durs
    durs = reverse $ map (flip NoteDuration False) [D1 .. D128]
    dot_durs = reverse
        [NoteDuration d dot | d <- [D1 .. D64], dot <- [True, False]]
    start = start_ `mod` measure_time meter

allowed_time :: Meter -> Time -> Time
allowed_time meter start =
    fromMaybe measure $
        find_rank start (rank - if is_duple meter then 2 else 1) meter
        -- Subtract 2 for duple means a note can span 2 ranks higher than its
        -- starting rank, which means notes will tie less.  The rationale is
        -- that duple is easier to read, so I can get away with fewer ties.
    where
    rank = rank_at meter start
    measure = measure_time meter
