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
    , is_binary
    -- * tie breaking
    , allowed_duration
#ifdef TESTING
    , module Perform.Lilypond.Meter
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector
import           Data.Vector.Unboxed ((!))

import qualified Util.Num as Num
import qualified Util.P as P
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq

import qualified Perform.Lilypond.Types as Types
import           Perform.Lilypond.Types (Duration(..), NoteDuration(..), Time)
import qualified Ui.Meter.Make as Meter.Make
import qualified Ui.Meter.Meter as Meter
import           Ui.Meter.Meter (AbstractMeter(..))

import           Global


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
time_num = Num.sum . meter_nums

rank_at :: Meter -> Time -> Int
rank_at meter t = v ! (time_index t `mod` Vector.length v)
    where v = meter_ranks meter

time_index :: Time -> Int
time_index = fromIntegral

index_time :: Int -> Time
index_time = fromIntegral

instance Pretty Meter where pretty = unparse_meter

instance Types.ToLily Meter where
    to_lily (Meter nums denom _) =
        showt (Num.sum nums) <> "/" <> Types.to_lily denom

is_binary :: Meter -> Bool
is_binary meter = case meter_nums meter of
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

p_meter :: Parse.Parser ([Int], Int)
p_meter = (,) <$> P.sepBy1 Parse.p_positive (P.char '+')
    <*> (P.char '/' *> Parse.p_positive)

make_meter :: [Int] -> Duration -> [AbstractMeter] -> Either Text Meter
make_meter nums denom meters = Meter nums denom <$> vector
    where
    vector
        | frac /= 0 = Left $ "can't fit " <> showt ranks <> " into "
            <> showt expected <> " by doubling"
        | otherwise = Right $ to_vector $ subdivides (replicate exp 2) meters
    (exp, frac) = properFraction $
        logBase 2 (fromIntegral expected / fromIntegral ranks)
    expected = Num.sum nums * time_index (Types.dur_to_time denom)
    ranks = Num.sum $ map abstract_length meters
    to_vector = Vector.fromList . map (fromEnum . fst)
        . Meter.Make.to_rank_durations . map (1,)

subdivides :: [Int] -> [AbstractMeter] -> [AbstractMeter]
subdivides divs meter = foldr subdivide meter (reverse divs)

subdivide :: Int -> [AbstractMeter] -> [AbstractMeter]
subdivide n = map (Meter.subdivide n)

abstract_length :: AbstractMeter -> Int
abstract_length (D ds) = Num.sum $ map abstract_length ds
abstract_length T = 1

-- * allowed time

{- | Figure out how much time a note at the given position should be allowed
    before it must tie.

    The heuristic is:

    - A binary meter is one whose numerator is a power of 2.

    - First, restrict the maximum 'allowed_time'.  For binary meters, this is
    up to the rank of the start point - 2, which means that if you start on
    an 8th note, you can span until the next half note.  Compound meters are
    different because the rank divisions don't correspond to binary note
    divisions (e.g. 3/4 goes dotted half, quarter, 8th, etc., instead of 4/4's
    whole, half, quarter, etc.).  So they only go up to the rank-1.  This is
    winds up being too restrictive though, because it means that e.g. you could
    never span a quarter note if you start on an eighth, but 8 4 8 4 is
    a perfectly readable 3/4 bar.  So the allowed duration is extended to
    twice the duration to the next rank-1 crossing, which allows 8 8~8 to
    become 8 4.

    - Next, if the allowed duration corresponds exactly to possible note
    duration, take that.  This expresses that it's preferable to spell without
    a tie if you can.  If it doesn't correspond to a note duration, then it
    has to be tied, and break on the place where it crosses the lowest rank.

    - Complex meters like 2+3/4 are treated as binary if you are in the binary
    part.  TODO not yet

    See NOTE [tie-breaking heuristic]
-}
allowed_duration :: Bool -> Meter -> Time -> Time -> NoteDuration
allowed_duration use_dot meter start_ dur =
    best_duration . min (start + dur) $ allowed_time meter start
    where
    best_duration end
        | Just ndur <- List.find ((== start+dur) . to_time) candidates = ndur
        | otherwise = fromMaybe (Types.NoteDuration Types.D128 False) $
            Seq.minimum_on (rank_at meter . to_time) candidates
        where
        candidates = takeWhile ((<=end) . to_time) $
            if use_dot then dotted_durs else durs
    durs = reverse $ map (flip NoteDuration False) [D1 .. D128]
    dotted_durs = reverse
        [NoteDuration d dot | d <- [D1 .. D64], dot <- [True, False]]
    start = start_ `mod` measure_time meter
    to_time dur = start + Types.note_dur_to_time dur

-- | See 'allowed_duration'.
allowed_time :: Meter -> Time -> Time
allowed_time meter start = fromMaybe (measure_time meter) $ if is_binary meter
    -- TODO is_binary in this part of the meter
    then at_rank (start_rank - 2)
    else case at_rank (start_rank - 1) of
        Nothing -> Nothing
        Just end -> min (start + (end - start) * 2) <$> at_rank (start_rank - 2)
    where
    at_rank n = find_rank start n meter
    start_rank = rank_at meter start

-- | Find the time of the rank <= the given one.  Rank 0 can never be spanned,
-- so always stop at a rank 0.
find_rank :: Time -> Rank -> Meter -> Maybe Time
find_rank start rank = fmap ((+ (start + 1)) . index_time)
    . Vector.findIndex (<= max 0 rank)
    . Vector.drop (time_index start + 1) . meter_ranks

{-
    NOTE [tie-breaking heuristic]

    Because it's 3/4, h is not a valid rank, because it doesn't divide at
    natural points.  Actually ranks are not note durations, they are metric
    divisions.

    But this should be ok:
    0 w                       | 3/4
    1 q       q       q       |
    2 e   e   e   e   e   e   |
          4------>4------>8-->|

    But if it's longer than 4, then switch to tieing on the low rank:
    0 w                       | 3/4
    1 q       q       q       |
    2 e   e   e   e   e   e   |
          8-->                |
          4------>            |
          8-->4------>        | 4., but 4. is too complicated
          8-->4.--------->    | 2, seems like jumping 2 ranks, 8,4,2.
          8-->2-------------->|

    0 w                       | 3/4
    1 q       q       q       |
    2 e   e   e   e   e   e   |
    3 s s s s s s s s s s s s |
        s>
        8-->
        s>8-->
        s>8-->s>
        s>4------>
        s>4------>s>
        s>8-->4------>

    So maybe what I want is not spanning ranks, but the duration spanning
    ranks.  E.g. I started on an 8th note rank, so I can have up to 4 duration,
    but not beyond.  Or maybe I could go up to rank-1, but add a candidate at
    distance * 2?

    0 w                                       | 5/4
    1 h               h                       |
    2 q       q       q       q       q       |
    3 e   e   e   e   e   e   e   e   e   e   |
    4 s s s s s s s s s s s s s s s s s s s s |

    I think this is like 4/4, if I don't cross rank 1 I should be ok.
    Or treat it as one 2/4, one 3/4?  In that case, I shouldn't do complicated
    things in the second part.

    0 w                       | 6/8
    1 q           q           |
    2 e   e   e   e   e   e   |
          8-->
          4------>
          4------>8-->

    1 q           q           |
    2 e   e   e   e   e   e   |
              8-->8-->

    0 w                       | 6/8
    1 q           q           |
    2 e   e   e   e   e   e   |
    3 s s s s s s s s s s s s |
        s>
        8-->
        s>8-->

    0 w                               | 4/4
    1 h               h               |
    2 q       q       q       q       |
    3 e   e   e   e   e   e   e   e   |
          8-->
          4------>
          8-->8.-->
          4.--------->
          4.--------->8-->            | could be 2, but crosses h
          4.--------->4------>        | so 3 to 1

    But

    1 h               h               | 16
    2 q       q       q       q       | 8
    3 e   e   e   e   e   e   e   e   | 4
          4------>4------>4------>
          4------>8-->8-->4------>
              2-------------->

    So for binary, go up to rank-2.  For compound, go up to rank-1, but allow
    a complete note as long as its duration is rank-1.
    Or maybe I could say I don't like 8~8

    2 q       q       q       q       | 8
    3 e   e   e   e   e   e   e   e   | 4
    4 s s s s s s s s s s s s s s s s | 2
        s>
        8-->
        8.--->
        8.--->s>
        8.--->8-->
        8.--->8.--->
        8.--->4------>

    4/4
    0 w                               | 32
    1 h               h               | 16
    2 q       q       q       q       | 8
    3 e   e   e   e   e   e   e   e   | 4
    4 s s s s s s s s s s s s s s s s | 2
    5 33333333333333333333333333333333| 1
          |------>
          |-------------------------->

    3/4
    0 w                       |
    1 h               h       |
    2 q       q       q       |
    3 e   e   e   e   e   e   |
    4 s s s s s s s s s s s s |
    5 333333333333333333333333|
      2.--------------------->  0->0
          8->                   3->2
             2--------------->  2->0
                 8-->           3->2
                     4------->  2->0
                         8--->  3->end
-}
