-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support for rhythmic spelling in different meters.
module Perform.Lilypond.Meter where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed ((!))

import qualified Util.Seq as Seq
import qualified Cmd.Ruler.Meter as Meter
import Cmd.Ruler.Meter (AbstractMeter(..))
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Time(..), Duration(..), NoteDuration(..))
import Global


data Meter = Meter {
    -- | NonEmpty list of numerators.  E.g. [2, 3] indicates 2+3.
    meter_nums :: ![Int]
    , meter_denom :: !Duration
    , meter_ranks :: !Ranks
    } deriving (Eq, Show)

time_num :: Meter -> Int
time_num = sum . meter_nums

rank_at :: Meter -> Time -> Int
rank_at meter t = v ! (fromIntegral t `mod` Vector.length v)
    where v = meter_ranks meter

-- | Find the time of the rank <= the given one.  Rank 0 can never be spanned,
-- so always stop at a rank 0.
find_rank :: Time -> Rank -> Meter -> Maybe Time
find_rank start rank = fmap ((+ (start + 1)) . Time)
    . Vector.findIndex (<= max 0 rank)
    . Vector.drop (fromIntegral start + 1) . meter_ranks

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
    Time (time_num meter) * Types.dur_to_time (meter_denom meter)

unparse_meter :: Meter -> Text
unparse_meter meter = Text.intercalate "+" (map showt (meter_nums meter))
    <> "/" <> Types.to_lily (meter_denom meter)

parse_meter :: Text -> Either Text Meter
parse_meter s = case Map.lookup s meter_map of
    Nothing -> Left $ "can't parse " <> showt s <> ", should be in "
        <> Text.intercalate ", " (Map.keys meter_map)
    Just meter -> Right meter

default_meter :: Meter
Right default_meter = parse_meter "4/4"

type Rank = Int
type Ranks = Vector.Vector Rank

-- D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
-- 0    1    2    3    4     5     6     7

meter_map :: Map Text Meter
meter_map = Map.fromList $ Seq.key_on unparse_meter $ map make
    [ ([1], D4, [T])
    , ([2], D4, [T])
    , ([3], D4, [D [T, T, T]])
    , ([4], D4, [T])
    , ([3, 2], D4, [D [T, T, T], D [T, T]])
    , ([2, 3], D4, [D [T, T], D [T, T, T]])
    , ([6], D4, [D [D [T, T, T], D [T, T, T]]])

    , ([3, 3], D8, [D [D [T, T, T], D [T, T, T]]])
    , ([2, 2, 2], D8, [D [D [T, T], D [T, T], D [T, T]]])
    ]
    where
    make (nums, denom, meters) = make_meter nums denom meters

make_meter :: [Int] -> Duration -> [AbstractMeter] -> Meter
make_meter nums denom meters = Meter nums denom vector
    where
    vector
        | frac /= 0 = error $ "can't fit " ++ show ranks ++ " into "
            ++ show expected ++ " by doubling"
        | otherwise = to_vector $ subdivides (replicate exp 2) meters
    (exp, frac) = properFraction $
        logBase 2 (fromIntegral expected / fromIntegral ranks)
    expected = sum nums * fromIntegral (Types.dur_to_time denom)
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
convert_duration :: Meter -> Bool -> Bool -> Time -> Time -> [NoteDuration]
convert_duration meter use_dot_ is_rest = go
    where
    -- Dotted rests are always allowed for triple meters.
    use_dot = use_dot_ || (is_rest && not (is_duple meter))
    go pos time_dur
        | time_dur <= 0 = []
        | allowed >= time_dur = to_durs time_dur
        | otherwise = dur : go (pos + allowed) (time_dur - allowed)
        where
        dur = Types.time_to_note_dur allowed
        allowed = (if is_rest then allowed_time_best else allowed_time_greedy)
            use_dot meter pos
        to_durs = if use_dot then Types.time_to_note_durs
            else map (flip NoteDuration False) . Types.time_to_durs

-- | Figure out how much time a note at the given position should be allowed
-- before it must tie.
allowed_time_greedy :: Bool -> Meter -> Time -> Time
allowed_time_greedy use_dot meter start_ =
    convert $ subtract start $ allowed_time meter start
    where
    start = start_ `mod` measure_time meter
    convert = if use_dot then Types.note_dur_to_time . Types.time_to_note_dur
        else Types.dur_to_time . fst . Types.time_to_dur

-- | The algorithm for note durations is greedy, in that it will seek to find
-- the longest note that doesn't span a beat whose rank is too low.  But that
-- results in rests being spelled @c4 r2 r4@ instead of @c4 r4 r2@.  Unlike
-- notes, all rests are the same.  So rests will pick the duration that ends on
-- the lowest rank.
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
    fromMaybe measure $ find_rank start
        (rank - if is_duple meter then 2 else 1) meter
    where
    rank = rank_at meter start
    measure = measure_time meter
