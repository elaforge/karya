-- | Support for rhythmic spelling in different meters.
module Perform.Lilypond.Meter where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed ((!))

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Cmd.Meter as Meter
import Cmd.Meter (AbstractMeter(..))
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Duration(..))


data Meter = Meter {
    -- | NonEmpty list of numerators.  E.g. [2, 3] indicates 2+3.
    meter_nums :: ![Int]
    , meter_denom :: !Duration
    , meter_ranks :: !Ranks
    } deriving (Eq, Show)

time_num :: Meter -> Int
time_num = sum . meter_nums

rank_at :: Meter -> Types.Time -> Int
rank_at meter t = v ! (fromIntegral t `mod` Vector.length v)
    where v = meter_ranks meter

-- | Find the time of the rank <= the given one.  Rank 0 can never be spanned,
-- so always stop at a rank 0.
find_rank :: Types.Time -> Rank -> Meter -> Maybe Types.Time
find_rank start rank = fmap ((+ (start + 1)) . Types.Time)
    . Vector.findIndex (<= max 0 rank)
    . Vector.drop (fromIntegral start + 1) . meter_ranks

instance Pretty.Pretty Meter where pretty = Types.to_lily
instance Types.ToLily Meter where
    to_lily (Meter nums denom _) =
        show (sum nums) ++ "/" ++ Types.to_lily denom

is_duple :: Meter -> Bool
is_duple meter = case meter_nums meter of
    [num] -> (==0) $ snd $ properFraction $ logBase 2 (fromIntegral num)
    _ -> False

-- | Duration of a measure, in Time.
measure_time :: Meter -> Types.Time
measure_time meter =
    Types.Time (time_num meter) * Types.dur_to_time (meter_denom meter)

unparse_meter :: Meter -> String
unparse_meter meter = Seq.join "+" (map show (meter_nums meter))
    ++ "/" ++ Types.to_lily (meter_denom meter)

parse_meter :: String -> Either String Meter
parse_meter s = case Map.lookup s meter_map of
    Nothing -> Left $ "can't parse " ++ show s ++ ", should be in "
        ++ Seq.join ", " (Map.keys meter_map)
    Just meter -> Right meter

default_meter :: Meter
Right default_meter = parse_meter "4/4"

type Rank = Int
type Ranks = Vector.Vector Rank

-- D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
-- 0    1    2    3    4     5     6     7

meter_map :: Map.Map String Meter
meter_map = Map.fromList $ Seq.key_on unparse_meter $ map make
    [ ([4], D4, [T])
    , ([2], D4, [T])
    , ([3], D4, [D [T, T, T]])
    , ([3, 2], D4, [D [T, T, T], D [T, T]])
    , ([2, 3], D4, [D [T, T], D [T, T, T]])

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
